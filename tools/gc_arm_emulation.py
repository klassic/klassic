#!/usr/bin/env python3
"""Execute emitted Darwin ARM64 helpers with a deterministic pthread API model.

Requires optional unicorn==2.1.4; no dependency of the compiler or Cargo tests.
Export with KLASSIC_GC_MACOS_WORKER_DIR=DIR cargo test -p klassic-native
gc_macos_worker_fixture, then run this script with DIR as its argument.

This tests ARM instructions and injected failures, NOT Darwin, actual pthread
scheduling, hardware memory ordering, or GC concurrency. The generated image
also has a separate native two-worker entry point for macOS ARM64 execution.
"""
import argparse
import collections
import struct
from pathlib import Path

from unicorn import Uc, UC_ARCH_ARM64, UC_MODE_ARM, UC_HOOK_CODE, UC_HOOK_MEM_READ
from unicorn import arm64_const as r


X = [getattr(r, f"UC_ARM64_REG_X{i}") for i in range(31)]
MASK = (1 << 64) - 1
MEMORY, STACK, STUBS = 0x200000000, 0x300000000, 0x400000000
STOP, CALLBACK = STUBS + 0xF00, STUBS + 0xF04
EVENT, LAUNCH, OUT, CONTEXT = (MEMORY + n for n in (0, 0x100, 0x200, 0x300))


def unpack(fmt, data, offset):
    return struct.unpack_from('<' + fmt, data, offset)


class Machine:
    def __init__(self, directory):
        self.uc = Uc(UC_ARCH_ARM64, UC_MODE_ARM)
        self.uc.mem_map(MEMORY, 0x10000)
        self.uc.mem_map(STACK, 0x10000)
        self.uc.mem_map(STUBS, 0x1000)
        self.uc.mem_write(STUBS, struct.pack('<I', 0xD65F03C0) * 1024)  # ret
        data = (directory / 'worker.macho').read_bytes()
        assert unpack('I', data, 0)[0] == 0xFEEDFACF
        assert unpack('I', data, 4)[0] == 0x100000C  # arm64
        segments, bindings, entry = [], None, None
        cursor = 32
        for _ in range(unpack('I', data, 16)[0]):
            cmd, length = unpack('II', data, cursor)
            assert length >= 8 and cursor + length <= len(data)
            if cmd == 0x19:
                name = data[cursor + 8:cursor + 24].rstrip(b'\0')
                va, vs, fo, fs = unpack('QQQQ', data, cursor + 24)
                segments.append((va, vs, fo, fs))
                if name != b'__PAGEZERO':
                    self.uc.mem_map(va, (vs + 4095) & ~4095)
                    self.uc.mem_write(va, data[fo:fo + fs])
                if name == b'__TEXT':
                    text = va
            elif cmd == 0x80000022:
                offset, size = unpack('II', data, cursor + 16)
                bindings = data[offset:offset + size]
            elif cmd == 0x80000028:
                entry = unpack('Q', data, cursor + 8)[0]
            cursor += length
        assert bindings and entry is not None
        self.functions = {name: text + entry + int(offset) for name, offset in
                          (line.split('\t') for line in
                           (directory / 'functions.tsv').read_text().splitlines())}
        self.context_layout = {name: int(offset) for name, offset in
                               (line.split('\t') for line in
                                (directory / 'context-layout.tsv').read_text().splitlines())}
        self.apis = {CALLBACK: 'callback'}
        pos, symbol, segment, offset = 0, None, None, 0
        while pos < len(bindings):
            op = bindings[pos]
            pos += 1
            kind, imm = op & 0xF0, op & 15
            if op == 0:
                break
            if kind in (0x10, 0x50):  # ordinal 1, pointer type 1
                assert imm == 1
            elif kind == 0x40:
                end = bindings.index(0, pos)
                symbol = bindings[pos:end].decode('ascii')
                pos = end + 1
            elif kind == 0x70:
                segment, offset, shift = imm, 0, 0
                while True:
                    b = bindings[pos]
                    pos += 1
                    offset |= (b & 127) << shift
                    shift += 7
                    if b < 128:
                        break
            elif op == 0x90:
                assert symbol and segment is not None
                address = STUBS + 4 * len(self.apis)
                self.apis[address] = symbol.removeprefix('_')
                self.put(segments[segment][0] + offset, address)
                offset += 8
            else:
                raise AssertionError(f'unknown binding opcode {op:x}')
        assert len(self.apis) == 19
        # Load the exact position-independent payload used by the C allocation
        # harness at a deliberately non-page-aligned address crossing a page.
        metadata = (directory / 'metadata.bin').read_bytes()
        positions = {name: int(offset) for name, offset in
                     (line.split('\t') for line in (directory / 'metadata.tsv').read_text().splitlines())}
        payload = 0x500000f80
        self.uc.mem_map(0x500000000, (0xf80 + len(metadata) + 4095) & ~4095)
        self.uc.mem_write(payload, metadata)
        for name, api in (('map_import', 'mmap'), ('unmap_import', 'munmap')):
            assert positions[name] + 8 <= len(metadata)
            self.put(payload + positions[name], next(address for address, symbol in self.apis.items() if symbol == api))
        self.functions['metadata_local_map'] = payload + positions['map']
        self.functions['metadata_local_unmap'] = payload + positions['unmap']
        self.functions['metadata_local_roots_prepare'] = payload + positions['roots_prepare']
        self.functions['metadata_local_roots_dispose'] = payload + positions['roots_dispose']
        self.functions['metadata_local_builtin_registry_prepare'] = payload + positions['builtin_registry_prepare']
        self.functions['metadata_local_startup_prepare'] = payload + positions['startup_prepare']
        self.uc.hook_add(UC_HOOK_CODE, self.hook)
        self.total_calls = 0
        self.reset()

    def put(self, address, value):
        self.uc.mem_write(address, struct.pack('<Q', value & MASK))

    def get(self, address):
        return unpack('Q', bytes(self.uc.mem_read(address, 8)), 0)[0]

    def reset(self):
        self.uc.mem_write(MEMORY, bytes(0x10000))
        self.failures = {}
        self.calls = collections.Counter()
        self.mutexes, self.conditions, self.locked = set(), set(), set()
        self.tls = {}
        self.keys = set()
        self.created = []
        self.metadata_mapped = set()
        self.payload_range = None
        self.payload_protected = self.payload_discarded = False
        self.callback_result = 0x1234567887654321
        self.callback_calls = 0
        self.spurious = 0
        self.waits = 0
        self.error = None

    def fail(self, api, *returns):
        self.failures['pthread_' + api] = list(returns)

    def hook(self, uc, address, size, user_data):
        if address not in self.apis:
            return
        try:
            self.api(self.apis[address])
        except Exception as error:
            self.error = error
            uc.emu_stop()

    def api(self, api):
        assert self.uc.reg_read(r.UC_ARM64_REG_SP) % 16 == 0
        args = [self.uc.reg_read(reg) for reg in X[:4]]
        a, b, c, d = args
        self.calls[api] += 1
        returns = self.failures.get(api, [])
        error = returns.pop(0) if returns else 0
        result = error
        if error and api == 'pthread_create':
            self.put(a, 0xBAD)  # output is unspecified on failure
        if api in ('mprotect', 'madvise'):
            assert (a, b) == self.payload_range
            assert c == (0 if api == 'mprotect' else 5)
        if not error:
            if api == 'mprotect':
                self.uc.mem_protect(a, b, 0)
                self.payload_protected = True
            elif api == 'madvise':
                assert self.payload_protected
                self.payload_discarded = True
            elif api == 'mmap':
                assert (a, b, c, d) == (0, 16384, 3, 0x1002)
                assert self.uc.reg_read(X[4]) & 0xffffffff == 0xffffffff
                assert self.uc.reg_read(X[5]) == 0
                result = next(p for p in (MEMORY+0x8000, MEMORY+0xc000) if p not in self.metadata_mapped)
                self.uc.mem_write(result, bytes(16384))
                self.metadata_mapped.add(result)
            elif api == 'munmap':
                assert a in self.metadata_mapped and b == 16384
                self.metadata_mapped.remove(a)
            elif api == 'pthread_create':
                assert b == 0 and c == self.functions['thunk'] and d == LAUNCH
                self.put(a, 0x12340000)
                self.created.append((c, d))
            elif api == 'pthread_join':
                assert a == 0x12340000
                self.put(b, self.callback_result)
            elif api == 'pthread_key_create':
                assert b == 0
                self.keys.add(0x123456789)
                self.put(a, 0x123456789)
            elif api == 'pthread_key_delete':
                assert a in self.keys
                self.keys.remove(a)
                self.tls.pop(a, None)
            elif api == 'pthread_setspecific':
                assert a in self.keys
                self.tls[a] = b
            elif api == 'pthread_getspecific':
                assert a in self.keys
                result = self.tls.get(a, 0)
            elif api == 'pthread_mutex_init':
                assert b == 0 and a not in self.mutexes
                self.mutexes.add(a)
            elif api == 'pthread_cond_init':
                assert b == 0 and a not in self.conditions
                self.conditions.add(a)
            elif api == 'pthread_mutex_destroy':
                assert a in self.mutexes and a not in self.locked
                self.mutexes.remove(a)
            elif api == 'pthread_cond_destroy':
                assert a in self.conditions
                self.conditions.remove(a)
            elif api == 'pthread_mutex_lock':
                assert a in self.mutexes and a not in self.locked
                self.locked.add(a)
            elif api == 'pthread_mutex_unlock':
                assert a in self.locked
                self.locked.remove(a)
            elif api == 'pthread_cond_signal':
                assert a in self.conditions and a - 64 in self.locked
                assert self.get(a + 48) == 1
            elif api == 'pthread_cond_wait':
                assert a in self.conditions and b in self.locked and a == b + 64
                assert self.get(b + 112) == 0
                # Models release/reacquire with no other actor inspecting the
                # object. First returns may be spurious; eventually a notifier
                # publishes the protected predicate before reacquisition.
                self.waits += 1
                if self.waits > self.spurious:
                    self.put(b + 112, 1)
            elif api == 'callback':
                assert a == CONTEXT and self.tls[self.get(LAUNCH + 16)] == a
                self.callback_calls += 1
                result = self.callback_result
            else:
                raise AssertionError(api)
        # Native calls may destroy volatile registers and return an int with an
        # unspecified high half. Pointers must retain all 64 bits.
        for i in range(1, 18):
            self.uc.reg_write(X[i], 0xBAADF000 + i)
        if api not in ('pthread_getspecific', 'callback', 'mmap'):
            result |= 0xBAD0BAD0 << 32
        self.uc.reg_write(X[0], result)

    def call(self, function, *arguments):
        assert not self.error
        sp = STACK + 0xF000
        self.uc.reg_write(r.UC_ARM64_REG_SP, sp)
        self.uc.reg_write(X[30], STOP)
        saved = {X[i]: 0xCA110000 + i for i in range(19, 30)}
        saved[r.UC_ARM64_REG_X18] = 0x18181818  # Darwin's reserved register
        for reg, value in saved.items():
            self.uc.reg_write(reg, value)
        for reg, value in zip(X, arguments):
            self.uc.reg_write(reg, value)
        # Metadata admission performs two bounded passes over up to 256 cells.
        limit = 100000 if function == "metadata_grow" else 20000
        self.uc.emu_start(self.functions[function], STOP, count=limit)
        if self.error:
            raise self.error
        assert self.uc.reg_read(r.UC_ARM64_REG_PC) == STOP, 'instruction limit'
        assert self.uc.reg_read(r.UC_ARM64_REG_SP) == sp
        for reg, value in saved.items():
            assert self.uc.reg_read(reg) == value, f'callee-saved register {reg}'
        self.total_calls += 1
        return self.uc.reg_read(X[0])


def verify(directory, handshake_vectors=None, tlab_vectors=None, pool_vectors=None, binding_vectors=None, descriptor_vectors=None, scan_vectors=None, root_vectors=None, root_scan_vectors=None, root_drain_vectors=None, root_life_vectors=None, root_clear_vectors=None, root_leased_vectors=None, root_walk_vectors=None, root_walk_scan_vectors=None, detach_vectors=None, buffer_step_vectors=None, buffer_drain_vectors=None, metadata_vectors=None, mark_vectors=None, mark_scan_vectors=None, mark_spill_vectors=None, budget_vectors=None, reference_lease_vectors=None, forward_vectors=None, source_access_vectors=None, forward_lookup_vectors=None, resolve_vectors=None, copy_vectors=None, copy_publish_vectors=None, target_vectors=None, page_release_vectors=None, store_vectors=None, load_vectors=None, root_follow_vectors=None, mark_prepare_vectors=None, mark_retarget_vectors=None, worker_vectors=None, root_journal_vectors=None, buffer_job_vectors=None, root_audit_vectors=None, mark_end_vectors=None, mark_phase_vectors=None, cycle_vectors=None, remembered_vectors=None, remembered_inventory_vectors=None, remembered_load_vectors=None, mark_route_vectors=None, remembered_transfer_vectors=None, remembered_owner_vectors=None, remembered_walk_vectors=None, remembered_lease_vectors=None, root_access_vectors=None, heal_vectors=None, source_drain_vectors=None, source_uncommit_vectors=None, forward_incoming_vectors=None, root_walk_heal_vectors=None, root_walk_item_vectors=None, relocation_gate_vectors=None, relocation_job_vectors=None, relocation_complete_vectors=None, relocation_claim_vectors=None, relocate_discard_vectors=None, relocation_cancel_vectors=None, alloc_into_vectors=None, initialization_vectors=None, alloc_start_vectors=None, initialization_store_vectors=None, initialization_copy_vectors=None, scalar_vectors=None):
    m = Machine(directory)
    verify_metadata_backing(m)
    verify_metadata_backing(m, "metadata_local_")
    verify_metadata_grow(m)
    verify_builtin_prepare(m)
    verify_allocation_shape(m)
    verify_root_cancel_empty(m)
    verify_root_copy(m)
    verify_root_storage_prepare(m)
    verify_root_storage_can_dispose(m)
    verify_native_startup(m)
    verify_native_startup_commit(m)
    verify_native_startup_release(m)
    verify_allocation_sample(m)
    verify_allocation_pressure(m)
    verify_heap_pressure(m)
    verify_cycle_pressure(m)
    verify_cycle_mark_start(m)
    verify_marked_relocation(m)
    verify_relocation_phase(m)
    verify_relocation_retire(m)
    verify_root_retire_audit(m)
    verify_retire_inventory(m)
    verify_source_retire(m)
    verify_logical_cas(m)
    verify_guarded_identity(m)
    verify_guarded_block_size(m)
    verify_survivor_age(m)
    verify_marked_survivor_age(m)
    verify_promotion_remember(m)
    verify_relocation_turn(m)
    verify_forward_compress(m)
    verify_heap_incoming(m)
    verify_heap_walk(m)
    verify_object_walk(m)
    verify_heap_audit_slice(m)
    verify_heap_audit_owner(m)
    verify_root_access_cleanup(m)
    verify_guarded_observe(m)
    verify_rooted_observation_owner(m)
    verify_rooted_scalar_owner(m)
    verify_rooted_mark_owner(m)
    verify_rooted_mark_set(m)
    verify_rooted_mark_publish(m)
    verify_rooted_store_commit(m)
    verify_rooted_store_attempt(m)
    verify_rooted_store(m)
    verify_rooted_store(m,True)
    verify_rooted_load_dispatch(m)
    verify_barrier_assist(m)
    verify_native_root_publish(m)
    verify_pending_follow(m)
    verify_pending_access(m)
    verify_pending_mark(m)
    verify_weak_load(m)
    verify_weak_load(m,True)
    verify_weak_create(m)
    verify_weak_mark(m)
    verify_weak_capture(m)
    verify_weak_access(m)
    verify_weak_finish(m)
    verify_weak_scan(m)
    verify_weak_relocation(m)
    verify_weak_release(m)
    verify_weak_follow(m)
    verify_weak_incoming(m)
    verify_weak_liveness(m)
    verify_pin_registry(m)
    verify_pin_source_release(m)
    verify_root_pin(m)
    verify_root_pin(m, True)
    verify_native_pin(m)
    verify_native_pin_dispose(m)
    verify_native_pin_release(m)
    verify_native_pin_close(m)
    verify_native_pin_close(m, True)
    verify_native_pin_wait(m)
    verify_native_source_uncommit(m)
    verify_heap_decommit(m)
    verify_root_finish_notify(m)
    verify_rooted_load(m)
    verify_rooted_load(m,True)
    verify_rooted_load_admit(m)
    verify_mark_scan_commit(m)
    verify_native_startup_prepare(m, 'native_startup_prepare', 'native_roots_dispose', 'metadata_unmap')
    verify_native_startup_prepare(m, 'metadata_local_startup_prepare', 'metadata_local_roots_dispose', 'metadata_local_unmap')
    verify_native_builtin_registry(m, "native_builtin_registry_prepare", "metadata_unmap")
    verify_native_builtin_registry(m, "metadata_local_builtin_registry_prepare", "metadata_local_unmap")
    verify_native_roots_prepare(m, "native_roots_prepare", "native_roots_dispose")
    verify_native_roots_prepare(m, "metadata_local_roots_prepare", "metadata_local_roots_dispose")
    scenarios = 0

    def clean():
        nonlocal scenarios
        m.reset()
        scenarios += 1

    # Initializing/destroying an event, duplicate init, repeated destroy, and
    # operations before init; reject misuse before calling any pthread API.
    clean()
    for function in ('park', 'wake'):
        assert m.call(function, EVENT) == 22 and not m.calls
    assert m.call('event_destroy', EVENT) == 0 and not m.calls
    assert m.call('event_init', EVENT) == 0 and m.get(EVENT + 120) == 3
    calls = m.calls.copy()
    assert m.call('event_init', EVENT) == 22 and m.calls == calls
    assert m.call('event_destroy', EVENT) == 0 and m.get(EVENT + 120) == 0
    calls = m.calls.copy()
    assert m.call('event_destroy', EVENT) == 0 and m.calls == calls
    assert not m.mutexes and not m.conditions

    for api, cleanup_error, expected_mask in [
        ('mutex_init', 0, 0), ('cond_init', 0, 0), ('cond_init', 16, 1)
    ]:
        clean()
        m.fail(api, 12)
        if cleanup_error:
            m.fail('mutex_destroy', cleanup_error)
        assert m.call('event_init', EVENT) == (cleanup_error or 12)
        assert m.get(EVENT + 120) == expected_mask
        assert bool(m.mutexes) == bool(expected_mask)
        assert not m.conditions
        assert m.call('event_destroy', EVENT) == 0
        assert not m.mutexes and not m.conditions

    for api, remaining in [('cond_destroy', 3), ('mutex_destroy', 1)]:
        clean()
        assert m.call('event_init', EVENT) == 0
        m.fail(api, 16)
        assert m.call('event_destroy', EVENT) == 16
        assert m.get(EVENT + 120) == remaining
        assert m.call('event_destroy', EVENT) == 0
        assert not m.mutexes and not m.conditions

    clean()
    assert m.call('event_init', EVENT) == 0
    for _ in range(32):
        assert m.call('wake', EVENT) == 0
        assert m.call('wake', EVENT) == 0  # coalesces, no semaphore count
        assert m.call('park', EVENT) == 0
        assert m.get(EVENT + 112) == 0 and m.waits == 0
    m.spurious = 3
    assert m.call('park', EVENT) == 0 and m.waits == 4
    assert not m.locked and m.get(EVENT + 112) == 0
    assert m.call('event_destroy', EVENT) == 0

    for function, failing in [
        ('park', 'mutex_lock'), ('wake', 'mutex_lock'),
        ('park', 'cond_wait'), ('wake', 'cond_signal'),
        ('park', 'mutex_unlock'), ('wake', 'mutex_unlock')
    ]:
        clean()
        assert m.call('event_init', EVENT) == 0
        m.fail(failing, 22)
        assert m.call(function, EVENT) == 22
        assert m.get(EVENT + 120) == 3  # never silently releases owned resources
        assert bool(m.locked) == (failing == 'mutex_unlock')
        if failing == 'mutex_lock':
            assert not m.calls['pthread_mutex_unlock']

    for function, failing in [('park', 'cond_wait'), ('wake', 'cond_signal')]:
        clean()
        assert m.call('event_init', EVENT) == 0
        m.fail(failing, 22)
        m.fail('mutex_unlock', 16)
        assert m.call(function, EVENT) == 22  # retain the first failure

    for failure in (0, 11):
        clean()
        m.fail('create', failure)
        assert m.call('start', LAUNCH, OUT) == failure
        assert m.get(OUT) == (0 if failure else 0x12340000)
        assert len(m.created) == (0 if failure else 1)

    for setup, teardown in [(0, 0), (22, 0), (0, 12)]:
        clean()
        key = 0x123456789  # keys are unsigned long, not truncated to 32 bits
        m.keys.add(key)
        for offset, value in [(0, CALLBACK), (8, CONTEXT), (16, key)]:
            m.put(LAUNCH + offset, value)
        m.fail('setspecific', setup, teardown)
        assert m.call('thunk', LAUNCH) == (setup or teardown or m.callback_result)
        assert m.callback_calls == (0 if setup else 1)
        assert m.tls.get(key, 0) == (CONTEXT if teardown else 0)

    # Execute the fixture's real ARM64 callback and CAS-based fetch-or in both
    # serial orders. This covers the callback code, not concurrent scheduling.
    for order in [(0, 1), (1, 0)]:
        clean()
        key, counter = 0x123456789, MEMORY + 0x400
        m.keys.add(key)
        previous = 0
        for i in order:
            event, context = EVENT + i * 128, CONTEXT + i * 64
            assert m.call('event_init', event) == 0
            assert m.call('wake', event) == 0
            for offset, value in [(0, event), (8, key), (16, counter), (24, 1 << i)]:
                m.put(context + offset, value)
            for offset, value in [(0, m.functions['entry']), (8, context), (16, key)]:
                m.put(LAUNCH + offset, value)
            assert m.call('thunk', LAUNCH) == 0
            assert m.get(context + 32) == context
            assert m.get(context + 40) == previous
            assert m.get(context + 48) % 16 == 0
            previous |= 1 << i
            assert m.get(counter) == previous and m.tls[key] == 0
            assert m.call('event_destroy', event) == 0
        assert m.get(counter) == 3

    clean()
    assert m.call('key_create', OUT, 0) == 0
    key = m.get(OUT)
    assert m.call('tls_set', key, CONTEXT) == 0
    assert m.call('tls_get', key) == CONTEXT
    assert m.call('key_delete', key) == 0 and not m.keys
    assert m.call('join', 0x12340000, OUT) == 0
    assert m.get(OUT) == m.callback_result
    for function, api, args in [
        ('key_create', 'key_create', (OUT, 0)),
        ('key_delete', 'key_delete', (key,)),
        ('tls_set', 'setspecific', (key, CONTEXT)),
        ('join', 'join', (0x12340000, OUT))
    ]:
        clean()
        m.put(OUT, 0xABCD)
        m.fail(api, 22)
        assert m.call(function, *args) == 22 and m.get(OUT) == 0xABCD
    print(f'PASS: {scenarios} API-model scenarios, {m.total_calls} emitted ARM64 helper calls')
    verify_context_atomics(m)
    if handshake_vectors:
        verify_handshake_vectors(m, handshake_vectors)
    if tlab_vectors:
        verify_tlab_vectors(m, tlab_vectors)
    if worker_vectors:
        verify_memory_steps(m, worker_vectors, b'KLWORK01', 'worker_step', 2101)
    if mark_retarget_vectors:
        verify_memory_steps(m, mark_retarget_vectors, b'KLMRTG01', 'mark_retarget', 587)
    if initialization_vectors:
        verify_memory_steps(m, initialization_vectors, b'KLINIT01', 'initialization', 536)
    if mark_prepare_vectors:
        verify_memory_steps(m, mark_prepare_vectors, b'KLMPRP01', 'mark_prepare', 628)
    if remembered_lease_vectors:
        verify_memory_steps(m, remembered_lease_vectors, b'KLRMLS01', 'remembered_lease', 842)
    if remembered_walk_vectors:
        verify_memory_steps(m, remembered_walk_vectors, b'KLRMWK01', 'remembered_walk', 731)
    if remembered_owner_vectors:
        verify_memory_steps(m, remembered_owner_vectors, b'KLROWN01', 'remembered_owner', 5938)
    if remembered_transfer_vectors:
        verify_memory_steps(m, remembered_transfer_vectors, b'KLREMT01', 'remembered_transfer', 1769)
    if mark_route_vectors:
        verify_memory_steps(m, mark_route_vectors, b'KLMRTE01', 'mark_route', 4987)
    if remembered_load_vectors:
        verify_memory_steps(m, remembered_load_vectors, b'KLRMLD01', 'remembered_load', 2333)
    if remembered_inventory_vectors:
        verify_memory_steps(m, remembered_inventory_vectors, b'KLREMI01', 'remembered_inventory', 1137)
    if remembered_vectors:
        verify_memory_steps(m, remembered_vectors, b'KLREM001', 'remembered_step', 6638)
    if cycle_vectors:
        verify_memory_steps(m, cycle_vectors, b'KLCYCL01', 'cycle_step', 480)
    if mark_phase_vectors:
        verify_memory_steps(m, mark_phase_vectors, b'KLMPHS01', 'mark_phase', 6528)
    if mark_end_vectors:
        verify_memory_steps(m, mark_end_vectors, b'KLMEND01', 'mark_end', 1101)
    if root_audit_vectors:
        verify_memory_steps(m, root_audit_vectors, b'KLRAUD01', 'root_audit', 621)
    if buffer_job_vectors:
        verify_memory_steps(m, buffer_job_vectors, b'KLBJOB01', 'buffer_job', 1263)
    if root_journal_vectors:
        verify_memory_steps(m, root_journal_vectors, b'KLRJNL01', 'root_journal', 930)
    if root_access_vectors:
        verify_memory_steps(m, root_access_vectors, b'KLRACC01', 'root_access', 1298)
    if root_follow_vectors:
        verify_memory_steps(m, root_follow_vectors, b'KLRFOL01', 'root_follow', 1338)
    if heal_vectors:
        verify_memory_steps(m, heal_vectors, b'KLGHEL01', 'guarded_heal', 1563)
    if load_vectors:
        verify_memory_steps(m, load_vectors, b'KLGLOD01', 'guarded_load', 828)
    if store_vectors:
        verify_memory_steps(m, store_vectors, b'KLGSTR01', 'guarded_store', 448)
    if page_release_vectors:
        verify_memory_steps(m, page_release_vectors, b'KLPREL01', 'page_release', 422)
    if target_vectors:
        verify_memory_steps(m, target_vectors, b'KLRTGT01', 'relocate_target', 240)
    if copy_vectors:
        verify_memory_steps(m, copy_vectors, b'KLRCOP01', 'relocate_copy', 4335)
    if copy_publish_vectors:
        verify_memory_steps(m, copy_publish_vectors, b'KLRPUB01', 'relocate_publish', 217)
    if resolve_vectors:
        verify_memory_steps(m, resolve_vectors, b'KLRSLV01', 'resolve_access', 1544)
    if forward_lookup_vectors:
        verify_memory_steps(m, forward_lookup_vectors, b'KLFLKP01', 'forward_lookup', 2329)
    if source_access_vectors:
        verify_memory_steps(m, source_access_vectors, b'KLSACC01', 'source_access', 154)
    if forward_vectors:
        verify_memory_steps(m, forward_vectors, b'KLFWD001', 'forward_step', 1673)
    if reference_lease_vectors:
        verify_memory_steps(m, reference_lease_vectors, b'KLREFL01', 'reference_lease', 1705)
    if budget_vectors:
        verify_memory_steps(m, budget_vectors, b'KLBUDG01', 'budget_step', 214)
    if mark_spill_vectors:
        verify_memory_steps(m, mark_spill_vectors, b'KLMSPL01', 'mark_spill', 959)
    if mark_vectors:
        verify_memory_steps(m, mark_vectors, b'KLMARK01', 'mark_step', 6117)
    if scalar_vectors:
        verify_memory_steps(m, scalar_vectors, b'KLSCAL01', 'guarded_scalar', 575)
    if initialization_copy_vectors:
        verify_memory_steps(m, initialization_copy_vectors, b'KLINCP01', 'initialization_copy', 796)
    if initialization_store_vectors:
        verify_memory_steps(m, initialization_store_vectors, b'KLINST01', 'initialization_store', 1310)
    if alloc_start_vectors:
        verify_memory_steps(m, alloc_start_vectors, b'KLALST01', 'allocation_start', 120)
    if alloc_into_vectors:
        verify_memory_steps(m, alloc_into_vectors, b'KLALIN01', 'alloc_into', 618)
    if relocation_cancel_vectors:
        verify_memory_steps(m, relocation_cancel_vectors, b'KLRCAN01', 'relocation_cancel', 769)
    if relocate_discard_vectors:
        verify_memory_steps(m, relocate_discard_vectors, b'KLRDSC01', 'relocate_discard', 386)
    if relocation_claim_vectors:
        verify_memory_steps(m, relocation_claim_vectors, b'KLRCLM01', 'relocation_claim', 431)
    if relocation_complete_vectors:
        verify_memory_steps(m, relocation_complete_vectors, b'KLRCMP01', 'relocation_complete', 1047)
    if relocation_job_vectors:
        verify_memory_steps(m, relocation_job_vectors, b'KLRJOB01', 'relocation_job', 1134)
    if relocation_gate_vectors:
        verify_memory_steps(m, relocation_gate_vectors, b'KLRGAT01', 'relocation_gate', 593)
    if root_walk_item_vectors:
        verify_memory_steps(m, root_walk_item_vectors, b'KLRWIT01', 'root_walk_item', 2749)
    if root_walk_heal_vectors:
        verify_memory_steps(m, root_walk_heal_vectors, b'KLRWHL01', 'root_walk_heal', 804)
    if forward_incoming_vectors:
        verify_memory_steps(m, forward_incoming_vectors, b'KLFINC01', 'forward_incoming', 134)
    if source_uncommit_vectors:
        verify_memory_steps(m, source_uncommit_vectors, b'KLSUNC01', 'source_uncommit', 154)
    if source_drain_vectors:
        verify_memory_steps(m, source_drain_vectors, b'KLSRCD01', 'source_drain', 112)
    if mark_scan_vectors:
        verify_memory_steps(m, mark_scan_vectors, b'KLMSCN01', 'mark_scan', 1578)
    if metadata_vectors:
        verify_memory_steps(m, metadata_vectors, b'KLMETA01', 'metadata_step', 2166)
    if pool_vectors:
        verify_memory_steps(m, pool_vectors, b'KLPAGE01', 'pool_step', 10496)
    if binding_vectors:
        verify_memory_steps(m, binding_vectors, b'KLBIND01', 'tlab_page', 62)
    if descriptor_vectors:
        verify_memory_steps(m, descriptor_vectors, b'KLDESC01', 'descriptor_register', 2539)
    if scan_vectors:
        verify_memory_steps(m, scan_vectors, b'KLSCAN01', 'descriptor_scan', 12498)
    if root_vectors:
        verify_memory_steps(m, root_vectors, b'KLROOT01', 'root_step', 969)
    if root_scan_vectors:
        verify_memory_steps(m, root_scan_vectors, b'KLRSCN01', 'root_scan', 16)
    if root_drain_vectors:
        verify_memory_steps(m, root_drain_vectors, b'KLRDRN01', 'root_drain', 737)
    if root_life_vectors:
        verify_memory_steps(m, root_life_vectors, b'KLRLIF01', 'root_life', 1261)
    if root_clear_vectors:
        verify_memory_steps(m, root_clear_vectors, b'KLRCLR01', 'root_clear', 150)
    if buffer_step_vectors:
        verify_memory_steps(m, buffer_step_vectors, b'KLBBST01', 'buffer_step', 2340)
    if buffer_drain_vectors:
        verify_memory_steps(m, buffer_drain_vectors, b'KLBBDR01', 'buffer_drain', 1244)
    if detach_vectors:
        verify_memory_steps(m, detach_vectors, b'KLDTCH01', 'detach_step', 1120)
    if root_walk_vectors:
        verify_memory_steps(m, root_walk_vectors, b'KLRWLK01', 'root_walk', 1891)
    if root_walk_scan_vectors:
        verify_memory_steps(m, root_walk_scan_vectors, b'KLRWSC01', 'root_walk_scan', 18335)
    if root_leased_vectors:
        verify_memory_steps(m, root_leased_vectors, b'KLRLSC01', 'root_scan_leased', 384)
    print('Darwin execution, native scheduling and hardware ordering remain unverified here.')


def verify_context_atomics(m):
    m.reset()
    # The upper neighbor of the second word is unmapped: a wide access masked
    # down afterward still faults. The first has canaries on both sides.
    for word in (MEMORY + 4, MEMORY + 0x10000 - 4):
        if word == MEMORY + 4:
            m.put(MEMORY, 0xBAD0BAD0BAD0BAD0)
            m.put(MEMORY + 8, 0x1234567887654321)
        for value in (0, 1, 0xffffffff, 0x80000000, 0x13579ace):
            next_value = (~value) & 0xffffffff
            assert m.call('atomic_store32', word, 0x1234567800000000 | value, 0) == 0
            assert m.call('atomic_load32', word, 0, 0) == value
            assert m.call('atomic_cas32', word, 0xffffffff00000000 | (value ^ 1), next_value) == value
            assert m.call('atomic_load32', word, 0, 0) == value
            assert m.call('atomic_cas32', word, 0xffffffff00000000 | value,
                          0x8765432100000000 | next_value) == value
            assert m.call('atomic_load32', word, 0, 0) == next_value
            if word == MEMORY + 4:
                assert m.get(MEMORY) & 0xffffffff == 0xBAD0BAD0
                assert m.get(MEMORY + 8) == 0x1234567887654321
    size, state, access_epoch = (m.context_layout[k] for k in ('size', 'state', 'access_epoch'))
    assert state == 4 and access_epoch % 8 == 0
    values = (0, 1, 2, 3, 4, 5, MASK, 1 << 32)
    edges = (0x0f, 0x03, 0x01, 0x10, 0)
    cases = 0
    for current in range(6):
        for expected in values:
            for desired in values:
                for abi in (m.context_layout["abi_version"]-1,m.context_layout["abi_version"]):
                    for access in range(2):
                        before = bytearray([0xa5] * size)
                        struct.pack_into('<I', before, 0, abi)
                        struct.pack_into('<I', before, state, current)
                        struct.pack_into('<Q', before, access_epoch, access)
                        struct.pack_into('<Q', before, m.context_layout['private_producers'], 0)
                        m.uc.mem_write(CONTEXT, bytes(before))
                        permitted = expected < 5 and desired < 5 and edges[expected] & (1 << desired)
                        result = (1 if abi != m.context_layout["abi_version"] else 2 if expected != current else
                                  3 if access else 4 if not permitted else 0)
                        assert m.call('context_transition', CONTEXT, expected, desired) == result
                        if result == 0:
                            struct.pack_into('<I', before, state, desired)
                        assert bytes(m.uc.mem_read(CONTEXT, size)) == bytes(before)
                        cases += 1
    assert cases == 1536
    print('PASS: ARM64 exact-width atomic/guard checks and 1536 shared-context transitions')


def verify_handshake_vectors(m, path):
    # Replay the C implementation's complete before/after byte records against
    # emitted ARM64. No reimplementation of the coordinator serves as oracle.
    data = path.read_bytes()
    assert data[:8] == b'KLHST002'
    size, reg_offset, reg_size, pointers = unpack('QQQQ', data, 8)
    assert reg_size == 56 and size < 0x8000 and pointers < 64
    fields = unpack('Q' * pointers, data, 40)
    assert all(field + 8 <= size for field in fields)
    first, record_size = 40 + 8 * pointers, 40 + 2 * size
    assert (len(data) - first) % record_size == 0
    address = MEMORY + 0x2000
    m.reset()
    cases = 0
    for pos in range(first, len(data), record_size):
        op, result, participant, before_mask, after_mask = unpack('QQQQQ', data, pos)
        before = bytearray(data[pos + 40:pos + 40 + size])
        after = bytearray(data[pos + 40 + size:pos + record_size])
        for part, mask in [(before, before_mask), (after, after_mask)]:
            for i, field in enumerate(fields):
                if mask & (1 << i):
                    offset = unpack('Q', part, field)[0]
                    assert offset < size
                    struct.pack_into('<Q', part, field, address + offset)
        m.uc.mem_write(address, bytes(before))
        reg = address + reg_offset + participant * reg_size if participant < 3 else 0
        assert m.call('handshake_step', address, reg, op) == result, cases
        assert bytes(m.uc.mem_read(address, size)) == bytes(after), cases
        cases += 1
    assert cases == 41615
    print(f'PASS: ARM64 replay of {cases} C handshake before/after vectors')


def verify_tlab_vectors(m, path):
    data = path.read_bytes()
    assert data[:8] == b'KLTLAB02'
    size, ctx_offset, request_offset, pointers = unpack('QQQQ', data, 8)
    assert size < 0x10000 and pointers < 64
    fields = unpack('Q' * pointers, data, 40)
    assert all(field + 8 <= size for field in fields)
    geometry = unpack('QQQQQQ', data, 40 + 8 * pointers)
    heap, starts, start, records, record_bytes, heap_bytes = geometry
    first, record_size = 40 + 8 * pointers + 48, 24 + 2 * size
    assert (len(data) - first) % record_size == 0
    address = MEMORY + 0x2000
    m.reset()
    cases = 0
    for pos in range(first, len(data), record_size):
        result, before_mask, after_mask = unpack('QQQ', data, pos)
        before = bytearray(data[pos + 24:pos + 24 + size])
        after = bytearray(data[pos + 24 + size:pos + record_size])
        for part, mask in [(before, before_mask), (after, after_mask)]:
            for i, field in enumerate(fields):
                if mask & (1 << i):
                    offset = unpack('Q', part, field)[0]
                    assert offset <= size
                    struct.pack_into('<Q', part, field, address + offset)
        for part in (before, after):
            for granule in range(512):
                if not (unpack('Q', part, starts + (granule // 64) * 8)[0] & (1 << (granule % 64))):
                    continue
                user = start + granule * 16
                if not 32 <= user < heap_bytes:
                    continue
                field = heap + user - 32 + 16
                offset = unpack('Q', part, field)[0]
                if records <= offset < records + record_bytes:
                    struct.pack_into('<Q', part, field, address + offset)
        m.uc.mem_write(address, bytes(before))
        assert m.call('tlab_allocate', address + ctx_offset, address + request_offset) == result, cases
        assert bytes(m.uc.mem_read(address, size)) == bytes(after), cases
        cases += 1
    assert cases == 2806
    print(f'PASS: ARM64 replay of {cases} C TLAB size/color/metadata/failure vectors')


def verify_native_builtin_registry(m, function, unmap):
    address, mapped = MEMORY+0x2000, MEMORY+0x8000
    for capacity in (5,64,1024):
        m.reset()
        charge = [16384,0x1000,0,1]
        m.uc.mem_write(address,struct.pack('<4Q',*charge))
        assert m.call(function,address,capacity) == 0
        charge[2] = mapped
        assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
        expected = bytearray(16384)
        struct.pack_into('<3Q',expected,0,mapped+216,capacity,1)
        for base,values in ((24,[1,16,0,1,0,0,0,0]),
                            (88,[2,8,0,8,0,mapped+16,0,1]),
                            (152,[4,8,8,8,0,mapped+16,0,1])):
            struct.pack_into('<8Q',expected,base,*values)
        for index,offset in ((1,24),(2,88),(4,152)):
            struct.pack_into('<Q',expected,216+index*8,mapped+offset)
        assert bytes(m.uc.mem_read(mapped,16384)) == bytes(expected)
        assert m.call(function,address,capacity) == 1 and m.calls['mmap'] == 1
        assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
        assert bytes(m.uc.mem_read(mapped,16384)) == bytes(expected)
        assert m.call(unmap,mapped,16384) == 1 and not m.metadata_mapped
    for fault in range(14):
        m.reset()
        charge, capacity, q = [16384,0x1000,0,1],5,address
        if fault == 0: charge[3] = 0
        elif fault == 1: charge[3] = 2
        elif fault == 2: charge[2] = 0x2000
        elif fault == 3: charge[1] = 0
        elif fault == 4: charge[1] += 1
        elif fault == 5: charge[0] = 0
        elif fault == 6: charge[0] += 1
        elif fault == 7: capacity = 0
        elif fault == 8: capacity = 4
        elif fault == 9: capacity = 0x100000001
        elif fault == 10: capacity = 2048
        elif fault == 11: capacity = MASK
        elif fault == 12: q = 0
        else: q += 1
        before = struct.pack('<4Q',*charge)
        m.uc.mem_write(address,before)
        assert m.call(function,q,capacity) == 1 and not m.calls['mmap'], fault
        assert bytes(m.uc.mem_read(address,32)) == before
    m.reset()
    before = struct.pack('<4Q',16384,0x1000,0,1)
    m.uc.mem_write(address,before)
    m.failures['mmap'] = [MASK]
    assert m.call(function,address,5) == 2 and not m.metadata_mapped
    assert bytes(m.uc.mem_read(address,32)) == before
    print(f'PASS: ARM64 {function} three registries, 14 rejections and map failure (OS model)')


def verify_native_roots_prepare(m, function, dispose):
    address, spec_address = MEMORY+0x2000, MEMORY+0x2040
    mapped = MEMORY+0x8000
    def install(charge, spec):
        m.uc.mem_write(address,struct.pack('<4Q',*charge))
        m.uc.mem_write(spec_address,struct.pack('<4Q',*spec))
    for cells in (1,3,256):
        m.reset()
        charge, spec = [16384,0x1000,0,1], [17,cells,7,16384]
        install(charge,spec)
        assert m.call(function,address,spec_address) == 0
        charge[2] = mapped
        assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
        assert bytes(m.uc.mem_read(spec_address,32)) == struct.pack('<4Q',*spec)
        expected = bytearray(16384)
        for offset,value in ((0,mapped+160),(160,17),(176,mapped+264),(184,cells),
                             (208,mapped+224),(224,mapped+264+cells*32),(232,7)):
            struct.pack_into('<Q',expected,offset,value)
        assert bytes(m.uc.mem_read(mapped,16384)) == bytes(expected)
        assert m.call(function,address,spec_address) == 1 and m.calls['mmap'] == 1
        assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
        assert bytes(m.uc.mem_read(mapped,16384)) == bytes(expected)
        for fault in range(11):
            bad = charge.copy()
            q = address
            if fault == 0: bad[3] = 0
            elif fault == 1: bad[3] = 3
            elif fault == 2: bad[1] = 0
            elif fault == 3: bad[1] += 1
            elif fault == 4: bad[0] = 0
            elif fault == 5: bad[0] += 1
            elif fault == 6: bad[2] = 0
            elif fault == 7: bad[2] += 1
            elif fault == 8: m.put(mapped+216,1)
            elif fault == 9: q = 0
            else: q += 1
            install(bad,spec)
            assert m.call(dispose,q) == 1 and not m.calls['munmap'], fault
            assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*bad)
            m.put(mapped+216,0)
        charge[3] = 2 if cells == 3 else 1
        install(charge,spec)
        m.failures['munmap'] = [1]
        assert m.call(dispose,address) == 2 and m.metadata_mapped
        assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
        assert bytes(m.uc.mem_read(mapped,16384)) == bytes(expected)
        assert m.call(dispose,address) == 0 and not m.metadata_mapped
        charge[2] = 0
        assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
        assert m.call(dispose,address) == 1 and m.calls['munmap'] == 2
    for fault in range(18):
        m.reset()
        charge, spec = [16384,0x1000,0,1], [17,3,2,16384]
        q, args = address, spec_address
        if fault == 0: charge[3] = 0
        elif fault == 1: charge[3] = 2
        elif fault == 2: charge[2] = 0x3000
        elif fault == 3: charge[1] = 0
        elif fault == 4: charge[1] += 1
        elif fault == 5: charge[0] = spec[3] = 0
        elif fault == 6: charge[0] += 1; spec[3] += 1
        elif fault == 7: spec[3] += 1
        elif fault == 8: spec[0] = 0
        elif fault == 9: spec[1] = 0
        elif fault == 10: spec[2] = 0
        elif fault == 11: spec[1] = 0x100000000
        elif fault == 12: spec[2] = 0x100000000
        elif fault == 13: spec[1] = 513
        elif fault == 14: q = 0
        elif fault == 15: args = 0
        elif fault == 16: q += 1
        else: args += 1
        install(charge,spec)
        assert m.call(function,q,args) == 1 and not m.calls['mmap'], fault
        assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
        assert bytes(m.uc.mem_read(spec_address,32)) == struct.pack('<4Q',*spec)
    m.reset()
    charge, spec = [16384,0x1000,0,1], [17,3,2,16384]
    install(charge,spec)
    m.failures['mmap'] = [MASK]
    assert m.call(function,address,spec_address) == 2 and m.calls['mmap'] == 1
    assert not m.metadata_mapped
    assert bytes(m.uc.mem_read(address,32)) == struct.pack('<4Q',*charge)
    assert bytes(m.uc.mem_read(spec_address,32)) == struct.pack('<4Q',*spec)
    print(f'PASS: ARM64 {function} three mappings, 18 rejected inputs and map failure (OS model)')


def verify_root_storage_can_dispose(m):
    m.reset()
    address = MEMORY + 0x2000
    base = address + 8
    def fixture():
        data = bytearray(280)
        data[:8] = data[-8:] = bytes([0xa5])*8
        return data
    count = 0
    for state in range(3):
        before = fixture()
        if state: struct.pack_into('<Q',before,8,base+160)
        if state == 2:
            for offset in (240,248,256): struct.pack_into('<Q',before,8+offset,MASK)
        m.uc.mem_write(address,bytes(before))
        assert m.call('root_storage_can_dispose',base) == 0
        assert bytes(m.uc.mem_read(address,len(before))) == bytes(before)
        count += 1
    for offset in list(range(0,160,8)) + [168,216,192,200,240,248,256]:
        before = fixture()
        struct.pack_into('<Q',before,8+offset,1)
        m.uc.mem_write(address,bytes(before))
        assert m.call('root_storage_can_dispose',base) == 1
        assert bytes(m.uc.mem_read(address,len(before))) == bytes(before)
        count += 1
    for argument in (0,base+1):
        before = fixture()
        m.uc.mem_write(address,bytes(before))
        assert m.call('root_storage_can_dispose',argument) == 1
        assert bytes(m.uc.mem_read(address,len(before))) == bytes(before)
        count += 1
    assert count == 32
    reads = []
    def trace(uc, access, location, size, value, unused):
        if base <= location < base + 264: reads.append((location, size))
    hook = m.uc.hook_add(UC_HOOK_MEM_READ, trace)
    m.uc.ctl_flush_tb()  # Instrument blocks translated before installing the read hook.
    for state in (1, 2, 3, 0):
        before = fixture()
        struct.pack_into('<Q', before, 8 + 200, state)
        m.uc.mem_write(address, bytes(before)); reads.clear()
        assert m.call('root_storage_can_dispose', base) == int(state != 0)
        assert reads[0] == (base + 200, 8)
        if state: assert reads == [(base + 200, 8)]
    m.uc.hook_del(hook)
    m.uc.ctl_flush_tb()
    print('PASS: ARM64 root disposal 32 private/drained/live-prefix cases and state-first completion reads')


def verify_native_startup_prepare(m, function, dispose, unmap):
    address, query = MEMORY+0x2000, MEMORY+0x2040
    root_map, desc_map = MEMORY+0x8000, MEMORY+0xc000
    for cells, entries, capacity in ((1,1,5),(3,2,64),(256,7,1024)):
        for failure in (0,1,2):
            m.reset()
            before = struct.pack('<8Q',16384,0x1000,0,1,16384,0x1000,0,1)
            request = struct.pack('<6Q',address,address+32,cells,entries,capacity,16384)
            m.uc.mem_write(address,before);m.uc.mem_write(query,request)
            if failure: m.failures['mmap'] = [0]*(failure-1)+[MASK]
            assert m.call(function,query,17) == (2 if failure else 0)
            expected = bytearray(before)
            if failure != 1: struct.pack_into('<Q',expected,16,root_map)
            if not failure: struct.pack_into('<Q',expected,48,desc_map)
            assert bytes(m.uc.mem_read(address,64)) == bytes(expected)
            assert bytes(m.uc.mem_read(query,48)) == request
            assert m.calls['mmap'] == (failure if failure else 2)
            if failure != 1:
                roots = bytearray(16384)
                for off,value in ((0,root_map+160),(160,17),(176,root_map+264),(184,cells),
                                  (208,root_map+224),(224,root_map+264+cells*32),(232,entries)):
                    struct.pack_into('<Q',roots,off,value)
                assert bytes(m.uc.mem_read(root_map,16384)) == bytes(roots)
                calls=m.calls['mmap']
                assert m.call(function,query,17) == 1 and m.calls['mmap'] == calls
                m.failures['munmap']=[1]
                assert m.call(dispose,address) == 2
                assert bytes(m.uc.mem_read(address,64)) == bytes(expected)
                assert bytes(m.uc.mem_read(root_map,16384)) == bytes(roots)
                assert m.call(dispose,address) == 0
            if not failure:
                desc = bytearray(16384)
                struct.pack_into('<3Q',desc,0,desc_map+216,capacity,1)
                for base,values in ((24,[1,16,0,1,0,0,0,0]),(88,[2,8,0,8,0,desc_map+16,0,1]),
                                    (152,[4,8,8,8,0,desc_map+16,0,1])):
                    struct.pack_into('<8Q',desc,base,*values)
                for index,off in ((1,24),(2,88),(4,152)):
                    struct.pack_into('<Q',desc,216+index*8,desc_map+off)
                assert bytes(m.uc.mem_read(desc_map,16384)) == bytes(desc)
                assert m.call(unmap,desc_map,16384) == 1
            assert not m.metadata_mapped
    for fault in range(19):
        m.reset()
        charges=[16384,0x1000,0,1,16384,0x1000,0,1]
        q=[address,address+32,3,2,64,16384]; owner=17; arg=query
        if fault==0: owner=0
        elif fault==1: q[2]=0
        elif fault==2: q[3]=0
        elif fault==3: q[4]=4
        elif fault==4: q[5]=4096
        elif fault==5: charges[3]=0
        elif fault==6: charges[7]=2
        elif fault==7: charges[2]=0x1000
        elif fault==8: charges[6]=0x1000
        elif fault==9: charges[5]+=8
        elif fault==10: charges[0]+=16384
        elif fault==11: charges[4]+=16384
        elif fault==12: q[0]=q[1]
        elif fault==13: q[0]+=1
        elif fault==14: q[1]=0
        elif fault==15: arg=0
        elif fault==16: arg+=1
        elif fault==17: charges[1]=charges[5]=0
        else: charges[1]=charges[5]=0x1001
        before=struct.pack('<8Q',*charges)+struct.pack('<6Q',*q)
        m.uc.mem_write(address,before)
        assert m.call(function,arg,owner)==1 and not m.calls['mmap'],fault
        assert bytes(m.uc.mem_read(address,len(before)))==before
    print(f'PASS: ARM64 {function} paired preparation, 19 rejections and map/unmap failure ownership (OS model)')


def verify_native_startup_release(m):
    base=MEMORY+0x2000;roots,descriptors,query=base+128,base+160,base+192
    count=0
    for stages in range(4):
        for fault in range(20):
            m.reset()
            h=[0,0,1,0,0,0,0,0,MASK,112+2048,1024,0,0,0]
            rc=[4096,base,0,1+(stages&1)];dc=[8192,base,0,1+((stages>>1)&1)]
            for c in (rc,dc):h[10 if c[3]==1 else 9]+=c[0]
            q=[roots,descriptors,0,0,0,0];ha,qa=base,query
            if fault==1:rc[2]=0x10000
            elif fault==2:dc[2]=0x20000
            elif fault==3:rc[3]=3
            elif fault==4:dc[3]=0
            elif fault==5:rc[0]=0
            elif fault==6:dc[1]+=8
            elif fault==7:h[10]=0;h[9]=112
            elif fault==8:h[10]=MASK
            elif fault==9:h[2]=0
            elif fault==10:q[0]=q[1]
            elif fault==11:qa=0
            elif fault==12:qa+=1
            elif fault==13:ha=0
            elif fault==14:ha+=1
            elif fault==15:q[1]+=1
            elif fault==16:h[9]=111
            elif fault==17:h[2]=MASK
            elif fault==18:rc[3]=dc[3]=1;rc[0]=dc[0]=4096;h[10]=4096
            elif fault==19:rc[3]=dc[3]=2;rc[0]=dc[0]=4096;h[9]=112+4096
            before=bytearray(512)
            struct.pack_into('<14Q',before,0,*h)
            struct.pack_into('<4Q',before,128,*rc);struct.pack_into('<4Q',before,160,*dc)
            struct.pack_into('<6Q',before,192,*q)
            after=bytearray(before)
            if not fault:
                struct.pack_into('<2Q',after,72,112+2048,1024)
                struct.pack_into('<Q',after,152,3);struct.pack_into('<Q',after,184,3)
            m.uc.mem_write(base,bytes(before))
            assert m.call('native_startup_release',ha,qa)==(1 if fault else 0),(stages,fault)
            assert bytes(m.uc.mem_read(base,len(after)))==bytes(after),(stages,fault)
            if not fault:
                assert m.call('native_startup_release',ha,qa)==1
                assert bytes(m.uc.mem_read(base,len(after)))==bytes(after)
            count+=1
    assert count==80
    print('PASS: ARM64 startup release 80 independent mixed-budget cleanup and rejection cases')


def verify_native_startup_commit(m):
    base=MEMORY+0x2000
    roots,descriptors,query=base+128,base+160,base+192
    for fault in range(35):
        m.reset()
        h=[0,0,1,0,0,0,0,0,16384,4096,12288,0,0,0]
        rc=[4096,base,0x10000,1];dc=[4096,base,0x20000,1]
        q=[roots,descriptors,3,2,64,4096];ha,qa=base,query
        if fault==1:rc[0]=0
        elif fault==2:dc[0]=0
        elif fault==3:rc[1]=0
        elif fault==4:dc[1]+=8
        elif fault==5:rc[3]=0
        elif fault==6:dc[3]=2
        elif fault==7:rc[2]=0
        elif fault==8:dc[2]=0
        elif fault==9:rc[2]+=1
        elif fault==10:dc[2]+=1
        elif fault==11:rc[2]=MASK-7
        elif fault==12:dc[2]=MASK-7
        elif fault==13:h[2]=0
        elif fault==14:h[2]=MASK
        elif fault==15:h[9]=0
        elif fault==16:h[9]=h[8]+1
        elif fault==17:h[10]=h[8]
        elif fault==18:h[10]=4096
        elif fault==19:rc[0]=16384
        elif fault==20:dc[0]=16384
        elif fault==21:q[0]=q[1]
        elif fault==22:q[0]=0
        elif fault==23:q[1]+=1
        elif fault==24:qa=0
        elif fault==25:qa+=1
        elif fault==26:ha=0
        elif fault==27:ha+=1
        elif fault==28:h[8]=MASK;h[9]=MASK-8192;h[10]=8192
        elif fault==29:dc[2]=rc[2]
        elif fault==30:dc[2]=rc[2]+8
        elif fault==31:rc[2]=dc[2]+8
        elif fault==32:dc[2]=rc[2]+rc[0]-8
        elif fault==33:dc[2]=rc[2]+rc[0]
        elif fault==34:rc[2]=dc[2]+dc[0]
        before=bytearray(512)
        struct.pack_into('<14Q',before,0,*h)
        struct.pack_into('<4Q',before,128,*rc);struct.pack_into('<4Q',before,160,*dc)
        struct.pack_into('<6Q',before,192,*q)
        after=bytearray(before);success=fault in (0,28,33,34)
        if success:
            struct.pack_into('<Q',after,72,h[9]+8192)
            struct.pack_into('<Q',after,80,h[10]-8192)
            struct.pack_into('<Q',after,152,2);struct.pack_into('<Q',after,184,2)
        m.uc.mem_write(base,bytes(before))
        assert m.call('native_startup_commit',ha,qa)==(0 if success else 1),fault
        assert bytes(m.uc.mem_read(base,len(after)))==bytes(after),fault
        if success:
            assert m.call('native_startup_commit',ha,qa)==1
            assert bytes(m.uc.mem_read(base,len(after)))==bytes(after)
    print('PASS: ARM64 startup commit 35 full-state paired accounting and rejection cases')


def verify_pin_registry(m):
    """Execute both admission orders; this is instruction emulation, not a race."""
    heap, page, pool, slots, records, registry, lease, permit, token, selection, stale = (
        MEMORY + n for n in range(0x2000, 0x2b00, 0x100))
    movers, source, queue, mark, work, request, mover, access, forward, release = (
        MEMORY + n for n in range(0x2b00, 0x3500, 0x100))
    source_offset = (1 << 17) + 32
    cases = 0
    for generation in (0, 1):
        for state in (1, 5):
            for pin_first in (False, True):
                m.reset()
                m.put(heap + 8, page)
                m.put(heap + 16, 1)
                for offset, value in ((8, 1 << 17), (16, 2 << 17), (24, 9),
                                      (48, generation), (56, state)):
                    m.put(page + offset, value)
                for offset, value in ((0, heap), (8, slots), (16, 2), (24, 1)):
                    m.put(pool + offset, value)
                m.put(slots + 24, 2)
                for offset, value in ((0, pool), (8, records), (16, 1)):
                    m.put(registry + offset, value)
                m.put(lease, page)
                m.put(lease + 8, 9)
                assert m.call('metadata_step', pool, lease, 0) == 0
                assert m.call('pin_registry_bind', registry, lease) == 0
                assert m.get(page + 96) == 2  # metadata_leases
                m.put(token, permit)
                m.put(page + 80, source_offset + 32)
                m.put(movers + 24, 1)
                m.put(source, source_offset)
                m.put(source + 8, 9)
                m.put(queue + 64, 3)
                m.put(queue + 72, generation)
                m.put(mark, 4)
                for offset, value in ((0, mover), (8, source), (16, source_offset),
                                      (24, 9), (48, queue), (56, mark), (64, 3)):
                    m.put(work + offset, value)
                for offset, value in ((0, lease), (8, selection), (16, movers), (24, work)):
                    m.put(request + offset, value)
                if pin_first:
                    assert m.call('pin_registry_pin', registry, lease, token) == 0
                    before = bytes(m.uc.mem_read(records, 64))
                    assert m.call('pin_registry_select', registry, lease, selection) == 2
                    assert bytes(m.uc.mem_read(records, 64)) == before
                    m.put(stale + 16, 1)
                    assert m.call('pin_selection', registry, lease, stale) == 2
                    m.put(request + 8, stale)
                    assert m.call('pin_relocation_claim', registry, request) == 6
                    assert m.get(source + 16) == 0 and m.get(movers + 8) == 0
                    m.put(request + 8, selection)
                    assert m.call('pin_registry_unpin', registry, lease, token) == 0
                    m.put(token + 8, 0)
                    m.put(token + 16, 0)
                assert m.call('pin_registry_select', registry, lease, selection) == 0
                assert m.call('pin_selection', registry, lease, selection) == 0
                before = bytes(m.uc.mem_read(records, 64))
                assert m.call('pin_registry_pin', registry, lease, token) == 2
                assert m.call('pin_registry_unbind', registry, lease) == 2
                assert bytes(m.uc.mem_read(records, 64)) == before
                m.uc.mem_write(stale, bytes(m.uc.mem_read(selection, 24)))
                m.put(stale + 8, 1)
                assert m.call('pin_selection', registry, lease, stale) == 1
                m.put(stale + 8, 0)
                if state == 1:
                    assert m.call('pin_relocation_claim', registry, request) == 1
                m.put(page + 56, 5)
                assert m.call('pin_relocation_claim', registry, request) == 7
                m.put(mark + 8 + generation * 8, 3)
                m.put(queue + 64, 4)
                assert m.call('pin_relocation_claim', registry, request) == 8
                m.put(queue + 64, 3)
                m.put(work + 16, source_offset - 16)
                assert m.call('pin_relocation_claim', registry, request) == 1
                m.put(work + 16, source_offset)
                assert m.get(source + 16) == 0 and m.get(movers + 16) == 0
                assert m.call('pin_relocation_claim', registry, request) == 0
                ticket = m.get(work + 32)
                assert ticket and m.get(source + 16) == (ticket << 2) | 1
                assert m.get(movers + 8) == 1
                for offset, value in ((8, source_offset), (16, 9), (32, ticket)):
                    m.put(access + offset, value)
                assert m.call('source_access', source, access, 2) == 0
                for offset, value in ((0, source_offset), (8, 9), (16, ticket)):
                    m.put(forward + offset, value)
                assert m.call('forward_step', source, forward, 3) == 0
                for offset, value in ((0, mover), (8, ticket), (16, m.get(work + 40))):
                    m.put(release + offset, value)
                assert m.call('relocation_gate', movers, release, 1) == 0
                assert m.get(movers + 8) == 0 and m.get(source + 16) == 3
                m.put(page + 56, 7)  # EVACUATED: cleanup stays legal
                assert m.call('pin_selection', registry, lease, selection) == 0
                assert m.call('pin_registry_reopen', registry, lease, selection) == 0
                assert m.call('pin_selection', registry, lease, stale) == 1
                before = bytes(m.uc.mem_read(records, 64))
                assert m.call('pin_registry_pin', registry, lease, token) == 1
                assert m.call('pin_registry_select', registry, lease, selection) == 1
                assert bytes(m.uc.mem_read(records, 64)) == before
                m.put(page + 56, state)
                assert m.call('pin_registry_pin', registry, lease, token) == 0
                m.put(page + 56, 7)
                assert m.call('pin_registry_unpin', registry, lease, token) == 0
                held = m.get(records + 16)
                m.put(records + 16, 0)
                assert m.call('pin_registry_unbind', registry, lease) == 1
                assert m.get(page + 96) == 2
                m.put(records + 16, held)
                assert m.call('pin_registry_unbind', registry, lease) == 0
                assert m.get(page + 96) == 1
                assert m.get(records + 48) == 2  # admission.epoch retained
                assert m.call('metadata_step', pool, lease, 1) == 0
                assert m.get(page + 96) == 0 and m.get(pool + 32) == 0
                cases += 1
    print(f'pin relocation ARM: {cases} page/mark admission, actual claims and mover cleanup PASS')
    print(f'pin registry ARM: {cases} admission-order and post-move cleanup lifecycles PASS')


def verify_pin_source_release(m):
    """Metadata transaction checks; OS decommit success is explicitly modeled."""
    heap, pages, pool, slots, records, registry, lease, permit, pin, selection, drain, request, table, cells, uncommit, forward, access = (
        MEMORY + n for n in range(0x2000, 0x3100, 0x100))
    page_bytes = 1 << 17
    for generation in (0, 1):
        m.reset()
        for offset, value in ((8, pages), (16, 2), (24, page_bytes),
                              (32, page_bytes), (48, page_bytes)):
            m.put(heap + offset, value)
        for offset, value in ((8, page_bytes), (16, 2 * page_bytes), (24, 7),
                              (32, 10), (48, generation), (56, 5),
                              (80, page_bytes + 96), (104, table)):
            m.put(pages + offset, value)
        for offset, value in ((0, heap), (8, slots), (16, 2), (24, 1)):
            m.put(pool + offset, value)
        m.put(slots + 24, 2)
        for offset, value in ((0, pool), (8, records), (16, 2)):
            m.put(registry + offset, value)
        m.put(lease, pages)
        m.put(lease + 8, 7)
        assert m.call('metadata_step', pool, lease, 0) == 0
        assert m.call('pin_registry_bind', registry, lease) == 0
        for offset, value in ((0, cells), (8, 2), (16, 2)):
            m.put(table + offset, value)
        for j in range(2):
            m.put(cells + j * 32, page_bytes + 32 + j * 48)
            m.put(cells + j * 32 + 8, 7)
        def fresh_drain():
            m.uc.mem_write(drain, bytes(80))
            for offset, value in ((32, 7), (40, page_bytes + 96), (48, 2), (64, 1)):
                m.put(drain + offset, value)
        fresh_drain()
        for offset, value in ((0, lease), (8, selection), (16, drain)):
            m.put(request + offset, value)
        m.put(pin, permit)
        assert m.call('pin_registry_pin', registry, lease, pin) == 0
        m.put(selection + 16, 1)
        before = bytes(m.uc.mem_read(drain, 80))
        assert m.call('pin_source_drain', registry, request) == 3
        assert bytes(m.uc.mem_read(drain, 80)) == before
        assert m.call('pin_registry_unpin', registry, lease, pin) == 0
        m.put(selection + 16, 0)
        assert m.call('pin_registry_select', registry, lease, selection) == 0
        m.put(drain + 64, 0)
        before = bytes(m.uc.mem_read(drain, 80))
        assert m.call('pin_source_drain', registry, request) == 1
        assert bytes(m.uc.mem_read(drain, 80)) == before
        m.put(drain + 64, 1)
        assert m.call('pin_source_drain', registry, request) == 3
        assert m.get(drain) == registry and m.get(drain + 24) == 1
        assert m.get(drain + 56) == 0 and m.get(drain + 72) == 1
        for j in range(2):
            source = page_bytes + 32 + j * 48
            for offset, value in ((0, source), (8, 7), (16, j + 1), (24, 0)):
                m.put(forward + offset, value)
            assert m.call('forward_step', cells + j * 32, forward, 0) == 0
            for offset, value in ((8, source), (16, 7), (32, j + 1)):
                m.put(access + offset, value)
            assert m.call('source_access', cells + j * 32, access, 2) == 0
            m.put(forward + 24, ((source + page_bytes) << 16) | 0x1510)
            assert m.call('forward_step', cells + j * 32, forward, 2) == 0
        assert m.call('pin_source_drain', registry, request) == 2
        assert m.get(drain + 56) == 1
        assert m.call('pin_registry_reopen', registry, lease, selection) == 0
        assert m.call('pin_registry_select', registry, lease, selection) == 0
        before = bytes(m.uc.mem_read(drain, 80))
        assert m.call('pin_source_drain', registry, request) == 1
        assert bytes(m.uc.mem_read(drain, 80)) == before
        fresh_drain()
        assert m.call('pin_source_drain', registry, request) == 2
        assert m.call('pin_source_drain', registry, request) == 0
        assert m.get(pages + 56) == 7
        m.put(request + 16, uncommit)
        m.put(uncommit + 24, 1)
        m.put(uncommit + 32, 7)
        assert m.call('pin_source_uncommit', registry, request, 0) == 0
        assert m.get(pages + 40) == uncommit + 16
        assert m.get(uncommit) == registry and m.get(uncommit + 8) == 2
        before = bytes(m.uc.mem_read(uncommit, 64))
        assert m.call('pin_source_uncommit', registry, request, 1) == 1
        assert bytes(m.uc.mem_read(uncommit, 64)) == before
        assert m.get(heap + 48) == page_bytes
        m.put(uncommit + 56, 0)  # Model OS success; no real mapping is touched.
        m.put(selection + 16, 3)
        assert m.call('pin_source_uncommit', registry, request, 1) == 1
        m.put(selection + 16, 2)
        assert m.call('pin_source_uncommit', registry, request, 1) == 0
        assert m.get(heap + 48) == 0 and m.get(pages + 56) == 8
        assert m.call('pin_source_uncommit', registry, request, 1) == 1
        assert m.get(heap + 48) == 0
        assert m.call('pin_registry_reopen', registry, lease, selection) == 0
        assert m.call('pin_registry_unbind', registry, lease) == 0
        assert m.call('metadata_step', pool, lease, 1) == 0
        assert m.get(pool + 32) == 0
    print('pin source release ARM: both generations, WAIT capture, stale cursors and exact credit PASS')


def verify_root_pin(m, use_dispatch=False):
    base = MEMORY + 0x2000
    ctx, pool, heap, page, table, cell = (base + n for n in (256, 640, 704, 832, 1024, 1056))
    segment, roots, journal, entries, starts, payload = (base + n for n in (1280, 1344, 1408, 1472, 2048, 4096))
    slots, registry, record, start, source, reserved, selection, copied = (
        base + n for n in range(0x1600, 0x1e00, 0x100))
    heap_base = payload - 131072
    runtime, lock = base + 0x1e00, base + 0x1f00
    native = {'held': False, 'pairs': 0}
    def callback(uc, address, size, unused):
        if address not in (STUBS + 0x700, STUBS + 0x704):
            return
        assert uc.reg_read(r.UC_ARM64_REG_SP) % 16 == 0
        assert uc.reg_read(X[0]) == lock
        if address == STUBS + 0x700:
            assert not native['held'] and m.get(ctx + 144) == 0
            native['held'] = True
        else:
            assert native['held']
            native['held'] = False
            native['pairs'] += 1
        for j in range(18):
            uc.reg_write(X[j], 0xbad00000 + j)
    hook = m.uc.hook_add(UC_HOOK_CODE, callback) if use_dispatch else None
    def invoke(function, *arguments):
        if use_dispatch and function in ('root_pin_enter', 'root_pin_exit', 'root_pin_cleanup',
                                         'root_pin_cancel', 'root_pin_finish'):
            return m.call('pin_dispatch', runtime, arguments[0], int(function == 'root_pin_cancel'))
        return m.call(function, *arguments)
    cases = 0
    for generation in (0, 1):
        reference = (131104 << 16) | (0x1520 if generation else 0x1510)
        for mode in range(8 if use_dispatch else 7):
            m.reset()
            m.uc.mem_write(ctx, struct.pack('<IIQQ', m.context_layout['abi_version'], 0, 17, 0x10000))
            def words(address, *values):
                for j, value in enumerate(values):
                    m.put(address + j * 8, value)
            words(heap, heap_base, page, 1)
            words(page, heap_base, 131072, 262144, 27, 1, 0, generation, 5,
                  starts, 128, 131120, 0, 0, table)
            words(table, cell, 1, 1)
            words(cell, 131104, 27, 0, 0)
            words(starts, 4)
            words(pool, heap, slots, 4, 1, 0)
            for j in range(4):
                m.put(slots + j * 32 + 24, j + 2 if j < 3 else 0)
            words(registry, pool, record, 1)
            # Temporary borrowed lease, then registry-owned lease only.
            words(selection, page, 27, 0)
            assert invoke('metadata_step', pool, selection, 0) == 0
            assert invoke('pin_registry_bind', registry, selection) == 0
            assert invoke('metadata_step', pool, selection, 1) == 0
            words(segment, 17, 101, roots, 2, 0, 1, journal, 0)
            words(journal, entries, 8, 0, 0, 0)
            words(source, segment, 0, 0, 0, 0)
            words(reserved, segment, 0, 0, 0, 0)
            assert invoke('root_step', ctx, source, 0) == 0
            assert invoke('root_step', ctx, reserved, 0) == 0
            m.put(source + 32, 0 if mode == 2 else reference)
            assert invoke('root_step', ctx, source, 1) == 0
            words(start, registry, ctx, source, reserved)
            words(runtime, registry, lock, STUBS + 0x700, STUBS + 0x704)
            if mode == 5:
                words(journal + 16, 8, 8, 0)
                assert invoke('root_pin_prepare', base, start) == 3
                assert bytes(m.uc.mem_read(base, 256)) == bytes(256)
                m.put(journal + 32, 8)
            if mode == 7:
                phase_roots, handshake, participant, worker_request = (base + n for n in (0x2400, 0x2500, 0x2600, 0x2700))
                words(phase_roots + 48, 0x10000); words(handshake + 48, 16)
                words(participant, ctx); words(worker_request, handshake, participant)
                m.put(ctx + 8, 0); m.put(ctx + 16, 0)
                assert m.call('worker_step', phase_roots, worker_request, 3) == 0
            assert invoke('root_pin_prepare', base, start) == 0
            assert m.get(base) == base and m.get(base + 16) == 1
            m.uc.mem_write(copied, bytes(m.uc.mem_read(base, 256)))
            assert invoke('root_pin_enter', copied) == 4
            m.put(source + 32, 0)
            assert invoke('root_step', ctx, source, 1) == 0
            assert m.get(roots) == 0
            if mode == 2:
                assert invoke('root_pin_enter', base) == 10
            else:
                if mode == 1:
                    assert invoke('pin_registry_select', registry, record, selection) == 0
                assert invoke('root_pin_enter', base) == 9
                assert m.get(ctx + 144) and m.get(base + 24) == 0
                if mode == 7:
                    package, closing, charged = base - 88, base + 0x2200, base + 0x2300
                    words(package, package, phase_roots, ctx, charged, 0, 3)
                    words(charged, 4096, heap, 0, 2)
                    words(closing, runtime, phase_roots, lock, STUBS + 0x700, STUBS + 0x704, heap, charged)
                    assert m.call('handshake_step', handshake, 0, 6) == 0
                    pairs = native['pairs']
                    assert m.call('native_pin_progress', closing, package, worker_request) == 0
                    assert m.get(ctx + 144) == 0 and native['pairs'] == pairs
                    assert m.get(base + 48) == 1  # Sticky cancellation, never READY.
                    assert m.call('native_pin_progress', closing, package, worker_request) == 17
                    assert m.call('handshake_step', handshake, 0, 7) == 0
                    assert m.call('native_pin_progress', closing, package, worker_request) == 0
                    assert m.call('native_pin_progress', closing, package, worker_request) == 0
                    assert m.get(base + 16) == 6 and m.get(roots + 32) == 0
                    assert invoke('pin_registry_unbind', registry, record) == 0
                    assert m.get(pool + 32) == 0 and not native['held']
                    cases += 1
                    continue
                if not use_dispatch:
                    assert invoke('root_pin_cancel', base) == 4
                if mode == 1:
                    assert invoke('root_pin_exit', base) == 7
                    assert m.get(ctx + 144) == 0
                    assert invoke('root_pin_cleanup', base) == 2
                    assert invoke('pin_registry_reopen', registry, record, selection) == 0
                    assert invoke('root_pin_enter', base) == 9
                assert invoke('root_pin_exit', base) == 1
                assert m.get(ctx + 144) == 0 and m.get(cell + 24) == 0
                assert m.get(base + 24) == payload + 32
                assert m.get(record + 32) == 1 and m.get(roots + 32) == reference
                assert invoke('pin_registry_select', registry, record, selection) == 2
                m.uc.mem_write(ctx + 4, struct.pack('<I', 2))
                m.put(m.get(base + 24), 123)  # Borrowed native access after guard exit.
                if mode != 4:
                    m.uc.mem_write(ctx + 4, struct.pack('<I', 0))
                if mode == 6:
                    free_head = m.get(pool + 24)
                    m.put(pool + 24, 0)
                    assert invoke('root_pin_cancel', base) == 7
                    assert m.get(base + 24) == payload + 32 and m.get(record + 32) == 1
                    m.put(pool + 24, free_head)
            assert invoke('root_pin_cancel', base) == 0
            assert m.get(base + 24) == 0 and m.get(pool + 32) == 1 and m.get(record + 32) == 0
            if mode == 3:
                words(journal + 16, 8, 8, 0)
                assert invoke('root_pin_finish', base) == 3
                assert m.get(roots + 32) == reference
                m.put(journal + 32, 8)
            if mode == 4:
                assert invoke('root_pin_finish', base) == (11 if use_dispatch else 4)
                m.uc.mem_write(ctx + 4, struct.pack('<I', 0))
            assert invoke('root_pin_finish', base) == 8
            assert m.get(roots + 32) == 0 and m.get(roots + 48) == 0
            assert m.get(base + 16) == 6
            assert invoke('root_pin_finish', base) == (8 if use_dispatch else 4)
            assert invoke('pin_registry_unbind', registry, record) == 0
            assert m.get(pool + 32) == 0
            assert not native['held']
            cases += 1
    if use_dispatch:
        m.uc.hook_del(hook)
        print(f"pin dispatch ARM: {cases} owner lifecycles, {native['pairs']} balanced clobbering lock callbacks PASS")
    else:
        print(f'root pin ARM: {cases} rooted acquisition, native access and retryable cleanup lifecycles PASS')


def verify_native_pin(m):
    storage, caller, ctx, registry, charge, spec, owner, start, source, pages, pool, heap = (
        MEMORY + n for n in (0x2000, 0x3000, 0x5000, 0x5200, 0x5300, 0x5400,
                            0x5500, 0x5700, 0x5800, 0x5900, 0x5a00, 0x5b00))
    caller_charge, same = charge + 32, MEMORY + 0x5c00
    for mode in range(5):
        m.reset()
        def words(address, *values):
            for j, value in enumerate(values):
                m.put(address + j * 8, value)
        m.uc.mem_write(ctx, struct.pack('<IIQQ', m.context_layout['abi_version'], 0, 17, 0x10000))
        m.put(registry + 48, 0x10000)
        words(spec, 17, 1, 8, 4096)
        words(charge, 4096, heap, storage, 2)
        words(caller_charge, 4096, heap, caller, 2)
        assert m.call('root_storage_prepare', caller, spec) == 0
        assert m.call('root_storage_prepare', storage, spec) == 0
        assert m.call('native_root_publish', registry, ctx, caller_charge) == 0
        words(source, caller + 160, 0, 0, 0, 0)
        assert m.call('root_step', ctx, source, 0) == 0  # Protected null caller root.
        words(pages, pool, 0, 0)
        words(start, registry, ctx, charge)
        assert m.call('native_pin_publish', owner, 0) == 4
        assert m.call('native_pin_publish', owner, start) == 0
        assert m.get(owner) == owner and m.get(owner + 40) == 1
        before = bytes(m.uc.mem_read(owner, 344))
        assert m.call('native_pin_publish', owner, start) == 4
        assert bytes(m.uc.mem_read(owner, 344)) == before
        words(same, storage + 160, 0, 0, 0, 0)
        assert m.call('native_pin_prepare', owner, pages, same) == 4
        m.put(charge + 16, 0)
        assert m.call('native_pin_prepare', owner, pages, source) == 4
        m.put(charge + 16, storage)
        assert bytes(m.uc.mem_read(owner, 344)) == before
        if mode in (2, 3):
            words(storage + 240, 8, 8, 0)
            assert m.call('native_pin_prepare', owner, pages, source) == 3
            assert m.get(owner + 40) == 2 and m.get(owner + 56) == storage + 264
            if mode == 2:
                m.put(storage + 256, 8)
        if mode in (1, 2, 4):
            assert m.call('native_pin_prepare', owner, pages, source) == 0
            assert m.get(owner + 40) == 3 and m.get(owner + 88) == owner + 88
            assert m.call('native_pin_retire', owner) == 2
            assert m.call('root_pin_enter', owner + 88) == 10
            assert m.call('root_pin_cancel', owner + 88) == 0
            assert m.call('native_pin_retire', owner) == 2
            assert m.call('root_pin_finish', owner + 88) == 8
        # Failed root-registry retirement retains the package. A stage-2
        # abandoned reservation is canceled exactly once before the retry.
        m.put(ctx + 136, MASK)
        assert m.call('native_pin_retire', owner) == 6
        if mode == 3:
            assert m.get(owner + 40) == 1 and m.get(owner + 56) == 0
            assert m.get(storage + 280) == 0
        m.put(ctx + 136, 0)
        assert m.call('native_pin_retire', owner) == 0
        assert m.get(owner + 40) == 4 and m.get(storage + 200) == 2
        assert m.get(registry + 16) == 1 and m.get(registry + 24) == 1
        assert m.get(charge + 16) == storage  # Disposal is a later owner action.
        assert m.call('native_pin_retire', owner) == 4
    print('native pin ARM: 5 publication/adoption/retirement lifecycles with partial retry PASS')


def verify_native_pin_dispose(m):
    owner, ctx, charge, ops, heap = (MEMORY + n for n in (0x2000, 0x2200, 0x2400, 0x2500, 0x2600))
    storage, callback_address = 0x600000000, STUBS + 0x708
    native = {'mapped': False, 'fail': False, 'calls': 0}
    def callback(uc, address, size, unused):
        if address != callback_address:
            return
        assert uc.reg_read(r.UC_ARM64_REG_SP) % 16 == 0
        assert uc.reg_read(X[0]) == storage and uc.reg_read(X[1]) == 4096
        assert native['mapped'] and m.get(ctx + 144) == 0
        assert unpack('I', bytes(uc.mem_read(ctx + 4, 4)), 0)[0] == 2
        native['calls'] += 1
        if not native['fail']:
            uc.mem_unmap(storage, 4096)
            native['mapped'] = False
        for j in range(18):
            uc.reg_write(X[j], 0xbad00000 + j)
        # Only the low byte carries the C bool receipt.
        uc.reg_write(X[0], 0xbad0000000000000 | int(not native['fail']))
    hook = m.uc.hook_add(UC_HOOK_CODE, callback)
    for fault in range(25):
        m.reset()
        m.uc.mem_map(storage, 4096)
        native.update(mapped=True, fail=fault == 24, calls=0)
        def words(address, *values):
            for j, value in enumerate(values):
                m.put(address + j * 8, value)
        words(owner, owner, heap, ctx, charge, storage, 4)
        m.uc.mem_write(ctx, struct.pack('<IIQQ', m.context_layout['abi_version'], 2, 17, 0x10000))
        words(charge, 4096, heap, storage, 2)
        words(ops, 4096, STUBS + 0x70c, callback_address)
        words(storage, storage + 160)
        words(storage + 240, 3, 3, 3)
        q, operations, expected = owner, ops, 4
        if fault == 0:
            expected = 8
        elif fault == 1:
            m.uc.mem_write(ctx + 4, struct.pack('<I', 0))
        elif fault == 2:
            m.put(ctx + 144, 27)
        elif fault == 3:
            m.uc.mem_write(ctx, struct.pack('<I', 0))
        elif fault == 4:
            m.put(ctx + 8, 0)
        elif fault == 5:
            m.put(owner, owner + 8)
        elif fault == 6:
            m.put(owner + 40, 3)
        elif fault == 7:
            m.put(charge + 16, 0)
        elif fault == 8:
            m.put(charge + 24, 1)
        elif fault == 9:
            m.put(owner + 32, 0)
        elif fault == 10:
            m.put(ops + 8, 0)
        elif fault == 11:
            m.put(ops + 16, 0)
        elif fault == 12:
            m.put(ops, 3)
        elif fault == 13:
            m.put(ops, 24)
        elif fault == 14:
            m.put(charge + 8, 0)
        elif fault == 15:
            m.put(charge, 0)
        elif fault == 16:
            m.put(charge, 4095)
        elif fault == 17:
            m.put(owner + 32, storage + 8)
            m.put(charge + 16, storage + 8)
            expected = 2
        elif fault == 18:
            m.put(ops, 8)
            m.put(charge, 256)
            expected = 2
        elif fault == 19:
            m.put(storage + 24, 1)
            expected = 2
        elif fault == 20:
            m.put(storage + 168, 1)
            expected = 2
        elif fault == 21:
            m.put(storage + 256, 2)
            expected = 2
        elif fault == 22:
            operations = 0
        elif fault == 23:
            q = 0
        elif fault == 24:
            expected = 7
        before = bytes(m.uc.mem_read(owner, 344))
        charged = bytes(m.uc.mem_read(charge, 32))
        backing = bytes(m.uc.mem_read(storage, 4096))
        assert m.call('native_pin_dispose', q, operations) == expected, fault
        if fault != 0:
            assert bytes(m.uc.mem_read(owner, 344)) == before, fault
            assert bytes(m.uc.mem_read(charge, 32)) == charged, fault
            assert bytes(m.uc.mem_read(storage, 4096)) == backing, fault
            assert native['calls'] == int(fault == 24), fault
        if fault == 24:
            native['fail'] = False
            assert m.call('native_pin_dispose', owner, ops) == 8
        if fault in (0, 24):
            assert not native['mapped']
            assert m.get(owner + 32) == 0 and m.get(owner + 40) == 5
            assert m.get(charge + 16) == 0 and m.get(charge) == 4096 and m.get(charge + 24) == 2
            calls = native['calls']
            assert m.call('native_pin_dispose', owner, ops) == 4
            assert native['calls'] == calls
            # Budget ownership remains live after the backing is actually unmapped.
            words(heap + 16, 1)
            words(heap + 64, 8192, 112, 0)
            saved = bytes(m.uc.mem_read(owner, 344))
            assert m.call('native_pin_release', owner, heap) == 4
            assert bytes(m.uc.mem_read(owner, 344)) == saved
            assert m.get(charge + 24) == 2 and m.get(heap + 72) == 112
            m.put(heap + 72, 112 + 4096)
            assert m.call('native_pin_release', owner, heap) == 8
            assert m.get(owner + 40) == 6 and m.get(charge + 24) == 3
            assert m.get(heap + 72) == 112
            assert m.call('native_pin_release', owner, heap) == 4
            assert m.get(heap + 72) == 112
        if native['mapped']:
            m.uc.mem_unmap(storage, 4096)
            native['mapped'] = False
    m.uc.hook_del(hook)
    print('native pin disposal ARM: 25 rejection/failure cases, narrow bool receipt and unmapped backing PASS')


def verify_native_pin_release(m):
    owner, ctx, charge, heap = (MEMORY + n for n in (0x2000, 0x2200, 0x2400, 0x2600))
    # Each fault must leave both the budget and unique ownership unchanged.
    for fault in range(21):
        m.reset()
        for off, value in ((0, owner), (16, ctx), (24, charge), (40, 5)):
            m.put(owner + off, value)
        m.uc.mem_write(ctx, struct.pack('<IIQ', m.context_layout['abi_version'], 2, 17))
        for off, value in ((0, 4096), (8, heap), (24, 2)):
            m.put(charge + off, value)
        for off, value in ((16, 1), (64, 8192), (72, 4208)):
            m.put(heap + off, value)
        q, h = owner, heap
        if fault == 1: q = 0
        elif fault == 2: h = 0
        elif fault == 3: m.put(owner, owner + 8)
        elif fault == 4: m.put(owner + 40, 4)
        elif fault == 5: m.put(owner + 32, 0x600000000)
        elif fault == 6: m.put(owner + 24, 0)
        elif fault == 7: m.put(owner + 16, 0)
        elif fault == 8: m.uc.mem_write(ctx, struct.pack('<I', 0))
        elif fault == 9: m.put(ctx + 8, 0)
        elif fault == 10: m.uc.mem_write(ctx + 4, struct.pack('<I', 0))
        elif fault == 11: m.put(ctx + 144, 27)
        elif fault == 12: m.put(charge + 8, heap + 8)
        elif fault == 13: m.put(charge + 24, 1)
        elif fault == 14: m.put(charge + 16, 0x600000000)
        elif fault == 15: m.put(charge, 0)
        elif fault == 16: m.put(heap + 16, 0)
        elif fault == 17: m.put(heap + 72, 111)
        elif fault == 18: m.put(heap + 64, 4207)
        elif fault == 19: m.put(heap + 80, 8192)
        elif fault == 20: m.put(charge, 4097)
        before = bytes(m.uc.mem_read(MEMORY, 0x3000))
        assert m.call('native_pin_release', q, h) == (8 if fault == 0 else 4), fault
        if fault:
            assert bytes(m.uc.mem_read(MEMORY, 0x3000)) == before, fault
        else:
            assert m.get(owner + 40) == 6 and m.get(charge + 24) == 3
            assert m.get(heap + 72) == 112 and m.get(heap + 80) == 0
    print('native pin budget ARM: 21 validity/accounting cases preserve failed obligations PASS')


def verify_native_pin_close(m, use_progress=False):
    owner, ctx, charge, ops, heap, runtime, pins = (MEMORY + n for n in
        (0x2000, 0x2200, 0x2400, 0x2500, 0x2600, 0x2800, 0x2900))
    storage, lock = 0x600000000, MEMORY + 0x2a00
    handshake, participant, request = (MEMORY + n for n in (0x2b00, 0x2c00, 0x2d00))
    def invoke():
        return m.call('native_pin_progress', runtime, owner, request) if use_progress else m.call('native_pin_close', runtime, owner)
    native = {'held': False, 'pairs': 0, 'fail': True, 'calls': 0}
    m.reset()
    m.uc.mem_map(storage, 4096)
    def words(address, *values):
        for j, value in enumerate(values): m.put(address + j * 8, value)
    def callback(uc, address, size, unused):
        if address not in (STUBS + 0x700, STUBS + 0x704, STUBS + 0x708): return
        assert uc.reg_read(r.UC_ARM64_REG_SP) % 16 == 0 and m.get(ctx + 144) == 0
        if address == STUBS + 0x708:
            assert not native['held'] and uc.reg_read(X[0]) == storage and uc.reg_read(X[1]) == 4096
            native['calls'] += 1
            if not native['fail']: uc.mem_unmap(storage, 4096)
        else:
            assert uc.reg_read(X[0]) == lock
            if address == STUBS + 0x700:
                assert not native['held']; native['held'] = True
            else:
                assert native['held']; native['held'] = False; native['pairs'] += 1
        for j in range(18): uc.reg_write(X[j], 0xbad00000 + j)
        uc.reg_write(X[0], 0xbad0000000000000 | int(not native['fail']))
    hook = m.uc.hook_add(UC_HOOK_CODE, callback)
    words(owner, owner, heap, ctx, charge, storage, 4)
    m.uc.mem_write(ctx, struct.pack('<IIQQ', m.context_layout['abi_version'], 0, 17, 0x10000))
    words(charge, 4096, heap, storage, 2)
    words(ops, 4096, STUBS + 0x70c, STUBS + 0x708)
    words(pins, heap, lock, STUBS + 0x700, STUBS + 0x704)
    words(runtime, pins, heap, lock, STUBS + 0x700, STUBS + 0x704, heap, ops)
    words(storage, storage + 160)
    words(storage + 24, 1)
    words(heap + 16, 1)
    words(heap + 64, 8192, 112, 0)
    if use_progress:
        m.put(ctx + 8, 0); m.put(ctx + 16, 0)
        m.put(heap + 48, 0x10000)
        words(participant, ctx)
        words(request, handshake, participant)
        assert m.call('worker_step', heap, request, 3) == 0
        # Service requests leave the entire package owned and do not unmap it.
        for offset, value, expected in ((280, 1, 18), (24, 16, 22), (64, 1, 20), (104, 1, 21)):
            m.put(ctx + offset, value)
            assert invoke() == expected
            assert m.get(owner + 40) == 4 and m.get(charge + 16) == storage
            m.put(ctx + offset, 0)
        m.put(heap + 48, 0x20000)
        assert m.call('handshake_step', handshake, 0, 6) == 0
        assert invoke() == 17 and invoke() == 17
        assert m.call('handshake_step', handshake, 0, 7) == 0
        assert invoke() == 0
        assert unpack('I', bytes(m.uc.mem_read(ctx + 4, 4)), 0)[0] == 2
    else:
        assert invoke() == 12
        assert native['calls'] == native['pairs'] == 0
        m.uc.mem_write(ctx + 4, struct.pack('<I', 2))
    pairs = native['pairs']
    assert invoke() == 2
    assert native['calls'] == 0 and native['pairs'] == pairs
    m.put(storage + 24, 0)
    assert invoke() == 7
    assert m.get(owner + 32) == storage and m.get(charge + 16) == storage
    native['fail'] = False
    assert invoke() == 0
    assert m.get(owner + 40) == 5 and m.get(owner + 32) == 0
    assert invoke() == 4
    assert native['pairs'] == pairs + 1 and not native['held'] and m.get(charge + 24) == 2
    m.put(heap + 72, 4208)
    assert invoke() == 8
    assert native['pairs'] == pairs + 2 and not native['held']
    assert m.get(owner + 40) == 6 and m.get(heap + 72) == 112
    m.put(owner + 16, 0)  # Terminal result no longer consults context or charge.
    m.put(owner + 24, 0)
    assert invoke() == 8
    assert native['pairs'] == pairs + 2
    m.uc.hook_del(hook)
    print(f'native pin {"progress" if use_progress else "close"} ARM: boundary/wait requests, OS and budget retries, balanced callbacks PASS')


def verify_heap_decommit(m):
    base, page = 0x600000000, 1 << 17
    address = base+page
    for pointer, size in ((0,page),(address+1,page),(address,0),(address,page+1),(MASK & ~16383,16384),(address,4096)):
        m.reset()
        assert m.call('heap_decommit',pointer,size) == 0
        assert not m.calls['mprotect'] and not m.calls['madvise']
    m.reset();m.uc.mem_map(address,page);m.payload_range=(address,page)
    m.failures['mprotect']=[1]
    assert m.call('heap_decommit',address,page) == 0
    assert not m.payload_protected and not m.calls['madvise']
    heap,pages,request=(MEMORY+n for n in (0x2000,0x2400,0x2800))
    def words(pointer,*values):
        for i,value in enumerate(values):m.put(pointer+8*i,value)
    words(heap,base,pages,1)
    words(pages,base,page,2*page,7,11,request,0,7,0,0,page+64,0,1,0)
    words(request,0,1,7,1,heap,1)
    m.failures['madvise']=[1]
    assert m.call('native_source_uncommit',heap,request,m.functions['heap_decommit']) == 2
    assert m.payload_protected and not m.payload_discarded and m.get(request+40)==1
    assert m.call('native_source_uncommit',heap,request,m.functions['heap_decommit']) == 0
    assert m.payload_discarded and m.get(request+40)==0 and m.get(pages+96)==1
    assert m.calls['mprotect']==3 and m.calls['madvise']==2
    m.uc.mem_unmap(address,page)
    print('Darwin payload decommit ARM: admission, partial OS retry and shared controller composition PASS (OS model)')


def verify_native_source_uncommit(m):
    heap, pages, request = (MEMORY + n for n in (0x2000, 0x2400, 0x2800))
    base, callback, page = 0x600000000, STUBS + 0x720, 1 << 17
    native = {'calls': 0, 'success': False, 'mapped': False}
    def words(address, *values):
        for j, value in enumerate(values): m.put(address + 8*j, value)
    def setup():
        m.reset(); native['calls'] = 0; native['success'] = False
        words(heap, base, pages, 2)
        words(request, 0, 2, 7, 1, heap, 1)
        words(pages, base, page, 3*page, 7, 11, request, 0, 7, 0, 0, page+64, 0, 3, MEMORY+0x3000)
        words(pages+112, base, 2*page, 3*page, 7, 11, 0, 0, 4, 0, 0, 2*page, 0, 0, 0)
    def os_callback(uc, address, size, _):
        if address != callback: return
        assert uc.reg_read(X[0]) == base+page and uc.reg_read(X[1]) == 2*page
        native['calls'] += 1
        if native['success'] and native['mapped']:
            uc.mem_unmap(base+page, 2*page); native['mapped'] = False
        for reg in X[:18]: uc.reg_write(reg, 0xbad0000000000000)
        uc.reg_write(X[0], 0xbad0000000000000 | int(native['success']))
    hook = m.uc.hook_add(UC_HOOK_CODE, os_callback)
    for fault in range(35):
        setup(); h_arg, q_arg, cb, expected = heap, request, callback, 1
        if fault == 0: expected = 2
        elif fault == 1: q_arg = 0
        elif fault == 2: h_arg = 0
        elif fault == 3: cb = 0
        elif fault == 4: m.put(heap, 0)
        elif fault == 5: m.put(heap, base+1)
        elif fault == 6: m.put(heap+8, 0)
        elif fault == 7: m.put(heap+16, 0)
        elif fault == 8: m.put(heap+16, 1 << 31)
        elif fault == 9: m.put(heap, MASK & ~15)
        elif fault == 10: m.put(heap+8, MASK & ~7)
        elif fault == 11: m.put(request+24, 0)
        elif fault == 12: m.put(request+32, heap+8)
        elif fault == 13: m.put(request+40, 2)
        elif fault == 14: m.put(request+16, 0)
        elif fault == 15: m.put(request+8, 0)
        elif fault == 16: m.put(request+8, 3)
        elif fault == 17: m.put(request, 2)
        elif fault == 18: m.put(pages+40, request+48)
        elif fault == 19: m.put(pages+56, 5)
        elif fault == 20: m.put(pages+112+56, 0)
        elif fault == 21: m.put(pages+112+24, 8)
        elif fault == 22: m.put(pages+112+32, 12)
        elif fault == 23: m.put(pages+112+48, 1)
        elif fault == 24: m.put(pages+112+40, request)
        elif fault == 25: m.put(pages+32, 0)
        elif fault == 26: m.put(pages+48, 2)
        elif fault == 27: m.put(pages+112+88, 1)
        elif fault == 28: m.put(pages+112+8, page)
        elif fault == 29: m.put(pages+112+16, 4*page)
        elif fault == 30: m.put(pages+112, base+16)
        elif fault == 31: q_arg += 1
        elif fault == 32: m.put(heap+8, pages+1)
        elif fault == 33: h_arg += 1
        elif fault == 34:
            q_arg = request+64
            m.uc.mem_write(q_arg, bytes(m.uc.mem_read(request, 48)))
        before = bytes(m.uc.mem_read(MEMORY, 0x4000))
        assert m.call('native_source_uncommit', h_arg, q_arg, cb) == expected, fault
        assert bytes(m.uc.mem_read(MEMORY, 0x4000)) == before, fault
        assert native['calls'] == int(fault == 0), fault
    setup(); m.uc.mem_map(base+page, 2*page); native['mapped'] = True
    before = bytearray(m.uc.mem_read(MEMORY, 0x4000))
    assert m.call('native_source_uncommit', heap, request, callback) == 2
    assert bytes(m.uc.mem_read(MEMORY, 0x4000)) == bytes(before)
    native['success'] = True
    assert m.call('native_source_uncommit', heap, request, callback) == 0
    struct.pack_into('<Q', before, request-MEMORY+40, 0)
    assert bytes(m.uc.mem_read(MEMORY, 0x4000)) == bytes(before)
    assert not native['mapped'] and native['calls'] == 2
    assert m.call('native_source_uncommit', heap, request, callback) == 1
    assert native['calls'] == 2
    m.uc.hook_del(hook)
    print('native source payload ARM: 35 admission cases, clobbered callbacks, narrow bool retry and actual unmap PASS')


def verify_native_pin_wait(m):
    owner, ctx, charge, heap, runtime, pins, storage = (MEMORY + n for n in
        (0x2000, 0x2200, 0x2400, 0x2600, 0x2800, 0x2900, 0x3000))
    event = MEMORY + 0x2a04  # Only four-byte aligned; neighboring words must survive.
    def words(address, *values):
        for j, value in enumerate(values): m.put(address + 8*j, value)
    for fault in range(24):
        m.reset()
        words(owner, owner, heap, ctx, charge, storage, 4)
        m.uc.mem_write(ctx, struct.pack('<IIQ', m.context_layout['abi_version'], 2, 17))
        words(charge, 4096, heap, storage, 2)
        words(pins, 0, 1, STUBS + 0x700, STUBS + 0x704)
        words(runtime, pins, heap, 1, STUBS + 0x700, STUBS + 0x704, heap, charge)
        words(storage, storage + 160); words(storage + 200, 2)
        m.uc.mem_write(event - 4, struct.pack('<III', 0xa55a1234, 1, 0x87654321))
        r_arg, q_arg, e_arg, expected = runtime, owner, event, 4
        if fault == 0: expected = 2
        elif fault == 1: m.put(storage + 200, 0); expected = 0
        elif fault == 2: q_arg = 0
        elif fault == 3: r_arg = 0
        elif fault == 4: e_arg = 0
        elif fault == 5: e_arg += 1
        elif fault == 6: m.put(owner, owner + 8)
        elif fault == 7: m.put(owner + 40, 3)
        elif fault == 8: m.put(owner + 32, 0)
        elif fault == 9: m.put(owner + 24, 0)
        elif fault == 10: m.put(owner + 16, 0)
        elif fault == 11: m.put(charge + 16, 0)
        elif fault == 12: m.put(charge + 24, 1)
        elif fault == 13: m.put(charge + 8, heap + 8)
        elif fault == 14: m.uc.mem_write(ctx, struct.pack('<I', 0))
        elif fault == 15: m.put(ctx + 8, 0)
        elif fault == 16: m.uc.mem_write(ctx + 4, struct.pack('<I', 0))
        elif fault == 17: m.put(ctx + 144, 27)
        elif fault == 18: m.put(runtime, 0)
        elif fault == 19: m.put(runtime + 8, heap + 8)
        elif fault == 20: m.put(runtime + 24, 0)
        elif fault == 21: m.put(pins + 8, 0)
        elif fault == 22: m.put(owner + 40, 6)
        elif fault == 23: m.put(storage + 200, 0); m.put(storage + 168, 1); expected = 2
        before = bytearray(m.uc.mem_read(MEMORY, 0x4000))
        if expected == 2: struct.pack_into('<I', before, event - MEMORY, 0)
        assert m.call('native_pin_wait', r_arg, q_arg, e_arg) == expected, fault
        assert bytes(m.uc.mem_read(MEMORY, 0x4000)) == bytes(before), fault
    print('native pin wait ARM: 24 validation/completion cases and exact-width event arming PASS')


def verify_root_finish_notify(m):
    ctx, roots, request, lease, spec, notice, event = (MEMORY + n for n in
        (0x2000, 0x2200, 0x2300, 0x2400, 0x2500, 0x2600, 0x2704))
    storage, callback = 0x600000000, STUBS + 0x710
    m.reset(); m.uc.mem_map(storage, 4096)
    def words(address, *values):
        for j, value in enumerate(values): m.put(address + 8*j, value)
    m.uc.mem_write(ctx, struct.pack('<IIQQ', m.context_layout['abi_version'], 0, 17, 0x10000))
    m.put(roots + 48, 0x10000); words(spec, 17, 1, 1, 4096)
    assert m.call('root_storage_prepare', storage, spec) == 0
    words(request, storage, 0, 0, 0, ctx)
    assert m.call('root_life', roots, request, 0) == 0
    assert m.call('root_life', roots, request, 3) == 0
    m.put(request + 8, lease)
    assert m.call('root_life', roots, request, 6) == 0
    assert m.call('root_clear', request) == 0
    words(notice, ctx, event, callback, 0, 0)
    native = {'calls': 0}
    def notify(uc, address, size, unused):
        if address != callback: return
        assert uc.reg_read(r.UC_ARM64_REG_SP) % 16 == 0 and uc.reg_read(X[0]) == event
        native['calls'] += 1
        if native['calls'] == 1:
            # FINISH has completed; no further backing access is allowed.
            assert m.get(storage + 200) == 0 and m.get(notice + 24) == 1
            uc.mem_unmap(storage, 4096)
        else:
            uc.mem_write(event, struct.pack('<I', 1))
        for j in range(18): uc.reg_write(X[j], 0xbad00000 + j)
        uc.reg_write(X[0], (-5 & MASK) if native['calls'] == 1 else 0)
    hook = m.uc.hook_add(UC_HOOK_CODE, notify)
    original = bytes(m.uc.mem_read(MEMORY, 0x3000))
    for fault in range(12):
        m.uc.mem_write(MEMORY, original)
        r_arg, q_arg, n_arg = roots, request, notice
        if fault == 0: n_arg = 0
        elif fault == 1: n_arg += 1
        elif fault == 2: m.put(notice + 24, 3)
        elif fault == 3: m.put(notice, 0)
        elif fault == 4: m.put(notice, ctx + 8)
        elif fault == 5: m.put(notice + 8, 0)
        elif fault == 6: m.put(notice + 8, event + 1)
        elif fault == 7: m.put(notice + 16, 0)
        elif fault == 8: r_arg = 0
        elif fault == 9: q_arg = 0
        elif fault == 10: m.put(request, 0)
        elif fault == 11: m.put(request + 16, 0)
        before = bytes(m.uc.mem_read(MEMORY, 0x3000))
        backing = bytes(m.uc.mem_read(storage, 4096))
        assert m.call('root_finish_notify', r_arg, q_arg, n_arg) == 1, fault
        assert bytes(m.uc.mem_read(MEMORY, 0x3000)) == before and not native['calls'], fault
        assert bytes(m.uc.mem_read(storage, 4096)) == backing, fault
    m.uc.mem_write(MEMORY, original)
    assert m.call('root_finish_notify', roots, request, notice) == 4
    assert m.get(notice + 24) == 1 and m.get(notice + 32) == (-5 & MASK)
    assert m.call('root_finish_notify', 0, 0, notice) == 0
    assert m.get(notice + 24) == 2 and m.get(notice + 32) == 0
    assert unpack('I', bytes(m.uc.mem_read(event, 4)), 0)[0] == 1
    assert m.call('root_finish_notify', 0, 0, notice) == 0 and native['calls'] == 2
    m.uc.hook_del(hook)
    print('root finish notify ARM: 12 invalid bindings, callback clobbers and retry after actual unmap PASS')


def verify_allocation_sample(m):
    base,query=MEMORY+0x2000,MEMORY+0x2200
    assert m.context_layout['allocated_bytes']==272 and m.context_layout['size']==288
    for fault in range(25):
        m.reset()
        q=[base,17,10,100,3,4];owner=17;abi=m.context_layout['abi_version'];total=200
        context,request=base,query;now=20;status=3
        if fault==0:status=0
        elif fault==1:q=[0]*6;status=1
        elif fault==2:now=10;status=2
        elif fault==3:now=9
        elif fault==4:total=MASK;status=4
        elif fault==5:q=[0]*6;total=MASK;status=4
        elif fault==6:q[0]+=8
        elif fault==7:q[1]+=1
        elif fault==8:owner=0
        elif fault==9:abi-=1
        elif fault==10:q[3]=MASK
        elif fault==11:q[3]=201
        elif fault==12:request=0
        elif fault==13:context=0
        elif fault==14:request+=1
        elif fault==15:context+=1
        elif fault==16:q=[0,0,0,1,0,0]
        elif fault==17:q=[0,0,1,0,0,0]
        elif fault==18:q=[0,1,0,0,0,0]
        elif fault==19:q=[0,0,0,0,0,1]
        elif fault==20:q=[0,0,0,0,1,0]
        elif fault==21:total=100;status=0
        elif fault==22:now=MASK;status=0
        elif fault==23:now=0;q=[0]*6;status=1
        elif fault==24:now=MASK;q[2]=q[3]=0;total=MASK-1;status=0
        before=bytearray(1024)
        struct.pack_into('<IIQ',before,0,abi,0,owner)
        struct.pack_into('<Q',before,272,total)
        struct.pack_into('<6Q',before,512,*q)
        after=bytearray(before)
        if status==1:struct.pack_into('<6Q',after,512,base,owner,now,total,0,0)
        elif status==0:struct.pack_into('<6Q',after,512,base,owner,now,total,now-q[2],total-q[3])
        m.uc.mem_write(base,bytes(before))
        assert m.call('allocation_sample',context,request,now)==status,fault
        assert bytes(m.uc.mem_read(base,len(after)))==bytes(after),fault
        if status in (0,1):
            assert m.call('allocation_sample',context,request,now)==2
            assert bytes(m.uc.mem_read(base,len(after)))==bytes(after)
    print('PASS: ARM64 allocation samples 25 independent time, identity and saturation cases')


def verify_mark_scan_commit(m):
    base=MEMORY+0x2000
    count=0
    for generation in range(2):
        for status in range(2):
            for fault in range(16):
                m.reset()
                g=[base+256,1,0,0,0,0,1,1,1,generation,0,0,0]
                q=[base+512,(1<<20)|0x1510,17,1,1,1,0]
                task=[0,1,2,*q[:4],0]
                scan=[base+128,0,0,0,2,16,1,2,0]
                op=status;result=1
                if fault==0:result=0
                elif fault==1:op=2
                elif fault==2:scan[4]=0
                elif fault==3:scan[4]=257
                elif fault==4:scan[7]=3
                elif fault==5:scan[6]=3
                elif fault==6:scan[0]=0
                elif fault==7:scan[0]+=1
                elif fault==8:q[4]=0
                elif fault==9:q[5]=0
                elif fault==10:q[6]=24
                elif fault==11:scan[5]=0;result=1 if status else 0
                elif fault==12:scan[5]=17;result=1 if status else 0
                elif fault==13:q[5]+=1
                elif fault==14:g[0]=0
                elif fault==15:g[7]=MASK;result=6 if status else 0
                before=bytearray(1024)
                def put(buffer,offset,words):
                    struct.pack_into('<'+'Q'*len(words),buffer,offset,*words)
                for offset,words in [(0,g),(128,q),(256,task),(768,scan)]:put(before,offset,words)
                expected=bytearray(before)
                if result==0:
                    g[6]=0;scan[0]=0
                    if status:
                        g[3]=g[4]=g[5]=1;g[7]+=1
                        task[1]=g[7];task[2]=1;task[7]=scan[5];q=[0]*7
                    else:
                        g[2]=1;task[2:]=[0]*6;q[4]=q[5]=0;q[6]=scan[5]
                    for offset,words in [(0,g),(128,q),(256,task),(768,scan)]:put(expected,offset,words)
                m.uc.mem_write(base,bytes(before))
                assert m.call('mark_scan_commit',base,base+768,op)==result,(generation,status,fault)
                assert bytes(m.uc.mem_read(base,1024))==bytes(expected),(generation,status,fault)
                if result==0:
                    assert m.call('mark_scan_commit',base,base+768,op)==1
                    assert bytes(m.uc.mem_read(base,1024))==bytes(expected)
                count+=1
    assert count==64
    print('PASS: ARM64 mark scan commit 64 independent full-state receipt and rollback cases')


def verify_guarded_observe(m):
    base=MEMORY+0x2000;scope=128;ctx=256;page=640;cell=768
    registry=832;desc=896;slots=976;bitmap=992;segment=1024;root=1088;journal=1152;entries=1216;heap=1536
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    count=0
    for value in (0,(128<<16)|0x1510,(160<<16)|0x8630,(192<<16)|0x2520):
        for fault in range(6):
            data=bytearray(2560)
            words(data,0,base+scope,base+page,base+registry,0,0x22,base+segment,base+root,1,101,0)
            words(data,scope,base+ctx,48,27,base+cell,0)
            struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,0x22)
            words(data,ctx+144,27,0)
            words(data,page,base+heap,16,1024,27,0,0,1,5,0,0,80,0,0,0)
            words(data,cell,48,27,0,1)
            words(data,registry,base+slots,2);words(data,slots,0,base+desc)
            words(data,desc,1,16,24,0,base+bitmap,0,1,0);words(data,bitmap,3)
            words(data,heap+16,24,1,0,2);words(data,heap+48,value)
            words(data,segment,17,101,base+root,1,1,1,base+journal,0)
            words(data,root,0,1,1,0);words(data,journal,base+entries,3,0,0,0)
            status=(0,1,3,4,1,1)[fault]
            if fault==1:words(data,72,1)
            elif fault==2:words(data,journal+16,3,3)
            elif fault==3:words(data,ctx+152,MASK)
            elif fault==4:words(data,heap+48,1)
            elif fault==5:words(data,root+16,0)
            expected=bytearray(data)
            if status==0:
                words(expected,72,value);words(expected,root,value)
                words(expected,entries,0,value,101,0x22)
                words(expected,journal+16,1,1);words(expected,ctx+152,1)
            m.reset();m.uc.mem_write(base,bytes(data))
            assert m.call('guarded_observe',base)==status
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected)
            count+=1
    for pointer in (0,1):
        m.reset();assert m.call('guarded_observe',pointer)==1;count+=1
    assert count==26
    print('PASS: ARM64 rooted observation 26 independent exact-bit and failure cases')


def verify_rooted_scalar_owner(m):
    # Independent exact-memory oracle, including payload changes and obligations.
    base=MEMORY+0x2000;ctx=256;pool=640;heap=704;page=832;slots=960;table=1024;cell=1056
    registry=1120;dslots=1152;desc=1184;bitmap=1264;segment=1280;roots=1344
    journal=1408;entries=1472;starts=2048;payload=4096
    receiver=(131104<<16)|0x1510;heap_base=base+payload-131072;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(value,mode):
        d=bytearray(5120)
        words(d,0,base+ctx,base+segment,base+roots,1,101,0)
        words(d,128,base+registry,16,value,mode,0,0)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,0x22)
        words(d,pool,base+heap,base+slots,1,1,0);words(d,heap,heap_base,base+page,1)
        words(d,page,heap_base,131072,262144,27,1,0,1,5,base+starts,128,131136,0,0,base+table)
        words(d,table,base+cell,1,1);words(d,cell,131104,27,0,0)
        words(d,registry,base+dslots,2);words(d,dslots,0,base+desc)
        words(d,desc,1,16,24,0,base+bitmap,0,1,0);words(d,bitmap,3)
        words(d,segment,17,101,base+roots,1,1,1,base+journal,0)
        words(d,roots,receiver,1,1,0);words(d,journal,base+entries,3,0,0,0)
        words(d,starts,4);words(d,payload,24,1,0,2,0,0,0xdeadbeef)
        return d
    def entered(d):
        words(d,48,base+page,27,(1<<32)|1)
        words(d,72,receiver,0,base+ctx,131104,27,base+cell,0)
        words(d,160,1);words(d,ctx+144,27);words(d,page+96,1)
        words(d,pool+24,0,1);words(d,slots,1,base+page,27,0);words(d,cell+24,1)
    def exited(d):
        d[72:128]=bytes(56);words(d,ctx+144,0);words(d,cell+24,0);words(d,160,3)
    def released(d):
        d[48:72]=bytes(24);words(d,page+96,0);words(d,pool+24,1,0)
        words(d,slots+8,0,0,0);words(d,160,4)
    def run(d,op,status,change=None,qa=None):
        nonlocal count
        expected=bytearray(d)
        if change:change(expected)
        m.reset();m.uc.mem_write(base,bytes(d))
        got=m.call('rooted_scalar',base+pool,base if qa is None else qa,op)
        assert got==status,(count,op,got,status)
        actual=bytes(m.uc.mem_read(base,len(d)))
        assert actual==bytes(expected),(count,op,[(i,actual[i],expected[i]) for i in range(len(d)) if actual[i]!=expected[i]][:12])
        count+=1;return expected
    for value in (0,MASK,0x301510,0xffff000000000000):
        for mode in range(2):
            for outcome in range(3): # success, typed-slot rejection, early abort
                d=fixture(value,mode)
                if outcome==1:words(d,136,0)
                d=run(d,0,8,entered);d=run(d,0,8);d=run(d,3,8)
                def access(x):
                    words(x,160,2,outcome)
                    if outcome==0:words(x,payload+48 if mode else 144,value if mode else 0xdeadbeef)
                d=run(d,4 if outcome==2 else 1,10,access)
                words(d,payload+48,73)
                d=run(d,1,10);d=run(d,4,10);d=run(d,3,10)
                words(d,120,1);d=run(d,2,10);words(d,120,0)
                d=run(d,2,9,exited);d=run(d,4,9)
                words(d,pool+24,2);d=run(d,3,7);words(d,pool+24,0)
                d=run(d,3,11,released);d=run(d,0,11);d=run(d,3,11);d=run(d,4,11)
    for fault in range(12):
        d=fixture(17,0);op=0;qa=base;status=1
        if fault==0:qa=0
        elif fault==1:qa+=1
        elif fault==2:op=5
        elif fault==3:words(d,160,5)
        elif fault==4:words(d,152,2)
        elif fault==5:words(d,40,1)
        elif fault==6:words(d,roots,0);status=0
        elif fault==7:op=1
        elif fault==8:op=2
        elif fault==9:words(d,pool+24,0,1);status=5
        elif fault==10:words(d,slots,(1<<32)-1);status=6
        elif fault==11:words(d,cell+16,5);status=2
        # WAIT acquires/releases a metadata token, including its generation.
        run(d,op,status,(lambda x:words(x,slots,1)) if fault==11 else None,qa)
    d=fixture(17,1);d=run(d,4,11,lambda x:words(x,160,4,2));run(d,0,11)
    d=fixture(17,1);entered(d);exited(d);words(d,160,0)
    d=run(d,0,7);d=run(d,4,7)
    def failed_enter_cleanup(x):released(x);words(x,160,0)
    run(d,3,0,failed_enter_cleanup)
    print(f'ARM rooted scalar: {count} exact-state access, abort, replay and cleanup cases PASS')

def verify_rooted_observation_owner(m):
    base=MEMORY+0x2000;ctx=256;pool=640;heap=704;page=832;slots=960;table=1024;cell=1056
    registry=1120;dslots=1152;desc=1184;bitmap=1264;segment=1280;roots=1344
    journal=1408;entries=1472;starts=2048;payload=4096
    receiver=(131104<<16)|0x1510;heap_base=base+payload-131072;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(value):
        d=bytearray(5120)
        words(d,0,base+ctx,base+segment,base+roots,1,101,0)
        words(d,128,base+registry,0,base+segment,base+roots+32,1,101,0,0,0)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,0x22)
        words(d,pool,base+heap,base+slots,1,1,0);words(d,heap,heap_base,base+page,1)
        words(d,page,heap_base,131072,262144,27,1,0,1,5,base+starts,128,131136,0,0,base+table)
        words(d,table,base+cell,1,1);words(d,cell,131104,27,0,0)
        words(d,registry,base+dslots,2);words(d,dslots,0,base+desc)
        words(d,desc,1,16,24,0,base+bitmap,0,1,0);words(d,bitmap,3)
        words(d,segment,17,101,base+roots,2,2,1,base+journal,0)
        words(d,roots,receiver,1,1,0,0,1,1,0);words(d,journal,base+entries,3,0,0,0)
        words(d,starts,4);words(d,payload,24,1,0,2,value)
        return d
    def entered(d):
        words(d,48,base+page,27,(1<<32)|1)
        words(d,72,receiver,0,base+ctx,131104,27,base+cell,0)
        words(d,184,1);words(d,ctx+144,27);words(d,page+96,1)
        words(d,pool+24,0,1);words(d,slots,1,base+page,27,0);words(d,cell+24,1)
    def exited(d):
        d[72:128]=bytes(56);words(d,ctx+144,0);words(d,cell+24,0);words(d,184,3)
    def released(d):
        d[48:72]=bytes(24);words(d,page+96,0);words(d,pool+24,1,0)
        words(d,slots+8,0,0,0);words(d,184,4)
    def run(d,op,status,change=None,qa=None):
        nonlocal count
        expected=bytearray(d)
        if change:change(expected)
        got=m.call('rooted_observation',base+pool,base if qa is None else qa,op)
        assert got==status,(count,op,got,status)
        actual=bytes(m.uc.mem_read(base,len(d)))
        assert actual==bytes(expected),(count,op,[(i,actual[i],expected[i]) for i in range(len(d)) if actual[i]!=expected[i]][:12])
        count+=1;return expected
    for value in (0,(131136<<16)|0x1510,(131168<<16)|0x8630,(131200<<16)|0x2520):
        for fault in range(4):
            d=fixture(value);result=(0,3,4,1)[fault]
            if fault==1:words(d,journal+16,3,3)
            elif fault==2:words(d,ctx+152,MASK)
            elif fault==3:words(d,payload+32,1)
            m.reset();m.uc.mem_write(base,bytes(d));d=run(d,0,8,entered)
            def observed(x):
                words(x,184,2,result)
                if not result:
                    words(x,176,value);words(x,roots+32,value);words(x,ctx+152,1)
                    words(x,journal+16,1,1);words(x,entries,0,value,101,0x22)
            d=run(d,1,10,observed);d=run(d,1,10);d=run(d,3,10)
            words(d,120,1);m.uc.mem_write(base,bytes(d));d=run(d,2,10)
            words(d,120,0);m.uc.mem_write(base,bytes(d));d=run(d,2,9,exited)
            words(d,pool+24,2);m.uc.mem_write(base,bytes(d));d=run(d,3,7)
            words(d,pool+24,0);m.uc.mem_write(base,bytes(d));d=run(d,3,11,released)
            d=run(d,0,11);d=run(d,3,11)
            d=run(d,4,0,lambda x:words(x,176,0,0,0))
    for fault in range(9):
        d=fixture(0);op=0;qa=base;status=1
        if fault==0:qa=0
        elif fault==1:qa+=1
        elif fault==2:op=5
        elif fault==3:words(d,184,5)
        elif fault==4:words(d,176,1)
        elif fault==5:words(d,152,base+roots)
        elif fault==6:words(d,roots,0);status=0
        elif fault==7:op=1
        elif fault==8:op=2
        m.reset();m.uc.mem_write(base,bytes(d));run(d,op,status,qa=qa)
    for fault in range(4):
        d=fixture(0);destination=(131136<<16)|0x1510
        words(d,cell+16,5 if fault==0 else destination|2)
        if fault==2:words(d,journal+16,3,3)
        elif fault==3:words(d,ctx+152,MASK)
        def advanced(x):
            # Acquisition/release consumes a slot generation even on failure.
            words(x,slots,1)
            if fault==1:
                words(x,roots,destination);words(x,ctx+152,1)
                words(x,journal+16,1,1);words(x,entries,receiver,destination,101,0x22)
        m.reset();m.uc.mem_write(base,bytes(d));run(d,0,(2,3,4,6)[fault],advanced)
    assert count==189
    print('PASS: ARM64 rooted observation owner 189 independent full-state protocol calls')


def verify_rooted_mark_owner(m):
    base=MEMORY+0x2000;ctx=384;pool=768;heap=832;page=960;slots=1088;table=1184;cell=1216
    queue=1280;task=1408;record=1536;segment=1792;root=1856;journal=1920;entries=1984
    starts=2304;other=3072;payload=4096;heap_base=base+payload-131072
    ref=(131104<<16)|0x1510;other_ref=(131168<<16)|0x1510;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(generation):
        d=bytearray(5120)
        words(d,0,base+ctx,base+segment,base+root,1,101,0)
        words(d,128,base+queue,generation,7,0x22,0,0,0)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,0x22)
        words(d,pool,base+heap,base+slots,2,1,0);words(d,slots,0,0,0,2)
        words(d,heap,heap_base,base+page,1)
        words(d,page,heap_base,131072,262144,27,1,0,0,5,base+starts,128,131184,0,0,base+table)
        words(d,table,base+cell,2,2);words(d,cell,131104,27,0,0,131168,27,0,0)
        words(d,queue,base+task,1,1,0,0,0,0,0,7,generation,0,0,0)
        words(d,record,1);words(d,other,2)
        words(d,segment,17,101,base+root,1,1,1,base+journal,0)
        words(d,root,ref,1,1,0);words(d,journal,base+entries,3,0,0,0)
        words(d,starts,68);words(d,payload,8,1,base+record,2)
        words(d,payload+64,8,1,base+other,2)
        return d
    def entered(d):
        words(d,48,base+page,27,(1<<32)|1)
        words(d,72,ref,0,base+ctx,131104,27,base+cell,0)
        words(d,160,1);words(d,ctx+144,27);words(d,page+96,1)
        words(d,pool+24,2,1);words(d,slots,1,base+page,27,0);words(d,cell+24,1)
    def routed(d,generation,result=0,phase=0x22):
        words(d,160,2,result)
        words(d,184,generation,base+88,base+page,phase,ref)
        if result:words(d,224,base+page,27,(1<<32)|1)
        else:words(d,248,base+record,ref,27,(1<<32)|1,0,0,0)
    def exited(d,result=0):
        d[72:128]=bytes(56);words(d,ctx+144,0);words(d,cell+24,0)
        words(d,160,4 if result else 3)
    def released(d,stage):
        d[48:72]=bytes(24);d[184:304]=bytes(120)
        words(d,160,stage);words(d,page+96,0);words(d,pool+24,1,0);words(d,slots+8,0,0,2)
    def run(d,op,status,change=None,name='rooted_mark',qa=None):
        nonlocal count
        e=bytearray(d)
        if change:change(e)
        actual=m.call(name,base+pool,base if qa is None else qa,op)
        assert actual==status,(count,name,op,actual,status)
        data=bytes(m.uc.mem_read(base,len(d)))
        assert data==bytes(e),(count,name,op,[(i,data[i],e[i]) for i in range(len(d)) if data[i]!=e[i]][:12])
        count+=1;return e
    for generation in (0,1):
        stamp=record+8+8*generation
        for mode in range(6):
            d=fixture(generation)
            if mode==1:words(d,stamp,7)
            m.reset();m.uc.mem_write(base,bytes(d));d=run(d,0,8,entered)
            d=run(d,1,11,lambda x:routed(x,generation));d=run(d,1,11)
            words(d,120,1);m.uc.mem_write(base,bytes(d));d=run(d,2,11)
            words(d,120,0);m.uc.mem_write(base,bytes(d));d=run(d,2,9,exited)
            if mode in (2,3):
                if mode==3:
                    words(d,queue+64,8);m.uc.mem_write(base,bytes(d));d=run(d,3,14)
                d=run(d,5,13,lambda x:released(x,6));d=run(d,5,13)
            elif mode==1:
                d=run(d,3,10,lambda x:words(x,160,4,0,2))
                words(d,pool+24,3);m.uc.mem_write(base,bytes(d));d=run(d,4,7)
                words(d,pool+24,2);m.uc.mem_write(base,bytes(d));d=run(d,4,12,lambda x:released(x,5))
            else:
                if mode==4:
                    words(d,queue+56,MASK);m.uc.mem_write(base,bytes(d))
                    d=run(d,3,16,lambda x:words(x,176,6))
                    words(d,queue+56,0);m.uc.mem_write(base,bytes(d))
                if mode==5:
                    # A real second queue lease protects a distinct fixture object.
                    words(d,slots+32,1,base+page,27,0);words(d,pool+24,0,2);words(d,page+96,2)
                    words(d,queue+16,0,1,1,1,0,1);words(d,other+8+8*generation,7)
                    words(d,task,0,1,1,base+other,other_ref,27,(1<<32)|2,0)
                    m.uc.mem_write(base,bytes(d));d=run(d,3,15,lambda x:words(x,176,3))
                def accepted(x):
                    x[48:72]=bytes(24);x[184:304]=bytes(120);words(x,160,5,0,0);words(x,stamp,7)
                    if mode==5:
                        words(x,queue+56,2);words(x,queue+80,base+record,base+record,1)
                        words(x,record+24+64*generation,0,2,1,base+record,ref,27,(1<<32)|1,0)
                    else:
                        words(x,queue+16,0,1,1,1,0,1)
                        words(x,task,0,1,1,base+record,ref,27,(1<<32)|1,0)
                d=run(d,3,12,accepted,name='rooted_mark_spill' if mode==5 else 'rooted_mark')
                d=run(d,0,12)
            d=run(d,6,0,lambda x:words(x,160,0,0,0))
        for result in (1,2,3):
            if result==2 and generation:continue
            d=fixture(generation);phase=0x33 if result==1 else 0x22
            words(d,152,phase)
            if result==2:words(d,page+48,1)
            elif result==3:words(d,payload+24,1)
            m.reset();m.uc.mem_write(base,bytes(d));d=run(d,0,8,entered)
            d=run(d,1,11,lambda x:routed(x,generation,result,phase))
            d=run(d,2,10,lambda x:exited(x,result));d=run(d,4,12,lambda x:released(x,5))
        d=fixture(generation);words(d,root,0);m.reset();m.uc.mem_write(base,bytes(d))
        run(d,0,12,lambda x:words(x,160,5,2))
    for fault in range(10):
        d=fixture(0);op=0;qa=base
        if fault==0:qa=0
        elif fault==1:qa+=1
        elif fault==2:op=7
        elif fault==3:words(d,160,7)
        elif fault==4:words(d,128,0)
        elif fault==5:words(d,136,2)
        elif fault==6:words(d,144,0)
        elif fault==7:words(d,152,8)
        elif fault==8:words(d,152,0)
        elif fault==9:op=3
        m.reset();m.uc.mem_write(base,bytes(d));run(d,op,1,qa=qa)
    print(f'PASS: ARM64 rooted mark owner {count} independent full-state ownership calls')


def verify_rooted_mark_set(m):
    base=MEMORY+0x2000;q=8192;ctx=384;pool=768;heap=832;page=960;slots=1088;table=1184;cell=1216
    queues=(1280,2080);tasks=(1408,2208);record=1536;segment=1792;root=1856;journal=1920;entries=1984
    starts=2304;payload=4096;heap_base=base+payload-131072;ref=(131104<<16)|0x1510;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def get(d,o):return struct.unpack_from('<Q',d,o)[0]
    def job(g):return q+120+304*g
    def fixture(phase,null=False):
        d=bytearray(9216)
        words(d,q,base+ctx,base+segment,base+root,1,101,0,base+queues[0],base+queues[1],7,9,phase)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,phase)
        words(d,pool,base+heap,base+slots,2,1,0);words(d,slots,0,0,0,2)
        words(d,heap,heap_base,base+page,1)
        words(d,page,heap_base,131072,262144,27,1,0,0,5,base+starts,128,131120,0,0,base+table)
        words(d,table,base+cell,1,1);words(d,cell,131104,27,0,0)
        for g in (0,1):words(d,queues[g],base+tasks[g],1,1,0,0,0,0,0,7+2*g,g,0,0,0)
        words(d,record,1);words(d,segment,17,101,base+root,1,1,1,base+journal,0)
        words(d,root,0 if null else ref,1,1,0);words(d,journal,base+entries,3,0,0,0)
        words(d,starts,4);words(d,payload,8,1,base+record,2)
        return d
    def initialized(d,mask):
        for g in (0,1):
            j=job(g);words(d,j,base+ctx,base+segment,base+root,1,101,0)
            words(d,j+128,base+queues[g],g,7+2*g,get(d,q+80))
        words(d,q+88,1 if mask else 2,mask,0,1 if mask&2 else 0)
    def reset(d):
        d[q+120:q+728]=bytes(608);words(d,q+88,0,0,0,0)
    def entered(d,g):
        j=job(g);index=get(d,pool+24);slot=slots+(index-1)*32;generation=get(d,slot)+1
        token=(generation<<32)|index;active=get(d,pool+32)+1;next_free=get(d,slot+24)
        words(d,j+48,base+page,27,token);words(d,j+72,ref,0,base+ctx,131104,27,base+cell,0)
        words(d,j+160,1);words(d,ctx+144,27);words(d,page+96,active)
        words(d,pool+24,next_free,active);words(d,slot,generation,base+page,27,0);words(d,cell+24,1)
    def routed(d,g):
        j=job(g);words(d,j+160,2)
        words(d,j+184,g,base+j+88,base+page,get(d,q+80),ref)
        words(d,j+248,base+record,ref,27,get(d,j+64),0,0,0)
    def exited(d,g):
        j=job(g);d[j+72:j+128]=bytes(56);words(d,ctx+144,0);words(d,cell+24,0);words(d,j+160,3)
    def completed(d,g,null=False):
        j=job(g)
        if null:words(d,j+160,5,2)
        else:
            token=get(d,j+64);d[j+48:j+72]=bytes(24);d[j+184:j+304]=bytes(120);words(d,j+160,5)
            words(d,record+8+8*g,7+2*g);words(d,queues[g]+16,0,1,1,1,0,1)
            words(d,tasks[g],0,1,1,base+record,ref,27,token,0)
        bits=get(d,q+104)|(1<<g);mask=get(d,q+96);words(d,q+104,bits)
        if bits==mask:words(d,q+88,2)
        else:words(d,q+112,1 if (mask&~bits)&2 else 0)
    def cancelled(d,g,lease=True):
        j=job(g)
        if lease:
            token=get(d,j+64);index=token&0xffffffff;slot=slots+(index-1)*32
            words(d,slot+8,0,0,get(d,pool+24));words(d,pool+24,index,get(d,pool+32)-1)
            words(d,page+96,get(d,page+96)-1);d[j+48:j+72]=bytes(24)
        d[j+184:j+304]=bytes(120);words(d,j+160,6);words(d,q+88,3)
    def run(d,op,status,change=None,qa=None):
        nonlocal count
        e=bytearray(d)
        if change:change(e)
        got=m.call('rooted_mark_set',base+pool,base+q if qa is None else qa,op)
        assert got==status,(count,op,got,status)
        data=bytes(m.uc.mem_read(base,len(d)))
        assert data==bytes(e),(count,op,[(i,data[i],e[i]) for i in range(len(d)) if data[i]!=e[i]][:12])
        count+=1;return e
    for young in range(8):
        for old in range(8):
            phase=(13<<16)|young|(old<<4);mask=(1 if 1<=young<=3 else 0)|(2 if 1<=old<=3 else 0)
            d=fixture(phase);m.reset();m.uc.mem_write(base,bytes(d))
            d=run(d,7,18 if mask else 17,lambda x:initialized(x,mask))
            if mask:d=run(d,8,22,lambda x:cancelled(x,1 if mask&2 else 0,False))
            d=run(d,9,0,reset)
    for null in (False,True):
        for mask in range(4):
            phase=(2 if mask&1 else 0)|(0x20 if mask&2 else 0)
            d=fixture(phase,null);m.reset();m.uc.mem_write(base,bytes(d))
            d=run(d,7,18 if mask else 17,lambda x:initialized(x,mask))
            for g in (1,0):
                if not mask&(1<<g):continue
                result=17 if (get(d,q+104)|(1<<g))==mask else 18
                if null:d=run(d,0,result,lambda x:completed(x,g,True))
                else:
                    d=run(d,0,8,lambda x:entered(x,g));d=run(d,1,11,lambda x:routed(x,g))
                    d=run(d,2,9,lambda x:exited(x,g));d=run(d,3,result,lambda x:completed(x,g))
            d=run(d,0,17)
            words(d,ctx+16,phase+0x10000);m.uc.mem_write(base,bytes(d));d=run(d,0,19)
            d=run(d,8,22,lambda x:words(x,q+88,3));d=run(d,9,0,reset)
    for queue_stale in (False,True):
        d=fixture(0x22);m.reset();m.uc.mem_write(base,bytes(d));d=run(d,7,18,lambda x:initialized(x,3))
        d=run(d,0,8,lambda x:entered(x,1));d=run(d,1,11,lambda x:routed(x,1));d=run(d,2,9,lambda x:exited(x,1))
        words(d,queues[1]+64 if queue_stale else ctx+16,10 if queue_stale else 0x10022)
        m.uc.mem_write(base,bytes(d));d=run(d,3,19);d=run(d,8,22,lambda x:cancelled(x,1));d=run(d,9,0,reset)
    d=fixture(0x22);words(d,payload+24,1);m.reset();m.uc.mem_write(base,bytes(d))
    d=run(d,7,18,lambda x:initialized(x,3));d=run(d,0,8,lambda x:entered(x,1))
    def waiting(x):
        j=job(1);words(x,j+160,2,3)
        words(x,j+184,1,base+j+88,base+page,0x22,ref)
        words(x,j+224,base+page,27,get(x,j+64))
    d=run(d,1,11,waiting)
    def wait_exited(x):exited(x,1);words(x,job(1)+160,4)
    d=run(d,2,10,wait_exited)
    def wait_released(x):
        j=job(1);x[j+48:j+72]=bytes(24);x[j+184:j+304]=bytes(120)
        words(x,j+160,0,0,0);words(x,pool+24,1,0);words(x,page+96,0);words(x,slots+8,0,0,2)
    d=run(d,4,20,wait_released)
    words(d,payload+24,2);m.uc.mem_write(base,bytes(d))
    d=run(d,0,8,lambda x:entered(x,1));d=run(d,1,11,lambda x:routed(x,1))
    d=run(d,2,9,lambda x:exited(x,1));d=run(d,3,18,lambda x:completed(x,1))
    d=run(d,8,22,lambda x:cancelled(x,0,False));d=run(d,9,0,reset)
    for fault in range(13):
        d=fixture(0x22);op=7;qa=base+q;status=21
        if fault==0:qa=0
        elif fault==1:qa+=1
        elif fault==2:op=5
        elif fault==3:op=6
        elif fault==4:op=10
        elif fault==5:words(d,q+88,4)
        elif fault==6:words(d,q,0)
        elif fault==7:words(d,q+40,1)
        elif fault==8:words(d,ctx+144,27)
        elif fault==9:words(d,q+48,0)
        elif fault==10:words(d,q+72,0)
        elif fault==11:words(d,q+80,8)
        elif fault==12:words(d,ctx+16,0x10022);status=19
        m.reset();m.uc.mem_write(base,bytes(d));run(d,op,status,qa=qa)
    print(f'PASS: ARM64 rooted mark set {count} independent requirement, receipt and restart calls')


def verify_rooted_load_admit(m):
    base=MEMORY+0x2000;ctx=2048;registry=2368;h=2464;p=2528;a=2592;queues=(2688,2816)
    count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(phase=0x22,recapture=False,admitted=False):
        d=bytearray(4096)
        put(d,0,base+ctx,base+3072,base+3136,1,101,0)
        put(d,144,base+3072,base+3168,1,101,0)
        put(d,200+48,91,92,93,94,95)
        put(d,1056,8 if recapture else 0);put(d,1096,int(admitted))
        if recapture:put(d,184,4,0)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,phase)
        put(d,ctx+280,2 if admitted else 0)
        put(d,registry+48,phase);put(d,h,4,1,0,0,0,0,17,base+p)
        put(d,p,base+ctx,17,4,1,base+h,0,0)
        put(d,a,base+registry,base+h,base+p,
            base+queues[0] if 1<=(phase&15)<=3 else 1,
            base+queues[1] if 1<=((phase>>4)&15)<=3 else 1)
        for g in (0,1):put(d,queues[g]+64,7+2*g,g)
        put(d,3072,17,101,base+3136,2,2,1,0,0)
        put(d,3136,0,1,1,0,0,1,1,0)
        return d
    def run(d,status,expected=None,ap=None,qp=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d))
        got=m.call('rooted_load_admit',base+a if ap is None else ap,base if qp is None else qp)
        assert got==status,(count,got,status)
        actual=bytes(m.uc.mem_read(base,len(d)));expected=d if expected is None else expected
        assert actual==bytes(expected),(count,[(i,actual[i],expected[i]) for i in range(len(d)) if actual[i]!=expected[i]][:12])
        count+=1
    for phases in range(64):
        for epochs in (0,31):
            phase=(7<<16)|(epochs<<8)|(phases&7)|((phases>>3)<<4)
            for recapture in (False,True):
                for admitted in (False,True):
                    d=fixture(phase,recapture,admitted);expected=bytearray(d)
                    for g in (0,1):
                        active=1<=((phase>>(4*g))&15)<=3
                        put(expected,248+8*g,base+queues[g] if active else 0)
                        put(expected,264+8*g,7+2*g if active else 0)
                    put(expected,280,phase);put(expected,1096,1)
                    if not admitted:put(expected,ctx+280,1)
                    run(d,0,expected);run(expected,0) # admitted repeats do not increment
    failures=[(1056,1,2),(1072,1,2),(1080,1,2),(1096,2,2),(288,1,2),
        (184,1,2),(176,1,2),(40,1,2),(0,0,2),(0,base+ctx+1,2),
        (a,0,2),(a+8,0,2),(a+16,0,2),(ctx+8,0,2),(ctx+144,1,2),
        (p,0,2),(p+32,0,2),(p+24,0,2),(p+8,18,2),
        (h+8,0,2),(h+56,0,2),(h+16,2,2),(h+40,2,2),(h+48,16,2),
        (p+16,5,2),(p+16,3,1),(h+24,1,2),(h+32,1,2),
        (registry+48,0xe022,2),(ctx+16,0x10022,1),
        (a+24,0,2),(a+32,1,2),(queues[0]+72,1,2),(queues[1]+64,0,2),(ctx+280,MASK,3)]
    for off,value,status in failures:
        d=fixture();put(d,off,value);run(d,status)
    for ac in (0,928,320,624):
        for off in range(48,128,8):
            d=fixture();put(d,ac+off,1);run(d,2)
    for fault in range(5):
        d=fixture()
        if fault==0:put(d,h,5);put(d,h+24,1);put(d,h+40,1);status=1
        elif fault==1:put(d,1096,1);status=2
        elif fault==2:struct.pack_into('<I',d,ctx+4,1);status=2
        elif fault==3:struct.pack_into('<I',d,ctx,m.context_layout['abi_version']-1);status=2
        else:put(d,1056,8);put(d,184,4,3);status=2
        run(d,status)
    for ap,qp in ((0,base),(base+a+1,base),(base+a,0),(base+a,base+1)):
        run(fixture(),2,ap=ap,qp=qp)
    print(f'PASS: ARM64 rooted load snapshot admission {count} independent full-state captures/rejections')


def verify_rooted_load(m,root_value=False):
    # The new dispatcher is compared with an explicit Python protocol model.
    # Model leaf calls use the separately verified emitted leaves; comparisons
    # cover the whole fixture, including guards, lease slots and queue transfers.
    base=MEMORY+0x2000;q=8192;obs=q;marks=q+200;pub=q+928
    ctx=256;pool=640;heap=704;page=832;slots=960;table=1088;cells=1120
    registry=1200;dslots=1232;desc=1280;bitmap=1360;segment=1408;roots=1472
    journal=1536;entries=1600;queues=(1792,1920);tasks=(2048,2112);record=2240
    starts=3072;payload=4096;heap_base=base+payload-131072;count=0
    receiver=(131104<<16)|0x1510;child=(131168<<16)|0x8630
    def get(d,o):return struct.unpack_from('<Q',d,o)[0]
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(phase,value=child):
        d=bytearray(10240)
        put(d,q,base+ctx,base+segment,base+roots,1,101,0)
        put(d,q+128,base+registry,0,base+segment,base+roots+32,1,101,0,0,0)
        put(d,marks+48,base+queues[0],base+queues[1],7,9,phase)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,phase)
        put(d,pool,base+heap,base+slots,4,1,0)
        for i in range(4):put(d,slots+32*i,0,0,0,i+2 if i<3 else 0)
        put(d,heap,heap_base,base+page,1)
        put(d,page,heap_base,131072,262144,27,1,0,0,5,base+starts,128,131184,0,0,base+table)
        put(d,table,base+cells,2,2);put(d,cells,131104,27,0,0,131168,27,0,0)
        put(d,registry,base+dslots,2);put(d,dslots,0,base+desc)
        put(d,desc,1,16,24,0,base+bitmap,0,1,0);put(d,bitmap,3)
        put(d,segment,17,101,base+roots,2,2,1,base+journal,0)
        put(d,roots,receiver,1,1,0,0,1,1,0);put(d,journal,base+entries,3,0,0,0)
        put(d,starts,68);put(d,payload,24,1,0,2,value)
        put(d,payload+64,8,1,base+record,2);put(d,record,1)
        for g in (0,1):put(d,queues[g],base+tasks[g],1,1,0,0,0,0,0,7+2*g,g,0,0,0)
        if root_value:
            d[q+8:q+48]=d[q+144:q+184]
            put(d,q+128,0,0);put(d,roots+32,value)
            put(d,payload+32,1) # invalid reference field; must never be observed
        return d
    def current_job(d):return marks+120+304*get(d,marks+112)
    def action(d):
        s=get(d,q+1056);cancel=get(d,q+1072)
        if s>16 or cancel>1 or get(d,q+1080)>1 or get(d,q+1096)>1:return 44
        if 9<=s<=12:return {9:35,10:36,11:42,12:43}[s]
        if s==0:
            js=get(d,obs+184)
            if js>4:return 44
            a=33 if js in (1,2) else 32
        elif s==2:
            if get(d,marks+112)>1:return 44
            a={1:33,2:33,3:34}.get(get(d,current_job(d)+160),32)
        elif s in (4,5,14,15):a=33
        elif s==8 and not cancel:return 37
        else:a=32
        return 32 if cancel and a!=33 else a
    def model_inner(d,op):
        def stage(s):put(d,q+1056,s)
        def leaf(name,first,second,operation=0,last=True):
            m.uc.mem_write(base,bytes(d))
            r=m.call(name,first,second,operation)
            d[:]=m.uc.mem_read(base,len(d))
            if last:put(d,q+1064,r)
            return r
        def clean(a,operation):return leaf('root_access_cleanup',0 if not operation else base+pool,base+a,operation,False)
        def marked(operation):return leaf('rooted_mark_set',base+pool if op==32 else 0,base+marks,operation)
        def lease(a):return any(get(d,a+48+i) for i in (0,8,16))
        def retry(r):return {2:38,4:39,5:40,6:41,3:32}.get(r)
        a=action(d);s=get(d,q+1056)
        if a==44:return a
        if root_value:
            if s>=13 or any(get(d,obs+off) for off in (128,136,176,192)) or get(d,obs+184) not in (0,4):return 44
            if any(get(d,obs+off) for off in range(48,128,8)) or d[obs+8:obs+48]!=d[obs+144:obs+184]:return 44
        if op==47:
            if s!=10 or get(d,q+1072)!=1 or get(d,q+1080) or get(d,q+1096) or get(d,obs+184) not in (0,3,4) or get(d,marks+88) not in (0,3):return 44
            context=get(d,obs)
            if not context or context&7:return 44
            c=context-base
            if unpack('I',d,c)[0]!=m.context_layout['abi_version'] or unpack('I',d,c+4)[0]!=0 or not get(d,c+8) or get(d,c+144):return 44
            for ac in (obs,pub,marks+120,marks+424):
                if any(get(d,ac+i) for i in range(48,128,8)):return 44
            if get(d,marks+88)==3 and marked(9)!=0:return 44
            captured=get(d,obs+184)>=3 and get(d,obs+192)==0
            d[pub:pub+128]=bytes(128);put(d,q+1064,0,0);put(d,q+1088,0)
            if captured:put(d,obs+184,4);stage(8);return 37
            put(d,obs+176,0,0,0);stage(0);return 32
        if a in (35,36,42,43):return a
        if op==45:put(d,q+1072,1);return action(d)
        if op==46:
            if s!=8 or get(d,q+1072):return 44
            stage(1);return 32
        if op!=a or op==37:return 44
        if not get(d,q+1096) and not get(d,q+1072) and s in (0,1) and op==32:
            context=get(d,obs)
            if not context or context&7:return 44
            c=context-base
            if unpack('I',d,c)[0]!=m.context_layout['abi_version'] or unpack('I',d,c+4)[0]!=0 or not get(d,c+8) or get(d,c+144):return 44
            producers=get(d,c+280)
            if producers==MASK:return 41
            put(d,c+280,producers+1);put(d,q+1096,1)
        if get(d,q+1072) and op==32:
            ac=obs if s==0 or s>=13 else pub
            if lease(ac) and clean(ac,1)!=0:return 32
            if get(d,marks+88) in (1,2) and marked(8)!=22:return 32
            stage(11 if get(d,q+1080) else 10);return action(d)
        if s==0:
            if root_value:put(d,obs+184,4);stage(1);return 32
            js=get(d,obs+184)
            if js==0 and lease(obs) and clean(obs,1)!=0:return 32
            r=leaf('rooted_observation',base+pool if op==32 else 0,base+obs,min(js,3))
            if r==11:
                result=get(d,obs+192)
                if result==0:stage(1);return 32
                if result==3 and leaf('rooted_observation',base+pool,base+obs,4)==0:return 39
                stage(11);return 42
            if js==0:
                if r==0:stage(12);return 43
                if retry(r) is not None:return retry(r)
                if r==7:return 32
                if r!=8:stage(11);return 42
            return action(d)
        if s==1:
            put(d,marks,get(d,obs));d[marks+8:marks+48]=d[obs+144:obs+184];put(d,marks+40,0)
            r=marked(7);stage({18:2,17:3,19:8}.get(r,11));return action(d)
        if s==2:
            j=current_job(d);js=get(d,j+160)
            if js==0 and lease(j):stage(7);return 32
            r=marked(min(js,4))
            if r==17:stage(3)
            elif r in (19,7):stage(7)
            elif r==20:return 38
            elif r in (15,16):put(d,q+1088,38 if r==15 else 41);stage(7)
            elif r in (21,1):put(d,q+1072,1,1)
            elif retry(r) is not None:return retry(r)
            return action(d)
        if s==7:
            if get(d,marks+88) in (1,2) and marked(8)!=22:return 32
            if get(d,marks+88)==3 and marked(9)!=0:return 32
            stage(8);event=get(d,q+1088);put(d,q+1088,0);return event or 37
        if s==3:
            if lease(pub) and clean(pub,1)!=0:return 32
            if get(d,ctx+16)!=get(d,marks+80):stage(7);return 32
            d[pub:pub+48]=d[marks:marks+48]
            r=leaf('root_access',base+pool,base+pub)
            if r==8:stage(4)
            elif r==0:stage(9)
            elif r!=7:
                if retry(r) is not None:return retry(r)
                stage(11)
            return action(d)
        if s==4:
            if not get(d,q+1072):leaf('rooted_mark_publish',base+marks,base+pub)
            stage(5);return 33
        if s==5:
            if clean(pub,0)!=0:return 33
            stage(6);return 32
        if s==6:
            if clean(pub,1)!=0:return 32
            stage(7 if get(d,ctx+16)!=get(d,marks+80) else {0:9 if root_value else 13,2:3}.get(get(d,q+1064),11))
            return action(d)
        if s==13:
            if lease(obs) and clean(obs,1)!=0:return 32
            if get(d,ctx+16)!=get(d,marks+80):stage(7);return 32
            r=leaf('root_access',base+pool,base+obs)
            if r==8:stage(14)
            elif r!=7:
                if retry(r) is not None:return retry(r)
                stage(11)
            return action(d)
        if s==14:
            if not get(d,q+1072):leaf('rooted_load_heal',base+q,0)
            stage(15);return 33
        if s==15:
            if clean(obs,0)!=0:return 33
            stage(16);return 32
        if s==16:
            if clean(obs,1)!=0:return 32
            stage(7 if get(d,ctx+16)!=get(d,marks+80) else {0:9,2:9,3:3}.get(get(d,q+1064),11))
            return action(d)
        return 44
    def model(d,op):
        result=model_inner(d,op)
        if result in (35,36,42,43) and get(d,q+1096):
            c=get(d,obs)-base;producers=get(d,c+280)
            if not producers:return 44
            put(d,c+280,producers-1);put(d,q+1096,0)
        return result
    def step(d,op):
        nonlocal count
        before=bytes(d);m.reset();m.uc.mem_write(base,before)
        expected=bytearray(d);status=model(expected,op)
        m.reset();m.uc.mem_write(base,before)
        result=m.call('rooted_value' if root_value else 'rooted_load',base+pool if op in (32,47) else 0,base+q,op)
        actual=bytes(m.uc.mem_read(base,len(d)))
        assert result==status,(count,op,get(d,q+1056),result,status)
        assert actual==bytes(expected),(count,op,[(i,actual[i],expected[i]) for i in range(len(d)) if actual[i]!=expected[i]][:16])
        count+=1;return expected,status
    for mask in range(4):
        phase=(17<<16)|(2 if mask&1 else 0)|(0x20 if mask&2 else 0)
        for value in (0,child):
            for boundary in (None,*range(26)):
                d=fixture(phase,value)
                for i in range(40):
                    a=action(d)
                    if a in (35,36):break
                    if i==boundary:d,_=step(d,45);a=action(d)
                    d,_=step(d,34 if a==33 else 33) # wrong lock domain
                    d,r=step(d,a)
                    assert r in (32,33,34,35,36),(mask,value,boundary,i,r)
                else:raise AssertionError('load owner did not finish')
                assert not get(d,ctx+144) and not get(d,ctx+280) and not get(d,q+1096)
                assert get(d,pool+32)==sum(get(d,queue+40)+get(d,queue+48) for queue in queues)
                for access in (obs,pub,marks+120,marks+424):
                    assert not any(get(d,access+48+i) for i in (0,8,16))
                if action(d)==35:
                    expected=0 if not value else (value&~0xffff)|(value&0x30)|0x1500
                    assert get(d,roots+32)==expected
                    assert get(d,pool+32)==(mask.bit_count() if value else 0)
                d,_=step(d,32) # stable terminal
                if action(d)==36:
                    revision=get(d,ctx+152);captured=get(d,obs+184)>=3 and get(d,obs+192)==0
                    struct.pack_into('<I',d,ctx+4,1);d,r=step(d,47);assert r==44
                    struct.pack_into('<I',d,ctx+4,0);put(d,ctx+16,phase+0x10000)
                    d,r=step(d,47);assert r==(37 if captured else 32)
                    for _ in range(40):
                        a=action(d)
                        if a==35:break
                        if a==37:put(d,marks+80,phase+0x10000);d,_=step(d,46)
                        else:d,_=step(d,a)
                    else:raise AssertionError('rearmed load did not finish')
                    assert get(d,ctx+152)==revision+(not captured and not root_value)
                    assert not get(d,ctx+144) and not get(d,ctx+280) and not get(d,q+1096)
    if root_value:
        for off,val in ((128,base+registry),(136,8),(176,child),(184,1),(192,3),(1056,13),(16,base+roots)):
            d=fixture(0x22);put(d,q+off,val);before=bytes(d)
            d,r=step(d,32);assert r==44 and bytes(d)==before
        for off in range(48,128,8):
            d=fixture(0x22);put(d,obs+off,1);before=bytes(d)
            d,r=step(d,32);assert r==44 and bytes(d)==before
        d=fixture(0x22)
        while action(d)!=34:d,_=step(d,action(d))
        put(d,ctx+16,0x10022);d,_=step(d,34);d,r=step(d,32)
        assert r==37 and not get(d,pool+32) and get(d,ctx+280)==1
        put(d,marks+80,0x10022);d,_=step(d,46)
        while action(d)!=35:d,_=step(d,action(d))
        assert not get(d,ctx+280) and get(d,payload+32)==1
        print(f'PASS: ARM64 rooted value owner {count} full-state preparation, cancellation and restart comparisons')
        return
    # Independent field-CAS outcomes: a logical update wins without changing
    # the captured load result; stale result color requests root repair.
    for competing in (False,True):
        d=fixture(0x22)
        while get(d,q+1056)!=14:d,_=step(d,action(d))
        original=get(d,obs+176);result=get(d,roots+32)
        if competing:put(d,payload+32,0)
        d,r=step(d,33)
        assert get(d,q+1064)==(2 if competing else 0)
        assert get(d,payload+32)==(0 if competing else result)
        assert get(d,obs+176)==original and get(d,roots+32)==result
        while action(d)!=35:d,_=step(d,action(d))
        assert get(d,roots+32)==result and not get(d,ctx+280)
    d=fixture(0x22)
    while get(d,q+1056)!=14:d,_=step(d,action(d))
    put(d,roots+32,child)
    d,_=step(d,33);assert get(d,q+1064)==3 and get(d,payload+32)==child
    for _ in range(32):
        if action(d)==35:break
        d,_=step(d,action(d))
    else:raise AssertionError('self-heal color retry did not finish')
    assert get(d,payload+32)==get(d,roots+32)==((child&~0xffff)|0x1530)
    # Journal pressure, failed EXIT/release, restart with a pending offer.
    d=fixture(0x22);put(d,journal+16,3,3)
    for op in (32,33):d,_=step(d,op)
    put(d,obs+120,1);d,_=step(d,33);put(d,obs+120,0);d,_=step(d,33)
    saved=get(d,pool+24);put(d,pool+24,5);d,_=step(d,32);put(d,pool+24,saved)
    d,r=step(d,32);assert r==39 and get(d,pool+32)==0
    put(d,journal+32,3)
    while action(d)!=34:d,_=step(d,action(d))
    put(d,ctx+16,0x10022);d,_=step(d,34);d,r=step(d,32)
    assert r==37 and not get(d,pool+32)
    revision=get(d,ctx+152);put(d,marks+80,0x10022);d,_=step(d,46)
    while action(d)!=35:d,_=step(d,action(d))
    assert get(d,ctx+152)==revision
    # Independent rejection expectations do not use any child leaf.
    for address in (0,base+q+1):
        d=fixture(0x22);m.reset();m.uc.mem_write(base,bytes(d))
        assert m.call('rooted_load',base+pool,address,32)==44
        assert bytes(m.uc.mem_read(base,len(d)))==bytes(d);count+=1
    for off,value in ((1056,17),(1072,2),(1080,2),(184,5)):
        d=fixture(0x22);put(d,q+off,value);d,r=step(d,32);assert r==44
    # Rearm must reject any retained access scratch before resetting the owner.
    for ac in (obs,pub,marks+120,marks+424):
        for off in range(48,128,8):
            d=fixture(0x22);put(d,q+1056,10,0,1);put(d,ac+off,1)
            before=bytes(d);d,r=step(d,47);assert r==44 and bytes(d)==before
    for off,value in ((obs+184,1),(obs+184,2),(marks+88,1),(marks+88,2),
                      (q+1072,0),(q+1080,1),(obs,0),(obs,base+ctx+1),(ctx+8,0),(ctx+144,1)):
        d=fixture(0x22);put(d,q+1056,10,0,1);put(d,off,value)
        before=bytes(d);d,r=step(d,47);assert r==44 and bytes(d)==before
    # Admission rejects wrap and coordinates the real worker/handshake leaves.
    d=fixture(0x22);put(d,ctx+280,MASK);before=bytes(d);d,r=step(d,32)
    assert r==41 and bytes(d)==before
    d=fixture(0x22)
    for op in (32,33,33,32):d,_=step(d,op)
    assert get(d,ctx+280)==1 and not get(d,ctx+144)
    h=6000;p=6064;registry=6128;request=6224
    put(d,h,0,1,0,0,0,0,17,base+p)
    put(d,p,base+ctx,17,0,1,base+h,0,0)
    put(d,registry+48,0x22);put(d,request,base+h,base+p)
    def admitted_call(name,args,result,changes=()):
        nonlocal d
        expected=bytearray(d)
        for off,value,width in changes:struct.pack_into('<I' if width==4 else '<Q',expected,off,value)
        m.reset();m.uc.mem_write(base,bytes(d));assert m.call(name,*args)==result
        assert bytes(m.uc.mem_read(base,len(d)))==bytes(expected)
        d=expected
    admitted_call('worker_step',(base+registry,base+request,1),2)
    admitted_call('context_transition',(base+ctx,0,2),5)
    admitted_call('handshake_step',(base+h,0,6),0,((h,1,8),(h+24,1,8),(h+40,1,8)))
    admitted_call('worker_step',(base+registry,base+request,0),2)
    admitted_call('handshake_step',(base+h,base+p,1),1)
    d,_=step(d,45);d,r=step(d,32);assert r==36 and not get(d,ctx+280)
    admitted_call('worker_step',(base+registry,base+request,0),1,((ctx+4,1,4),(p+16,1,8),(h+32,1,8)))
    admitted_call('handshake_step',(base+h,0,7),0,((h+24,0,8),(h+32,0,8),(h+40,0,8)))
    admitted_call('worker_step',(base+registry,base+request,0),0,((ctx+4,0,4),))
    print('PASS: ARM64 private load admission blocks ACK/native/state publication until cancellation (8 protocol calls)')
    print(f'PASS: ARM64 rooted load owner {count} full-state dispatcher comparisons')


def verify_barrier_assist(m):
    base=MEMORY+0x2000;ctx=2048;runtime=4400;pool=5000;heap=5064;pages=5200
    def put(o,*v):m.uc.mem_write(base+o,struct.pack('<'+'Q'*len(v),*v))
    def get(o):return struct.unpack('<Q',m.uc.mem_read(base+o,8))[0]
    native={'held':False,'pairs':0}
    def callback(uc,address,size,unused):
        if address not in (STUBS+0x700,STUBS+0x704):return
        assert uc.reg_read(r.UC_ARM64_REG_SP)%16==0 and uc.reg_read(X[0])==base+4600
        if address==STUBS+0x700:assert not native['held'] and not get(ctx+144);native['held']=True
        else:assert native['held'];native['held']=False;native['pairs']+=1
        for i in range(18):uc.reg_write(X[i],0xbad00000+i)
    hook=m.uc.hook_add(UC_HOOK_CODE,callback);cases=0
    for head in (0,2):
        for action in (83,84,85):
            for fault in range(16):
                m.reset();m.uc.mem_write(base,bytes(12288));page=pages+112*head
                heap_base=0x100000000;start=(head+1)<<17;limit=(head+2+(head==2))<<17
                m.uc.mem_write(base+ctx,struct.pack('<IIQQ',m.context_layout['abi_version'],0,17,0x10022))
                put(ctx+24,heap_base+start,heap_base+start+48,heap_base+limit)
                put(ctx+48,base+6000,1,1,0,0);put(ctx+88,base+6048,1,1,0,0)
                put(pool,base+heap,base+6200,2,0,2)
                put(heap,heap_base,base+pages,4)
                put(page,heap_base,start,limit,27,1024,17,0,1,base+6400,(limit-start)>>10,start+48,head,2,0)
                put(runtime,base+pool,0,base+4600,0,0,0,STUBS+0x700,STUBS+0x704)
                buffer=ctx+(48 if action==83 else 88);argument=base+runtime;context=base+ctx;op=action
                if fault==1:put(ctx+144,27)
                elif fault==2:m.uc.mem_write(base+ctx,struct.pack('<I',m.context_layout['abi_version']+1))
                elif fault==3:m.uc.mem_write(base+ctx+4,struct.pack('<I',1))
                elif fault==4:put(ctx+8,0)
                elif fault==5:argument=0
                elif fault==6:op=82
                elif action==85:
                    if fault==7:put(runtime,0)
                    elif fault==8:put(runtime+16,0)
                    elif fault==9:put(runtime+48,0)
                    elif fault==10:put(pool,0)
                    elif fault==11:put(heap+8,(1<<64)-8)
                    elif fault==12:put(page+88,head+1)
                    elif fault==13:put(ctx+24,heap_base+start+16)
                    elif fault==14:put(page+40,18)
                    elif fault==15:put(page+48,1)
                else:
                    if fault==7:put(buffer+8,0)
                    elif fault==8:put(buffer,0)
                    elif fault==9:put(buffer+24,2)
                    elif fault==10:put(buffer+32,1)
                    elif fault==11:put(buffer+16,2)
                    elif fault==12:put(buffer,base+6001)
                    elif fault==13:context+=1
                    elif fault==14:op=86
                    elif fault==15:put(buffer,(1<<64)-8)
                before=bytearray(m.uc.mem_read(base,12288));expected=before[:];pairs=native['pairs']
                if not fault:
                    if action==85:
                        struct.pack_into('<QQQ',expected,ctx+24,0,0,0);struct.pack_into('<Q',expected,page+40,0);struct.pack_into('<Q',expected,page+56,5)
                    else:struct.pack_into('<Q',expected,buffer+24,1)
                assert m.call('barrier_assist',argument,context,op)==(44 if fault else 82),(head,action,fault)
                assert bytes(m.uc.mem_read(base,12288))==bytes(expected),(head,action,fault)
                assert not native['held'] and native['pairs']==pairs+(action==85 and (fault==0 or fault>=10))
                cases+=1
    m.uc.hook_del(hook)
    print(f"PASS: ARM64 barrier assistance {cases} exact-state cases, {native['pairs']} balanced heap-lock pairs")


def verify_rooted_load_dispatch(m):
    base=MEMORY+0x2000;ctx=2048;registry=4096;hs=4192;participant=4256;admission=4320;runtime=4400
    def put(o,*v):m.uc.mem_write(base+o,struct.pack('<'+'Q'*len(v),*v))
    def get(o):return struct.unpack('<Q',m.uc.mem_read(base+o,8))[0]
    native={'held':0,'pairs':0}
    def callback(uc,address,size,unused):
        if address not in (STUBS+0x700,STUBS+0x704):return
        assert uc.reg_read(r.UC_ARM64_REG_SP)%16==0
        lock=uc.reg_read(X[0]);assert lock in (base+4600,base+4608)
        if address==STUBS+0x700:
            assert not native['held'] and not get(ctx+144);native['held']=lock
        else:
            assert native['held']==lock;native['held']=0;native['pairs']+=1
        for i in range(18):uc.reg_write(X[i],0xbad00000+i)
    hook=m.uc.hook_add(UC_HOOK_CODE,callback);cases=0
    for recapture in (False,True):
        for fault in range(9):
            m.reset();m.uc.mem_write(base,bytes(8192))
            m.uc.mem_write(base+ctx,struct.pack('<IIQQ',m.context_layout['abi_version'],0,17,0x10000))
            put(0,base+ctx,base+3072,base+3168,1,101,0)
            put(144,base+3072,base+3200,1,101,0)
            put(3072,17,101,base+3168,2,2,1,base+3264,0)
            put(3168,0,1,1,0,0,1,1,0);put(3264,base+3328,3,0,0,0)
            put(registry+48,0x10000);put(hs,0,1,0,0,0,0,17,base+participant)
            put(participant,base+ctx,17,0,1,base+hs,0,0)
            put(admission,base+registry,base+hs,base+participant,0,0)
            put(runtime,base+5000,base+admission,base+4600,base+4608,base+4616,base+4624,STUBS+0x700,STUBS+0x704)
            if recapture:put(1056,8);put(184,4)
            expected=32
            if fault==1:put(hs,1,1,0,1,0,1,17,base+participant);expected=38
            elif fault==2:put(runtime+8,0);expected=44
            elif fault==3:put(runtime+24,0);expected=44
            elif fault==4:put(runtime+48,0);expected=44
            elif fault==5:put(runtime+56,0);expected=44
            elif fault==6:put(ctx+280,(1<<64)-1);expected=41
            elif fault==7:put(participant,0);expected=44
            elif fault==8:put(ctx+144,27);expected=44
            before=bytes(m.uc.mem_read(base,1104));pairs=native['pairs']
            assert m.call('rooted_load_dispatch',base+runtime,base,0)==expected
            assert not native['held'] and native['pairs']==pairs+(fault in (0,1,6,7))
            if fault:
                assert bytes(m.uc.mem_read(base,1104))==before;cases+=1;continue
            assert get(1096)==1 and get(ctx+280)==1 and get(280)==0x10000
            assert get(1056)==(1 if recapture else 0)
            assert m.call('rooted_load_dispatch',base+runtime,base,1)==32
            assert m.call('rooted_load_dispatch',base+runtime,base,0)==36
            assert get(ctx+280)==0
            saved=bytes(m.uc.mem_read(base,1104));m.uc.mem_write(base+ctx+4,struct.pack('<I',1))
            assert m.call('rooted_load_dispatch',base+runtime,base,2)==44
            assert bytes(m.uc.mem_read(base,1104))==saved
            m.uc.mem_write(base+ctx+4,struct.pack('<I',0))
            assert m.call('rooted_load_dispatch',base+runtime,base,2)==(37 if recapture else 32)
            assert m.call('rooted_load_dispatch',base+runtime,base,0)==32
            assert get(1096)==1 and get(ctx+280)==1
            assert m.call('barrier_poll',base+runtime,base,0)==80
            assert get(ctx+280)==0;cases+=1
    # Poll terminal requests without replaying them. Assistance returns keep
    # the request unchanged; the runtime caller must perform that work.
    pristine=bytes(m.uc.mem_read(base,8192));poll_cases=0
    for fault in range(9):
        m.reset();m.uc.mem_write(base,pristine);put(1056,9,0,0,0,0,0)
        put(ctx+144,0);put(ctx+280,0);expected=35
        if fault==1:put(ctx+280,1);expected=82
        elif fault==2:put(ctx+24,8);expected=85
        elif fault==3:put(ctx+48+16,1);expected=83
        elif fault==4:put(ctx+88+16,1);expected=84
        elif fault==5:put(hs,1,1,0,1,0,1,17,base+participant);expected=81
        elif fault==6:put(participant,0);expected=44
        elif fault==7:put(runtime+48,0);expected=44
        elif fault==8:put(ctx+144,27);expected=44
        before=bytes(m.uc.mem_read(base,1104));pairs=native['pairs']
        assert m.call('barrier_poll',base+runtime,base,0)==expected,(fault,expected)
        assert bytes(m.uc.mem_read(base,1104))==before and not native['held']
        assert native['pairs']==pairs+(fault not in (7,8))
        if fault==5:assert get(hs+32)==1 and struct.unpack('<I',m.uc.mem_read(base+ctx+4,4))[0]==1
        poll_cases+=1
    print(f'PASS: ARM64 barrier poll {poll_cases} assistance/terminal cases plus cancelled-load rearm')
    m.uc.hook_del(hook)
    print(f"PASS: ARM64 load native dispatch {cases} admission/rearm cases, {native['pairs']} balanced caller-clobber lock pairs")


def verify_rooted_store(m,dispatch=False):
    # End-to-end protocol checks use an independent action/obligation oracle.
    # The individual leaves also have exact-image tests elsewhere in this file.
    base=MEMORY+0x2000;ctx=2048;pool=2432;heap=2496;page=2624;slots=2752
    table=3072;cell=3104;registry=3168;dslots=3200;desc=3232;bitmap=3312
    segment=3360;roots=3424;journal=3552;entries=3616;starts=5120
    satb=6144;remembered=6400;payload=8192;job=9216;out=9280;lease=9344
    obs=1104;att=1304;stage=1672;ref=(131104<<16)|0x1510;count=0;schedules=0;restarts=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def get(o):return struct.unpack('<Q',m.uc.mem_read(base+o,8))[0]
    def put(o,*v):m.uc.mem_write(base+o,struct.pack('<'+'Q'*len(v),*v))
    def fixture(desired,previous):
        d=bytearray(12288)
        words(d,0,base+ctx,base+segment,base+roots+32,1,101,0)
        words(d,144,base+segment,base+roots+32,1,101,0)
        words(d,280,0x10000);words(d,1096,1)
        words(d,obs,base+ctx,base+segment,base+roots,1,101,0)
        words(d,obs+128,base+registry,0,base+segment,base+roots+64,1,101,0)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,0x10000)
        words(d,ctx+48,base+satb,4,0,0,0);words(d,ctx+88,base+remembered,4,0,0,0);words(d,ctx+280,1)
        words(d,pool,base+heap,base+slots,8,1,0)
        for i in range(7):words(d,slots+i*32+24,i+2)
        words(d,heap,base+payload-131072,base+page,1)
        words(d,page,base+payload-131072,131072,262144,27,1,0,1,5,base+starts,128,131120,0,0,base+table)
        words(d,table,base+cell,1,1);words(d,cell,131104,27,0,0)
        words(d,registry,base+dslots,2);words(d,dslots,0,base+desc)
        words(d,desc,1,8,8,0,base+bitmap,0,1,0);words(d,bitmap,1)
        words(d,segment,17,101,base+roots,3,3,1,base+journal,0)
        words(d,roots,ref,1,1,0,desired,1,1,0,0,1,1,0)
        words(d,journal,base+entries,32,0,0,0);words(d,starts,4)
        words(d,payload,8,1,0,2,previous)
        if dispatch:
            words(d,280,0);words(d,1096,0);words(d,ctx+280,0)
            words(d,10000+48,0x10000);words(d,10096,0,1,0,0,0,0,17,base+10160)
            words(d,10160,base+ctx,17,0,1,base+10096,0,0)
            words(d,10224,base+10000,base+10096,base+10160,0,0)
            words(d,11000,base+pool,base+10224,base+11200,base+11208,base+11216,base+11224,STUBS+0x700,STUBS+0x704)
        return d
    def action():
        s=get(stage)
        if s>=5:return {5:35,6:36,7:42,8:43}[s]
        if s==0:
            v=get(1056)
            if v in (4,5):return 33
            if v==2:
                js=get(320+304*get(312)+160)
                return 33 if js in (1,2) else 34 if js==3 else 32
            assert v in (0,1,3,6,7,8),(s,v)
            return 37 if v==8 else 32
        if s==1:return 33 if get(obs+184) in (1,2) else 32
        if s==3:return 33 if get(att+344) in (1,2) else 32
        return 32
    def call(op):
        nonlocal count
        if dispatch:r=m.call('rooted_store_dispatch',base+11000,base,1 if op==45 else 2 if op==47 else 0)
        else:r=m.call('rooted_store',base+pool,base,op)
        count+=1
        assert r not in (37,44),(schedules,count,op,r,get(stage),get(1680))
        return r
    native={'held':0,'acquired':0,'released':0,'heap':0,'registry':0}
    def native_lock(uc,address,size,unused):
        if address not in (STUBS+0x700,STUBS+0x704):return
        assert uc.reg_read(r.UC_ARM64_REG_SP)%16==0
        lock=uc.reg_read(X[0]);assert lock in (base+11200,base+11208,base+11216,base+11224)
        if address==STUBS+0x700:
            assert not native['held'] and get(ctx+144)==0
            assert action()!=33
            native['held']=lock;native['acquired']+=1
            if lock==base+11200:native['heap']+=1
            elif lock==base+11208:native['registry']+=1
        else:
            assert native['held']==lock;native['held']=0;native['released']+=1
        # The dispatcher must retain everything live across an ABI callback.
        for i in range(18):uc.reg_write(X[i],0xbad00000+i)
    native_hook=m.uc.hook_add(UC_HOOK_CODE,native_lock) if dispatch else None
    for desired in (0,ref):
        for previous in (0,ref):
            for scenario in range(5):
                for boundary in range(28 if scenario==1 else 1):
                    d=fixture(desired,previous);m.reset();m.uc.mem_write(base,bytes(d))
                    changed=False;cancelled=False;fired=[False];hook=None
                    if scenario==4:
                        def interfere(uc,address,size,unused):
                            if not fired[0] and get(stage)==3 and get(att+344)==1 and bytes(uc.mem_read(address,4))==struct.pack('<I',0xc85ffc00|(9<<5)):
                                assert uc.reg_read(X[9])==base+payload+32
                                put(payload+32,0 if previous else ref);fired[0]=True
                        hook=m.uc.hook_add(UC_HOOK_CODE,interfere)
                    try:
                        for tick in range(160):
                            op=action()
                            if op in (35,36,42,43):break
                            if scenario==1 and tick==boundary:
                                call(45);cancelled=True;op=action()
                            if get(stage)==3 and get(att+344)==1 and not changed:
                                if scenario==2:put(payload+32,0 if previous else ref);changed=True
                                if scenario==3 and desired:put(roots+32,desired^0x300);changed=True
                            call(op)
                            assert get(pool+32)==get(page+96)
                            assert sum(bool(get(slots+i*32+8)) for i in range(8))==get(pool+32)
                            if get(stage)<5:assert get(ctx+280)>=1
                        else:raise AssertionError(('unbounded store',schedules))
                    finally:
                        if hook is not None:m.uc.hook_del(hook)
                    terminal=action();assert terminal in (35,36),(schedules,terminal)
                    if terminal==36:assert cancelled
                    else:assert get(payload+32)==desired
                    assert get(ctx+144)==0 and get(ctx+280)==0
                    logs=get(ctx+104)
                    assert get(pool+32)==logs and get(ctx+64)==0
                    if scenario==4:assert fired[0] and logs==2
                    saved=bytes(m.uc.mem_read(base,1712));saved_field=get(payload+32)
                    for op in (32,33,34,45,46):
                        assert call(op)==terminal
                        assert bytes(m.uc.mem_read(base,1712))==saved and get(payload+32)==saved_field
                    # Consume actual buffer jobs, then discharge their tokens.
                    for _ in range(logs):
                        m.uc.mem_write(base+job,bytes(64));put(job,17,1,0,0,0,base+out,1,0)
                        for op in (0,1,2):assert m.call('buffer_job',base+ctx,base+job,op)==0
                        assert get(out+8)==ref and get(out+16)==0
                        put(lease,0,0,get(out+40))
                        assert m.call('metadata_step',base+pool,base+lease,2)==0
                        assert m.call('metadata_step',base+pool,base+lease,1)==0
                    assert get(pool+32)==0 and get(page+96)==0
                    if terminal==35:
                        assert call(47)==35 and bytes(m.uc.mem_read(base,1712))==saved
                    else:
                        reg=10000;hs=10096;participant=10160;admit=10224;worker=10304
                        new_phase=0x20100
                        put(reg+48,new_phase);put(hs,3,1,0,0,0,0,17,base+participant)
                        put(participant,base+ctx,17,3,1,base+hs,0,0)
                        put(worker,base+hs,base+participant)
                        put(admit,base+reg,base+hs,base+participant,0,0)
                        assert m.call('handshake_step',base+hs,0,6)==0
                        assert m.call('worker_step',base+reg,base+worker,0)==1
                        parked=bytes(m.uc.mem_read(base,12288))
                        assert m.call('rooted_store',base+pool,base,47)==44
                        assert bytes(m.uc.mem_read(base,12288))==parked
                        assert m.call('handshake_step',base+hs,0,7)==0
                        assert m.call('worker_step',base+reg,base+worker,0)==0
                        clean=bytes(m.uc.mem_read(base,12288))
                        # Every retained local obligation rejects rearm without
                        # changing either the controller or external memory.
                        faults=[(1696,1),(1096,1),(1080,1),(1056,3),(288,1),
                                (att+344,1),(928+48,8),(320+120,1),(624+48,8),
                                (obs+72,1),(att+80,1),(att+296,8),(att+336,1)]
                        for off,bad in faults:
                            d=bytearray(clean);words(d,off,bad);m.uc.mem_write(base,bytes(d))
                            assert m.call('rooted_store',base+pool,base,47)==44
                            assert bytes(m.uc.mem_read(base,len(d)))==bytes(d)
                        m.uc.mem_write(base,clean)
                        expected=bytearray(clean);expected[:1712]=fixture(desired,previous)[:1712]
                        words(expected,280,0);words(expected,1096,0)
                        assert call(47)==32
                        assert bytes(m.uc.mem_read(base,len(expected)))==bytes(expected)
                        assert m.call('rooted_store',base+pool,base,32)==44
                        assert bytes(m.uc.mem_read(base,len(expected)))==bytes(expected)
                        assert m.call('rooted_load_admit',base+admit,base)==0
                        assert get(280)==new_phase and get(1096)==1
                        changed_value=0 if get(payload+32) else ref;put(payload+32,changed_value)
                        for _ in range(80):
                            op=action()
                            if op==35:break
                            call(op)
                        else:raise AssertionError('rearmed store did not finish')
                        assert get(payload+32)==((desired&~0xffff)|0x1610 if desired else 0)
                        assert get(att+168)==changed_value and get(obs+176)==changed_value
                        assert get(ctx+280)==0 and get(ctx+144)==0
                        m.uc.mem_write(base+job,bytes(64));put(job,17,1,0,0,0,base+out,1,0)
                        for op in (0,1,2):assert m.call('buffer_job',base+ctx,base+job,op)==0
                        put(lease,0,0,get(out+40))
                        assert m.call('metadata_step',base+pool,base+lease,2)==0
                        assert m.call('metadata_step',base+pool,base+lease,1)==0
                        assert get(pool+32)==0 and get(page+96)==0
                        restarts+=1
                    schedules+=1
    for bad_cancel,bad_admission in ((2,1),(0,2)):
        d=fixture(ref,0);words(d,stage,5,0,bad_cancel,bad_admission,0)
        m.reset();m.uc.mem_write(base,bytes(d))
        assert m.call('rooted_store',base+pool,base,32)==44
        assert bytes(m.uc.mem_read(base,len(d)))==bytes(d)
    if native_hook is not None:
        m.uc.hook_del(native_hook)
        assert native['acquired']==native['released'] and not native['held']
        assert native['heap'] and native['registry']
        print(f"PASS: ARM64 store native callbacks {native['acquired']} balanced lock pairs, caller-clobber and stack alignment checks")
    mode='dispatch' if dispatch else 'owner'
    print(f'PASS: ARM64 rooted store {mode} rearm {restarts} handshake/admission restarts with fresh observations and exact rejection checks')
    print(f'PASS: ARM64 rooted store {mode} {schedules} complete preparation/observation/retry/cancel schedules, {count} owner calls')

def verify_rooted_store_attempt(m):
    # Independently predict the complete memory image, including token ownership.
    base=MEMORY+0x2000;ctx=384;pool=768;heap=832;page=960;slots=1088;table=1184;cell=1216
    registry=1280;dslots=1312;desc=1344;bitmap=1424;segment=1472;roots=1536
    journal=1600;entries=1664;starts=2048;payload=4096;marks=8192;satb=3072;remembered=3264
    receiver=(131104<<16)|0x1510;desired=(131168<<16)|0x1510;phase=0x10022;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def get(d,o):return struct.unpack_from('<Q',d,o)[0]
    def fixture(generation,previous):
        d=bytearray(9216)
        words(d,0,base+ctx,base+segment,base+roots,1,101,0)
        words(d,128,base+marks,0,0,base+registry,0,previous)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,phase)
        words(d,ctx+48,base+satb,3,0,0,0);words(d,ctx+88,base+remembered,3,0,0,0)
        words(d,ctx+280,1);words(d,pool,base+heap,base+slots,3,1,0)
        words(d,slots+24,2);words(d,slots+56,3)
        words(d,heap,base+payload-131072,base+page,1)
        words(d,page,base+payload-131072,131072,262144,27,1,0,generation,5,base+starts,128,131184,0,0,base+table)
        words(d,table,base+cell,1,1);words(d,cell,131104,27,0,0)
        words(d,registry,base+dslots,2);words(d,dslots,0,base+desc)
        words(d,desc,1,16,24,0,base+bitmap,0,1,0);words(d,bitmap,3)
        words(d,segment,17,101,base+roots,2,2,1,base+journal,0)
        words(d,roots,receiver,1,1,0,desired,1,1,0);words(d,journal,base+entries,3,0,0,0)
        words(d,starts,68);words(d,payload,24,1,0,2,previous)
        words(d,marks,base+ctx,base+segment,base+roots+32,1,101,0,0,0,0,0,phase,2,3,3,0)
        return d
    def entered(d,acquired=3,failure=0):
        words(d,48,base+page,27,(1<<32)|1)
        words(d,72,receiver,0,base+ctx,131104,27,base+cell,0)
        words(d,ctx+144,27);words(d,page+96,acquired);words(d,cell+24,1)
        for i in range(acquired):words(d,slots+i*32,1,base+page,27,0)
        words(d,pool+24,0 if acquired==get(d,pool+16) else acquired+1,acquired)
        for i in range(2):
            if i<acquired:words(d,296+24*i,base+page,27,((1<<32)|(i+2)) if i+1<acquired else 0)
        if failure:words(d,344,2,failure);return
        words(d,136,base+88,base+page);words(d,184,phase)
        for i in range(2):words(d,200+48*i,get(d,168),receiver,0,27,phase,(1<<32)|(i+2))
        words(d,344,1)
    def exited(d):
        d[72:128]=bytes(56);words(d,ctx+144,0);words(d,cell+24,0);words(d,344,3)
    def free(d,index):
        words(d,slots+index*32+8,0,0,get(d,pool+24));words(d,pool+24,index+1,get(d,pool+32)-1)
        words(d,page+96,get(d,page+96)-1)
    def release(d):
        if get(d,344)==3:
            free(d,0);d[48:72]=bytes(24);words(d,344,4);return
        i=get(d,360)
        if get(d,312+24*i):free(d,i+1)
        d[296+24*i:320+24*i]=bytes(24);words(d,360,i+1)
        if i==1:words(d,344,5)
    def run(d,op,status,change=None,qa=None,interference=False):
        nonlocal count
        e=bytearray(d)
        if change:change(e)
        m.reset();m.uc.mem_write(base,bytes(d))
        fired=[False]
        def interfere(uc,address,size,unused):
            if not fired[0] and bytes(uc.mem_read(address,4))==struct.pack('<I',0xc85ffc00|(9<<5)):
                assert uc.reg_read(X[9])==base+payload+32
                uc.mem_write(base+payload+32,struct.pack('<Q',73));fired[0]=True
        hook=m.uc.hook_add(UC_HOOK_CODE,interfere) if interference else None
        try:got=m.call('rooted_store_attempt',base+pool,base if qa is None else qa,op)
        finally:
            if hook is not None:m.uc.hook_del(hook)
        if interference:assert fired[0]
        assert got==status,(count,op,got,status)
        actual=bytes(m.uc.mem_read(base,len(d)))
        assert actual==bytes(e),(count,op,[(i,actual[i],e[i]) for i in range(len(d)) if actual[i]!=e[i]][:16])
        count+=1;return e
    for generation in (0,1):
        for previous in (0,desired):
            for scenario in range(10):
                d=fixture(generation,previous);result=0
                if scenario==2:words(d,roots+32,desired^0x300);result=6
                if scenario==3:words(d,marks+104,1);result=1
                if scenario==4:words(d,payload+32,0 if previous else desired);result=4
                if scenario==5 and previous:words(d,ctx+64,3,3);result=2
                if scenario==6 and generation:words(d,ctx+104,3,3);result=2
                if scenario in (7,8):
                    capacity=scenario-6;words(d,pool+16,capacity);words(d,slots+32*(capacity-1)+24,0)
                    d=run(d,0,10,lambda x:entered(x,capacity,8));result=8
                else:
                    d=run(d,0,8,entered);d=run(d,0,8);d=run(d,3,8)
                    def access(x):
                        words(x,344,2,7 if scenario==1 else 4 if scenario==9 else result)
                        if scenario==1 or result:return
                        mask=int(bool(previous))|(2 if generation else 0);words(x,192,mask)
                        words(x,payload+32,73 if scenario==9 else desired)
                        for i in range(2):
                            if mask&(1<<i):
                                out=satb if i==0 else remembered
                                x[out:out+48]=x[200+i*48:248+i*48];words(x,ctx+64+i*40,1,1)
                                x[296+i*24:320+i*24]=bytes(24)
                    d=run(d,4 if scenario==1 else 1,10,access,interference=scenario==9)
                d=run(d,1,10);d=run(d,4,10);d=run(d,3,10)
                words(d,120,1);d=run(d,2,10);words(d,120,0)
                d=run(d,2,9,exited);d=run(d,4,9)
                old=get(d,pool+24);words(d,pool+24,4);d=run(d,3,9);words(d,pool+24,old)
                d=run(d,3,9,release);d=run(d,3,9,release);d=run(d,3,11,release)
                for op in range(5):d=run(d,op,11)
                assert get(d,pool+32)==get(d,192).bit_count()
    for fault in range(8):
        d=fixture(1,desired);op=0;qa=base;status=1
        if fault==0:qa=0
        elif fault==1:qa+=1
        elif fault==2:op=5
        elif fault==3:words(d,344,6)
        elif fault==4:words(d,360,3)
        elif fault==5:words(d,roots,0);status=0
        elif fault==6:op=1
        elif fault==7:op=2
        run(d,op,status,qa=qa)
    d=fixture(1,desired);run(d,4,11,lambda x:words(x,344,5,7))
    print(f'PASS: ARM64 rooted store attempt {count} independent full-state ownership and replay cases')

def verify_rooted_store_commit(m):
    base=MEMORY+0x2000;scope=192;ctx=256;page=640;cell=768;desc=832;bitmap=920
    registry=960;dslots=992;segment=1088;root=1152;satb=1280;remembered=1424;payload=4096;marks=8192
    heap_base=base+payload-131072;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def get(d,o):return struct.unpack_from('<Q',d,o)[0]
    def color(p):return (1<<(4+((p>>12)&1)))|(1<<(8+((p>>8)&1)))|(1<<(10+((p>>9)&1)))|(1<<(12+((p>>10)&3)))
    def fixture(phase,generation,previous,nonnull):
        d=bytearray(9216);old=(131168<<16)|0x1510 if previous else 0;owner=(131104<<16)|0x1510
        words(d,0,base+scope,base+page,base+registry,0,old,0,phase,0)
        words(d,64,old,owner,0,27,phase,11);words(d,112,old,owner,0,27,phase,12)
        words(d,scope,base+ctx,131104,27,base+cell,0)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,phase)
        words(d,ctx+48,base+satb,3,0,0,0);words(d,ctx+88,base+remembered,3,0,0,0)
        words(d,ctx+144,27);words(d,ctx+280,1)
        words(d,page,heap_base,131072,262144,27,1,0,generation,5,0,0,131136,0,0,0)
        words(d,cell,131104,27,0,1);words(d,registry,base+dslots,2);words(d,dslots,0,base+desc)
        words(d,desc,1,16,24,0,base+bitmap,0,1,0);words(d,bitmap,3)
        words(d,payload,24,1,0,2,old)
        words(d,segment,17,101,base+root,1,1,1,0,0)
        words(d,root,((131200<<16)|(color(phase)^0x30)) if nonnull else 0,1,1,0)
        required=sum(1<<g for g in (0,1) if 1<=((phase>>(g*4))&15)<=3)
        words(d,marks,base+ctx,base+segment,base+root,1,101,0,0,0,0,0,phase,2,required,required,0)
        return d
    def effects(d,winning=True):
        e=bytearray(d);p=get(d,48);old=get(d,32)
        mask=int(bool(old) and any(1<=((p>>(g*4))&15)<=3 for g in (0,1)))|(2 if get(d,page+48) else 0)
        for g in (0,1):
            if mask&(1<<g):
                b=ctx+48+g*40;out=satb if g==0 else remembered
                e[out:out+48]=d[64+g*48:112+g*48];words(e,b+16,1,1)
        words(e,56,mask)
        if winning:words(e,payload+32,((get(d,root)&~0xffff)|color(p)) if get(d,root) else 0)
        return e
    def run(d,status,expected=None,mp=None,qp=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d))
        got=m.call('rooted_store_commit',base+marks if mp is None else mp,base if qp is None else qp)
        assert got==status,(count,got,status)
        expected=d if expected is None else expected;actual=bytes(m.uc.mem_read(base,len(d)))
        assert actual==bytes(expected),(count,[(i,actual[i],expected[i]) for i in range(len(d)) if actual[i]!=expected[i]][:12])
        count+=1
    for phases in range(64):
        for epoch in (0,31):
            phase=(17<<16)|(epoch<<8)|(phases&7)|((phases>>3)<<4)
            for generation in (0,1):
                for previous in (False,True):
                    for nonnull in (False,True):
                        d=fixture(phase,generation,previous,nonnull);run(d,0,effects(d))
    failures=[(marks+88,1,1),(marks+40,1,1),(40,1,1),(56,1,1),(marks+80,0x20022,1),
        (marks+96,1,1),(marks+104,1,1),(marks,0,1),(ctx+280,0,1),(marks+8,0,1),
        (segment,18,1),(segment+40,2,1),(marks+32,102,1),(segment+32,2,1),(marks+16,base+root+8,1),
        (marks+24,2,1),(root+16,0,1),(root,1,1),(root,(131200<<16)|0x2520,6),
        (payload+32,0,4),(24,16,1),(104,12,1)]
    for off,value,status in failures:
        d=fixture(0x10022,1,True,True);words(d,off,value);run(d,status)
    for b in (ctx+48,ctx+88):
        d=fixture(0x10022,1,True,True);words(d,b+16,3,3);run(d,2)
        d=fixture(0x10022,1,True,True);words(d,b+16,MASK,MASK,MASK);run(d,3)
    for mp,qp in ((0,base),(base+marks+1,base),(base+marks,0),(base+marks,base+1)):
        run(fixture(0x10022,1,True,True),1,mp=mp,qp=qp)
    # A real generated CAS loses after logging. Both entry leases still transfer.
    d=fixture(0x10022,1,True,True);e=effects(d,False);words(e,payload+32,0)
    fired=[False];m.reset();m.uc.mem_write(base,bytes(d))
    def interfere(uc,address,size,unused):
        if not fired[0] and bytes(uc.mem_read(address,4))==struct.pack('<I',0xc85ffc00|(9<<5)):
            assert uc.reg_read(X[9])==base+payload+32
            uc.mem_write(base+payload+32,struct.pack('<Q',0));fired[0]=True
    hook=m.uc.hook_add(UC_HOOK_CODE,interfere)
    try:assert m.call('rooted_store_commit',base+marks,base)==4
    finally:m.uc.hook_del(hook)
    assert fired[0] and bytes(m.uc.mem_read(base,len(d)))==bytes(e);count+=1
    print(f'PASS: ARM64 rooted store commit {count} independent full-state and losing-CAS transfer cases')

def verify_rooted_mark_publish(m):
    base=MEMORY+0x2000;marks=8192;ctx=384;pool=768;heap=832;page=960;slots=1088;table=1184;cell=1216
    record=1536;segment=1792;root=1856;journal=1920;entries=1984;starts=2304;payload=4096
    queues=(2048,2176);tasks=(3504,3568);heap_base=base+payload-131072;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(phase,color):
        d=bytearray(9216);ref=(131104<<16)|color
        words(d,0,base+ctx,base+segment,base+root,1,101,0)
        words(d,48,base+page,27,(1<<32)|1);words(d,72,ref,0,base+ctx,131104,27,base+cell,0)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,phase);words(d,ctx+144,27,77)
        words(d,pool,base+heap,base+slots,2,2,1);words(d,slots,1,base+page,27,0)
        words(d,heap,heap_base,base+page,1)
        words(d,page,heap_base,131072,262144,27,1,0,0,5,base+starts,128,131120,0,1,base+table)
        words(d,table,base+cell,1,1);words(d,cell,131104,27,0,1);words(d,starts,4)
        words(d,payload,8,1,base+record,2);words(d,record,1,7,9)
        words(d,segment,17,101,base+root,1,1,1,base+journal,0)
        words(d,root,ref,1,1,0);words(d,journal,base+entries,3,0,0,0)
        words(d,marks,base+ctx,base+segment,base+root,1,101,0,base+queues[0],base+queues[1],7,9,phase,2,3,3,0)
        for g in (0,1):
            words(d,queues[g],base+tasks[g],1,1,0,0,0,0,1,7+2*g,g,0,0,0);words(d,tasks[g]+8,1)
            j=marks+120+304*g;words(d,j,base+ctx,base+segment,base+root,1,101,0)
            words(d,j+128,base+queues[g],g,7+2*g,phase,5,0,0)
        return d
    def run(d,status,expected=None,mp=None,ap=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d))
        result=m.call('rooted_mark_publish',base+marks if mp is None else mp,base if ap is None else ap)
        assert result==status,(count,result,status)
        target=d if expected is None else expected;actual=bytes(m.uc.mem_read(base,len(d)))
        assert actual==bytes(target),(count,[(i,actual[i],target[i]) for i in range(len(d)) if actual[i]!=target[i]][:12])
        count+=1
    for epochs in range(32):
        phase=(13<<16)|(epochs<<8)|0x22
        for color in (0x1510,0x2520,0x8630):
            d=fixture(phase,color);e=bytearray(d)
            desired=(131104<<16)|(color&0x30)|(1<<(8+((phase>>8)&1)))|(1<<(10+((phase>>9)&1)))|(1<<(12+((phase>>10)&3)))
            words(e,root,desired);run(d,0,e)
    for fault in range(28):
        d=fixture(0x22,0x8630)
        if fault==0:words(d,marks+88,1)
        elif fault==1:words(d,marks+104,2)
        elif fault==2:words(d,marks+80,0x10022)
        elif fault==3:words(d,marks+96,0,0)
        elif fault==4:words(d,marks,0)
        elif fault==5:words(d,marks+16,base+root+32)
        elif fault==6:words(d,marks+32,102)
        elif fault==7:words(d,56,28)
        elif fault==8:words(d,64,0)
        elif fault==9:words(d,64,1)
        elif fault==10:words(d,64,1<<32)
        elif fault==11:words(d,marks+40,1)
        elif fault==12:words(d,40,1)
        elif fault==13:words(d,ctx+144,0)
        elif fault==14:words(d,ctx+16,0x10022)
        elif fault==15:words(d,cell+24,0)
        elif fault==16:words(d,root,0)
        elif fault==17:words(d,root,1)
        elif fault==18:words(d,root,(131120<<16)|0x1510)
        elif fault==19:words(d,segment+40,2)
        elif fault==20:words(d,segment+32,0)
        elif fault==21:words(d,root+16,0)
        elif fault==22:words(d,root+8,2)
        elif fault==23:words(d,segment+8,0)
        elif fault==24:words(d,segment+24,0)
        elif fault==25:words(d,marks+8,0);words(d,8,0)
        elif fault==26:words(d,120,1)
        elif fault==27:words(d,page+24,28)
        run(d,1)
    for mp,ap in ((0,base),(1,base),(base+marks,0),(base+marks,1)):
        run(fixture(0x22,0x8630),1,mp=mp,ap=ap)
    # Inject a same-object physical recoloring AFTER the initial load, at the
    # exclusive CAS load. The exact CAS must retain the intervening root bits.
    d=fixture(0x22,0x8630);e=bytearray(d);replacement=(131104<<16)|0x2530;words(e,root,replacement)
    fired=[False];m.reset();m.uc.mem_write(base,bytes(d))
    def interfere(uc,address,size,unused):
        if not fired[0] and bytes(uc.mem_read(address,4))==struct.pack('<I',0xc85ffc00|(9<<5)):
            assert uc.reg_read(X[9])==base+root
            uc.mem_write(base+root,struct.pack('<Q',replacement));fired[0]=True
    hook=m.uc.hook_add(UC_HOOK_CODE,interfere)
    try:assert m.call('rooted_mark_publish',base+marks,base)==2
    finally:m.uc.hook_del(hook)
    assert fired[0] and bytes(m.uc.mem_read(base,len(d)))==bytes(e);count+=1
    words(e,root,(131104<<16)|0x1530)
    assert m.call('rooted_mark_publish',base+marks,base)==0
    assert bytes(m.uc.mem_read(base,len(d)))==bytes(e);count+=1
    assert count==130
    print('PASS: ARM64 rooted mark publication 130 independent color, rejection and intervening-CAS cases')


def verify_root_access_cleanup(m):
    base=MEMORY+0x2000;ctx=256;pool=640;heap=704;page=832;slots=1024;table=1152;cell=1184
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    count=0
    for phase in (0,0x22):
        for fault in range(14):
            data=bytearray(2048);qa=base;pa=base+pool;op=0;status=1
            words(data,0,base+ctx);words(data,48,base+page,27,(1<<32)|1)
            words(data,72,(131104<<16)|0x1510,0,base+ctx,131104,27,base+cell,0)
            struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,phase)
            words(data,ctx+144,27)
            words(data,pool,base+heap,base+slots,2,2,1)
            words(data,heap,0x100000000,base+page,1)
            words(data,page,0x100000000,131072,262144,27,1024,0,0,5,0,0,131120,0,1,base+table)
            words(data,slots,1,base+page,27,0)
            words(data,table,base+cell,1,1);words(data,cell,131104,27,0,1)
            if fault>=10:
                data[72:128]=bytes(56);words(data,ctx+144,0);words(data,cell+24,0);op=1
            if fault==0:status=0
            elif fault==1:op=1
            elif fault==2:words(data,0,0)
            elif fault==3:words(data,48,0)
            elif fault==4:words(data,56,28)
            elif fault==5:words(data,64,0)
            elif fault==6:words(data,88,0)
            elif fault==7:op=2
            elif fault==8:qa=0
            elif fault==9:qa+=1
            elif fault==10:words(data,pool+32,0);status=7
            elif fault==11:words(data,ctx+144,1)
            elif fault==12:pa=0
            elif fault==13:words(data,72,1)
            expected=bytearray(data)
            if not fault:
                expected[72:128]=bytes(56);words(expected,ctx+144,0);words(expected,cell+24,0)
            m.reset();m.uc.mem_write(base,bytes(data))
            assert m.call('root_access_cleanup',pa,qa,op)==status,(phase,fault)
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected),(phase,fault)
            if fault==10:
                words(expected,pool+32,1)
                m.uc.mem_write(base+pool+32,struct.pack('<Q',1))
            if not fault or fault==10:
                expected[48:72]=bytes(24);words(expected,pool+24,1,0)
                words(expected,page+96,0);words(expected,slots+8,0,0,2)
                assert m.call('root_access_cleanup',pa,qa,1)==0
                assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected)
                assert m.call('root_access_cleanup',pa,qa,1)==1
                assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected)
            count+=1
    assert count==28
    print('PASS: ARM64 root access cleanup 28 independent ordered-ownership cases')


def verify_heap_audit_owner(m):
    base=MEMORY+0x2000;pool=512;heap=640;ctx=768;page=1152;slot=1280;gate=1344
    table=1408;cell=1472;registry=1536;slots=1600;desc=1664;bitmap=1792;payload=2048
    ref=(128<<16)|0x1510;count=0
    def words(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(phase=0x22,positive=False):
        d=bytearray(4096)
        words(d,0,base+ctx,base+registry,base+gate,7,phase,128,160,0,0)
        words(d,72,base+pool,base+heap,base+page,1,0,0,0,0)
        words(d,pool,base+heap,base+slot,1,1,0);words(d,heap,0,base+page,1)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],0,17,phase)
        words(d,page,base+payload,16,1024,27,0,0,1,5,0,0,80,0,0,base+table)
        words(d,gate,0,0,0,7,1);words(d,table,base+cell,1,1);words(d,cell,48,27,0,0)
        words(d,registry,base+slots,2);words(d,slots,0,base+desc)
        words(d,desc,1,16,24,0,base+bitmap,0,1,0);words(d,bitmap,3)
        words(d,payload+16,24,1,0,2);words(d,payload+48,ref if positive else 0)
        return d
    def taken(d):
        words(d,56,1);words(d,104,1,base+page,27,(1<<32)|1)
        words(d,136,base+112,base+page,27,base+table,1,0,0)
        words(d,pool+24,0,1);words(d,slot,1,base+page,27,0);words(d,page+96,1)
    def prepared(d,phase):
        color=(1<<(4+((phase>>12)&1)))|(1<<(8+((phase>>8)&1)))|(1<<(10+((phase>>9)&1)))|(1<<(12+((phase>>10)&3)))
        words(d,56,2);words(d,176,1,48)
        words(d,192,base+page,base+ctx,(48<<16)|color,base+registry,phase,128,160,0,0)
    def released(d,cancel=False):
        words(d,pool+24,1,0);words(d,slot+8,0,0,0);words(d,page+96,0)
        d[112:352]=bytes(240);words(d,56,0)
        if cancel:words(d,64,0);words(d,104,0)
    def run(d,op,status,change=None):
        nonlocal count
        expected=bytearray(d)
        if change:change(expected)
        assert m.call('heap_audit',base+pool,base,op)==status,(count,op,status)
        assert bytes(m.uc.mem_read(base,len(d)))==bytes(expected),(count,op,status)
        count+=1
        return expected
    for epochs in range(32):
        phase=(epochs<<8)|0x22
        for positive in (False,True):
            d=fixture(phase,positive);m.reset();m.uc.mem_write(base,bytes(d))
            d=run(d,0,1,taken);d=run(d,0,2,lambda x:prepared(x,phase))
            if positive:
                def found(x):words(x,56,3);words(x,256,ref)
                d=run(d,1,3,found)
                def consume(x):words(x,56,2,1);words(x,248,8,0)
                d=run(d,2,2,consume)
            else:d=run(d,1,2,lambda x:words(x,248,8))
            d=run(d,1,2,lambda x:words(x,248,16))
            def scanned(x):words(x,56,1);words(x,184,0);x[192:352]=bytes(160)
            d=run(d,1,1,scanned);d=run(d,0,1,released)
            d=run(d,0,9 if positive else 0,lambda x:words(x,56,4))
            d=run(d,0,9 if positive else 0)
            def cancelled(x):words(x,56,0,0);words(x,104,0)
            d=run(d,3,11,cancelled)
    assert count==608
    for fault in range(17):
        d=fixture(positive=True);op=0;status=4
        if fault==0:op=4
        elif fault==1:words(d,56,5)
        elif fault==2:words(d,0,0)
        elif fault==3:words(d,8,0)
        elif fault==4:words(d,16,0)
        elif fault==5:words(d,24,0)
        elif fault==6:words(d,40,0)
        elif fault==7:words(d,48,128)
        elif fault==8:words(d,32,8)
        elif fault==9:words(d,72,0)
        elif fault==10:words(d,gate+32,0);status=5
        elif fault==11:op=1
        elif fault==12:words(d,24,6);status=7
        elif fault==13:words(d,ctx+16,0x33);status=7
        elif fault==14:
            words(d,pool+24,0,1);words(d,slot,1,base+page,27,0);words(d,page+96,1);status=12
        elif fault==15:words(d,ctx+144,27)
        elif fault==16:
            words(d,gate,base+1856,1,1,7,1);words(d,1856,base+gate,1,7,0,0,0);status=5
        m.reset();m.uc.mem_write(base,bytes(d));run(d,op,status)
    for pa,qa in ((0,base),(base+pool,0),(base+pool,1),(1,base)):
        d=fixture();m.reset();m.uc.mem_write(base,bytes(d))
        assert m.call('heap_audit',pa,qa,0)==4;count+=1
    for stale in range(3):
        d=fixture(positive=True)
        if stale==2:words(d,table+8,2)
        m.reset();m.uc.mem_write(base,bytes(d));d=run(d,0,1,taken)
        if stale==0:words(d,gate+24,8,0)
        elif stale==1:words(d,ctx+16,0x33)
        else:
            words(d,table+16,2);words(d,cell+32,112,27,0,0);words(d,page+80,144)
            words(d,payload+80,24,1,0,2)
        m.uc.mem_write(base,bytes(d));d=run(d,0,7)
        d=run(d,3,11,lambda x:released(x,True))
    d=fixture(positive=True);m.reset();m.uc.mem_write(base,bytes(d))
    d=run(d,0,1,taken);d=run(d,0,2,lambda x:prepared(x,0x22))
    words(d,264,(48<<16)|0x1510,0,base+ctx,48,27,base+cell,0)
    words(d,320,1,0,ref,3);words(d,ctx+144,27);words(d,cell+24,1);words(d,192,0)
    m.uc.mem_write(base,bytes(d))
    for op in range(4):d=run(d,op,8)
    words(d,192,base+page);m.uc.mem_write(base,bytes(d))
    def exited(x):
        words(x,56,3);words(x,256,ref);x[264:352]=bytes(88)
        words(x,ctx+144,0);words(x,cell+24,0)
    d=run(d,1,3,exited);run(d,3,11,lambda x:released(x,True))
    for offset,value in ((192,0),(208,(64<<16)|0x1510),(200,0),(216,0),
                         (224,0x33),(232,144),(240,176),(136,0),(152,28)):
        d=fixture(positive=True);m.reset();m.uc.mem_write(base,bytes(d))
        d=run(d,0,1,taken);d=run(d,0,2,lambda x:prepared(x,0x22))
        words(d,offset,value);m.uc.mem_write(base,bytes(d))
        d=run(d,1,4)
        run(d,3,11,lambda x:released(x,True))
    assert count==682
    print('PASS: ARM64 heap audit owner 682 independent full-state sequence and recovery calls')


def verify_heap_audit_slice(m):
    base=MEMORY+0x2000;ctx=256;page=640;cell=768;registry=832;desc=896;slots=976;bitmap=992;table=1024;heap=1536
    ref=(128<<16)|0x1510
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    def fixture(value=ref,cursor=0):
        data=bytearray(2560)
        words(data,0,base+page,base+ctx,(48<<16)|0x1510,base+registry,0x22,128,160,cursor,0)
        struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,0x22)
        words(data,page,base+heap,16,1024,27,0,0,1,5,0,0,80,0,0,base+table)
        words(data,cell,48,27,0,0);words(data,table,base+cell,1,1)
        words(data,registry,base+slots,2);words(data,slots,0,base+desc)
        words(data,desc,1,16,24,0,base+bitmap,0,1,0);words(data,bitmap,3)
        words(data,heap+16,24,1,0,2);words(data,heap+48,value)
        return data
    count=0
    for value in (0,ref,(192<<16)|0x1510):
        for cursor in (0,8,16,24):
            data=fixture(value,cursor);expected=bytearray(data)
            status=0 if cursor>=16 else 3 if cursor==0 and value==ref else 1
            words(expected,56,cursor if status==3 else 24 if cursor>=16 else cursor+8,ref if status==3 else 0)
            m.reset();m.uc.mem_write(base,bytes(data))
            assert m.call('heap_audit_slice',base)==status,(value,cursor)
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected),(value,cursor)
            count+=1
    for fault in range(20):
        data=fixture();status=2
        if fault==0:words(data,0,0)
        elif fault==1:words(data,8,0)
        elif fault==2:words(data,16,1)
        elif fault==3:words(data,24,0)
        elif fault==4:words(data,32,0)
        elif fault==5:words(data,40,0)
        elif fault==6:words(data,56,1)
        elif fault==7:words(data,64,1)
        elif fault==8:words(data,128,2)
        elif fault in (9,10,11):words(data,136+(fault-9)*8,1)
        elif fault==12:words(data,112,1)
        elif fault==13:words(data,cell+16,ref|2,1<<63);words(data,page+56,8);status=4
        elif fault==14:words(data,cell+16,5);status=5
        elif fault==15:words(data,cell+24,1<<63);status=5
        elif fault==16:words(data,cell+24,(1<<63)-1);status=6
        elif fault==17:words(data,heap+48,1)
        elif fault==18:words(data,heap+40,1)
        elif fault==19:words(data,0,base+page+1)
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('heap_audit_slice',base)==status,fault
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(data),fault
        count+=1
    for kind in range(4):
        for fault in range(3):
            cursor=24 if kind==2 else 1 if kind==3 else 0
            data=fixture(ref if kind==1 else 0,cursor)
            status=(1,3,0,2)[kind];next_cursor=(8,0,24,1)[kind];observed=ref if kind==1 else 0
            words(data,72,(48<<16)|0x1510,0,base+ctx,48,27,base+cell,0)
            words(data,128,1,next_cursor,observed,status)
            words(data,ctx+144,27);words(data,cell+24,1)
            if fault==0:words(data,0,0)
            elif fault==1:struct.pack_into('<I',data,ctx+4,2)
            elif fault==2:words(data,cell+24,0)
            m.reset();m.uc.mem_write(base,bytes(data))
            assert m.call('heap_audit_slice',base)==7,(kind,fault)
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(data),(kind,fault)
            count+=1
            words(data,0,base+page);struct.pack_into('<I',data,ctx+4,0);words(data,cell+24,1)
            expected=bytearray(data);expected[72:160]=bytes(88)
            words(expected,ctx+144,0);words(expected,cell+24,0)
            if status!=2:words(expected,56,next_cursor,observed)
            m.uc.mem_write(base,bytes(data))
            assert m.call('heap_audit_slice',base)==status,(kind,fault)
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected),(kind,fault)
            count+=1
    for pointer in (0,1):
        m.reset();assert m.call('heap_audit_slice',pointer)==2;count+=1
    assert count==58
    print('PASS: ARM64 heap audit slice 58 independent scans and retained cleanup calls')


def verify_object_walk(m):
    base=MEMORY+0x2000;pool=128;heap=256;page=512;slot=640;lease=704;table=768;cells=832
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    def fixture(state=5,word=0,cursor=0):
        data=bytearray(1024)
        words(data,0,base+lease,base+page,27,base+table,3,cursor,0)
        words(data,pool,base+heap,base+slot,1,0,1)
        words(data,heap,0,base+page,1)
        words(data,page,MASK-15,16,1024,27,0,0,0,state,0,0,256,0,1,base+table)
        words(data,slot,1,base+page,27,0);words(data,lease,base+page,27,(1<<32)|1)
        words(data,table,base+cells,4,3)
        for i in range(4):words(data,cells+i*32,48+i*48,27,word,0)
        return data
    count=0
    for state in (1,5,7,8):
        for word in (0,5,3,(512<<16)|0x1512):
            for cursor in range(4):
                data=fixture(state,word,cursor);expected=bytearray(data)
                status=1 if cursor==3 else 0
                if not status:words(expected,40,cursor+1,48+cursor*48)
                m.reset();m.uc.mem_write(base,bytes(data))
                assert m.call('object_walk',base+pool,base)==status,(state,word,cursor)
                assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected),(state,word,cursor)
                count+=1
    for fault in range(32):
        data=fixture();status=2
        if fault==0:words(data,0,0)
        elif fault==1:words(data,0,1)
        elif fault==2:words(data,8,base+page+1)
        elif fault==3:words(data,16,28)
        elif fault==4:words(data,48,48)
        elif fault==5:words(data,lease+16,0)
        elif fault==6:words(data,lease+16,(2<<32)|1)
        elif fault==7:words(data,pool,0)
        elif fault==8:words(data,page+56,0)
        elif fault==9:words(data,page+8,0)
        elif fault==10:words(data,page+16,16)
        elif fault==11:words(data,page+16,1<<48)
        elif fault==12:words(data,page+8,17)
        elif fault==13:words(data,24,0)
        elif fault==14:words(data,24,base+table+1)
        elif fault==15:words(data,page+104,0)
        elif fault==16:words(data,table,0)
        elif fault==17:words(data,table+8,0)
        elif fault==18:words(data,table+8,1<<32)
        elif fault==19:words(data,table,MASK-7)
        elif fault==20:words(data,32,5)
        elif fault==21:words(data,40,4)
        elif fault==22:words(data,table+16,5)
        elif fault==23:words(data,32,2);status=3
        elif fault==24:words(data,page+80,0)
        elif fault==25:words(data,page+80,1025)
        elif fault==26:words(data,cells,32)
        elif fault==27:words(data,cells,256)
        elif fault==28:words(data,cells+8,28)
        elif fault==29:words(data,40,1);words(data,cells+32,48)
        elif fault==30:words(data,40,3);words(data,table+16,4);status=3
        elif fault==31:words(data,lease,0,0,0)
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('object_walk',base+pool,base)==status,fault
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(data),fault
        count+=1
    for pa,qa in ((0,base),(base+pool,0),(base+pool,1),(1,base)):
        m.reset();assert m.call('object_walk',pa,qa)==2;count+=1
    assert count==100
    print('PASS: ARM64 object walk 100 independent leased-key and unchanged rejection cases')


def verify_heap_walk(m):
    base=MEMORY+0x2000;pool=128;heap=256;slots=512;pages=1024
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    def fixture(cursor):
        data=bytearray(2048)
        words(data,0,base+pool,base+heap,base+pages,9,cursor,0,0,0)
        words(data,pool,base+heap,base+slots,1,1,0)
        words(data,heap,0,base+pages,9)
        for i in range(9):
            words(data,pages+i*112+24,27+i);words(data,pages+i*112+56,i)
            words(data,pages+i*112+88,i)
        words(data,pages+4*112+24,28);words(data,pages+4*112+88,1)
        return data
    count=0
    for cursor in range(10):
        data=fixture(cursor);expected=bytearray(data)
        status=2 if cursor==9 else 6 if cursor in (2,6) else 0 if cursor in (1,5,7,8) else 1
        if status in (0,1):words(expected,32,cursor+1)
        if status==0:
            address=base+pages+cursor*112
            words(expected,40,address,27+cursor,(1<<32)|1)
            words(expected,pool+24,0,1)
            words(expected,slots,1,address,27+cursor,0)
            words(expected,pages+cursor*112+96,1)
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('heap_walk',base+pool,base)==status,cursor
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected),cursor
        count+=1
    for fault in range(25):
        data=fixture(1);status=3
        if fault==0:words(data,0,0)
        elif fault==1:words(data,8,base+heap+1)
        elif fault==2:words(data,16,base+pages+1)
        elif fault==3:words(data,24,10)
        elif fault==4:words(data,32,10)
        elif fault in (5,6,7):words(data,40+(fault-5)*8,1)
        elif fault==8:words(data,pool,0)
        elif fault==9:words(data,heap+8,0)
        elif fault==10:words(data,heap+16,0)
        elif fault==11:words(data,heap+16,MASK)
        elif fault==12:words(data,pages+112+56,9)
        elif fault==13:words(data,pages+112+24,0)
        elif fault==14:words(data,pages+112+88,0)
        elif fault==15:words(data,pool+8,0)
        elif fault==16:words(data,slots,(1<<32)-1);status=5
        elif fault==17:words(data,pages+112+96,MASK);status=5
        elif fault==18:words(data,32,4);words(data,pages+4*112+88,4)
        elif fault==19:words(data,32,4);words(data,pages+4*112+24,29)
        elif fault==20:words(data,32,4);words(data,pages+112+56,0)
        elif fault==21:words(data,32,0);words(data,pages+96,1)
        elif fault==22:words(data,32,3);words(data,pages+3*112+80,16);status=6
        elif fault==23:words(data,32,4);words(data,pages+112+88,0)
        elif fault==24:words(data,pool+24,0,1);status=4
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('heap_walk',base+pool,base)==status,fault
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(data),fault
        count+=1
    for pa,qa in ((0,base),(base+pool,0),(base+pool,1),(1,base)):
        m.reset();assert m.call('heap_walk',pa,qa)==3;count+=1
    assert count==39
    print('PASS: ARM64 heap walk 39 independent full-state lease and cursor cases')


def verify_heap_incoming(m):
    base=MEMORY+0x2000;scope=128;ctx=256;page=640;cell=768
    registry=832;desc=896;slots=976;bitmap=992;heap=1536
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    def fixture(value,cursor):
        data=bytearray(2560)
        words(data,0,base+scope,base+page,base+registry,0x22,128,160,cursor,0)
        words(data,scope,base+ctx,48,27,base+cell,0)
        struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,0x22)
        words(data,ctx+144,27)
        words(data,page,base+heap,16,1024,27,0,0,1,5,0,0,80,0,0,0)
        words(data,cell,48,27,0,1)
        words(data,registry,base+slots,2);words(data,slots,0,base+desc)
        words(data,desc,1,16,24,0,base+bitmap,0,1,0);words(data,bitmap,2)
        words(data,heap+16,24,1,0,2)
        words(data,heap+48,1,value,(128<<16)|0x1510)
        return data
    count=0
    for value in (0,(112<<16)|0x1510,(128<<16)|0x1510,(144<<16)|0x2620,(160<<16)|0x4930):
        for cursor in (0,8,16,24):
            data=fixture(value,cursor);expected=bytearray(data)
            found=cursor==8 and value and 128<=value>>16<160
            status=3 if found else (0 if cursor>=16 else 1)
            words(expected,48,cursor if found else min(cursor+8,24),value if found else 0)
            m.reset();m.uc.mem_write(base,bytes(data))
            assert m.call('heap_incoming',base)==status,(value,cursor)
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected),(value,cursor)
            count+=1
    for fault in range(18):
        data=fixture((128<<16)|0x1510,8)
        if fault==0:words(data,16,0)
        elif fault==1:words(data,16,base+registry+1)
        elif fault==2:words(data,32,0)
        elif fault==3:words(data,40,128)
        elif fault==4:words(data,40,1<<48)
        elif fault==5:words(data,32,129)
        elif fault==6:words(data,48,1)
        elif fault==7:words(data,48,32)
        elif fault==8:words(data,56,1)
        elif fault==9:words(data,24,0)
        elif fault==10:words(data,ctx+144,0)
        elif fault==11:words(data,cell+24,0)
        elif fault==12:words(data,cell+16,((128<<16)|0x1510)|2)
        elif fault==13:words(data,slots+8,0)
        elif fault==14:words(data,desc+16,32)
        elif fault==15:words(data,heap+56,1)
        elif fault==16:words(data,heap+40,1)
        elif fault==17:words(data,page+56,7)
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('heap_incoming',base)==2,fault
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(data),fault
        count+=1
    for pointer in (0,1):
        m.reset();assert m.call('heap_incoming',pointer)==2;count+=1
    assert count==40
    print('PASS: ARM64 heap incoming 40 independent full-state descriptor and rejection cases')


def verify_forward_compress(m):
    base=MEMORY+0x2000;cell=base+128;q=base+256;count=0
    def words(data,address,*values):struct.pack_into('<'+'Q'*len(values),data,address-base,*values)
    for color in (0x1510,0x2620,0x4930):
        for fault in range(26):
            old=(128<<16)|0x1510;destination=(256<<16)|color
            data=bytearray(384);words(data,base,0,0,0,11,1)
            words(data,cell,16,7,old|2,1<<63);words(data,q,cell,16,7,old,destination,11)
            status=1
            if fault==0:status=0
            elif fault==1:words(data,q+8,0)
            elif fault==2:words(data,q+8,17)
            elif fault==3:words(data,q+16,0)
            elif fault==4:words(data,q,0)
            elif fault==5:words(data,q,cell+1)
            elif fault==6:words(data,q+40,0)
            elif fault==7:words(data,q+40,12);status=3
            elif fault==8:words(data,base+32,0);status=2
            elif fault==9:words(data,base+24,0)
            elif fault==10:words(data,base,base+64,1,1);status=2
            elif fault==11:words(data,cell,32)
            elif fault==12:words(data,cell+8,8)
            elif fault==13:words(data,cell+24,0)
            elif fault==14:words(data,cell+24,(1<<63)|1)
            elif fault==15:words(data,q+24,0)
            elif fault==16:words(data,q+32,0)
            elif fault==17:words(data,q+24,(16<<16)|0x1510)
            elif fault==18:words(data,q+32,(16<<16)|0x1510)
            elif fault==19:words(data,q+32,destination|1)
            elif fault==20:words(data,q+24,old|1)
            elif fault==21:words(data,cell+16,destination|2);status=3
            elif fault==22:words(data,cell+16,5);status=3
            elif fault==23:words(data,cell+16,3);status=3
            elif fault==24:words(data,q+32,old);status=0
            elif fault==25:words(data,base+32,2)
            expected=bytearray(data)
            if fault==0:words(expected,cell+16,destination|2)
            m.reset();m.uc.mem_write(base,bytes(data))
            assert m.call('forward_compress',base,q)==status
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected)
            count+=1
    assert count==78
    print('PASS: ARM64 forwarding compression 78 independent exact-edge and frozen-epoch cases')


def verify_relocation_turn(m):
    base=MEMORY+0x2000;q=base+128;gates=[base+256,base+320];count=0
    def words(data,address,*values):struct.pack_into('<'+'Q'*len(values),data,address-base,*values)
    def setup():
        data=bytearray(512)
        for gate in gates:words(data,gate,0,0,0,1,1)
        words(data,q,0,0,0,gates[0]);return data
    def check(expected,op,status):
        nonlocal count
        assert m.call('relocation_turn',base,q,op)==status
        assert bytes(m.uc.mem_read(base,len(expected)))==bytes(expected)
        count+=1
    for fault in range(14):
        data=setup();words(data,base,1);status=2
        if fault==0:words(data,base,2)
        elif fault==1:words(data,base+16,3)
        elif fault==2:words(data,base+24,2)
        elif fault==3:words(data,q+8,2)
        elif fault==4:words(data,q,1)
        elif fault==5:words(data,q+16,1)
        elif fault==6:words(data,q+24,0)
        elif fault==7:words(data,gates[0]+32,0);status=1
        elif fault==8:words(data,gates[0]+24,0)
        elif fault==9:words(data,base+32,MASK);status=3
        elif fault==10:words(data,gates[0]+24,MASK);status=3
        elif fault==11:words(data,base,0);status=4
        elif fault==12:words(data,base+8,1);words(data,base+24,1);status=1
        else:words(data,q+24,gates[0]+1)
        m.reset();m.uc.mem_write(base,bytes(data));check(data,1,status)
    data=setup();m.reset();m.uc.mem_write(base,bytes(data))
    epochs=[1,1]
    for round in range(8):
        k=round%2
        for generation in (0,1):
            words(data,q,0,generation,0,gates[generation]);m.uc.mem_write(q,bytes(data[128:160]))
            words(data,base+8*generation,1);check(data,0,0)
        other=k^1;words(data,q,0,other,0,gates[other]);m.uc.mem_write(q,bytes(data[128:160]));check(data,1,1)
        words(data,q,0,k,0,gates[k]);m.uc.mem_write(q,bytes(data[128:160]))
        words(data,base+8*k,0);words(data,base+16,k+1,k^1,round+1,gates[k],epochs[k]+1)
        words(data,q,base,k,round+1,gates[k]);check(data,1,0);check(data,2,2)
        assert m.call('relocation_gate',gates[k],0,4)==0
        epochs[k]+=1;words(data,gates[k]+24,epochs[k],0);check(data,2,1)
        assert m.call('relocation_gate',gates[k],0,2)==0
        words(data,gates[k]+32,1)
        # Recovery reopens while retaining this generation's receipt/priority.
        epochs[k]+=1;words(data,gates[k]+24,epochs[k],0);words(data,base+48,epochs[k])
        check(data,3,0);check(data,2,1)
        assert m.call('relocation_gate',gates[k],0,2)==0
        words(data,gates[k]+32,1);words(data,base+16,0);words(data,base+40,0,0);words(data,q,0,k,0,gates[k])
        check(data,2,0)
    assert count==86
    print('PASS: ARM64 relocation turns 86 independent fairness, gate-epoch and rejection calls')


def verify_promotion_remember(m):
    base=MEMORY+0x2000
    g,reg,rs,registry,desc,slots,bitmap,bank0,bank1,q=[base+x for x in (0,128,256,512,544,624,640,768,896,1024)]
    def words(data,addr,*values):struct.pack_into('<'+'Q'*len(values),data,addr-base,*values)
    def setup():
        data=bytearray(1152)
        words(data,g,reg,1,1,0x10000,0,0,0,0,0x10000,0)
        words(data,reg,g,rs,1,0,0,0x10000)
        words(data,rs,4096,12288,7,bank0,bank1,16,0,0x10000,0,0x10000)
        words(data,registry,slots,2);words(data,slots,0,desc)
        words(data,desc,1,16,24,0,bitmap,0,1,0);words(data,bitmap,2)
        words(data,q,registry,reg,4128,1,24,0x10000,0)
        return data
    for fault in range(10):
        data=setup();status=2
        if fault==0:words(data,q+16,4129)
        elif fault==1:words(data,q+16,12304)
        elif fault==2:words(data,q+32,MASK)
        elif fault==3:words(data,q+48,32)
        elif fault==4:words(data,q+24,2)
        elif fault==5:words(data,q,0)
        elif fault==6:words(data,q+8,0)
        elif fault==7:words(data,q+40,0x20000);status=3
        elif fault==8:words(data,q+16,4112)
        else:words(data,q+48,1)
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('promotion_remember',g,q)==status
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(data)
    expected=setup();m.reset();m.uc.mem_write(base,bytes(expected))
    for cursor,status in ((8,1),(16,1),(24,0),(24,0)):
        words(expected,q+48,cursor)
        if cursor>=16:words(expected,bank0,1<<5);words(expected,rs+80,1)
        assert m.call('promotion_remember',g,q)==status
        assert bytes(m.uc.mem_read(base,len(expected)))==bytes(expected)
    print('PASS: ARM64 promotion remember 14 independent cursor and unchanged rejection cases')


def verify_marked_survivor_age(m):
    from itertools import product
    base=MEMORY+0x2000;count=0
    for gen,epoch,stamp,age,threshold in product((0,1,2),(0,5,MASK),(0,4,5,MASK),(0,3,15,16),(0,1,4,15,16)):
        before=bytearray(320);age_epoch=4 if age else 0
        struct.pack_into('<2Q',before,64,epoch,gen)
        struct.pack_into('<3Q',before,128,17,stamp,11)
        struct.pack_into('<2Q',before,128+152,age_epoch,age)
        after=bytearray(before)
        if gen or not epoch or not 1<=threshold<=15 or stamp>epoch:expected=2
        elif stamp!=epoch:expected=3
        elif age>15 or epoch<age_epoch:expected=2
        else:
            next_age=age if epoch==age_epoch else min(age+1,15)
            expected=int(next_age>=threshold)
            struct.pack_into('<2Q',after,128+152,epoch,next_age)
        m.reset();m.uc.mem_write(base,bytes(before))
        assert m.call('marked_survivor_age',base,base+128,threshold)==expected,(gen,epoch,stamp,age,threshold)
        assert bytes(m.uc.mem_read(base,len(before)))==bytes(after)
        count+=1
    for fault in range(5):
        before=bytearray(320);struct.pack_into('<2Q',before,64,5,0)
        struct.pack_into('<3Q',before,128,0 if fault==4 else 17,5,11)
        q=0 if fault==0 else base+1 if fault==1 else base
        r=0 if fault==2 else base+129 if fault==3 else base+128
        m.reset();m.uc.mem_write(base,bytes(before))
        assert m.call('marked_survivor_age',q,r,1)==2
        assert bytes(m.uc.mem_read(base,len(before)))==bytes(before);count+=1
    assert count==725
    print('PASS: ARM64 marked survivor age 725 independent epoch/generation/aging cases')


def verify_survivor_age(m):
    base=MEMORY+0x2000;count=0
    def check(epoch,age,cycle,threshold,expected_age,expected_epoch,status,pointer=None,identity=17):
        nonlocal count
        data=bytearray(192)
        struct.pack_into('<3Q',data,0,identity,7,11)
        struct.pack_into('<Q',data,32,19);struct.pack_into('<Q',data,96,23)
        struct.pack_into('<2Q',data,152,epoch,age);data[168:]=bytes([0xab])*24
        expected=bytearray(data);struct.pack_into('<2Q',expected,152,expected_epoch,expected_age)
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('survivor_age',base if pointer is None else pointer,cycle,threshold)==status
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected)
        count+=1
    for age in range(16):
        for threshold in range(1,16):
            for retry in (False,True):
                epoch=10 if age else 0;cycle=10 if age and retry else 11
                new=age if age and retry else min(age+1,15)
                check(epoch,age,cycle,threshold,new,cycle,int(new>=threshold))
    for fault in range(10):
        epoch,age,cycle,threshold,identity=10,3,11,4,17;pointer=None
        if fault==0:pointer=0
        elif fault==1:pointer=base+1
        elif fault==2:identity=0
        elif fault==3:cycle=0
        elif fault==4:cycle=9
        elif fault==5:threshold=0
        elif fault==6:threshold=16
        elif fault==7:age=16
        elif fault==8:epoch=0
        elif fault==9:age=0
        check(epoch,age,cycle,threshold,age,epoch,2,pointer,identity)
    check(MASK-1,15,MASK,15,15,MASK,1)
    check(MASK,15,MASK,15,15,MASK,1)
    assert count==492
    print('PASS: ARM64 survivor age 492 independent cycle, threshold and saturation cases')


def verify_guarded_block_size(m):
    from itertools import product
    base=MEMORY+0x2000;scope=0;ctx=128;page=512;cell=768;heap=1024;count=0
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    for generation,payload,word,fault in product((0,1),(0,1,15,16,17,31,32,33,MASK),(0,3,5),range(8)):
        data=bytearray(2048);sp=base+scope;pp=base+page
        words(data,scope,base+ctx,48,27,base+cell,0)
        struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,0x22)
        words(data,ctx+144,27)
        words(data,page,base+heap,16,1024,27,0,0,generation,5,0,0,80,0,0,0)
        words(data,cell,48,27,word,((1<<63)|1) if word==5 else 1)
        words(data,heap+16,payload,1,0,2)
        if fault==1:words(data,scope+24,0)
        elif fault==2:words(data,ctx+16,0)
        elif fault==3:words(data,heap+40,1)
        elif fault==4:pp=0
        elif fault==5:sp+=1
        elif fault==6:words(data,page+48,2)
        elif fault==7:words(data,ctx+144,0)
        expected=32+max(16,((payload+15)//16)*16) if not fault and payload<=32 else 0
        m.reset();m.uc.mem_write(base,bytes(data))
        result=m.call('guarded_block_size',sp,pp,0x22)
        assert result==expected,(generation,payload,word,fault,result,expected)
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(data);count+=1
    assert count==432
    print('PASS: ARM64 guarded block size 432 independent size and access cases')


def verify_weak_liveness(m):
    from itertools import product
    base=MEMORY+0x2000;ctx=128;page=512;cell=768;record=896;heap=1024;q=1280;count=0
    def put(data,o,*values):struct.pack_into('<'+'Q'*len(values),data,o,*values)
    def fixture(generation,stamp,initializing):
        data=bytearray(2048)
        put(data,0,base+ctx,48,27,base+cell,0)
        struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,0x90044)
        put(data,ctx+144,27)
        put(data,page,base+heap,16,1024,27,0,0,generation,5,0,0,80,0,0,0)
        put(data,cell,48,27,0,1);put(data,record,42,stamp,stamp)
        put(data,heap+16,24,1,base+record,1 if initializing else 2)
        put(data,q,base,base+page,0x90044,(48<<16)|0x1510,base+page,27,(1<<32)|1)
        return data
    def check(data,args,expected):
        m.reset();m.uc.mem_write(base,bytes(data))
        result=m.call('weak_liveness',*args)
        assert result==expected,(args,expected,result)
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(data)
    for gen,obj,stamp,init in product((0,1),(0,1),(0,60,61,62),(0,1)):
        data=fixture(obj,stamp,init)
        expected=3 if init else 0 if gen==0 and obj==1 else 2 if stamp>61 else 0 if stamp==61 else 1
        check(data,[base+q,gen,61],expected);count+=1
    for fault in range(20):
        data=fixture(0,0,0);args=[base+q,0,61]
        if fault==0:args[0]=0
        elif fault==1:args[0]+=1
        elif fault==2:args[1]=2
        elif fault==3:args[2]=0
        elif fault==4:put(data,q+16,0x90022);put(data,ctx+16,0x90022)
        elif fault==5:put(data,ctx+16,0)
        elif fault==6:put(data,ctx+144,0)
        elif fault==7:put(data,24,0)
        elif fault==8:put(data,cell+16,2)
        elif fault==9:put(data,q+24,1)
        elif fault==10:put(data,q+40,28)
        elif fault==11:put(data,q+48,0)
        elif fault==12:put(data,record,0)
        elif fault==13:put(data,heap+32,0)
        elif fault==14:put(data,q+56,8)
        elif fault==15:put(data,record+8,62)
        elif fault==16:put(data,q+8,0)
        elif fault==17:put(data,page+48,2)
        elif fault==18:put(data,page+24,28)
        elif fault==19:put(data,heap+40,7)
        check(data,args,2);count+=1
    assert count==52
    print('PASS: ARM64 weak liveness 52 exact-state epoch, generation and guard observations')


def verify_guarded_identity(m):
    base=MEMORY+0x2000;scope=0;ctx=128;page=512;cell=768;record=896;heap=1024
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    count=0
    for generation in (0,1):
        for fault in range(16):
            data=bytearray(2048);sp=base+scope;pp=base+page
            words(data,scope,base+ctx,48,27,base+cell,0)
            struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,0x22)
            words(data,ctx+144,27)
            words(data,page,base+heap,16,1024,27,0,0,generation,5,0,0,80,0,0,0)
            words(data,cell,48,27,0,1);words(data,record,42)
            words(data,heap+16,24,1,base+record,2)
            if fault==1:words(data,cell+16,3)
            elif fault==2:words(data,cell+24,(1<<63)|1)
            elif fault==3:words(data,heap+32,0)
            elif fault==4:words(data,heap+32,base+record+1)
            elif fault==5:words(data,record,0)
            elif fault==6:words(data,scope+24,0)
            elif fault==7:words(data,ctx+144,0)
            elif fault==8:words(data,ctx+16,0)
            elif fault==9:words(data,heap+40,1)
            elif fault==10:words(data,page+24,28)
            elif fault==11:struct.pack_into('<I',data,ctx+4,1)
            elif fault==12:sp=0
            elif fault==13:pp=0
            elif fault==14:sp+=1
            elif fault==15:pp+=1
            m.reset();m.uc.mem_write(base,bytes(data))
            assert m.call('guarded_identity',sp,pp,0x22)==(base+record if fault<3 else 0)
            assert bytes(m.uc.mem_read(base,len(data)))==bytes(data)
            count+=1
    assert count==32
    print('PASS: ARM64 guarded identity 32 independent full-state access cases')


def verify_logical_cas(m):
    from itertools import product
    base=MEMORY+0x2000;scope=256;ctx=512;page=1024;cell=1536
    registry=1600;desc=1664;slots=1736;bitmap=1792
    satb=2048;remembered=2304;a=2560;b=2816;record=3072;heap=4096
    def words(data,offset,*values):struct.pack_into('<'+'Q'*len(values),data,offset,*values)
    def fixture(phase,generation,observed):
        data=bytearray(12288)
        words(data,0,base+scope,base+page,base+registry,0,observed,(144<<16)|0x1510,phase,0)
        words(data,64,observed,(48<<16)|0x1510,0,27,phase,11)
        words(data,112,observed,(48<<16)|0x1510,0,27,phase,12)
        words(data,scope,base+ctx,48,27,base+cell,0)
        struct.pack_into('<IIQQ',data,ctx,m.context_layout['abi_version'],0,17,phase)
        words(data,ctx+48,base+satb,3,0,0,0);words(data,ctx+88,base+remembered,3,0,0,0)
        words(data,ctx+144,27)
        words(data,page,base+heap,16,8192,27,0,0,generation,5,0,0,80,0,0,0)
        words(data,cell,48,27,0,1)
        words(data,registry,base+slots,2)
        words(data,desc,1,16,24,0,base+bitmap,0,1,0)
        words(data,slots,0,base+desc);words(data,bitmap,3)
        words(data,a,17);words(data,b,17);words(data,record,42)
        words(data,heap+16,24,1,base+record,2);words(data,heap+48,observed)
        return data
    def check(data,expected_record,observed_record,status):
        expected=bytearray(data)
        if status==0:
            observed,desired,phase=struct.unpack_from('<3Q',data,32)
            generation=struct.unpack_from('<Q',data,page+48)[0]
            mask=int(bool(observed) and phase==0x22)|(2 if generation else 0)
            words(expected,56,mask);words(expected,heap+48,desired)
            for bit,request,buffer,destination in ((1,64,ctx+48,satb),(2,112,ctx+88,remembered)):
                if mask&bit:
                    expected[destination:destination+48]=data[request:request+48]
                    words(expected,buffer+16,1,1)
        m.reset();m.uc.mem_write(base,bytes(data))
        assert m.call('guarded_logical_cas',base,expected_record,observed_record)==status
        assert bytes(m.uc.mem_read(base,len(data)))==bytes(expected)
    count=0
    for phase,generation,color,identity in product((0,0x22),(0,1),(0,0x1510,0x2520,0x8630),range(3)):
        observed=(96<<16)|color if color else 0
        data=fixture(phase,generation,observed)
        if color==0x2520:words(data,40,0)
        actual=base+a if observed else 0;expected=(0,base+a,base+b)[identity]
        check(data,expected,actual,0 if expected==actual else 5);count+=1
    for fault in range(12):
        data=fixture(0x22,1,(96<<16)|0x1510);expected=actual=base+a;status=1
        if fault==0:expected=base+b;words(data,ctx+64,3,3);status=5
        elif fault==1:words(data,ctx+64,3,3);status=2
        elif fault==2:words(data,ctx+104,3,3);status=2
        elif fault==3:words(data,heap+48,(144<<16)|0x1510);status=4
        elif fault==4:expected+=1
        elif fault==5:actual+=1
        elif fault==6:actual=0
        elif fault==7:words(data,b,0);expected=base+b
        elif fault==8:words(data,a,0)
        elif fault==9:words(data,scope+24,0)
        elif fault==10:words(data,56,1)
        elif fault==11:words(data,32,0);words(data,heap+48,0)
        check(data,expected,actual,status);count+=1
    assert count==60
    print('PASS: ARM64 logical CAS commit 60 independent identity, color, logging and failure cases')


def verify_source_retire(m):
    from itertools import product
    base=MEMORY+0x2000;q=128;pages=256;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def get(d,o):return unpack('Q',d,o)[0]
    def fixture(head=0,length=3,gen=0,total=4):
        d=bytearray(pages+112*total+128)
        put(d,0,0x100000000,base+pages,total,total<<17,1<<17,0,0,0,112*total+total*1024+4096,112*total+total*1024,0,10000,27,0)
        put(d,q,head,length,27)
        for i in range(head,head+length):
            p=pages+i*112
            put(d,p,0x100000000,(i+1)<<17,(head+length+1)<<17,27,1000,0,gen,8 if i==head else 4,0,0,((i+1)<<17)+48,head,0,0)
        put(d,pages+head*112+64,base+0xf000,length*128)
        put(d,pages+head*112+104,base+len(d)-64)
        return d
    def check(d,op,result,args=None):
        nonlocal count
        e=bytearray(d);head=get(d,q);length=get(d,q+8);p=pages+112*head
        if result in (0,2):
            if op==0:
                put(e,q+24,1,base,get(d,p+64),get(d,p+104),1)
                put(e,p+40,base+q);put(e,p+56,6,0,0);put(e,p+104,0)
            else:
                cursor=get(d,q+56)
                if cursor<length:put(e,pages+112*(head+cursor)+56,0);put(e,q+56,cursor+1)
                if result==0:
                    put(e,72,get(d,72)-length*1024);put(e,p+40,0);put(e,p+56,0);put(e,q+24,2)
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('source_retire',*(args or [base,base+q,op]))
        assert actual==result,(count,op,actual,result)
        assert bytes(m.uc.mem_read(base,len(d)))==bytes(e),(count,op)
        count+=1;return e
    for gen in range(2):
        for head in range(4):
            for length in range(1,5-head):
                d=fixture(head,length,gen);p=pages+112*head
                put(d,p+96,1);check(d,0,3);put(d,p+96,0)
                d=check(d,0,2);check(d,1,3);put(d,q+40,0);check(d,1,3);put(d,q+48,0)
                while get(d,q+24)==1:
                    cursor=get(d,q+56);d=check(d,1,0 if cursor+1>=length else 2)
                    if get(d,q+24)==1:
                        put(d,pages+112*(head+cursor)+24,99);put(d,pages+112*(head+cursor)+56,1)
                check(d,1,1)
    for op,fault in product(range(2),range(34)):
        d=fixture();p=pages;result=1
        if op:
            # Independent prebuilt cleanup state, not a call to the candidate.
            put(d,q+24,1,base,0,0,1);put(d,p+40,base+q);put(d,p+56,6,0,0);put(d,p+104,0)
        if fault==0:put(d,8,0)
        elif fault==1:put(d,8,base+pages+1)
        elif fault==2:put(d,16,0)
        elif fault==3:put(d,16,MASK)
        elif fault==4:put(d,8,MASK-7)
        elif fault==5:put(d,0,0x100000001)
        elif fault==6:put(d,0,MASK-15)
        elif fault==7:put(d,24,0)
        elif fault==8:put(d,24,5<<17)
        elif fault==9:put(d,32,(4<<17)+1)
        elif fault==10:put(d,40,(4<<17)+1)
        elif fault==11:put(d,48,(4<<17)+1)
        elif fault==12:put(d,56,(4<<17)+1)
        elif fault==13:put(d,72,0)
        elif fault==14:put(d,72,get(d,64)+1)
        elif fault==15:put(d,80,get(d,64))
        elif fault==16:put(d,q+8,0)
        elif fault==17:put(d,q+8,5)
        elif fault==18:put(d,q,2)
        elif fault==19:put(d,q+16,0)
        elif fault==20:put(d,q+24,op+1)
        elif fault==21:put(d,q+32,0 if op else base)
        elif fault==22:put(d,p+32,0)
        elif fault==23:put(d,p+48,2)
        elif fault==24:put(d,p+24,28)
        elif fault==25:put(d,p+56,5)
        elif fault==26:put(d,p+16,5<<17)
        elif fault==27:put(d,q+40,1);result=3 if op else 1
        elif fault==28:put(d,q+48,1);result=3 if op else 1
        elif fault==29:put(d,q+56,0 if op else 1)
        elif fault==30:put(d,p+96,1);result=3
        elif fault==31:put(d,p+40,8)
        elif fault==32:put(d,p+104,get(d,p+104)+1)
        elif fault==33:put(d,72,4*112+3*1024-1)
        check(d,op,result)
    for fault in range(11):
        d=fixture(0,4,1);p=pages;t=pages+112
        put(d,q+24,1,base,0,0,1);put(d,p+40,base+q);put(d,p+56,6,0,0);put(d,p+104,0)
        if fault==0:put(d,t,0x100000001)
        elif fault==1:put(d,t+88,1)
        elif fault==2:put(d,t+8,(2<<17)+16)
        elif fault==3:put(d,t+16,(5<<17)+16)
        elif fault==4:put(d,t+24,28)
        elif fault==5:put(d,t+32,1001)
        elif fault==6:put(d,t+48,0)
        elif fault==7:put(d,t+40,1)
        elif fault==8:put(d,t+96,1)
        elif fault==9:put(d,t+64,8)
        else:put(d,t+56,0)
        check(d,1,1)
    for gen in range(2):
        d=check(fixture(0,128,gen,128),0,2);put(d,q+40,0);put(d,q+48,0)
        for cursor in range(1,128):
            assert get(d,q+56)==cursor;d=check(d,1,0 if cursor==127 else 2)
        assert get(d,72)==128*112 and get(d,48)==0
    for args in ([0,base+q,0],[base+1,base+q,0],[base,0,0],[base,base+q+1,0],[base,base+q,2]):
        check(fixture(),args[2],1,args)
    d=check(fixture(),0,2);put(d,q+40,0);put(d,q+48,0)
    copied=len(d)-64;d[copied:copied+64]=d[q:q+64]
    check(d,1,1,[base,base+copied,1])
    print(f'PASS: ARM64 source retirement {count} independent exact-state and 128-page sliced release cases')


def verify_retire_inventory(m):
    # Read-only full-registration predicate. Expected statuses and whole-memory
    # preservation are independent of the C implementation/vector producer.
    from itertools import product
    base=MEMORY+0x2000;a,queue,hs,q,rem,task=128,384,512,1472,1536,1664
    p=[640,704];ctx=[832,1152];w=a+64;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def get(d,o):return unpack('Q',d,o)[0]
    def fixture(gen,colors=0,other=0,native=0,roots=1):
        d=bytearray(2176);phase=0x90000|(colors<<8)|(7<<(gen*4))|(other<<((1-gen)*4))
        put(d,0,2*roots,8,2*roots,0,0,(base+1792 if roots else 0),phase,(base+1792 if roots else 0),0)
        put(d,a,7,2*roots,phase,2,0,base+queue,61,gen)
        put(d,w,base,phase,0,0,0,1<<gen)
        put(d,queue,base+task,1,1);put(d,queue+64,61,gen)
        n=native.bit_count();put(d,hs,9,2,n,2-n,2-n,1,18,base+p[1])
        for i in range(2):
            is_native=bool(native&(1<<i))
            put(d,p[i],base+ctx[i],17+i,0 if is_native else 9,1,base+hs,base+p[1] if i==0 else 0,base+p[0] if i else 0)
            struct.pack_into('<IIQQ',d,ctx[i],m.context_layout['abi_version'],2 if is_native else 1,17+i,0 if is_native else phase)
            put(d,ctx[i]+128,base+1792+i*160 if roots else 0,0,0,9+i)
            put(d,ctx[i]+224+gen*8,7);put(d,ctx[i]+240+gen*8,17+i);put(d,ctx[i]+256+gen*8,9+i)
        put(d,q,base+hs,base+a,9,base+rem);put(d,rem+24,phase);put(d,rem+64,phase)
        return d
    def check(d,result,op=0):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('retire_inventory',base,base+q,op)
        assert actual==result,(count,op,actual,result)
        assert bytes(m.uc.mem_read(base,len(d)))==bytes(d),(count,op)
        count+=1
    for gen,colors,other,native,roots in product(range(2),range(32),range(5),range(4),range(2)):
        check(fixture(gen,colors,other,native,roots),0)
    for gen,owner,native,roots,fault in product(range(2),range(2),range(4),range(2),range(19)):
        d=fixture(gen,native=native,roots=roots);c=ctx[owner];r=p[owner];result=1
        if fault==0:put(d,c+280,1);result=2
        elif fault==1:put(d,c+168,1);result=2
        elif fault==2:put(d,c+216,1);result=2
        elif fault==3:put(d,c+64,1);result=2
        elif fault==4:put(d,c+112,1)
        elif fault==5:put(d,c+152,99);result=3 if roots else 0
        elif fault==6:put(d,c+224+gen*8,8);result=3 if roots else 0
        elif fault==7:put(d,c+240+gen*8,99);result=3 if roots else 0
        elif fault==8:put(d,c+144,1)
        elif fault==9:put(d,c+136,1);result=2
        elif fault==10:put(d,c+8,99)
        elif fault==11:put(d,r+24,0)
        elif fault==12:put(d,r+40,base+r)
        elif fault==13:put(d,r+16,0);result=0 if native&(1<<owner) else 1
        elif fault==14:put(d,c+16,0);result=0 if native&(1<<owner) else 1
        elif fault==15:struct.pack_into('<I',d,c+4,0)
        elif fault==16:struct.pack_into('<I',d,c,0)
        elif fault==17:put(d,r+48,base+r)
        elif fault==18:put(d,c+80,1)
        check(d,result)
    for gen,fault in product(range(2),range(30)):
        d=fixture(gen,native=1);result=1;op=0
        if fault==0:op=1
        elif fault==1:put(d,q,0)
        elif fault==2:put(d,q+8,0)
        elif fault==3:put(d,q+16,0)
        elif fault==4:put(d,q+16,10);result=3
        elif fault==5:put(d,hs+24,0,0,0);result=2
        elif fault==6:put(d,hs+32,0);result=2
        elif fault==7:put(d,hs+56,0)
        elif fault in (8,9,10):put(d,(24,32,64)[fault-8],1);result=2
        elif fault==11:put(d,48,get(d,48)+0x10000);result=3
        elif fault==12:put(d,a+8,3);result=3
        elif fault==13:put(d,queue+64,62);result=3
        elif fault==14:put(d,queue+16,0)
        elif fault==15:put(d,queue+16,0,1,1,1);result=2
        elif fault==16:put(d,queue+16,0);put(d,queue+48,1);result=2
        elif fault==17:put(d,queue+80,base+task,base+task,1);result=2
        elif fault==18:put(d,q+24,0)
        elif fault==19:put(d,rem+72,1);result=2
        elif fault==20:
            phase=get(d,rem+24)^0x1000;put(d,rem+24,phase);put(d,rem+64,phase);result=3
        elif fault==21:put(d,rem+64,get(d,48)+0x10000);result=3
        elif fault==22:put(d,hs+8,3);put(d,hs+24,2,2)
        elif fault==23:put(d,hs+16,0,2,2)
        elif fault==24:put(d,p[1]+48,0)
        elif fault==25:put(d,a+24,1)
        elif fault==26:put(d,queue+72,1-gen);result=3
        elif fault==27:put(d,48,get(d,48)-(1<<(gen*4)));result=3
        else:
            put(d,rem,base+1792,1,1);put(d,rem+(48 if fault==28 else 56),1);result=2
        check(d,result,op)
    for gen in range(2):
        d=fixture(gen,roots=0);put(d,hs+8,0,0,0,0,1,18,0);check(d,0)
    assert count==3230
    print(f'PASS: ARM64 Retire inventory {count} independent all-owner, queue and rendezvous cases')


def verify_root_retire_audit(m):
    # Independent single-segment inventory model, including complete memory
    # effects, owner certificates and strict snapshot rejection in Retire.
    from itertools import product
    base=MEMORY+0x2000; a,q,ctx,reg,seg,cells,journal=128,384,512,832,1024,1088,1120
    w=a+64;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def get(d,o):return unpack('Q',d,o)[0]
    def fixture(gen,colors,other):
        d=bytearray(1200);phase=0x90000|(colors<<8)|(7<<(gen*4))|(other<<((1-gen)*4))
        put(d,0,3,17,1,0,0,base+reg,phase,base+reg,0)
        put(d,a+32,base+ctx,base+q);put(d,q+64,61,gen)
        struct.pack_into('<IIQQ',d,ctx,m.context_layout['abi_version'],1,17,phase)
        put(d,ctx+128,base+reg,0,0,9)
        put(d,reg,base+seg,3,1);put(d,reg+120,base+ctx)
        put(d,seg,17,3,base+cells,1,1,1,base+journal,base+reg)
        put(d,cells,0x101510,1,1);return d
    def check(d,op,result):
        nonlocal count
        e=bytearray(d);gen=get(d,q+72)
        if op==0 and result==0:
            put(e,8,19);put(e,32,1);put(e,64,1);put(e,reg+24,1)
            put(e,a,18,3,get(d,48),1,base+ctx,base+q,61,gen)
            put(e,w,base,get(d,48),1,1,0,1<<gen,0,base+reg,3,19,1,0)
            put(e,ctx+224+gen*8,18);put(e,ctx+240+gen*8,17);put(e,ctx+256+gen*8,9)
        elif op==1 and result==2:
            put(e,32,0);put(e,64,0);put(e,reg+24,0);put(e,a+24,2)
            put(e,w+16,0,0,0,1<<gen,0);put(e,w+80,0)
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('root_retire_audit',base,base+a,op)
        assert actual==result,(count,op,actual,result)
        assert bytes(m.uc.mem_read(base,len(d)))==bytes(e),(count,op)
        count+=1;return e
    for gen,colors,other in product(range(2),range(32),range(8)):
        d=check(fixture(gen,colors,other),0,0)
        check(d,1,4)  # cells have not been processed
        put(d,w+32,1);put(d,w+48,1<<gen)
        for offset in (journal+24,reg+144,w+96,w+104,w+112,w+120):
            busy=bytearray(d);put(busy,offset,1);check(busy,1,4)
        d=check(d,1,2);check(d,2,2);check(d,3,2)
        for bit in range(32):
            changed=bytearray(d);phase=get(d,48)^(1<<bit);put(changed,48,phase)
            valid=not phase&0xe000 and phase&15<=7 and (phase>>4)&15<=7
            check(changed,2,5 if valid else 1)
        for offset,result in ((0,5),(q+64,5),(ctx+152,5),(ctx+136,4),(ctx+144,1),
                              (ctx+224+gen*8,5),(ctx+240+gen*8,5),(ctx+256+gen*8,5)):
            changed=bytearray(d);put(changed,offset,get(changed,offset)+1);check(changed,3,result)
    for gen,phase in product(range(2),range(7)):
        d=fixture(gen,0,0);put(d,48,phase<<(gen*4));check(d,0,1)
    for gen,has_roots,fault in product(range(2),range(2),range(17)):
        d=check(fixture(gen,0,0),0,0)
        put(d,w+32,1);put(d,w+48,1<<gen);d=check(d,1,2)
        if not has_roots:put(d,ctx+128,0)
        result=4
        if fault==0:put(d,ctx+280,1)
        elif fault<=6:
            consumer=ctx+(160 if fault<=3 else 192)
            put(d,consumer+8+((fault-1)%3)*8,1)
        elif fault<=14:
            buffer=ctx+(48 if fault<=10 else 88);kind=(fault-7)%4
            if kind==0:put(d,buffer+16,1)
            elif kind==1:put(d,buffer+16,1,1)
            elif kind==2:put(d,buffer+32,1);result=1
            else:put(d,buffer+24,1);result=1
        else:
            buffer=ctx+(48 if fault==15 else 88)
            put(d,buffer+16,MASK,MASK,MASK);put(d,ctx+160,17);put(d,ctx+192,19);result=2
        check(d,3,result)
    assert count==26330
    print(f'PASS: ARM64 Retire root audit {count} independent inventory, owner and snapshot cases')


def verify_relocation_retire(m):
    from itertools import product
    base=MEMORY+0x2000;count=0
    hs,gate,q=128,256,320
    def put(d,o,*values):struct.pack_into('<'+'Q'*len(values),d,o,*values)
    def get(d,o):return struct.unpack_from('<Q',d,o)[0]
    def fixture(gen,epochs=0,other=2,native=0):
        d=bytearray(512)
        phase=0x90000|(epochs<<8)|(6<<(gen*4))|(other<<((1-gen)*4))
        put(d,48,phase);put(d,hs,7,2,native,0,0,0,2,base+480)
        put(d,gate,0,0,0,8,1);put(d,q,base+hs,base+gate,gen,0,0,0,0)
        return d
    def check(before,op,result,args=None):
        nonlocal count
        after=bytearray(before);gen=get(before,q+16)
        if result==0:
            serial=get(before,hs)+1
            phase=((get(before,48)+0x10000)&~(15<<(gen*4)))|(7<<(gen*4))
            put(after,48,phase);put(after,hs,serial)
            put(after,hs+24,2-get(before,hs+16),0,1)
            put(after,q+24,1,serial,phase,get(before,gate+24))
        elif result==6:
            put(after,hs+24,0,0,0);put(after,q+24,2)
        m.reset();m.uc.mem_write(base,bytes(before))
        actual=m.call('relocation_retire',*(args or [base,base+q,op]))
        assert actual==result,(count,op,actual,result)
        assert bytes(m.uc.mem_read(base,len(before)))==bytes(after),(count,op)
        count+=1;return after
    for gen,epochs,other,native in product(range(2),range(32),range(5),range(3)):
        d=check(fixture(gen,epochs,other,native),0,0)
        if native<2:check(d,1,1)
        put(d,hs+32,2-native);check(d,1,6)
    for gen in range(2):
        for fault in range(30):
            d=fixture(gen);op=0;result=2;args=[base,base+q,op]
            if fault>=18:
                phase=get(d,48)+0x10000+(1<<(gen*4))
                put(d,48,phase);put(d,hs,8);put(d,hs+24,2,2,1)
                put(d,q+24,1,8,phase,8);op=1;args[2]=1
            if fault==0:args[0]=0
            elif fault==1:args[0]+=1
            elif fault==2:args[1]=0
            elif fault==3:args[1]+=1
            elif fault==4:args[2]=2
            elif fault==5:put(d,q,0)
            elif fault==6:put(d,q,base+hs+1)
            elif fault==7:put(d,q+8,0)
            elif fault==8:put(d,q+8,base+gate+1)
            elif fault==9:put(d,q+16,2)
            elif fault==10:put(d,48,get(d,48)|0xe000)
            elif fault==11:put(d,hs+16,3)
            elif fault==12:put(d,q+40,1)
            elif fault==13:put(d,48,0x90000|(6<<(gen*4))|(7<<((1-gen)*4)));result=1
            elif fault==14:put(d,gate+32,0);result=1
            elif fault==15:put(d,gate+24,0)
            elif fault==16:put(d,hs,MASK);result=3
            elif fault==17:put(d,48,(MASK&~0xffff)|(get(d,48)&0xffff));result=3
            elif fault==18:put(d,q+24,0)
            elif fault==19:put(d,q+32,0)
            elif fault==20:put(d,q+48,0)
            elif fault==21:put(d,q+40,get(d,48)^0x10000);result=4
            elif fault==22:put(d,q+32,9);result=4
            elif fault==23:put(d,gate+24,9);result=4
            elif fault==24:put(d,hs+24,0,0,0)
            elif fault==25:put(d,hs+32,0);result=1
            elif fault==26:put(d,gate,base+464,1,1);result=1
            elif fault==27:put(d,gate+32,2)
            elif fault==28:put(d,hs,MASK);put(d,q+32,MASK);result=6
            elif fault==29:
                phase=(MASK&~0xffff)|(get(d,48)&0xffff)
                put(d,48,phase);put(d,q+40,phase);result=6
            check(d,op,result,args)
        d=fixture(gen,native=2)
        put(d,hs,MASK-1);put(d,gate+24,MASK)
        put(d,48,((MASK>>16)-1)<<16|(get(d,48)&0xffff))
        d=check(d,0,0);check(d,1,6)
    assert count==2624,count
    print('PASS: ARM64 relocation retire 2624 independent whole-memory closed-gate adoption cases')


def verify_relocation_phase(m):
    from itertools import product
    base=MEMORY+0x2000;count=0
    roots,hs,gate,q=0,128,256,320
    def put(d,o,*values):struct.pack_into('<'+'Q'*len(values),d,o,*values)
    def get(d,o):return struct.unpack_from('<Q',d,o)[0]
    def fixture(gen,epochs=0,other=2,native=0):
        d=bytearray(512)
        phase=0x90000|(epochs<<8)|(4<<(gen*4))|(other<<((1-gen)*4))
        put(d,48,phase);put(d,hs,7,2,native,0,0,0,2,base+480)
        put(d,gate,0,0,0,8,1);put(d,q,base+hs,base+gate,gen,0,0,0,0)
        return d
    def check(before,op,result,args=None):
        nonlocal count
        after=bytearray(before);gen=get(before,q+16)
        if result==0:
            serial=get(before,hs)+1;phase=get(before,48)
            phase=((phase+0x10000)&~(15<<(gen*4)))|((5+op)<<(gen*4))
            if op==0:phase^=1<<(10+gen)
            put(after,48,phase);put(after,hs,serial)
            put(after,hs+24,2-get(before,hs+16),0,1)
            put(after,q+24,op+1,serial,phase,get(before,gate+24))
        elif result==5:
            epoch=get(before,gate+24)+1
            put(after,gate+24,epoch,0);put(after,hs+24,0,0,0)
            put(after,q+24,3);put(after,q+48,epoch)
        m.reset();m.uc.mem_write(base,bytes(before))
        actual=m.call('relocation_phase',*(args or [base,base+q,op]))
        assert actual==result,(count,op,actual,result)
        assert bytes(m.uc.mem_read(base,len(before)))==bytes(after),(count,op)
        count+=1;return after
    for gen,epochs,other,native in product(range(2),range(32),range(5),range(3)):
        d=check(fixture(gen,epochs,other,native),0,0)
        if native<2:check(d,1,1)
        put(d,hs+32,2-native);d=check(d,1,0)
        if native<2:check(d,2,1)
        put(d,hs+32,2-native);check(d,2,5)
    for gen in range(2):
        for fault in range(30):
            d=fixture(gen);op=0;result=2;args=[base,base+q,op]
            if fault>=18:
                # An independently constructed acknowledged start boundary.
                phase=(get(d,48)+0x10000+(1<<(gen*4)))^(1<<(10+gen))
                put(d,48,phase);put(d,hs,8);put(d,hs+24,2,2,1)
                put(d,q+24,1,8,phase,8);op=1;args[2]=1
            if fault==0:args[0]=0
            elif fault==1:args[0]+=1
            elif fault==2:args[1]=0
            elif fault==3:args[1]+=1
            elif fault==4:args[2]=3
            elif fault==5:put(d,q,0)
            elif fault==6:put(d,q,base+hs+1)
            elif fault==7:put(d,q+8,0)
            elif fault==8:put(d,q+8,base+gate+1)
            elif fault==9:put(d,q+16,2)
            elif fault==10:put(d,48,get(d,48)|0xe000)
            elif fault==11:put(d,hs+16,3)
            elif fault==12:put(d,q+24,1)
            elif fault==13:put(d,48,0x90000|(4<<(gen*4))|(7<<((1-gen)*4)));result=1
            elif fault==14:put(d,gate+32,0);result=1
            elif fault==15:put(d,gate+24,MASK);result=3
            elif fault==16:put(d,hs,MASK-1);result=3
            elif fault==17:put(d,48,(MASK&~0xffff)|(get(d,48)&0xffff));result=3
            elif fault==18:put(d,q+24,0)
            elif fault==19:put(d,q+32,0)
            elif fault==20:put(d,q+48,0)
            elif fault==21:put(d,q+40,get(d,48)^0x10000);result=4
            elif fault==22:put(d,q+32,9);result=4
            elif fault==23:put(d,gate+24,9);result=4
            elif fault==24:put(d,hs+24,0,0,0)
            elif fault==25:put(d,hs+32,0);result=1
            elif fault==26:put(d,gate,base+464,1,1);result=1
            elif fault==27:put(d,gate+32,2)
            elif fault==28:put(d,hs,MASK);put(d,q+32,MASK);result=3
            elif fault==29:
                phase=(MASK&~0xffff)|(get(d,48)&0xffff)
                put(d,48,phase);put(d,q+40,phase);result=3
            check(d,op,result,args)
    assert count==4220,count
    print('PASS: ARM64 relocation phase 4220 independent whole-memory adoption and rejection cases')


def verify_marked_relocation(m):
    from itertools import product
    base=MEMORY+0x2000;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(gen,stamp,closed=0,word=0):
        d=bytearray(768);put(d,0,0,0,0,11,closed)
        put(d,128,16,7,word,0)
        put(d,192,base+64,base+128,16,7,0,0,base+320,base+512,5)
        put(d,320+64,5,gen);put(d,512,17,stamp if not gen else 9,stamp if gen else 9)
        return d
    for gen,stamp,closed,word in product((0,1,2),(4,5,6),(0,1),(0,5,3,0x201512)):
        before=fixture(gen,stamp,closed,word);after=bytearray(before)
        expected=1 if gen>1 else 7 if stamp<5 else 8 if stamp>5 else 6 if closed else {0:0,5:2,3:3,0x201512:4}[word]
        if expected in (0,2,3,4):put(after,16,1)
        if expected==0:
            put(after,0,base+64,1)
            put(after,64,base,1,11,0,0,0)
            put(after,128+16,5);put(after,192+32,1,11)
        m.reset();m.uc.mem_write(base,bytes(before))
        result=m.call('marked_relocation',base,base+192)
        assert result==expected,(gen,stamp,closed,word,result,expected)
        assert bytes(m.uc.mem_read(base,len(before)))==bytes(after);count+=1
        if expected==0:
            assert m.call('marked_relocation',base,base+192)==1
            assert bytes(m.uc.mem_read(base,len(before)))==bytes(after);count+=1
    for gen in (0,1):
        for fault in range(20):
            before=fixture(gen,5);gate=base;q=base+192;expected=1
            if fault==0:gate=0
            elif fault==1:gate+=1
            elif fault==2:q=0
            elif fault==3:q+=1
            elif fault==4:put(before,240,0)
            elif fault==5:put(before,240,base+321)
            elif fault==6:put(before,248,0)
            elif fault==7:put(before,248,base+513)
            elif fault==8:put(before,256,0)
            elif fault==9:put(before,384,0)
            elif fault==10:put(before,512,0)
            elif fault==11:put(before,384,6);expected=8
            elif fault==12:put(before,192,0)
            elif fault==13:put(before,200,0)
            elif fault==14:put(before,208,0)
            elif fault==15:put(before,216,0)
            elif fault==16:put(before,224,1)
            elif fault==17:put(before,232,1)
            elif fault==18:put(before,32,2)
            elif fault==19:put(before,24,0)
            m.reset();m.uc.mem_write(base,bytes(before))
            assert m.call('marked_relocation',gate,q)==expected,(gen,fault)
            assert bytes(m.uc.mem_read(base,len(before)))==bytes(before);count+=1
    assert count==114
    print('PASS: ARM64 marked relocation 114 independent selection/admission cases')


def verify_cycle_mark_start(m):
    base=MEMORY+0x2000;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    for gen in (0,1):
        for native in range(3):
            for fault in range(20):
                before=bytearray(1024)
                put(before,gen*8,1);put(before,64,0,gen,0)
                put(before,96,base+128,base+256)
                put(before,128+48,0x90011)
                put(before,256,base+384,base+512,gen,0,0,0,0,0,0)
                put(before,384,7,2,native,0,0,0,2,base+960)
                put(before,512+40,base+768);put(before,768+64,9,gen)
                args=[base,base+64,base+96];expected=0
                if fault==1:put(before,gen*8,0);expected=4
                elif fault==2:put(before,16+gen*8,1);expected=1
                elif fault==3:put(before,72,2);expected=2
                elif fault==4:put(before,64,1);expected=2
                elif fault==5:put(before,80,1);expected=2
                elif fault==6:put(before,272,1-gen);expected=2
                elif fault==7:put(before,280,4);expected=2
                elif fault==8:put(before,176,0x90022);expected=2
                elif fault==9:put(before,384+24,2-native,0,1);expected=1
                elif fault==10:put(before,384,MASK);expected=3
                elif fault==11:put(before,176,(MASK&~0xffff)|0x11);expected=3
                elif fault==12:put(before,768+64,0);expected=2
                elif fault==13:put(before,768+72,1-gen);expected=2
                elif fault==14:put(before,96,0);expected=2
                elif fault==15:put(before,104,base+257);expected=2
                elif fault==16:args[0]=0;expected=2
                elif fault==17:args[1]+=1;expected=2
                elif fault==18:args[2]=0;expected=2
                elif fault==19:put(before,384+24,1);expected=2
                after=bytearray(before)
                if expected==0:
                    put(after,16+gen*8,1);put(after,64,base,gen,1)
                    put(after,256+24,4,8,0x90011,9,0)
                    put(after,384,8,2,native,2-native,0,1)
                m.reset();m.uc.mem_write(base,bytes(before))
                result=m.call('cycle_mark_start',*args)
                assert result==expected,(gen,native,fault,result,expected)
                assert bytes(m.uc.mem_read(base,len(before)))==bytes(after),(gen,native,fault)
                count+=1
    assert count==120
    print('PASS: ARM64 cycle mark start 120 independent atomic admission cases')


def verify_cycle_pressure(m):
    from itertools import product
    base=MEMORY+0x2000;count=0
    # Independent arithmetic and exact whole-memory effects, including lanes
    # with an active cycle or a previously coalesced next request.
    for kind,busy,pending,pressure in product(range(2),repeat=4):
        for fault in range(12):
            before=bytearray(320);heap=[0]*14
            heap[3:8]=[1000,600,0,500,0]
            sample=[0x1000,17,1,100,1,100 if pressure else 99]
            ledger=[5,5,5,5,5,5];ledger[4+kind]-=busy;ledger[kind]+=pending
            ticket=[0,kind,0];q=[base,base+128,1];args=[base+192,base+240,base+272]
            expected=0 if pressure else 4
            if fault==1:ticket[1]=2;expected=2
            elif fault==2:ticket[0]=1;expected=2
            elif fault==3:ticket[2]=1;expected=2
            elif fault==4:q[2]=0;expected=2
            elif fault==5:sample[4]=0;expected=2
            elif fault==6:
                ledger[kind]=ledger[kind+2]=ledger[kind+4]=MASK
                expected=3 if pressure else 4
            elif fault==7:ledger[kind]=0;expected=2 if pressure else 4
            elif fault==8:heap[3]=0;expected=2
            elif fault==9:args[0]=0;expected=2
            elif fault==10:args[1]+=1;expected=2
            elif fault==11:args[2]=0;expected=2
            for off,values in ((0,heap),(128,sample),(192,ledger),(240,ticket),(272,q)):
                struct.pack_into('<'+'Q'*len(values),before,off,*values)
            after=bytearray(before)
            if expected==0:
                struct.pack_into('<Q',after,192+8*kind,6)
                struct.pack_into('<3Q',after,240,base+192,kind,6)
            m.reset();m.uc.mem_write(base,bytes(before))
            result=m.call('cycle_pressure',*args)
            assert result==expected,(kind,busy,pending,pressure,fault,result,expected)
            assert bytes(m.uc.mem_read(base,len(before)))==bytes(after)
            count+=1
    assert count==192
    print('PASS: ARM64 cycle pressure 192 independent request and nonmutation cases')


def verify_heap_pressure(m):
    from itertools import product
    base=MEMORY+0x2000;sample=base+128;count=0
    for hard in (1,1000,MASK):
        for soft,reserve,committed in product((0,hard//2,hard),repeat=3):
            for pending,delta,horizon in product((0,hard-committed,hard),(0,1,MASK-1),(1,2)):
                m.reset();before=bytearray(256)
                heap=[0]*14;heap[3:8]=[hard,soft,reserve,committed,pending];heap[13]=count&1
                struct.pack_into('<14Q',before,0,*heap)
                struct.pack_into('<6Q',before,128,0x1000,17,1,MASK-1,1,delta)
                headroom=max(0,min(soft,hard-reserve)-committed-pending)
                expected=2 if committed+pending>hard else int(delta*horizon>=headroom)
                m.uc.mem_write(base,bytes(before))
                assert m.call('heap_allocation_pressure',base,sample,horizon)==expected,(hard,soft,reserve,committed,pending,delta,horizon)
                assert bytes(m.uc.mem_read(base,256))==bytes(before)
                count+=1
    for fault in range(11):
        m.reset();before=bytearray(256);heap=[0]*14
        heap[3:8]=[1000,600,200,0,0];q=[0x1000,17,1,MASK-1,1,1]
        h=base;s=sample;horizon=1
        if fault==0:heap[3]=0
        elif fault==1:heap[4]=1001
        elif fault==2:heap[5]=1001
        elif fault==3:heap[6]=1001
        elif fault==4:heap[7]=1001
        elif fault==5:h=0
        elif fault==6:h+=1
        elif fault==7:s=0
        elif fault==8:s+=1
        elif fault==9:horizon=0
        elif fault==10:q[5]=MASK
        struct.pack_into('<14Q',before,0,*heap);struct.pack_into('<6Q',before,128,*q)
        m.uc.mem_write(base,bytes(before))
        assert m.call('heap_allocation_pressure',h,s,horizon)==2,fault
        assert bytes(m.uc.mem_read(base,256))==bytes(before)
        count+=1
    assert count==1469
    print('PASS: ARM64 heap pressure 1469 independent soft/reserve/full-width and rejection cases')


def verify_allocation_pressure(m):
    address=MEMORY+0x2000
    values=(0,1,(1<<32)-1,1<<32,MASK-1)
    times=(1,1<<32,MASK)
    headrooms=(0,1,(1<<32)-1,1<<32,MASK)
    count=0
    for delta in values:
        for elapsed in times:
            for headroom in headrooms:
                for horizon in times:
                    m.reset()
                    before=struct.pack('<6Q',0x1000,17,elapsed,MASK-1,elapsed,delta)
                    m.uc.mem_write(address,before)
                    expected=int(delta*horizon>=headroom*elapsed)
                    assert m.call('allocation_pressure',address,headroom,horizon)==expected,(delta,elapsed,headroom,horizon)
                    assert bytes(m.uc.mem_read(address,48))==before
                    count+=1
    for fault in range(11):
        m.reset()
        q=[0x1000,17,20,100,10,40];arg=address;horizon=10
        if fault==0:arg=0
        elif fault==1:arg+=1
        elif fault==2:q[0]=0
        elif fault==3:q[0]+=1
        elif fault==4:q[1]=0
        elif fault==5:q[4]=0
        elif fault==6:horizon=0
        elif fault==7:q[3]=MASK
        elif fault==8:q[5]=101
        elif fault==9:q[4]=21
        elif fault==10:q=[0]*6
        before=struct.pack('<6Q',*q);m.uc.mem_write(address,before)
        assert m.call('allocation_pressure',arg,100,horizon)==2,fault
        assert bytes(m.uc.mem_read(address,48))==before
        count+=1
    assert count==236
    print('PASS: ARM64 allocation pressure 236 unbounded-integer oracle and rejection cases')


def verify_native_startup(m):
    m.reset()
    base = MEMORY + 0x2000
    roots, descriptors, query = base + 128, base + 160, base + 192
    count = 0
    for quantum in (8,4096,16384):
        for cells, entries, capacity in ((1,1,5),(3,2,64),(0xffffffff,0xffffffff,0x100000000)):
            rb = (264 + (cells+entries)*32 + quantum-1) & -quantum
            db = (216 + capacity*8 + quantum-1) & -quantum
            for fault in range(11):
                before = bytearray(512)
                heap = [0,0,1,0,0,0,0,0,8192+rb+db,4096,4096,0,0,0]
                request = [roots,descriptors,cells,entries,capacity,quantum]
                harg, qarg, expected = base, query, 1
                if fault == 0: expected = 0
                elif fault == 1: heap[8] -= 1; expected = 2
                elif fault == 2: request[5] = 3
                elif fault == 3: request[5] = 1 << 63
                elif fault == 4: request[2] = 0
                elif fault == 5: struct.pack_into('<Q',before,128,8)
                elif fault == 6: harg = 0
                elif fault == 7: qarg = 0
                elif fault == 8: request[1] += 1
                elif fault == 9: request[1] = roots
                elif fault == 10: heap[2] = 0
                struct.pack_into('<14Q',before,0,*heap)
                struct.pack_into('<6Q',before,192,*request)
                after = bytearray(before)
                if expected == 0:
                    struct.pack_into('<Q',after,80,4096+rb+db)
                    struct.pack_into('<4Q',after,128,rb,base,0,1)
                    struct.pack_into('<4Q',after,160,db,base,0,1)
                m.uc.mem_write(base,bytes(before))
                assert m.call('native_startup',harg,qarg) == expected, (quantum,cells,fault)
                assert bytes(m.uc.mem_read(base,len(after))) == bytes(after), (quantum,cells,fault)
                count += 1
    assert count == 99
    print('PASS: ARM64 startup admission 99 independent sizing, budget and preservation cases')


def verify_weak_mark(m):
    base=MEMORY+0x2000;hs=128;audit=256;queue=512;mark=640;gate=736;owner=800;size=896;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(gen):
        d=bytearray(size);put(d,24,1);put(d,48,0x90022)
        put(d,hs,7,0,0,0,0,0,0,0);put(d,audit+40,base+queue)
        put(d,queue+64,61,gen);put(d,mark,base+hs,base+audit,gen,0,0,0,0,0,0)
        put(d,gate,base+864,0,base,base+hs);put(d,owner,base+gate,base+mark,0,0,0);return d
    def check(d,op,status,expected=None,args=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('weak_mark',*(args or [base,base+owner,op]))
        assert actual==status,(count,actual,status)
        result=d if expected is None else expected
        assert bytes(m.uc.mem_read(base,size))==bytes(result),count
        count+=1;return bytearray(result)
    for gen in range(2):
        for fault in range(16):
            d=fixture(gen);status=2;args=[base,base+owner,0]
            if fault==0:put(d,gate+16,base+8)
            elif fault==1:put(d,gate+24,base+hs+8)
            elif fault==2:put(d,owner+16,3)
            elif fault==3:put(d,gate+8,2)
            elif fault==4:put(d,gate,0)
            elif fault==5:put(d,gate+8,1);status=1
            elif fault==6:put(d,mark+16,2)
            elif fault==7:put(d,hs,MASK);status=3
            elif fault==8:put(d,owner+16,1);args[2]=1
            elif fault==9:args[2]=3
            elif fault==10:put(d,owner+24,base+864)
            elif fault==11:put(d,owner+32,base+864)
            elif fault==12:args[0]=0
            elif fault==13:args[1]+=1
            elif fault==14:put(d,mark+8,0)
            elif fault==15:put(d,hs+40,1);status=1
            check(d,args[2],status,args=args)
        d=fixture(gen);expected=bytearray(d);phase=(0xa0022&~(15<<(4*gen)))|(3<<(4*gen))
        put(expected,48,phase);put(expected,hs,8);put(expected,hs+40,1)
        put(expected,mark+24,1,8,phase,61,0);put(expected,gate+8,1);put(expected,owner+16,1)
        d=check(d,0,0,expected)
        # The real end predicate sees pending retiring roots and chooses RETRY.
        expected=bytearray(d);put(expected,48,0xb0022);put(expected,hs,9)
        put(expected,mark+24,2,9,0xb0022,61,2);put(expected,owner+16,2)
        d=check(d,1,0,expected)
        expected=bytearray(d);put(expected,hs+40,0);put(expected,mark+24,3)
        put(expected,gate+8,0);put(expected,owner+16,0);check(d,2,5,expected)
        # Isolate SELECT adoption completion; full SELECT decision is exercised
        # by the native two-actor suite, not manufactured as mark-end evidence.
        d=fixture(gen);phase=(0xb0022&~(15<<(4*gen)))|(4<<(4*gen))
        put(d,48,phase);put(d,hs,9);put(d,hs+40,1)
        put(d,mark+24,2,9,phase,61,4);put(d,gate+8,1);put(d,owner+16,2)
        expected=bytearray(d);put(expected,hs+40,0);put(expected,mark+24,3);put(expected,owner+16,3)
        d=check(d,2,6,expected);check(d,0,2)
    assert count==42
    print('PASS: ARM64 weak mark 42 exact-state closure, rollback, RETRY and SELECT-adoption steps')


def verify_weak_relocation(m):
    base=MEMORY+0x2000;size=2048;hs=128;audit=256;queue=512;mark=640
    gate=736;owner=800;table=864;entry=928;ctx=1024;scan=1408;relocate=1792;movers=1888;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    for gen in range(2):
        for fault in range(20):
            d=bytearray(size);phase=0x90024 if gen==0 else 0x90042
            put(d,48,phase);put(d,hs,7);put(d,audit+40,base+queue);put(d,queue+64,61,gen)
            put(d,mark,base+hs,base+audit,gen,3,7,phase,61,4,0)
            put(d,gate,base+2000,1,base,base+hs);put(d,owner,base+gate,base+mark,3,0,base+scan+96)
            put(d,table,base+gate,base+entry,1,1,0,0)
            put(d,ctx,m.context_layout['abi_version'],17,phase)
            put(d,scan,base+table,base+owner,base+ctx,base+entry,1,0,phase,61,gen,7,1,2)
            put(d,relocate,base+hs,base+movers,gen,0,0,0,0);put(d,movers,0,0,0,8,1)
            args=[base+scan,base,base+relocate];status=2
            if fault==0:status=0
            elif fault==1:args[0]=0
            elif fault==2:args[1]=0
            elif fault==3:args[2]=0
            elif fault==4:args[1]+=8
            elif fault==5:put(d,relocate,base+hs+8)
            elif fault==6:put(d,relocate+16,1-gen)
            elif fault==7:put(d,scan+88,1);status=1
            elif fault==8:put(d,scan+80,0)
            elif fault==9:put(d,owner+24,base+scan+96);status=1
            elif fault==10:put(d,scan+96+144,1)
            elif fault==11:put(d,relocate+24,1)
            elif fault==12:put(d,movers+32,0);status=1
            elif fault==13:put(d,movers+24,MASK);status=3
            elif fault==14:put(d,owner+32,0)
            elif fault==15:put(d,ctx+16,0)
            elif fault==16:put(d,scan+96+152,1)
            elif fault==17:put(d,mark+48,62)
            elif fault==18:put(d,table+40,1)
            elif fault==19:put(d,ctx+m.context_layout['access_epoch'],27)
            expected=bytearray(d)
            if status==0:
                adopted=(((phase+0x10000)&~(15<<(4*gen)))|(5<<(4*gen)))^(1<<(10+gen))
                put(expected,48,adopted);put(expected,hs,8);put(expected,hs+40,1)
                put(expected,relocate+24,1,8,adopted,8)
                put(expected,owner+16,0,0,0);put(expected,gate+8,0);put(expected,scan+88,5)
            m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('weak_relocation',*args)
            assert actual==status,(gen,fault,actual,status)
            assert bytes(m.uc.mem_read(base,size))==bytes(expected),(gen,fault)
            count+=1
    assert count==40
    print('PASS: ARM64 weak relocation 40 exact-state covered adoption and closure-preserving rejection steps')


def verify_weak_scan(m):
    base=MEMORY+0x2000;size=12288;pool=128;page=256;slots=384;bitmap=768
    registry=1856;hs=1952;audit=2048;queue=2176;ctx=2304;mark=2800
    gate=2880;owner=2920;table=2960;entries=3024;scan=3264;work=scan+96;start=3632
    forwarding=3680;cells=3712;record=3840;dead_record=4032;header=8192;count=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def word(d,o):return struct.unpack_from('<Q',d,o)[0]
    def fixture(gen,moved=False):
        d=bytearray(size);phase=0xb0024 if gen==0 else 0xb0042;heap=base+header-131072
        put(d,0,heap,base+page,1);put(d,pool,base,base+slots,8,3,2)
        put(d,slots,1,base+page,27,0,1,base+page,27,0)
        for i in range(2,8):put(d,slots+32*i+24,i+2 if i<7 else 0)
        put(d,page,heap,131072,262144,27,1024,0,0,5,base+bitmap,128,131264,0,2,base+forwarding)
        put(d,bitmap,(1<<2)|(1<<6)|(1<<10));put(d,registry+48,phase);put(d,hs,7)
        put(d,audit+40,base+queue);put(d,queue+64,61,gen);put(d,ctx,m.context_layout['abi_version'],17,phase)
        put(d,mark,base+hs,base+audit,gen,3,7,phase,61,4,0)
        put(d,gate,base+pool,1,base+registry,base+hs);put(d,owner,base+gate,base+mark,3,0,0)
        put(d,table,base+gate,base+entries,4,4,3,13);put(d,start,base+table,base+owner,base+ctx)
        put(d,forwarding,base+cells,3,3);put(d,record,42,61,61);put(d,dead_record,43,0,0)
        for i in range(3):
            offset=131104+64*i;ref=(offset<<16)|0x1510
            put(d,cells+32*i,offset,27,0,0)
            put(d,header+64*i,24,1,base+(dead_record if i==1 else record),2)
            if i<2:put(d,entries+56*i,base+gate,11+i,ref,base+page,27,(1<<32)|(i+1),0)
        put(d,entries+112,base+gate,13)
        if moved:
            put(d,cells+16,((131232<<16)|0x1510)|2);d[header:header+64]=bytes(64)
        return d
    def acquire(d):
        index=word(d,pool+24);slot=slots+32*(index-1);serial=word(d,slot)+1
        put(d,pool+24,word(d,slot+24),word(d,pool+32)+1);put(d,page+96,word(d,page+96)+1)
        put(d,slot,serial,base+page,27,0);return (serial<<32)|index
    def release(d,token):
        index=token&0xffffffff;slot=slots+32*(index-1)
        put(d,slot+8,0,0,word(d,pool+24));put(d,pool+24,index,word(d,pool+32)-1)
        put(d,page+96,word(d,page+96)-1)
    def check(d,name,args,status,expected=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call(name,*args)
        assert actual==status,(count,name,args,actual,status)
        expected=d if expected is None else expected;actual_bytes=bytes(m.uc.mem_read(base,size))
        assert actual_bytes==bytes(expected),(count,name,[i for i,(x,y) in enumerate(zip(actual_bytes,expected)) if x!=y][:16])
        count+=1;return bytearray(expected)
    def step(d,op,status,e=None):return check(d,'weak_scan',[base+scan,base+start if op==0 else 0,op],status,e)
    def begin(d,gen):
        e=bytearray(d);put(e,scan,base+table,base+owner,base+ctx,base+entries,4,13,word(d,mark+40),61,gen,7,0,1)
        put(e,owner+32,base+work);return step(d,0,0,e)
    def capture(d,gen,index):
        entry=entries+56*index;e=bytearray(d);token=acquire(e)
        source=struct.unpack_from('<6Q',d,entry)
        put(e,work,base+table,base+owner,base+pool,base+ctx,base+table,index+1,11+index,*source,
            word(d,mark+40),61,gen,7,1,0,word(d,entry+16),base+page,27,token)
        put(e,owner+24,base+work);return step(d,1,0,e)
    def cancel(d):
        e=bytearray(d);release(e,word(d,work+176));put(e,work+136,6,8)
        e[work+152:work+184]=bytes(32);put(e,owner+24,0)
        return check(d,'weak_finish',[base+work,1,0],8,e)
    def process(d,index,moved):
        if moved:
            e=bytearray(d);old=word(d,work+176);token=acquire(e);release(e,old)
            put(e,work+152,(131232<<16)|0x1510,base+page,27,token)
            d=check(d,'weak_access',[base+work,0,0],5,e)
        ref=word(d,work+152);cell=cells+32*(2 if moved else index);e=bytearray(d)
        put(e,work+136,2);put(e,work+208,ref,0,base+ctx,ref>>16,27,base+cell,0)
        put(e,ctx+m.context_layout['access_epoch'],27);put(e,cell+24,1)
        d=check(d,'weak_access',[base+work,0,0],0,e)
        status=7 if index==0 else 6;e=bytearray(d);put(e,work+136,3,status)
        d=check(d,'weak_access',[base+work,1,0],status,e)
        e=bytearray(d);put(e,work+136,4);e[work+208:work+264]=bytes(56)
        put(e,ctx+m.context_layout['access_epoch'],0);put(e,cell+24,0)
        d=check(d,'weak_access',[base+work,2,0],0,e)
        entry=entries+56*index;e=bytearray(d);release(e,word(d,entry+40))
        if index==0:e[entry+16:entry+48]=d[work+152:work+184]
        else:put(e,entry+16,0,0,0,0);release(e,word(d,work+176))
        e[work+152:work+184]=bytes(32);put(e,work+136,6,status);put(e,owner+24,0)
        return check(d,'weak_finish',[base+work,0,0],status,e)
    for gen in range(2):
        for fault in range(12):
            d=fixture(gen);args=[base+scan,base+start,0];status=2
            if fault==0:args[0]=0
            elif fault==1:args[1]=0
            elif fault==2:args[2]=4
            elif fault==3:put(d,work+136,1)
            elif fault==4:put(d,table+16,0)
            elif fault==5:put(d,gate,0)
            elif fault==6:put(d,ctx,0)
            elif fault==7:put(d,ctx+m.context_layout['access_epoch'],27)
            elif fault==8:put(d,owner+16,2)
            elif fault==9:put(d,owner+24,base+work);status=1
            elif fault==10:put(d,owner+32,base+work);status=1
            elif fault==11:put(d,queue+64,62)
            check(d,'weak_scan',args,status)
        original=begin(fixture(gen),gen)
        for fault in range(12):
            d=bytearray(original);op=1;status=2
            if fault==0:put(d,scan+80,5)
            elif fault==1:put(d,scan+48,0)
            elif fault==2:put(d,scan+56,62)
            elif fault==3:put(d,scan+64,gen+1)
            elif fault==4:put(d,scan+72,8)
            elif fault==5:put(d,scan+24,base+entries+56)
            elif fault==6:put(d,scan+40,14)
            elif fault==7:put(d,owner+32,base+work+8)
            elif fault==8:put(d,mark+24,2)
            elif fault==9:put(d,ctx+16,0)
            elif fault==10:op=2
            elif fault==11:put(d,work+136,1);status=1
            step(d,op,status)
        for mode in range(3):
            d=begin(fixture(gen,moved=mode==1),gen);step(d,2,2)
            d=capture(d,gen,0)
            if mode==2:
                d=cancel(d);e=bytearray(d);e[work:work+264]=bytes(264)
                d=step(d,1,5,e);d=capture(d,gen,0)
            for index in range(2):
                if index:d=capture(d,gen,index)
                d=process(d,index,mode==1 and index==0)
                e=bytearray(d);e[work:work+264]=bytes(264);put(e,scan+80,index+1)
                d=step(d,1,5,e)
            for cursor in (3,4):
                e=bytearray(d);put(e,scan+80,cursor);d=step(d,1,5,e)
            e=bytearray(d);put(e,scan+88,2);d=step(d,1,11,e)
            e=bytearray(d);put(e,scan+88,3);put(e,owner+16,0,0,0);put(e,gate+8,0)
            d=step(d,2,11,e);step(d,2,2)
            assert word(d,pool+32)==1 and word(d,entries+16)==(((131232 if mode==1 else 131104)<<16)|0x1510)
        d=begin(fixture(gen),gen);e=bytearray(d);put(e,scan+88,4);put(e,owner+32,0);step(d,3,12,e)
        d=capture(begin(fixture(gen),gen),gen,0);token=word(d,work+176)
        put(d,work+176,token+(1<<32));e=bytearray(d);put(e,work+136,5,8)
        d=step(d,3,10,e);put(d,work+176,token)
        e=bytearray(d);release(e,token);e[work+152:work+184]=bytes(32);put(e,work+136,6,8)
        put(e,scan+88,4);put(e,owner+24,0,0);step(d,3,12,e)
    assert count==184,count
    print('PASS: ARM64 weak scan 184 exact-state bounded, capture/process, cancellation, abort and covered-reopen steps')


def verify_weak_finish(m):
    base=MEMORY+0x2000;size=4096;pool=128;slots=384;page=256
    registry=1600;hs=1728;audit=1856;queue=1920;ctx=2304;mark=2800
    gate=2880;owner=2920;table=2960;entry=3024;process=3160;count=0
    ref=(131104<<16)|0x1510;token=(1<<32)|1;private=(1<<32)|2
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def word(d,o):return struct.unpack_from('<Q',d,o)[0]
    def fixture(gen,live=False):
        d=bytearray(size);phase=0xb0024 if gen==0 else 0xb0042
        # Heap payload is deliberately unmapped: commit/cleanup are metadata-only.
        put(d,0,0x500000000,base+page,1);put(d,pool,base,base+slots,3,3,2)
        put(d,slots,1,base+page,27,0,1,base+page,27,0)
        put(d,page,0x500000000,131072,262144,27,1024,0,0,5,0,0,131136,0,2,0)
        put(d,registry+48,phase);put(d,hs,7);put(d,audit+40,base+queue);put(d,queue+64,61,gen)
        put(d,ctx,m.context_layout['abi_version'],17,phase)
        put(d,mark,base+hs,base+audit,gen,3,7,phase,61,4,0)
        put(d,gate,base+pool,1,base+registry,base+hs);put(d,owner,base+gate,base+mark,3,base+process,0)
        put(d,table,base+gate,base+entry,1,0,1,11);put(d,entry,base+gate,11,ref,base+page,27,token,0)
        put(d,process,base+table,base+owner,base+pool,base+ctx,
            base+table,1,11,base+gate,11,ref,base+page,27,token,
            phase,61,gen,7,4,7 if live else 6,ref,base+page,27,private)
        return d
    def release(d,index):
        put(d,slots+32*(index-1)+8,0,0,word(d,pool+24))
        put(d,pool+24,index,word(d,pool+32)-1);put(d,page+96,word(d,page+96)-1)
    def complete(d,status):
        put(d,process+136,6,status);d[process+152:process+184]=bytes(32);put(d,owner+24,0)
    def check(d,op,status,expected=None,args=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('weak_finish',*(args or [base+process,op,0]))
        assert actual==status,(count,op,actual,status)
        expected=d if expected is None else expected
        actual_bytes=bytes(m.uc.mem_read(base,size))
        assert actual_bytes==bytes(expected),(count,[i for i,(x,y) in enumerate(zip(actual_bytes,expected)) if x!=y][:16])
        count+=1;return bytearray(expected)
    for gen in range(2):
        for live in (False,True):
            d=fixture(gen,live);e=bytearray(d);release(e,1)
            if live:e[entry+16:entry+48]=d[process+152:process+184]
            else:put(e,entry+16,0,0,0,0);release(e,2)
            complete(e,7 if live else 6);d=check(d,0,7 if live else 6,e)
            check(d,0,2);check(d,1,2)
        for fault in range(26):
            d=fixture(gen);args=[base+process,0,0];status=2;cleanup=False
            if fault==0:args[0]=0
            elif fault==1:args[2]=1
            elif fault==2:args[1]=2
            elif fault==3:put(d,process+136,2)
            elif fault==4:put(d,ctx+m.context_layout['access_epoch'],27)
            elif fault==5:struct.pack_into('<I',d,ctx+4,1)
            elif fault==6:put(d,owner+24,0)
            elif fault==7:put(d,process+112,62)
            elif fault==8:put(d,process+16,0)
            elif fault==9:put(d,process+144,0)
            elif fault==10:put(d,process+176,private+(1<<32))
            elif fault==11:put(d,entry+32,28);status=8
            elif fault==12:put(d,table+16,0);status=8
            elif fault==13:put(d,process+48,10);status=8
            elif fault==14:put(d,entry+8,12);put(d,table+40,12);status=8
            elif fault==15:put(d,process+72,ref^16);status=8
            elif fault==16:args[1]=1;put(d,process+176,private+(1<<32));status=10
            elif fault==17:args[1]=1;put(d,owner+24,0)
            elif fault==18:args[1]=1;put(d,process+184,base+page,27,private+(1<<32));status=10
            elif fault==19:put(d,process+136,5);put(d,process+176,private+(1<<32));status=10
            elif fault==20:args[1]=1;put(d,owner+16,2);status=8
            elif fault==21:args[1]=1;put(d,process+136,1);status=8
            elif fault==22:put(d,process+136,5);put(d,owner+16,2);status=6
            elif fault==23:put(d,process+160,base+page+8)
            elif fault in (24,25):
                put(d,process+136,5);put(d,process+184,base+page,27,token)
                put(d,entry+16,0,0,0,0);status=6 if fault==24 else 10
                if fault==25:put(d,process+176,private+(1<<32))
            e=bytearray(d)
            if fault in (16,17,18):put(e,process+136,5,8)
            if fault in (24,25):release(e,1);put(e,process+184,0,0,0)
            if status in (6,8):release(e,2);complete(e,status)
            d=check(d,args[1],status,e,args)
            if fault==25:
                assert word(d,process+136)==5 and word(d,owner+24)==base+process and word(d,pool+32)==1
                put(d,process+176,private);e=bytearray(d);release(e,2);complete(e,6)
                check(d,0,6,e)
    assert count==66,count
    print('PASS: ARM64 weak finish 66 exact-state dead/live commits, stale-slot cancellation and partial-cleanup retries')


def verify_weak_access(m):
    base=MEMORY+0x2000;size=8192;pool=128;slots=384;page=256;bitmap=512
    registry=1600;hs=1728;audit=1856;queue=1920;ctx=2304;mark=2800
    gate=2880;owner=2920;table=2960;entry=3024;process=3160
    forwarding=3520;cell=3560;record=3648;header=4096;count=0
    ref=(131104<<16)|0x1510;token=(1<<32)|1;private=(1<<32)|2
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture(gen,stamp=0,initializing=False,old=False):
        d=bytearray(size);phase=0xb0024 if gen==0 else 0xb0042
        heap=base+header-131072
        put(d,0,heap,base+page,1);put(d,pool,base,base+slots,2,0,2)
        put(d,slots,1,base+page,27,0,1,base+page,27,0)
        put(d,page,heap,131072,262144,27,1024,0,int(old),5,base+bitmap,128,131136,0,2,base+forwarding)
        put(d,bitmap,4);put(d,registry+48,phase);put(d,hs,7)
        put(d,audit+40,base+queue);put(d,queue+64,61,gen)
        put(d,ctx,m.context_layout['abi_version'],17,phase)
        put(d,mark,base+hs,base+audit,gen,3,7,phase,61,4,0)
        put(d,gate,base+pool,1,base+registry,base+hs)
        put(d,owner,base+gate,base+mark,3,base+process,0)
        put(d,table,base+gate,base+entry,1,0,1,11)
        put(d,entry,base+gate,11,ref,base+page,27,token,0)
        put(d,process,base+table,base+owner,base+pool,base+ctx,
            base+table,1,11,base+gate,11,ref,base+page,27,token,
            phase,61,gen,7,1,0,ref,base+page,27,private)
        put(d,forwarding,base+cell,1,1);put(d,cell,131104,27,0,0)
        put(d,record,42,stamp,stamp)
        put(d,header,24,1,base+record,1 if initializing else 2)
        return d
    def check(d,op,status,expected=None,args=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('weak_access',*(args or [base+process,op,0]))
        assert actual==status,(count,op,actual,status)
        result=d if expected is None else expected
        actual_bytes=bytes(m.uc.mem_read(base,size))
        assert actual_bytes==bytes(result),(count,op,[i for i,(x,y) in enumerate(zip(actual_bytes,result)) if x!=y][:16])
        count+=1;return bytearray(result)
    def enter(d,reference=ref,held=cell):
        e=bytearray(d);put(e,process+136,2)
        put(e,process+208,reference,0,base+ctx,reference>>16,27,base+held,0)
        put(e,ctx+m.context_layout['access_epoch'],27);put(e,held+24,1)
        return check(d,0,0,e)
    def leave(d,observed,held=cell):
        e=bytearray(d);put(e,process+136,4 if observed else 1)
        e[process+208:process+264]=bytes(56)
        put(e,ctx+m.context_layout['access_epoch'],0);put(e,held+24,0)
        return check(d,2,0,e)
    for gen in range(2):
        for fault in range(24):
            d=fixture(gen);args=[base+process,0,0];status=2
            if fault==0:args[0]=0
            elif fault==1:args[2]=1
            elif fault==2:args[1]=3
            elif fault==3:put(d,process+136,2)
            elif fault==4:put(d,ctx+m.context_layout['access_epoch'],27)
            elif fault==5:put(d,ctx,0)
            elif fault==6:put(d,ctx+8,0)
            elif fault==7:struct.pack_into('<I',d,ctx+4,1)
            elif fault==8:put(d,owner+24,0)
            elif fault==9:put(d,owner+16,2)
            elif fault==10:put(d,gate+8,0)
            elif fault==11:put(d,process+112,62)
            elif fault==12:put(d,process+120,gen+1)
            elif fault==13:put(d,process+128,8)
            elif fault==14:put(d,process+104,0)
            elif fault==15:put(d,queue+64,62)
            elif fault==16:put(d,ctx+16,0)
            elif fault==17:put(d,process+16,0)
            elif fault==18:put(d,process+184,base+page,27,private+(1<<32));status=10
            elif fault==19:put(d,process+152,0)
            elif fault==20:put(d,cell+16,5);status=1
            elif fault==21:put(d,cell+24,1<<63);status=1
            elif fault==22:put(d,cell+24,(1<<63)-1);status=4
            elif fault==23:put(d,process+176,private+(1<<32))
            check(d,0,status,args=args)
        for mode in range(7):
            d=fixture(gen,stamp=61 if mode==1 else 62 if mode==4 else 0,
                      initializing=mode==2,old=mode==3)
            d=enter(d);check(d,0,2)
            status=1 if mode==2 else 2 if mode==4 else 7 if mode==1 or (mode==3 and gen==0) else 6
            if mode==5:put(d,mark+48,62);status=2
            expected=bytearray(d)
            observed=status in (6,7)
            if observed:put(expected,process+136,3,status)
            d=check(d,1,status,expected)
            if observed:check(d,1,2)
            if mode==6:put(d,owner+16,2)
            # Invalid EXIT retains the live guard for correction and retry.
            bad=bytearray(d);put(bad,process+248,0);check(bad,2,2)
            d=leave(d,observed);check(d,2,2)
        # Forwarding backpressure and one-edge progress keep slot identity and
        # independent ownership. Both physical copies share the same record.
        for room in (False,True):
            d=fixture(gen);dest=(131168<<16)|0x1510
            put(d,page+80,131200);put(d,bitmap,68);put(d,forwarding+8,2,2)
            put(d,cell+16,dest|2);put(d,cell+32,131168,27,0,0)
            put(d,header+64,24,1,base+record,2)
            if not room:check(d,0,3);continue
            put(d,pool+16,3,3,2);e=bytearray(d)
            put(e,pool+24,2,2);put(e,slots+32,1,0,0,0)
            put(e,slots+64,1,base+page,27,0)
            put(e,process+152,dest,base+page,27,(1<<32)|3)
            d=check(d,0,5,e);d=enter(d,dest,cell+32)
            e=bytearray(d);put(e,process+136,3,6);d=check(d,1,6,e)
            leave(d,True,cell+32)
    assert count==150,count
    print('PASS: ARM64 weak access 150 exact-state guard, liveness, stale-owner, retry and forwarding steps')


def verify_weak_capture(m):
    base=MEMORY+0x2000;size=4096;pool=128;slots=176;page=256;bitmap=512
    registry=1600;hs=1728;audit=1856;queue=1920;ctx=2304;mark=2800
    gate=2880;owner=2920;table=2960;entry=3024;start=3096;process=3160;count=0
    ref=(131104<<16)|0x1510;token=(1<<32)|1
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    # Isolate capture of a completed owner. Actual mark termination is exercised
    # by the native lifecycle fixture; these cases verify exact ARM mutations.
    for gen in range(2):
        for mode in range(32):
            m.reset();d=bytearray(size);phase=0xb0024 if gen==0 else 0xb0042
            put(d,0,0x500000000,base+page,1)
            put(d,pool,base,base+slots,2,2,1)
            put(d,slots,1,base+page,27,0)
            put(d,page,0x500000000,131072,262144,27,1024,0,0,5,base+bitmap,128,131120,0,1,0)
            put(d,bitmap,4);put(d,registry+48,phase);put(d,hs,7)
            put(d,audit+40,base+queue);put(d,queue+64,61,gen)
            put(d,ctx,m.context_layout['abi_version'],17,phase)
            put(d,mark,base+hs,base+audit,gen,3,7,phase,61,4,0)
            put(d,gate,base+pool,1,base+registry,base+hs)
            put(d,owner,base+gate,base+mark,3,0,0)
            put(d,table,base+gate,base+entry,1,0,1,11)
            put(d,entry,base+gate,11,ref,base+page,27,token,0)
            put(d,start,base+table,base+owner,base+ctx,base+table,1,11)
            args=[base+process,base+start,0];status=2
            if mode==0:status=0
            elif mode==1:put(d,owner+32,base+process);status=0
            elif mode==2:args[2]=1
            elif mode==3:args[0]=0
            elif mode==4:args[1]+=1
            elif mode==5:put(d,process+256,1)
            elif mode==6:put(d,ctx,0)
            elif mode==7:put(d,ctx+8,0)
            elif mode==8:struct.pack_into('<I',d,ctx+4,1)
            elif mode==9:put(d,ctx+m.context_layout['access_epoch'],1)
            elif mode==10:put(d,owner+16,2)
            elif mode==11:put(d,owner+24,base+process)
            elif mode==12:put(d,owner+32,base+process+8)
            elif mode==13:put(d,gate+8,0)
            elif mode==14:put(d,mark+24,2)
            elif mode==15:put(d,mark+56,2)
            elif mode==16:put(d,mark+16,2)
            elif mode==17:put(d,mark+48,0)
            elif mode==18:put(d,ctx+16,0)
            elif mode==19:put(d,registry+48,0)
            elif mode==20:put(d,hs+40,1)
            elif mode==21:put(d,hs,8)
            elif mode==22:put(d,queue+64,62)
            elif mode==23:put(d,start+40,10);status=8
            elif mode==24:put(d,start+24,base+table+8);status=8
            elif mode==25:put(d,entry+16,0,0,0,0);status=9
            elif mode==26:put(d,entry+16,0)
            elif mode==27:put(d,entry+40,token+(1<<32))
            elif mode==28:put(d,entry+32,28)
            elif mode==29:put(d,pool+24,0,2);status=3
            elif mode==30:put(d,slots+32,0xffffffff);status=4
            elif mode==31:put(d,page+96,MASK);status=4
            expected=bytearray(d)
            if status==0:
                new_token=(1<<32)|2
                put(expected,pool+24,0,2);put(expected,page+96,2)
                put(expected,slots+32,1,base+page,27,0)
                put(expected,process,base+table,base+owner,base+pool,base+ctx,
                    base+table,1,11,base+gate,11,ref,base+page,27,token,
                    phase,61,gen,7,1,0,ref,base+page,27,new_token)
                put(expected,owner+24,base+process)
            m.uc.mem_write(base,bytes(d));actual=m.call('weak_capture',*args)
            assert actual==status,(gen,mode,actual,status)
            assert bytes(m.uc.mem_read(base,size))==bytes(expected),(gen,mode)
            count+=1
    assert count==64
    print('PASS: ARM64 weak capture 64 exact-state ownership and failure-preservation cases')


def verify_weak_create(m):
    base=MEMORY+0x2000;size=4096;pool=128;slots=176;page=256;bitmap=512
    journal=1536;changes=1600;ctx=2048;segment=2400;cell=2464;registry=2520
    registration=2624;handshake=2848;participant=2920;admission=3072;source=3120
    gate=3256;entries=3440;table=3568;out=3624;q=3664;access=3744;count=0
    ref=(131104<<16)|0x1510
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    for mode in range(32):
        m.reset();d=bytearray(size)
        put(d,0,0x500000000,base+page,1)
        put(d,pool,base,base+slots,2,1,0);put(d,slots+24,2)
        put(d,page,0x500000000,131072,262144,27,1024,0,0,5,base+bitmap,128,131120,0,0,0)
        put(d,bitmap,4);put(d,journal,base+changes,3,0,0,0)
        put(d,ctx,m.context_layout['abi_version'],17,0x10022);put(d,ctx+128,base+registration)
        put(d,segment,17,1,base+cell,1,1,1,base+journal,base+registration);put(d,cell,ref,1,1,0)
        put(d,registry,1,0,1,0,0,base+registration,0x10022,base+registration,0)
        put(d,registration,base+segment,1,1);put(d,registration+120,base+ctx)
        put(d,handshake,1,1,0,0,0,0,17,base+participant)
        put(d,participant,base+ctx,17,1,1,base+handshake,0,0)
        put(d,admission,base+registry,base+handshake,base+participant,0,0)
        put(d,source,base+segment,base+cell,1,1,ref)
        put(d,gate,base+pool,0,base+registry,base+handshake)
        put(d,entries+48,2);put(d,table,base+gate,base+entries,2,1,0,0)
        put(d,q,base+admission,base+ctx,base+source,base+out)
        args=[base+table,base+q,0];status=3
        if mode==0:status=0
        elif mode==1:put(d,cell,0);put(d,source+32,0);status=0
        elif mode==2:put(d,gate+8,1);status=2
        elif mode==3:put(d,source+16,2)
        elif mode==4:put(d,registration+16,0)
        elif mode==5:put(d,admission,base+registry+8)
        elif mode==6:put(d,admission+8,base+handshake+8)
        elif mode==7:put(d,handshake,2,1,0,1,0,1);status=2
        elif mode==8:put(d,ctx+16,0x20022);status=2
        elif mode in (9,11):
            put(d,pool+24,0,2);put(d,slots,1,base+page,27,0,1,base+page,27,0);put(d,page+96,2);status=6
            if mode==11:
                put(d,table+24,0,2,2)
                for i in range(2):put(d,entries+i*56,base+gate,i+1,ref,base+page,27,(1<<32)|(i+1),0)
        elif mode==10:put(d,table+40,MASK);status=5
        elif mode==12:put(d,entries+48,1)
        elif mode==13:put(d,q+16,0)
        elif mode==14:put(d,out+16,1)
        elif mode==15:put(d,registration+120,base+ctx+8)
        elif mode==16:put(d,ctx+144,27)
        elif mode==17:put(d,ctx,m.context_layout['abi_version']+(2<<32))
        elif mode==18:put(d,cell,1)
        elif mode==19:put(d,bitmap,0)
        elif mode==20:put(d,entries+8,1)
        elif mode==21:put(d,segment,18)
        elif mode==22:put(d,participant+24,0)
        elif mode==23:put(d,slots,0xffffffff);status=5
        elif mode==24:args[2]=1
        elif mode==25:args[1]=0
        elif mode==26:args[1]+=1
        elif mode==27:put(d,source+32,0);status=0
        elif mode==28:put(d,q+24,0)
        elif mode==29:args[0]=0
        elif mode==30:put(d,table+8,MASK-7)
        elif mode==31:put(d,out+8,1)
        expected=bytearray(d)
        if status==0:
            value=0 if mode==1 else ref
            put(expected,table+24,2,1,1)
            put(expected,entries,base+gate,1,value,base+page if value else 0,27 if value else 0,(1<<32)|1 if value else 0,0)
            put(expected,out,base+table,1,1)
            if value:
                put(expected,pool+24,2,1);put(expected,slots,1,base+page,27,0);put(expected,page+96,1)
        m.uc.mem_write(base,bytes(d));actual=m.call('weak_create',*args)
        assert actual==status,(mode,actual,status)
        assert bytes(m.uc.mem_read(base,size))==bytes(expected),mode;count+=1
        if mode==0:
            put(expected,access,base+table,1,1,0,0,0);m.uc.mem_write(base,bytes(expected))
            put(expected,table+24,1,0);put(expected,entries,0,0,0,0,0,0,2)
            put(expected,pool+24,1,0);put(expected,slots,1,0,0,2);put(expected,page+96,0)
            assert m.call('weak_table_access',base+table,base+access,1)==0
            assert bytes(m.uc.mem_read(base,size))==bytes(expected);count+=1
            put(expected,out,0,0,0);m.uc.mem_write(base,bytes(expected))
            put(expected,table+24,2,1,2);put(expected,entries,base+gate,2,ref,base+page,27,(2<<32)|1,0)
            put(expected,out,base+table,1,2);put(expected,pool+24,2,1);put(expected,slots,2,base+page,27,0);put(expected,page+96,1)
            assert m.call('weak_create',*args)==0
            assert bytes(m.uc.mem_read(base,size))==bytes(expected);count+=1
            assert m.call('weak_table_access',base+table,base+access,1)==3
            assert bytes(m.uc.mem_read(base,size))==bytes(expected);count+=1
    assert count==35
    print('PASS: ARM64 weak creation 35 exact-state source-root, metadata, publication and serial-reuse steps')


def verify_weak_load(m,table_api=False):
    base=MEMORY+0x2000;size=4096;pool=128;slots=176;page=256;bitmap=512
    journal=1536;entries=1600;ctx=2048;segment=2400;cell=2464;registry=2520
    registration=2624;handshake=2848;participant=2920;admission=3072
    output=3120;weak=3200;gate=3256;request=3320;table=3440;count=0
    ref=(131104<<16)|0x1510
    def put(data,o,*v):struct.pack_into('<'+'Q'*len(v),data,o,*v)
    for mode in range(29):
        m.reset();data=bytearray(size)
        put(data,0,0x500000000,base+page,1)
        put(data,pool,base,base+slots,2,2,1);put(data,slots,1,base+page,27,0)
        put(data,page,0x500000000,131072,262144,27,1024,0,0,5,base+bitmap,128,131120,0,1,0)
        put(data,bitmap,4);put(data,journal,base+entries,3,0,0,0)
        put(data,ctx,m.context_layout['abi_version'],17,0x10022);put(data,ctx+128,base+registration)
        put(data,segment,17,1,base+cell,1,1,1,base+journal,base+registration)
        put(data,cell,0,1,1,0)
        put(data,registry,1,0,1,0,0,base+registration,0x10022,base+registration,0)
        put(data,registration,base+segment,1,1);put(data,registration+120,base+ctx)
        put(data,handshake,1,1,0,0,0,0,17,base+participant)
        put(data,participant,base+ctx,17,1,1,base+handshake,0,0)
        put(data,admission,base+registry,base+handshake,base+participant,0,0)
        put(data,output,base+segment,base+cell,1,1,0)
        put(data,weak,base+gate,11,ref,base+page,27,(1<<32)|1)
        put(data,gate,base+pool,0,base+registry,base+handshake);put(data,request,base+admission,base+ctx,base+weak,11,base+output)
        if table_api:
            put(data,table,base+gate,base+weak,1,0,1,11)
            put(data,request,base+table,1,11,base+admission,base+ctx,base+output)
        result=3;op=0
        if mode==0:result=0
        elif mode==1:
            put(data,weak+16,0,0,0,0);put(data,pool+24,1,0);put(data,slots,1,0,0,2);put(data,page+96,0);result=1
        elif mode==2:put(data,gate+8,1);result=2
        elif mode==3:put(data,request+(16 if table_api else 24),12)
        elif mode==4:put(data,weak+40,(2<<32)|1)
        elif mode==5:put(data,weak+24,base+page+8)
        elif mode==6:put(data,cell,ref)
        elif mode==7:put(data,journal+16,3,3);result=4
        elif mode==8:put(data,ctx+152,MASK);result=5
        elif mode==9:put(data,segment+56,0)
        elif mode==10:put(data,ctx+16,0x20022);result=2
        elif mode==11:put(data,ctx,m.context_layout['abi_version']+(2<<32))
        elif mode==12:put(data,handshake,2,1,0,1,0,1);result=2
        elif mode==13:put(data,participant+24,0)
        elif mode==14:put(data,ctx+144,27)
        elif mode==15:put(data,ctx+8,0)
        elif mode==16:put(data,gate+8,2)
        elif mode==17:put(data,output+16,2)
        elif mode==18:op=2 if table_api else 1
        elif mode==19:put(data,weak+16,0)
        elif mode==20:put(data,registration+120,0)
        elif mode==21:put(data,participant+16,2)
        elif mode==22:put(data,weak+16,ref|1)
        elif mode==23:put(data,weak+16,ref+(131072<<16))
        elif mode==24:put(data,bitmap,0)
        elif mode==25:put(data,page+64,0)
        elif mode==26:put(data,weak,base+gate+8)
        elif mode==27:put(data,gate+16,base+registry+8)
        elif mode==28:put(data,gate+24,base+handshake+8)
        if table_api and mode==0:
            for offset,value in [(request,base+table+8),(request+8,0),(request+16,12),(table+24,1),(request+24,0)]:
                failed=bytearray(data);put(failed,offset,value);m.reset();m.uc.mem_write(base,bytes(failed))
                assert m.call('weak_table_access',base+table,base+request,0)==3
                assert bytes(m.uc.mem_read(base,size))==bytes(failed);count+=1
            disposed=bytearray(data);put(disposed,request+24,MASK,MASK,MASK)
            expected_disposed=bytearray(disposed)
            put(expected_disposed,weak,0,0,0,0,0,0,0);put(expected_disposed,table+24,1,0)
            put(expected_disposed,pool+24,1,0);put(expected_disposed,slots,1,0,0,2);put(expected_disposed,page+96,0)
            m.reset();m.uc.mem_write(base,bytes(disposed))
            assert m.call('weak_table_access',base+table,base+request,1)==0
            assert bytes(m.uc.mem_read(base,size))==bytes(expected_disposed);count+=1
            assert m.call('weak_table_access',base+table,base+request,1)==3
            assert bytes(m.uc.mem_read(base,size))==bytes(expected_disposed);count+=1
        expected=bytearray(data)
        if result==0:
            put(expected,cell,ref);put(expected,output+32,ref);put(expected,ctx+152,1)
            put(expected,journal+16,1,1);put(expected,entries,0,ref,1,0x10022)
        m.uc.mem_write(base,bytes(data))
        def check(status):
            nonlocal count
            assert m.call('weak_table_access' if table_api else 'weak_load',base+(table if table_api else gate),base+request,op)==status,mode
            actual=bytes(m.uc.mem_read(base,size))
            assert actual==bytes(expected),(mode,[(o,unpack('Q',actual,o)[0],unpack('Q',expected,o)[0]) for o in range(0,size,8) if actual[o:o+8]!=expected[o:o+8]])
            count+=1
        check(result)
        if result==0:
            check(3);put(expected,gate+8,1);m.uc.mem_write(base+gate+8,struct.pack('<Q',1));check(2)
            # Explicit disposal returns only the weak lease: the published
            # strong root and its unconsumed journal must remain intact.
            put(expected,weak,0,0,0,0,0,0);put(expected,pool+24,1,0)
            put(expected,slots,1,0,0,2);put(expected,page+96,0)
            if table_api:put(expected,table+24,1,0)
            release_args=[base+table,base+request,1] if table_api else [base+gate,base+weak,11]
            release_name='weak_table_access' if table_api else 'weak_release'
            assert m.call(release_name,*release_args)==0
            assert bytes(m.uc.mem_read(base,size))==bytes(expected);count+=1
            assert m.call(release_name,*release_args)==3
            assert bytes(m.uc.mem_read(base,size))==bytes(expected);count+=1
            put(expected,gate+8,0);m.uc.mem_write(base+gate+8,struct.pack('<Q',0));check(3)
    assert count==(41 if table_api else 34)
    print(f'PASS: ARM64 weak {"table access" if table_api else "load/dispose"} {count} exact-state rooted publication, gate, handle, journal and metadata steps')


def verify_weak_release(m):
    base=MEMORY+0x2000;size=2048;pool=128;slots=176;page=256;gate=1024;weak=1088;count=0
    def put(data,o,*v):struct.pack_into('<'+'Q'*len(v),data,o,*v)
    for mode in range(16):
        m.reset();data=bytearray(size)
        put(data,0,0x500000000,base+page,1)
        put(data,pool,base,base+slots,2,2,1);put(data,slots,1,base+page,27,0)
        put(data,page,0x500000000,131072,262144,27,1024,0,0,5);put(data,page+96,1)
        put(data,gate,base+pool,0,0,0);put(data,weak,base+gate,11,(131104<<16)|0x1510,base+page,27,(1<<32)|1)
        args=[base+gate,base+weak,11];result=3
        if mode==0:result=0
        elif mode==1:put(data,gate+8,1);result=0
        elif mode==2:
            put(data,weak+16,0,0,0,0);put(data,pool+24,1,0);put(data,slots,1,0,0,2);put(data,page+96,0);result=0
        elif mode==3:put(data,gate+8,2)
        elif mode==4:args[2]=12
        elif mode==5:put(data,weak,base+gate+8)
        elif mode==6:put(data,weak+40,(2<<32)|1)
        elif mode==7:put(data,weak+24,base+page+8)
        elif mode==8:put(data,weak+32,28)
        elif mode==9:put(data,weak+16,0)
        elif mode==10:put(data,weak+24,0,0,0)
        elif mode==11:args[2]=0
        elif mode==12:args[0]=0
        elif mode==13:args[1]+=1
        elif mode==14:
            put(data,slots,0xffffffff);put(data,weak+40,(0xffffffff<<32)|1);result=0
        elif mode==15:
            put(data,weak,0,0,0,0,0,0);put(data,pool+24,1,0);put(data,slots,1,0,0,2);put(data,page+96,0)
        expected=bytearray(data)
        if result==0:
            put(expected,weak,0,0,0,0,0,0)
            if mode!=2:
                put(expected,pool+24,1,0);put(expected,slots,0xffffffff if mode==14 else 1,0,0,2);put(expected,page+96,0)
        m.uc.mem_write(base,bytes(data))
        assert m.call('weak_release',*args)==result,mode
        actual=bytes(m.uc.mem_read(base,size))
        assert actual==bytes(expected),(mode,[(o,unpack('Q',actual,o)[0],unpack('Q',expected,o)[0]) for o in range(0,size,8) if actual[o:o+8]!=expected[o:o+8]])
        count+=1
        if result==0:
            assert m.call('weak_release',*args)==3
            assert bytes(m.uc.mem_read(base,size))==bytes(expected);count+=1
    assert count==20
    print('PASS: ARM64 weak release 20 exact-state disposal, replay and metadata rejection steps')


def verify_pending_mark(m):
    base=MEMORY+0x2000;pool=128;slots=176;page=256;table=512;cell=600
    q=1600;ctx=2048;queue=2400;size=4096;count=0
    reference=(131104<<16)|0x1510
    def put(data,o,*v):struct.pack_into('<'+'Q'*len(v),data,o,*v)
    for mode in range(4):
        m.reset();data=bytearray(size)
        put(data,0,0x500000000,base+page,1)
        put(data,pool,base,base+slots,2,1,0);put(data,slots+24,2)
        put(data,page,0x500000000,131072,262144,27,1024,0,0,5,base+1024,128,131120,0,0,base+table)
        put(data,1024,4);put(data,table,base+cell,1,1);put(data,cell,131104,27,5 if mode==1 else 0,0)
        put(data,q,base+ctx,0 if mode==0 else reference)
        put(data,q+120,base+queue,0,1,0x10022)
        put(data,ctx,m.context_layout['abi_version'],17,0x10022)
        if mode==3:put(data,q+64,reference)  # dirty access scratch rejects before acquisition
        expected=bytearray(data)
        result=[12,2,8,1][mode]
        if mode==0:put(expected,q+152,5,2)
        elif mode in (1,2):
            put(expected,pool+24,2,1);put(expected,slots,1,base+page,27,0)
            put(expected,page+96,1);put(expected,q+16,base+page,27,(1<<32)|1)
            if mode==2:
                put(expected,q+64,reference,0,base+ctx,131104,27,base+cell,0)
                put(expected,q+152,1);put(expected,cell+24,1);put(expected,ctx+144,27)
        m.uc.mem_write(base,bytes(data))
        def step(op,status):
            nonlocal count
            assert m.call('pending_mark',base+pool,base+q,op)==status,(mode,op)
            actual=bytes(m.uc.mem_read(base,size))
            assert actual==bytes(expected),(mode,op,[(o,unpack('Q',actual,o)[0],unpack('Q',expected,o)[0]) for o in range(0,size,8) if actual[o:o+8]!=expected[o:o+8]])
            count+=1
        step(0,result);step(0,result)
        if mode==0:
            put(expected,q+152,0,0,0);step(6,0)
        elif mode==1:
            put(expected,pool+24,1,0);put(expected,slots,1,0,0,2)
            put(expected,page+96,0);put(expected,q+16,0,0,0);put(expected,q+152,6)
            step(5,13)
        elif mode==2:step(5,8)  # cancellation cannot release a live guard
    assert count==11
    print('PASS: ARM64 pending mark 11 exact-state acquisition, retained retry, null, cancellation and rejection steps')


def verify_pending_access(m):
    base=MEMORY+0x2000;pool=128;slots=176;page=256;table=512;cell=600
    current=1800;access=1840;ctx=2048;size=4096;count=0
    reference=(131104<<16)|0x1510;destination=(262176<<16)|0x1510
    def put(data,o,*v):struct.pack_into('<'+'Q'*len(v),data,o,*v)
    for mode in range(19):
        m.reset();data=bytearray(size)
        put(data,0,0x500000000,base+page,1)
        put(data,pool,base,base+slots,2,2,1)
        put(data,slots,1,base+page,27,0)
        put(data,page,0x500000000,131072,262144,27,1024,0,0,5,base+1024,128,131120,0,1,base+table)
        put(data,1024,4);put(data,table,base+cell,1,1);put(data,cell,131104,27,0,0)
        put(data,current,reference,base+page,27,(1<<32)|1)
        put(data,access+16,base+ctx);put(data,ctx,m.context_layout['abi_version'],17)
        args=[base+pool,base+current,base+access];result=1
        if mode==0:result=8
        elif mode==1:put(data,cell+16,destination|2);result=3
        elif mode==2:put(data,cell+16,5);result=2
        elif mode==3:put(data,cell+16,3);result=8
        elif mode==4:put(data,current,0,0,0,0);result=0
        elif mode==5:put(data,current+16,28)
        elif mode==6:put(data,ctx,m.context_layout['abi_version']+1)
        elif mode==7:put(data,ctx,m.context_layout['abi_version']+(2<<32))
        elif mode==8:put(data,ctx+8,0)
        elif mode==9:put(data,ctx+144,27)
        elif mode==10:put(data,access,reference)
        elif mode==11:put(data,access+8,destination)
        elif mode==12:put(data,access+16,0)
        elif mode==13:args[1]+=1
        elif mode==14:args[0]=0
        elif mode==15:put(data,current+24,(2<<32)|1)
        elif mode==16:put(data,page+104,0)
        elif mode==17:put(data,cell+24,1<<63);result=2
        elif mode==18:put(data,cell+24,(1<<63)-1);result=6
        expected=bytearray(data)
        if result==8:
            put(expected,access,reference,0,base+ctx,131104,27,base+cell,0)
            put(expected,cell+24,1);put(expected,ctx+144,27)
        m.uc.mem_write(base,bytes(data))
        assert m.call('pending_access',*args)==result,mode
        actual=bytes(m.uc.mem_read(base,size))
        assert actual==bytes(expected),(mode,[(o,unpack('Q',actual,o)[0],unpack('Q',expected,o)[0]) for o in range(0,size,8) if actual[o:o+8]!=expected[o:o+8]])
        count+=1
        if result==8:
            assert m.call('pending_access',*args)==1
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            assert m.call('resolve_access',base+page,base+access,1)==0
            put(expected,access+24,0,0,0);put(expected,ctx+144,0);put(expected,cell+24,0)
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            count+=2
    assert count==23
    print('PASS: ARM64 pending access 23 exact-state guard, forwarding-discard, retry and rejection steps')


def verify_weak_incoming(m):
    base=MEMORY+0x2000;pool=128;slots=176;pages=256;tables=640;cells=768
    table=4304;entry=4352;follow_q=4416;gate=4480;q=4608;size=4800;count=0
    refs=[(((i+1)*131072+32)<<16)|0x1510 for i in range(3)]
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    def fixture():
        d=bytearray(size);put(d,0,0x500000000,base+pages,3)
        put(d,pool,base,base+slots,2,2,1);put(d,slots,1,base+pages,27,0)
        for i in range(3):
            start=(i+1)*131072
            put(d,pages+i*112,0x500000000,start,start+131072,27+i,1024,0,0,5,
                base+1024+i*1024,128,start+48,i,int(i==0),base+tables+i*24)
            put(d,1024+i*1024,4);put(d,tables+i*24,base+cells+i*32,1,1)
            put(d,cells+i*32,start+32,27+i,refs[i+1]|2 if i<2 else 0,0)
        put(d,table,base+gate,base+entry,1,0,1,11)
        put(d,entry,base+gate,11,refs[0],base+pages,27,(1<<32)|1,0)
        put(d,gate,base+pool,1,0,0);put(d,q+48,131072,262144)
        put(d,follow_q,1,11,0,0,0);return d
    def check(d,op,status,expected=None,args=None):
        nonlocal count
        m.reset();m.uc.mem_write(base,bytes(d));actual=m.call('weak_incoming',*(args or [base+table,base+q,op]))
        assert actual==status,(count,actual,status)
        result=expected if expected is not None else d
        assert bytes(m.uc.mem_read(base,size))==bytes(result),count
        count+=1;return bytearray(result)
    for mode in range(32):
        d=fixture();expected=bytearray(d)
        put(expected,q,base+table,base+entry,base+gate,base+pool,1,11)
        put(expected,q+72,1);d=check(d,0,1,expected);status=3;op=1
        if mode==0:status=2
        elif mode==1:put(d,q+48,262144,393216);status=1
        elif mode==2:put(d,entry+40,(2<<32)|1)
        elif mode==3:put(d,entry+24,0,0,0)
        elif mode==4:put(d,entry,base+gate+8)
        elif mode==5:put(d,entry+8,12)
        elif mode==6:put(d,entry+48,1)
        elif mode==7:put(d,entry+16,refs[0]|1)
        elif mode==8:put(d,pages+80,131104)
        elif mode==9:put(d,pages+8,262144)
        elif mode==10:put(d,pages+16,1<<48)
        elif mode in (11,13,14,15):
            put(d,entry+16,0,0,0,0);put(d,pool+24,1,0);put(d,slots,1,0,0,2);put(d,pages+96,0)
            status=1
            if mode!=11:
                put(d,entry,0,0);put(d,table+24,1,0)
                if mode==14:put(d,entry+16,refs[0]);status=3
                elif mode==15:put(d,entry+48,2);status=3
        elif mode==12:put(d,entry+16,0)
        elif mode==16:put(d,table+40,12);status=4
        elif mode==17:put(d,gate+8,0);status=4
        elif mode==18:put(d,table+8,base+entry+8);status=4
        elif mode==19:put(d,table+16,2,2);status=4
        elif mode==20:put(d,q+24,base+pool+8);status=4
        elif mode==21:put(d,q+72,3)
        elif mode==22:put(d,q+64,2)
        elif mode==23:put(d,q,base+table+8)
        elif mode==24:put(d,q+48,0)
        elif mode==25:put(d,q+48,262144)
        elif mode==26:put(d,q+56,262145)
        elif mode==27:op=2
        elif mode==28:put(d,q+64,1);status=0
        elif mode==29:put(d,q+64,1,2);status=0
        elif mode==30:put(d,q+72,2)
        elif mode==31:put(d,q+56,1<<48)
        expected=bytearray(d)
        if status==2:put(expected,q+80,base+table,1,11,refs[0])
        elif status==1:put(expected,q+64,1)
        elif mode==28:put(expected,q+72,2)
        d=check(d,op,status,expected)
        if mode==0:
            d=check(d,1,2)
            expected=bytearray(d)
            put(expected,pool+24,1,1);put(expected,slots,1,0,0,0)
            put(expected,slots+32,1,base+pages+112,28,0)
            put(expected,pages+96,0);put(expected,pages+112+96,1)
            put(expected,entry+16,refs[1],base+pages+112,28,(1<<32)|2)
            assert m.call('weak_follow',base+table,base+follow_q,0)==3
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            d=expected;expected=bytearray(d);put(expected,q+64,1);put(expected,q+80,0,0,0,0)
            d=check(d,1,1,expected);expected=bytearray(d);put(expected,q+72,2)
            d=check(d,1,0,expected);d=check(d,1,0)
            put(d,gate+8,0);check(d,1,4)
    for mode in range(21):
        d=fixture();args=[base+table,base+q,0];status=3
        fresh=[0,8,16,24,32,40,64,72,80,88,96,104]
        if mode<12:put(d,q+fresh[mode],1)
        elif mode==12:put(d,q+48,0)
        elif mode==13:put(d,q+48,262144)
        elif mode==14:put(d,q+56,1<<48)
        elif mode==15:put(d,q+48,131073)
        elif mode==16:put(d,q+56,262145)
        elif mode==17:put(d,gate+8,0);status=4
        elif mode==18:args[1]+=1
        elif mode==19:args[0]=0
        elif mode==20:args[2]=2
        check(d,0,status,args=args)
    assert count==90
    print('PASS: ARM64 weak incoming 90 exact-state steps, source repair, bounded coverage and stale-capture failures')


def verify_weak_follow(m):
    base=MEMORY+0x2000;pool=128;slots=176;pages=256;tables=640;cells=768
    table=4304;entry=4352;q=4416;gate=4480;size=4608;calls=0
    def put(d,o,*v):struct.pack_into('<'+'Q'*len(v),d,o,*v)
    refs=[(((i+1)*131072+32)<<16)|0x1510 for i in range(3)]
    for mode in range(32):
        m.reset();d=bytearray(size)
        put(d,0,0x500000000,base+pages,3)
        put(d,pool,base,base+slots,2,2,1);put(d,slots,1,base+pages,27,0)
        for i in range(3):
            start=(i+1)*131072
            put(d,pages+i*112,0x500000000,start,start+131072,27+i,1024,0,0,5,
                base+1024+i*1024,128,start+48,i,int(i==0),base+tables+i*24)
            put(d,1024+i*1024,4);put(d,tables+i*24,base+cells+i*32,1,1)
            put(d,cells+i*32,start+32,27+i,refs[i+1]|2 if i<2 else 0,0)
        put(d,table,base+gate,base+entry,1,0,1,11)
        put(d,entry,base+gate,11,refs[0],base+pages,27,(1<<32)|1,0)
        put(d,q,1,11,0,0,0);put(d,gate,base+pool,1,0,0)
        args=[base+table,base+q,0];result=1
        if mode==0:result=3
        elif mode==1:put(d,cells+16,5);result=2
        elif mode in (2,3):put(d,cells+16,0 if mode==2 else 3);result=0
        elif mode==4:put(d,pool+16,1,0,1);result=5
        elif mode==5:
            put(d,entry+16,0,0,0,0);put(d,pool+24,1,0);put(d,slots,1,0,0,2);put(d,pages+96,0);result=0
        elif mode==6:put(d,q,0)
        elif mode==7:put(d,q+8,12)
        elif mode==8:put(d,entry,base+gate+8)
        elif mode==9:put(d,entry+48,1)
        elif mode==10:put(d,gate+8,2)
        elif mode==11:put(d,table+16,0)
        elif mode==12:put(d,table+8,MASK-7)
        elif mode==13:put(d,table+16,1<<32)
        elif mode==14:put(d,table+32,2)
        elif mode==15:put(d,table+40,0)
        elif mode==16:put(d,table+24,2)
        elif mode==17:put(d,table+24,1)
        elif mode==18:put(d,table,base+gate+1)
        elif mode==19:put(d,gate,base+pool+1)
        elif mode==20:put(d,q+32,1);result=7
        elif mode==21:args[1]+=1
        elif mode==22:args[0]=0
        elif mode==23:args[2]=1
        elif mode==24:put(d,entry+24,0,0,0)
        elif mode==25:put(d,entry+40,(2<<32)|1)
        elif mode==26:put(d,slots+32,0xffffffff);result=6
        elif mode==27:put(d,entry+16,0)
        elif mode==28:put(d,gate+8,0);result=3
        elif mode==29:put(d,table+16,2)
        elif mode==30:put(d,table+8,0)
        elif mode==31:args[1]=0
        expected=bytearray(d)
        if result==3:
            put(expected,pool+24,1,1);put(expected,slots,1,0,0,0)
            put(expected,slots+32,1,base+pages+112,28,0)
            put(expected,pages+96,0);put(expected,pages+112+96,1)
            put(expected,entry+16,refs[1],base+pages+112,28,(1<<32)|2)
        m.uc.mem_write(base,bytes(d));actual=m.call('weak_follow',*args)
        assert actual==result,(mode,actual,result)
        assert bytes(m.uc.mem_read(base,size))==bytes(expected),mode
        calls+=1
        if mode==0:
            put(expected,pool+24,2,1);put(expected,slots,2,base+pages+224,29,0)
            put(expected,slots+32,1,0,0,0)
            put(expected,pages+112+96,0);put(expected,pages+224+96,1)
            put(expected,entry+16,refs[2],base+pages+224,29,(2<<32)|1)
            assert m.call('weak_follow',*args)==3
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            assert m.call('weak_follow',*args)==0
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            calls+=2
    assert calls==34
    print('PASS: ARM64 weak follow 34 exact-state steps, two-hop slot transfer and geometry failures')


def verify_pending_follow(m):
    base=MEMORY+0x2000;pool=128;slots=176;pages=256;tables=640;cells=768
    q=4096;retired=4128;size=4608;calls=0
    def put(data,o,*values):
        struct.pack_into('<'+'Q'*len(values),data,o,*values)
    refs=[(((i+1)*131072+32)<<16)|0x1510 for i in range(3)]
    for mode in range(6):
        m.reset();data=bytearray(size)
        put(data,0,0x500000000,base+pages,3)
        put(data,pool,base,base+slots,1 if mode==4 else 2,1,0)
        put(data,slots+24,0 if mode==4 else 2)
        for i in range(3):
            start=(i+1)*131072
            put(data,pages+i*112,0x500000000,start,start+131072,27+i,1024,0,0,5,
                base+1024+i*1024,128,start+48,i,0,base+tables+i*24)
            put(data,1024+i*1024,4)
            put(data,tables+i*24,base+cells+i*32,1,1)
            put(data,cells+i*32,start+32,27+i,refs[i+1]|2 if i<2 else 0,0)
        put(data,q,0 if mode==5 else refs[0])
        if mode in (1,2,3):put(data,cells+16,{1:5,2:0,3:3}[mode])
        m.uc.mem_write(base,bytes(data));expected=bytearray(data)
        if mode!=5:
            put(expected,pool+24,0 if mode==4 else 2,1)
            put(expected,slots,1,base+pages,27,0)
            put(expected,pages+96,1)
            put(expected,q+8,base+pages,27,(1<<32)|1)
        if mode==0:
            put(expected,pool+24,1,1)
            put(expected,slots,1,0,0,0)
            put(expected,slots+32,1,base+pages+112,28,0)
            put(expected,pages+96,0);put(expected,pages+112+96,1)
            put(expected,q,refs[1],base+pages+112,28,(1<<32)|2)
        status=[3,2,0,0,5,0][mode]
        assert m.call('pending_follow',base+pool,base+q,base+retired)==status,mode
        actual=bytes(m.uc.mem_read(base,size))
        assert actual==bytes(expected),(mode,[(o,unpack('Q',actual,o)[0],unpack('Q',expected,o)[0]) for o in range(0,size,8) if actual[o:o+8]!=expected[o:o+8]])
        calls+=1
        if mode==0:
            put(expected,pool+24,2,1)
            put(expected,slots,2,base+pages+224,29,0)
            put(expected,slots+32,1,0,0,0)
            put(expected,pages+112+96,0);put(expected,pages+224+96,1)
            put(expected,q,refs[2],base+pages+224,29,(2<<32)|1)
            assert m.call('pending_follow',base+pool,base+q,base+retired)==3
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            assert m.call('pending_follow',base+pool,base+q,base+retired)==0
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            calls+=2
        elif mode!=5:
            # A retained request must not acquire another token on retry.
            assert m.call('pending_follow',base+pool,base+q,base+retired)==status
            assert bytes(m.uc.mem_read(base,size))==bytes(expected)
            calls+=1
    assert calls==12
    print('PASS: ARM64 pending follow 12 exact-state steps, two-hop lease transfer and retained retries')


def verify_native_root_publish(m):
    base=MEMORY+0x2000;ctx=8192;registry=8600;charge=8704;spec=8800
    def put(o,*v):m.uc.mem_write(base+o,struct.pack('<'+'Q'*len(v),*v))
    cases=0
    for fault in range(36):
        m.reset();m.uc.mem_write(base,bytes(12288))
        m.uc.mem_write(base+ctx,struct.pack('<IIQQ',m.context_layout['abi_version'],0,17,0x10000))
        put(registry+48,0x10000);put(charge,4096,base+9000,base,2)
        put(spec,17,3,7,4096)
        assert m.call('root_storage_prepare',base,base+spec)==0
        args=[base+registry,base+ctx,base+charge];expected=1
        edits={1:(charge+24,0),2:(charge+24,1),3:(charge+24,3),
               4:(charge+8,0),5:(charge+8,base+9001),6:(charge+16,0),
               7:(charge+16,base+1),8:(charge+16,MASK-7),9:(charge,263),
               10:(charge,583),11:(0,0),12:(176,base+272),13:(208,0),
               14:(184,0),15:(184,1<<32),16:(232,0),17:(232,1<<32),
               18:(224,base+368),19:(240,1),20:(248,1),21:(256,1),
               22:(160,18),23:(192,1),24:(200,1),
               25:(ctx,m.context_layout['abi_version']+1),26:(ctx+8,18),
               27:(ctx,m.context_layout['abi_version']+(2<<32)),
               28:(ctx+144,27),29:(ctx+16,0x20000),
               32:(registry,MASK),33:(registry+16,MASK),34:(ctx+128,1)}
        if fault in edits:put(*edits[fault])
        if fault==30:args[2]=0
        if fault==31:args[0]+=1
        if fault==35:args[1]=0
        if fault in (32,33):expected=3
        before=bytes(m.uc.mem_read(base,12288));after=bytearray(before)
        if fault==0:
            expected=0
            for o,v in ((8,1),(16,1),(120,base+ctx),(168,1),(200,1),
                        (216,base),(ctx+128,base),(registry,1),
                        (registry+16,1),(registry+56,base)):
                struct.pack_into('<Q',after,o,v)
        assert m.call('native_root_publish',*args)==expected, fault
        assert bytes(m.uc.mem_read(base,12288))==bytes(after), fault
        if fault==0:
            assert m.call('native_root_publish',*args)==1
            assert bytes(m.uc.mem_read(base,12288))==bytes(after)
            # A second package must join both inventories without disturbing
            # the first package's cells, journal, scope or budget charge.
            second=4096
            assert m.call('root_storage_prepare',base+second,base+spec)==0
            put(charge+32,4096,base+9000,base+second,2)
            after=bytearray(m.uc.mem_read(base,12288))
            for o,v in ((second+8,2),(second+16,1),(second+112,base),
                        (second+120,base+ctx),(second+136,base),
                        (second+168,2),(second+200,1),(second+216,base+second),
                        (104,base+second),(128,base+second),
                        (ctx+128,base+second),(registry,2),
                        (registry+16,2),(registry+56,base+second)):
                struct.pack_into('<Q',after,o,v)
            assert m.call('native_root_publish',base+registry,base+ctx,base+charge+32)==0
            assert bytes(m.uc.mem_read(base,12288))==bytes(after)
        cases+=1
    assert cases==36
    print('PASS: ARM64 native root publication 36 exact-state cases, duplicate rejection and linked package growth')


def verify_root_storage_prepare(m):
    m.reset()
    address, spec_address = MEMORY + 0x2000, MEMORY + 0x8000
    base = address + 8
    def fixture():
        data = bytearray(16384)
        data[:8] = data[-8:] = bytes([0xa5]) * 8
        return data
    count = 0
    for cells in (1,3,17,256):
        for entries in (1,2,7):
            before = fixture()
            after = bytearray(before)
            # Independently specified ABI prefix: registration 160, segment 64,
            # journal 40 bytes; cells and entries are each 32 bytes.
            for offset, value in ((0,base+160), (160,17), (176,base+264),
                                  (184,cells), (208,base+224),
                                  (224,base+264+cells*32), (232,entries)):
                struct.pack_into('<Q', after, offset+8, value)
            spec = struct.pack('<4Q',17,cells,entries,264+(cells+entries)*32)
            m.uc.mem_write(address,bytes(before))
            m.uc.mem_write(spec_address,spec)
            assert m.call('root_storage_prepare',base,spec_address) == 0
            assert bytes(m.uc.mem_read(address,len(after))) == bytes(after)
            assert bytes(m.uc.mem_read(spec_address,len(spec))) == spec
            count += 1
    for fault in range(9):
        before = fixture()
        values = [17,3,2,424]
        argument = base
        if fault < 3: values[fault] = 0
        elif fault in (3,4): values[fault-2] = 0x100000000
        elif fault == 5: values[3] -= 1
        elif fault == 6: values[3] = MASK
        elif fault == 7: argument = 0
        else: argument += 1
        spec = struct.pack('<4Q',*values)
        m.uc.mem_write(address,bytes(before))
        m.uc.mem_write(spec_address,spec)
        assert m.call('root_storage_prepare',argument,spec_address) == 1
        assert bytes(m.uc.mem_read(address,len(before))) == bytes(before)
        assert bytes(m.uc.mem_read(spec_address,len(spec))) == spec
        count += 1
    assert count == 21
    print('PASS: ARM64 root storage 21 independent layout and unchanged rejection cases')


def verify_root_copy(m):
    base=MEMORY+0x2000
    segment=m.context_layout['size'];source=segment+64;destination=source+40
    cells=512;journal=600;entries=640;phase=0x12340122
    def put(data,offset,value,width=8):
        struct.pack_into('<I' if width==4 else '<Q',data,offset,value)
    count=0
    for test in range(24):
        m.reset();data=bytearray(1024)
        struct.pack_into('<IIQQ',data,0,m.context_layout['abi_version'],0,17,phase)
        struct.pack_into('<8Q',data,segment,17,101,base+cells,2,2,1,base+journal,0)
        struct.pack_into('<5Q',data,source,base+segment,base+cells,1,101,111)
        struct.pack_into('<5Q',data,destination,base+segment,base+cells+32,1,101,222)
        struct.pack_into('<8Q',data,cells,0x401510,1,1,0,0x801510,1,1,0)
        struct.pack_into('<5Q',data,journal,base+entries,7,0,0,0)
        ctx=base;src=base+source;dst=base+destination;target=cells+32
        result=0 if test<5 else 1
        if test==1:put(data,cells,0)
        elif test==2:put(data,cells+32,0)
        elif test==3:put(data,destination+8,base+cells);target=cells
        elif test==4:dst=src;target=cells
        elif test==5:put(data,journal+16,7);put(data,journal+24,7);result=3
        elif test==6:put(data,0,m.context_layout['abi_version']-1,4)
        elif test==7:put(data,4,2,4)
        elif test==8:put(data,144,1)
        elif test==9:put(data,source+16,2)
        elif test==10:put(data,destination+16,2)
        elif test==11:put(data,segment+40,2)
        elif test==12:put(data,source+24,102)
        elif test==13:put(data,destination+24,102)
        elif test==14:put(data,journal+24,1)
        elif test==15:put(data,152,MASK);result=4
        elif test==16:
            for offset in (16,24,32):put(data,journal+offset,MASK)
            result=4
        elif test==17:put(data,8,18)
        elif test==18:put(data,cells+16,0)
        elif test==19:put(data,cells+48,0)
        elif test==20:src=0
        elif test==21:dst=0
        elif test==22:ctx=0
        elif test==23:dst+=1
        expected=bytearray(data)
        if result==0:
            new=struct.unpack_from('<Q',data,cells)[0]
            old=struct.unpack_from('<Q',data,target)[0]
            struct.pack_into('<4Q',expected,entries,old,new,101,phase)
            put(expected,target,new);put(expected,152,1)
            put(expected,journal+16,1);put(expected,journal+24,1)
        m.uc.mem_write(base,bytes(data))
        assert m.call('root_copy',ctx,src,dst)==result,test
        assert bytes(m.uc.mem_read(base,1024))==bytes(expected),test
        count+=1
        if test==5:
            # Model serviced pressure and a separately protected source update.
            put(data,cells,0xc01510);put(data,152,1)
            put(data,journal+16,8);put(data,journal+24,8);put(data,journal+32,7)
            struct.pack_into('<4Q',data,entries,0x401510,0xc01510,101,phase)
            expected=bytearray(data)
            put(expected,cells+32,0xc01510);put(expected,152,2)
            put(expected,journal+16,9);put(expected,journal+24,9)
            struct.pack_into('<4Q',expected,entries+32,0x801510,0xc01510,101,phase)
            m.uc.mem_write(base,bytes(data))
            assert m.call('root_copy',ctx,src,dst)==0
            assert bytes(m.uc.mem_read(base,1024))==bytes(expected)
            count+=1
    assert count==25
    print('PASS: ARM64 root copy 25 independent full-state alias, journal and retry cases')


def verify_root_cancel_empty(m):
    # Independent ABI fixture: context, segment, request, cell, journal, entries.
    # Compare every byte, including query.value and the published scan bound.
    m.reset()
    base = MEMORY + 0x2000
    segment=m.context_layout["size"]
    query,cell,journal,entries=segment+64,segment+104,segment+136,segment+176
    def fixture(pressure=0):
        data = bytearray(680)
        struct.pack_into('<IIQQ', data, 0, m.context_layout['abi_version'], 0, 17, 0x12340122)
        struct.pack_into('<Q', data, 152, MASK if pressure & 2 else 7)
        struct.pack_into('<8Q', data, segment, 17, 101, base+cell, 1, 1, 1, base+journal, 0)
        struct.pack_into('<5Q', data, query, base+segment, base+cell, 1, 101, 0xabc)
        struct.pack_into('<4Q', data, cell, 0, 1, 1, 0)
        top = 7 if pressure & 1 else 0
        struct.pack_into('<5Q', data, journal, base+entries, 7, top, top, 0)
        data[entries:] = bytes([0xa5]) * (len(data)-entries)
        return data
    count = 0
    for pressure in range(4):
        before = fixture(pressure)
        after = bytearray(before)
        struct.pack_into('<Q', after, cell+16, 0)
        m.uc.mem_write(base, bytes(before))
        assert m.call('root_cancel_empty', base, base+query) == 0
        assert bytes(m.uc.mem_read(base, len(after))) == bytes(after)
        assert m.call('root_cancel_empty', base, base+query) == 1
        assert bytes(m.uc.mem_read(base, len(after))) == bytes(after)
        count += 2
    for offset, value, width in ((0,m.context_layout["abi_version"]-1,4), (8,18,8), (4,2,4), (144,1,8),
                                (query+24,102,8), (query+8,base+cell+32,8),
                                (query+8,base+cell+1,8), (cell+16,0,8),
                                (query+16,0,8), (query+16,2,8),
                                (cell,0x12340000,8), (segment+40,2,8)):
        before = fixture()
        struct.pack_into('<I' if width == 4 else '<Q', before, offset, value)
        m.uc.mem_write(base, bytes(before))
        assert m.call('root_cancel_empty', base, base+query) == 1
        assert bytes(m.uc.mem_read(base, len(before))) == bytes(before)
        count += 1
    assert count == 20
    print('PASS: ARM64 root cancel 20 pressure/stale/nonempty cases preserve journal and scan bounds')


def verify_allocation_shape(m):
    m.reset()
    base = MEMORY + 0x2000
    specs = ((1,16,0,1,0,0,0,0), (2,8,0,8,0,base+400,0,1),
             (3,8,32,0,0,0,0,0), (4,8,8,8,0,base+400,0,1))
    def fixture(identity, size):
        data = bytearray(416)
        struct.pack_into('<QQ', data, 0, base+16, 8)
        for i, spec in enumerate(specs):
            struct.pack_into('<Q', data, 16+8*(i+1), base+80+64*i)
            struct.pack_into('<8Q', data, 80+64*i, *spec)
        struct.pack_into('<3Q', data, 336, identity, size, base+360)
        struct.pack_into('<5Q', data, 360, 11,22,33,44,55)
        struct.pack_into('<QQ', data, 400, 1, 0xa5a5a5a5a5a5a5a5)
        return data
    count = 0
    for index, spec in enumerate(specs):
        for size in (0,1,7,8,9,16,24,32,33,64,2048,0xffffffffffff,0x1000000000000):
            before = fixture(index+1, size)
            after = bytearray(before)
            fixed, element = spec[2:4]
            valid = size <= 0xffffffffffff and size >= fixed and ((size-fixed) % element == 0 if element else size == fixed)
            if valid:
                struct.pack_into('<QQ', after, 368, base+80+64*index, (size-fixed)//element if element else 0)
            m.uc.mem_write(base, bytes(before))
            assert m.call('allocation_shape', base, base+336) == (0 if valid else 2)
            assert bytes(m.uc.mem_read(base, len(after))) == bytes(after)
            count += 1
    for offset, value in ((336,0),(336,5),(336,8),(336,MASK),(352,0),(352,base+361),
                          (0,0),(0,base+17),(8,1),(8,MASK)):
        before = fixture(1,8)
        struct.pack_into('<Q', before, offset, value)
        m.uc.mem_write(base, bytes(before))
        assert m.call('allocation_shape', base, base+336) == 2
        assert bytes(m.uc.mem_read(base, len(before))) == bytes(before)
        count += 1
    assert count == 62
    print('PASS: ARM64 allocation shape 62 ID/payload and unchanged rejection cases')


def verify_builtin_prepare(m):
    m.reset()
    address = MEMORY + 0x2000
    for offset in (8, 16, 24):
        before = bytearray([0xa5] * 240)
        after = bytearray(before)
        base = address + offset
        struct.pack_into('<Q', after, offset, 1)
        for start, values in ((8, (1,16,0,1,0,0,0,0)),
                              (72, (2,8,0,8,0,base,0,1)),
                              (136, (4,8,8,8,0,base,0,1))):
            struct.pack_into('<8Q', after, offset + start, *values)
        m.uc.mem_write(address, bytes(before))
        m.call('builtin_prepare', base)
        assert bytes(m.uc.mem_read(address, len(after))) == bytes(after)
    print('PASS: ARM64 builtin descriptor preparation matches all 200 bytes and preserves canaries')


def verify_metadata_backing(m, prefix="metadata_"):
    m.reset()
    for size in (0, 1, 4096, 16385):
        assert m.call(prefix + 'map', size) == 0
    assert not m.calls['mmap']
    m.failures['mmap'] = [MASK]
    assert m.call(prefix + 'map', 16384) == 0 and not m.metadata_mapped
    base = m.call(prefix + 'map', 16384)
    assert base == MEMORY + 0x8000 and m.metadata_mapped
    assert bytes(m.uc.mem_read(base, 16384)) == bytes(16384)
    m.put(base, 0x51)
    m.put(base + 16376, 0xa7)
    for address, size in ((0, 16384), (base + 1, 16384), (base, 0), (base, 4096)):
        assert m.call(prefix + 'unmap', address, size) == 0
    assert not m.calls['munmap']
    m.failures['munmap'] = [0xffffffff]
    assert m.call(prefix + 'unmap', base, 16384) == 0 and m.metadata_mapped
    assert m.get(base) == 0x51 and m.get(base + 16376) == 0xa7
    assert m.call(prefix + 'unmap', base, 16384) == 1 and not m.metadata_mapped
    print(f'PASS: ARM64 {prefix}mmap/munmap ABI, zero backing, failures and retry (OS model)')


def verify_metadata_grow(m):
    """Independent complete-memory expectations, including the 256-slot bound."""
    m.reset()
    address = MEMORY + 0x2000
    slots = address + 48
    count = 0
    for old in range(1, 5):
        for active in range(old + 1):
            for added in (1, 2, 63, 64, 255, 256):
                size = old + added
                head = active + 1 if active < old else 0
                before = bytearray(48 + 260 * 32)
                struct.pack_into('<QQQQQ', before, 0, address + 40, slots, old, head, active)
                for i in range(old):
                    cell = (23, 0x1000 + 8*i, 17, 0) if i < active else (0, 0, 0, i+2 if i+1 < old else 0)
                    struct.pack_into('<QQQQ', before, 48 + 32*i, *cell)
                after = bytearray(before)
                for i in range(old, size):
                    struct.pack_into('<Q', after, 48 + 32*i + 24, i+2 if i+1 < size else head)
                struct.pack_into('<QQ', after, 16, size, old + 1)
                m.uc.mem_write(address, bytes(before))
                assert m.call('metadata_grow', address, size) == 0
                assert bytes(m.uc.mem_read(address, len(after))) == bytes(after)
                assert m.call('metadata_grow', address, size) == 1
                assert bytes(m.uc.mem_read(address, len(after))) == bytes(after)
                count += 2
    for fault in range(12):
        before = bytearray(48 + 260 * 32)
        struct.pack_into('<QQQQQ', before, 0, address + 40, slots, 2, 1, 0)
        struct.pack_into('<Q', before, 48 + 24, 2)
        size = 3
        if fault in (0, 1, 2):
            struct.pack_into('<Q', before, fault * 8, 0)
        elif fault == 3:
            size = 1
        elif fault == 4:
            size = 259
        elif fault == 5:
            struct.pack_into('<Q', before, 32, 3)
        elif fault in (6, 7):
            struct.pack_into('<Q', before, 24, 3 if fault == 6 else 0)
        else:
            struct.pack_into('<Q', before, 48 + 64 + (8, 16, 0, 24)[fault - 8], 8 if fault == 8 else 1)
        m.uc.mem_write(address, bytes(before))
        assert m.call('metadata_grow', address, size) == 1
        assert bytes(m.uc.mem_read(address, len(before))) == bytes(before)
        count += 1
    assert count == 180
    print('PASS: ARM64 metadata grow 180 admission and unchanged rejection cases')


def verify_memory_steps(m, path, magic, function, count):
    data = path.read_bytes()
    assert data[:8] == magic
    size, arg0, arg1, pointers = unpack('QQQQ', data, 8)
    assert size < 0x10000 and pointers < 64
    fields = unpack('Q' * pointers, data, 40)
    assert all(field + 8 <= size for field in fields)
    first, record_size = 40 + 8 * pointers, 32 + 2 * size
    assert (len(data) - first) % record_size == 0
    address = MEMORY + 0x2000
    m.reset()
    cases = 0
    for pos in range(first, len(data), record_size):
        op, result, before_mask, after_mask = unpack('QQQQ', data, pos)
        before = bytearray(data[pos + 32:pos + 32 + size])
        after = bytearray(data[pos + 32 + size:pos + record_size])
        for part, mask in [(before, before_mask), (after, after_mask)]:
            for i, field in enumerate(fields):
                if mask & (1 << i):
                    offset = unpack('Q', part, field)[0]
                    assert offset <= size
                    struct.pack_into('<Q', part, field, address + offset)
        m.uc.mem_write(address, bytes(before))
        args = (address + arg0, op) if function in {"root_walk_item", "guarded_scalar"} else (address + arg0, address + arg1, op)
        assert m.call(function, *args) == result, cases
        assert bytes(m.uc.mem_read(address, size)) == bytes(after), cases
        cases += 1
    assert cases == count
    print(f'PASS: ARM64 replay of {cases} C {function} memory/state vectors')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directory', type=Path)
    parser.add_argument('--handshake-vectors', type=Path)
    parser.add_argument('--tlab-vectors', type=Path)
    parser.add_argument('--mark-spill-vectors', type=Path)
    parser.add_argument('--mark-vectors', type=Path)
    parser.add_argument('--forward-incoming-vectors', type=Path)
    parser.add_argument('--root-walk-heal-vectors', type=Path)
    parser.add_argument('--root-walk-item-vectors', type=Path)
    parser.add_argument('--relocation-gate-vectors', type=Path)
    parser.add_argument('--relocation-job-vectors', type=Path)
    parser.add_argument('--relocation-complete-vectors', type=Path)
    parser.add_argument('--relocation-claim-vectors', type=Path)
    parser.add_argument('--relocate-discard-vectors', type=Path)
    parser.add_argument('--relocation-cancel-vectors', type=Path)
    parser.add_argument('--alloc-into-vectors', type=Path)
    parser.add_argument('--alloc-start-vectors', type=Path)
    parser.add_argument('--initialization-store-vectors', type=Path)
    parser.add_argument('--initialization-copy-vectors', type=Path)
    parser.add_argument('--scalar-vectors', type=Path)
    parser.add_argument('--source-uncommit-vectors', type=Path)
    parser.add_argument('--source-drain-vectors', type=Path)
    parser.add_argument('--mark-scan-vectors', type=Path)
    parser.add_argument('--metadata-vectors', type=Path)
    parser.add_argument('--budget-vectors', type=Path)
    parser.add_argument('--reference-lease-vectors', type=Path)
    parser.add_argument('--forward-vectors', type=Path)
    parser.add_argument('--source-access-vectors', type=Path)
    parser.add_argument('--forward-lookup-vectors', type=Path)
    parser.add_argument('--resolve-vectors', type=Path)
    parser.add_argument('--load-vectors', type=Path)
    parser.add_argument('--heal-vectors', type=Path)
    parser.add_argument('--root-follow-vectors', type=Path)
    parser.add_argument('--root-access-vectors', type=Path)
    parser.add_argument('--root-journal-vectors', type=Path)
    parser.add_argument('--buffer-job-vectors', type=Path)
    parser.add_argument('--root-audit-vectors', type=Path)
    parser.add_argument('--mark-end-vectors', type=Path)
    parser.add_argument('--mark-phase-vectors', type=Path)
    parser.add_argument('--remembered-vectors', type=Path)
    parser.add_argument('--remembered-inventory-vectors', type=Path)
    parser.add_argument('--remembered-load-vectors', type=Path)
    parser.add_argument('--mark-route-vectors', type=Path)
    parser.add_argument('--remembered-transfer-vectors', type=Path)
    parser.add_argument('--remembered-owner-vectors', type=Path)
    parser.add_argument('--remembered-walk-vectors', type=Path)
    parser.add_argument('--remembered-lease-vectors', type=Path)
    parser.add_argument('--cycle-vectors', type=Path)
    parser.add_argument('--initialization-vectors', type=Path)
    parser.add_argument('--mark-prepare-vectors', type=Path)
    parser.add_argument('--mark-retarget-vectors', type=Path)
    parser.add_argument('--worker-vectors', type=Path)
    parser.add_argument('--store-vectors', type=Path)
    parser.add_argument('--page-release-vectors', type=Path)
    parser.add_argument('--target-vectors', type=Path)
    parser.add_argument('--copy-vectors', type=Path)
    parser.add_argument('--copy-publish-vectors', type=Path)
    parser.add_argument('--pool-vectors', type=Path)
    parser.add_argument('--binding-vectors', type=Path)
    parser.add_argument('--descriptor-vectors', type=Path)
    parser.add_argument('--scan-vectors', type=Path)
    parser.add_argument('--root-vectors', type=Path)
    parser.add_argument('--root-scan-vectors', type=Path)
    parser.add_argument('--root-drain-vectors', type=Path)
    parser.add_argument('--root-life-vectors', type=Path)
    parser.add_argument('--root-clear-vectors', type=Path)
    parser.add_argument('--root-leased-vectors', type=Path)
    parser.add_argument('--root-walk-vectors', type=Path)
    parser.add_argument('--root-walk-scan-vectors', type=Path)
    parser.add_argument('--detach-vectors', type=Path)
    parser.add_argument('--buffer-step-vectors', type=Path)
    parser.add_argument('--buffer-drain-vectors', type=Path)
    args = parser.parse_args()
    verify(args.directory, args.handshake_vectors, args.tlab_vectors, args.pool_vectors, args.binding_vectors, args.descriptor_vectors, args.scan_vectors, args.root_vectors, args.root_scan_vectors, args.root_drain_vectors, args.root_life_vectors, args.root_clear_vectors, args.root_leased_vectors, args.root_walk_vectors, args.root_walk_scan_vectors, args.detach_vectors, args.buffer_step_vectors, args.buffer_drain_vectors, args.metadata_vectors, args.mark_vectors, args.mark_scan_vectors, args.mark_spill_vectors, args.budget_vectors, args.reference_lease_vectors, args.forward_vectors, args.source_access_vectors, args.forward_lookup_vectors, args.resolve_vectors, args.copy_vectors, args.copy_publish_vectors, args.target_vectors, args.page_release_vectors, args.store_vectors, args.load_vectors, args.root_follow_vectors, args.mark_prepare_vectors, args.mark_retarget_vectors, args.worker_vectors, args.root_journal_vectors, args.buffer_job_vectors, args.root_audit_vectors, args.mark_end_vectors, args.mark_phase_vectors, args.cycle_vectors, args.remembered_vectors, args.remembered_inventory_vectors, args.remembered_load_vectors, args.mark_route_vectors, args.remembered_transfer_vectors, args.remembered_owner_vectors, args.remembered_walk_vectors, args.remembered_lease_vectors, args.root_access_vectors, args.heal_vectors, args.source_drain_vectors, args.source_uncommit_vectors, args.forward_incoming_vectors, args.root_walk_heal_vectors, args.root_walk_item_vectors, args.relocation_gate_vectors, args.relocation_job_vectors, args.relocation_complete_vectors, args.relocation_claim_vectors, args.relocate_discard_vectors, args.relocation_cancel_vectors, args.alloc_into_vectors, args.initialization_vectors, args.alloc_start_vectors, args.initialization_store_vectors, args.initialization_copy_vectors, args.scalar_vectors)
