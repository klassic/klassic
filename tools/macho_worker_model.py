"""Deterministic pthread API model for generated Darwin ARM64 worker code.

Runs actual emitted instructions with distinct register/stack contexts and
shared memory. This is an API/scheduling model, never native Darwin evidence.
"""
import os
import struct
import sys
import time
from collections import deque
from unicorn import UC_HOOK_BLOCK, UC_HOOK_CODE
from unicorn.arm64_const import *


def primary_owner(uc):
    # Runtime.primary_owner is its first word; OwnerState.ctx is first too.
    owner = struct.unpack('<Q', uc.mem_read(0x200000000000, 8))[0]
    assert owner and owner % 8 == 0, 'invalid primary owner link'
    return owner


class WorkerModel:
    def __init__(self, uc, state):
        self.uc, self.state = uc, state
        self.history = deque(maxlen=30)
        if os.environ.get('KLASSIC_MACHO_WORKER_TRACE'):
            uc.hook_add(UC_HOOK_BLOCK, lambda u, pc, size, data: self.history.append((self.current, pc)))
        self.contexts = {}
        self.blocked = {}
        self.finished, self.joined = {}, set()
        self.native_entry = {}
        self.stacks = {}
        self.mutexes, self.conditions = {}, set()
        self.tls_keys, self.tls_values, self.next_tls_key = set(), {}, 0
        self.current = 0
        self.next_thread = 1
        self.fault = os.environ.get('KLASSIC_MACHO_WORKER_FAULT', '')
        self.quantum = int(os.environ.get('KLASSIC_MACHO_WORKER_QUANTUM', '50000'))
        assert self.quantum > 0
        self.counts = dict(threads=0, waits=0, owner_waits=0, timeouts=0, signals=0, clocks=0, yields=0,
                           mutex_init=0, mutex_destroy=0, cond_init=0, cond_destroy=0, decommits=0,
                           returns=0, join_calls=0, joins=0, stack_releases=0,
                           native_sleeps=0, native_sleep_ns=0, sleep_interrupts=0,
                           native_sleep_progress=0, native_read_blocks=0, native_read_progress=0)

    def install_yields(self, addresses):
        # YIELD is an architectural hint with no register/flag effects. Model
        # an allowed scheduling point there instead of spending a whole time
        # slice in a ticket-lock wait when another CPU context can release it.
        def yield_cpu(uc, pc, size, data):
            self.counts['yields'] += 1
            uc.reg_write(UC_ARM64_REG_PC, pc + 4)
            uc.emu_stop()
        for address in addresses:
            self.uc.hook_add(UC_HOOK_CODE, yield_cpu, begin=address, end=address)

    def result(self, result):
        # Foreign C APIs may destroy all volatile registers; x18 is reserved.
        for n in range(1, 18):
            self.uc.reg_write(UC_ARM64_REG_X0 + n, 0xdead00000000 + n)
        for n in range(32):
            if n < 8 or n >= 16:
                self.uc.reg_write(UC_ARM64_REG_Q0 + n, 0xdeadbeef)
        self.uc.reg_write(UC_ARM64_REG_X0, 0xa55a5aa500000000 | result)
        self.uc.reg_write(UC_ARM64_REG_NZCV, 0xf0000000)

    def suspend(self, reason):
        self.blocked[self.current] = reason
        self.uc.reg_write(UC_ARM64_REG_PC, self.uc.reg_read(UC_ARM64_REG_LR))
        self.uc.emu_stop()

    def call(self, symbol):
        u = self.uc
        x = [u.reg_read(UC_ARM64_REG_X0 + n) for n in range(4)]
        if symbol == '___error':
            assert hasattr(self, 'sleep_errno')
            # Model a native errno cell outside request/remainder storage.
            u.mem_write(self.sleep_errno, struct.pack('<I', self.sleep_error))
            self.result(0)
            u.reg_write(UC_ARM64_REG_X0, self.sleep_errno)  # pointer, not C int
            return True
        if symbol == '_nanosleep':
            assert self.current == 0 and not any(v == 0 for v in self.mutexes.values())
            assert struct.unpack('<II', u.mem_read(primary_owner(u), 8)) == (4, 2)
            sec, ns = struct.unpack('<qq', u.mem_read(x[0], 16))
            assert sec >= 0 and 0 <= ns < 1000000000 and x[1]
            self.counts['native_sleeps'] += 1
            duration = sec * 1000000000 + ns
            self.sleep_errno = x[0] - 16
            if self.fault == 'sleep-interrupt' and duration and not self.counts['sleep_interrupts']:
                self.counts['sleep_interrupts'] += 1
                self.sleep_error = 4
                # Interrupt halfway through a nonempty interval. The resumed
                # call must request only the remainder, not the original wait.
                remaining = duration // 2
                elapsed = duration - remaining
                self.counts['native_sleep_ns'] += elapsed
                self.state['monotonic_ns'] += elapsed
                u.mem_write(x[1], struct.pack('<qq', remaining // 1000000000, remaining % 1000000000))
                self.result(0xffffffff)
            elif self.fault == 'sleep-error':
                self.sleep_error = 22
                self.result(0xffffffff)
            else:
                self.counts['native_sleep_ns'] += duration
                self.result(0)
                if duration:
                    self.suspend(('native-sleep', self.state['monotonic_ns'] + duration,
                                  self.counts['decommits'], self.counts['signals']))
            return True
        if symbol == '_clock_gettime':
            assert x[0] == 6
            self.counts['clocks'] += 1
            if self.fault == 'clock':
                self.result(-1 & 0xffffffff)
            else:
                ns = self.state['monotonic_ns']
                u.mem_write(x[1], struct.pack('<qq', ns // 1000000000, ns % 1000000000))
                self.result(0)
            return True
        if symbol == '_pthread_key_create':
            assert x[1] == 0
            if self.fault == 'tls-create': self.result(12)
            else:
                key = self.next_tls_key
                self.next_tls_key += 1
                self.tls_keys.add(key)
                u.mem_write(x[0], struct.pack('<Q', key))
                self.result(0)
        elif symbol == '_pthread_key_delete':
            if x[0] not in self.tls_keys or self.fault == 'tls-delete': self.result(22)
            else:
                self.tls_keys.remove(x[0])
                self.tls_values = {k: v for k, v in self.tls_values.items() if k[1] != x[0]}
                self.result(0)
        elif symbol == '_pthread_setspecific':
            if x[0] not in self.tls_keys or self.fault == 'tls-set': self.result(22)
            else:
                self.tls_values[self.current, x[0]] = x[1]
                self.result(0)
        elif symbol == '_pthread_self':
            self.result(0)
            u.reg_write(UC_ARM64_REG_X0, self.current + 1)  # opaque nonzero thread identity
        elif symbol == '_pthread_getspecific':
            value = self.tls_values.get((self.current, x[0]), 0) if x[0] in self.tls_keys else 0
            self.result(0)  # also destroy native volatile registers
            u.reg_write(UC_ARM64_REG_X0, value)  # pointer result, not dirty C int
        elif symbol == '_pthread_mutex_init':
            self.counts['mutex_init'] += 1
            assert x[1] == 0 and x[0] not in self.mutexes
            if self.fault == 'mutex-init': self.result(12)
            else: self.mutexes[x[0]] = None; self.result(0)
        elif symbol == '_pthread_cond_init':
            self.counts['cond_init'] += 1
            assert x[1] == 0 and x[0] not in self.conditions
            if self.fault == 'cond-init': self.result(12)
            else: self.conditions.add(x[0]); self.result(0)
        elif symbol == '_pthread_mutex_destroy':
            self.counts['mutex_destroy'] += 1
            assert all(t in self.joined for t in self.contexts if t != 0), 'destroy before join'
            assert x[0] in self.mutexes and self.mutexes[x[0]] is None
            assert not any((w[0] == 'lock' and w[1] == x[0]) or
                           (w[0] == 'cond' and w[2] == x[0]) for w in self.blocked.values())
            if self.fault in ('cleanup', 'mutex-destroy'): self.result(16)
            else: del self.mutexes[x[0]]; self.result(0)
        elif symbol == '_pthread_cond_destroy':
            self.counts['cond_destroy'] += 1
            assert all(t in self.joined for t in self.contexts if t != 0), 'destroy before join'
            assert x[0] in self.conditions
            assert not any(w[0] == 'cond' and w[1] == x[0] for w in self.blocked.values())
            if self.fault in ('cleanup', 'cond-destroy'): self.result(16)
            else: self.conditions.remove(x[0]); self.result(0)
        elif symbol == '_pthread_create':
            assert x[1] == x[3] == 0 and x[2], tuple(hex(v) for v in x)
            if self.fault in ('thread-create', 'cleanup'): self.result(11)
            else:
                parent = u.context_save()
                tid = self.next_thread; self.next_thread += 1
                stack = 0x710000000000 + tid * 0x400000
                u.mem_map(stack, 0x200000)
                self.stacks[tid] = stack
                for n in range(29): u.reg_write(UC_ARM64_REG_X0 + n, 0)
                for n in range(19, 29):
                    u.reg_write(UC_ARM64_REG_X0 + n, 0xfeed000000000000 + n)
                for n in range(8, 16):
                    u.reg_write(UC_ARM64_REG_Q0 + n, (0xface000000000000 + n) << 64 | (0xbeef000000000000 + n))
                u.reg_write(UC_ARM64_REG_SP, stack + 0x200000 - 16)
                u.reg_write(UC_ARM64_REG_FP, stack + 0x200000 - 256)
                u.reg_write(UC_ARM64_REG_LR, 0)
                u.reg_write(UC_ARM64_REG_PC, x[2])
                self.native_entry[tid] = self.native_registers()
                self.contexts[tid] = u.context_save()
                u.context_restore(parent)
                u.mem_write(x[0], struct.pack('<Q', tid))
                self.counts['threads'] += 1
                self.result(0)
                u.emu_stop()  # child can run before pthread_create returns
        elif symbol == '_pthread_join':
            self.counts['join_calls'] += 1
            assert self.current == 0 and x[0] in self.contexts and x[0] != 0
            assert x[0] not in self.joined and x[1] != 0
            if self.fault == 'join': self.result(22)
            elif x[0] in self.finished: self.complete_join(x[0], x[1])
            else: self.suspend(('join', x[0], x[1]))
        elif symbol == '_pthread_mutex_lock':
            assert x[0] in self.mutexes and self.mutexes[x[0]] != self.current
            self.result(0)
            if self.mutexes[x[0]] is None: self.mutexes[x[0]] = self.current
            else: self.suspend(('lock', x[0], 0))
        elif symbol == '_pthread_mutex_unlock':
            assert self.mutexes[x[0]] == self.current
            self.mutexes[x[0]] = None
            self.result(0)
        elif symbol == '_pthread_cond_signal':
            self.counts['signals'] += 1
            assert x[0] in self.conditions
            if self.fault == 'signal': self.result(22)
            else:
                for tid, wait in list(self.blocked.items()):
                    if wait[0] == 'cond' and wait[1] == x[0]:
                        self.blocked[tid] = ('lock', wait[2], 0)
                        break
                self.result(0)
        elif symbol == '_pthread_cond_timedwait_relative_np':
            self.counts['waits'] += 1
            self.counts['owner_waits'] += int(self.current == 0)
            assert x[0] in self.conditions and self.mutexes[x[1]] == self.current
            assert struct.unpack('<qq', u.mem_read(x[2], 16)) == (1, 0)
            if self.fault == 'wait': self.result(22)
            else:
                self.mutexes[x[1]] = None
                self.result(0)
                self.suspend(('cond', x[0], x[1], self.state['monotonic_ns'] + 1000000000))
        else:
            return False
        return True

    def idle_owner(self):
        if (self.current != 0 or not os.environ.get('KLASSIC_MACHO_WORKER_IDLE_WRITE')
                or hasattr(self, 'idle_before')):
            return
        self.idle_before = self.counts['decommits']
        self.idle_signals = self.counts['signals']
        self.blocked[0] = ('sleep', self.state['monotonic_ns'] + 5000000000)
        # The SVC interrupt hook already points past the syscall; retain it.
        self.uc.emu_stop()

    def block_native_read(self):
        if (not os.environ.get('KLASSIC_MACHO_REQUIRE_READ_PROGRESS')
                or self.counts['native_read_blocks']):
            return
        assert self.current == 0 and not any(v == 0 for v in self.mutexes.values())
        assert struct.unpack('<II', self.uc.mem_read(primary_owner(self.uc), 8)) == (4, 2)
        self.counts['native_read_blocks'] += 1
        self.blocked[0] = ('native-read', self.state['monotonic_ns'] + 5000000000,
                           self.counts['decommits'], self.counts['signals'],
                           self.counts['timeouts'], bytes(self.uc.mem_read(primary_owner(self.uc) + 16, 8)))
        # READ is an SVC, not a C call: keep post-SVC PC, X0 and NZCV.
        self.uc.emu_stop()

    def unblock(self):
        now = self.state['monotonic_ns']
        for tid, wait in list(self.blocked.items()):
            if wait[0] == 'native-read' and now >= wait[1]:
                assert tid == 0
                assert struct.unpack('<II', self.uc.mem_read(primary_owner(self.uc), 8)) == (4, 2)
                assert not any(v == 0 for v in self.mutexes.values())
                assert bytes(self.uc.mem_read(primary_owner(self.uc) + 16, 8)) == wait[5], 'owner adopted a phase during native read'
                assert self.counts['signals'] == wait[3], 'owner signaled during native read'
                assert self.counts['decommits'] > wait[2], 'native read made no worker progress'
                assert self.counts['timeouts'] > wait[4], 'native read did not observe a worker timeout'
                self.counts['native_read_progress'] += 1
                self.state['err'].append(('native-read-progress: before=' + str(wait[2])
                    + ' after=' + str(self.counts['decommits']) + ' no-owner-signal=1\n').encode())
                del self.blocked[tid]
            if wait[0] == 'native-sleep' and now >= wait[1]:
                assert tid == 0
                assert struct.unpack('<II', self.uc.mem_read(primary_owner(self.uc), 8)) == (4, 2)
                if os.environ.get('KLASSIC_MACHO_REQUIRE_SLEEP_PROGRESS'):
                    assert self.counts['decommits'] > wait[2], self.counts
                    assert self.counts['signals'] == wait[3], self.counts
                    self.counts['native_sleep_progress'] += 1
                del self.blocked[tid]
            if wait[0] == 'sleep' and now >= wait[1]:
                assert self.counts['decommits'] > self.idle_before, self.counts
                assert self.counts['signals'] == self.idle_signals, self.counts
                assert self.counts['timeouts'] > 0, self.counts
                self.state['err'].append(('macos-worker-idle: before='+str(self.idle_before)
                    +' after='+str(self.counts['decommits'])+' no-owner-signal=1\n').encode())
                del self.blocked[tid]
            if wait[0] == 'cond'  and now >= wait[3]:
                self.counts['timeouts'] += 1
                self.blocked[tid] = ('lock', wait[2], 60)
        for tid, wait in list(self.blocked.items()):
            if wait[0] == 'join' and wait[1] in self.finished:
                saved = self.uc.context_save()
                self.uc.context_restore(self.contexts[tid])
                self.complete_join(wait[1], wait[2])
                self.contexts[tid] = self.uc.context_save()
                self.uc.context_restore(saved)
                del self.blocked[tid]
            if wait[0] == 'lock' and self.mutexes[wait[1]] is None:
                self.mutexes[wait[1]] = tid
                saved = self.uc.context_save()
                self.uc.context_restore(self.contexts[tid])
                self.result(wait[2])
                self.contexts[tid] = self.uc.context_save()
                self.uc.context_restore(saved)
                del self.blocked[tid]

    def native_registers(self):
        u = self.uc
        return ([u.reg_read(UC_ARM64_REG_X0 + n) for n in range(19, 29)]
                + [u.reg_read(UC_ARM64_REG_FP), u.reg_read(UC_ARM64_REG_SP)]
                + [u.reg_read(UC_ARM64_REG_Q0 + n) & ((1 << 64) - 1) for n in range(8, 16)])

    def complete_join(self, tid, output):
        assert tid in self.finished and tid not in self.joined
        value = self.finished[tid] if self.fault != 'join-result' else 1
        self.uc.mem_write(output, struct.pack('<Q', value))
        self.joined.add(tid)
        self.counts['joins'] += 1
        self.uc.mem_unmap(self.stacks[tid], 0x200000)
        self.counts['stack_releases'] += 1
        self.result(0)

    def run(self, entry):
        self.uc.reg_write(UC_ARM64_REG_PC, entry)
        self.contexts[0] = self.uc.context_save()
        cursor = 0
        steps = 0
        budget = int(os.environ.get('KLASSIC_MACHO_WORKER_SECONDS', '180'))
        assert 1 <= budget <= 7200
        deadline = time.monotonic() + budget
        while self.state['exit'] is None:
            steps += 1
            progress = self.state.get('progress_callback')
            if progress is not None:
                progress()
            if time.monotonic() > deadline:
                self.state['err'].append(f'[emu] worker model exceeded {budget} seconds\n'.encode())
                self.state['exit'] = 124
                break
            if os.environ.get('KLASSIC_MACHO_WORKER_PROGRESS') and steps % 2000 == 0:
                pcs = {}
                saved = self.uc.context_save()
                for t,c in self.contexts.items():
                    self.uc.context_restore(c)
                    pcs[t] = hex(self.uc.reg_read(UC_ARM64_REG_PC))
                self.uc.context_restore(saved)
                print('worker-progress:', steps, pcs, self.counts, self.blocked, file=sys.stderr, flush=True)
            self.unblock()
            ready = sorted(t for t in self.contexts if t not in self.blocked and t not in self.finished)
            if not ready:
                deadlines = [w[3] if w[0] == 'cond' else w[1] for w in self.blocked.values() if w[0] in ('cond', 'sleep', 'native-sleep', 'native-read')]
                assert deadlines, 'modeled pthread deadlock'
                self.state['monotonic_ns'] = min(deadlines)
                continue
            tid = next((t for t in ready if t >= cursor), ready[0])
            self.current = tid
            self.uc.context_restore(self.contexts[tid])
            pc = self.uc.reg_read(UC_ARM64_REG_PC)
            self.uc.emu_start(pc, 0, 0, self.quantum)
            self.contexts[tid] = self.uc.context_save()
            if tid != 0 and self.uc.reg_read(UC_ARM64_REG_PC) == 0:
                assert tid not in self.blocked and tid not in self.finished
                assert self.native_registers() == self.native_entry[tid], 'pthread callback ABI corrupted'
                assert not any(owner == tid for owner in self.mutexes.values()), 'return holding mutex'
                self.finished[tid] = self.uc.reg_read(UC_ARM64_REG_X0)
                self.counts['returns'] += 1
            cursor = tid + 1
            self.state['monotonic_ns'] += 10000
        if self.state['exit'] and self.history:
            self.state['err'].append(('worker block history: '+str([(t, hex(pc)) for t,pc in self.history])+'\n').encode())
        if self.state['exit'] not in (1, 124):
            assert all(t in self.joined for t in self.contexts if t != 0), 'process exited with unjoined worker'
            assert not self.conditions and not self.mutexes, 'process exited with owned event'
        text = ' '.join(f'{k}={v}' for k, v in self.counts.items())
        self.state['err'].append(f'macos-worker-model: {text}\n'.encode())
