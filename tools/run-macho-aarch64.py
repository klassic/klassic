#!/usr/bin/env python3
"""Run an aarch64 Mach-O built by klassic on a machine that is not one.

    klassic --target aarch64-apple-darwin build prog.kl -o prog.macho
    python3 tools/run-macho-aarch64.py prog.macho

Development happens on Linux x86-64, where the aarch64 backend can be built
but not executed, so until now its only real test was a macOS CI runner --
every semantic bug in it (a missing barrier, a wrong root, a decimal
formatter that disagrees with the evaluator) was invisible until a push.
This maps the Mach-O's segments into a unicorn ARM64 instance and emulates
the small set of Darwin syscalls the backend emits, which is enough to run
the programs under tests/cross-exec/ and diff them against the evaluator
locally.

Requires `pip install unicorn==2.1.4`. Directed cross-target tests use this
API model; CI installs the tested interpreter explicitly. CI also runs the
real thing on arm64 hardware, which remains the native-execution authority. Known gaps -- syscalls outside the list below, and any
program whose answer depends on the host being macOS."""
import struct, sys, os, json, time
from unicorn import *
from unicorn.arm64_const import *
from macho_worker_model import primary_owner

STACK_BASE = 0x0000_7000_0000_0000
STACK_SIZE = 8 * 1024 * 1024
MMAP_BASE  = 0x0000_2000_0000_0000

SYS_EXIT, SYS_READ, SYS_WRITE, SYS_OPEN, SYS_CLOSE = 1, 3, 4, 5, 6
SYS_UNLINK, SYS_GETPID, SYS_GETTIMEOFDAY = 10, 20, 116
SYS_MMAP, SYS_MUNMAP, SYS_ACCESS, SYS_RENAME = 197, 73, 33, 128
SYS_MPROTECT, SYS_MADVISE = 74, 75
SYS_MKDIR, SYS_RMDIR, SYS_GETCWD, SYS_FSTATAT64 = 136, 137, 326, 470

# Darwin's open(2) flag values, which are not Linux's.
DARWIN_O = {0x0001: os.O_WRONLY, 0x0002: os.O_RDWR, 0x0008: os.O_APPEND,
            0x0200: os.O_CREAT, 0x0400: os.O_TRUNC, 0x0800: os.O_EXCL}
ENVIRON = [b'PATH=/usr/bin:/bin', b'HOME=/emu', b'TMPDIR=/tmp']
# Any instant comfortably past the constants the fixtures compare against.
NOW_SECONDS = 1_750_000_000

def cstring(uc, addr):
    out = bytearray()
    while True:
        byte = uc.mem_read(addr + len(out), 1)[0]
        if byte == 0:
            return bytes(out).decode('utf-8', 'surrogateescape')
        out.append(byte)


def load(path):
    d = open(path, 'rb').read()
    ncmds = struct.unpack_from('<I', d, 16)[0]
    segs, entry, bindings = [], None, b''
    off = 32
    for _ in range(ncmds):
        cmd, cmdsize = struct.unpack_from('<2I', d, off)
        if cmd == 0x19:  # LC_SEGMENT_64
            name = d[off+8:off+24].rstrip(b'\0').decode()
            vmaddr, vmsize, fileoff, filesize = struct.unpack_from('<4Q', d, off+24)
            segs.append((name, vmaddr, vmsize, fileoff, filesize))
        elif cmd == 0x80000022:  # LC_DYLD_INFO_ONLY bind opcodes
            start, size = struct.unpack_from('<2I', d, off+16)
            bindings = d[start:start+size]
        elif cmd == 0x80000028:  # LC_MAIN
            entry = struct.unpack_from('<Q', d, off+8)[0]
        off += cmdsize
    return d, segs, entry, bindings

def run(path, argv=None, trace=False):
    d, segs, entryoff, bindings = load(path)
    uc = Uc(UC_ARCH_ARM64, UC_MODE_ARM)
    text_base = None
    for name, vmaddr, vmsize, fileoff, filesize in segs:
        if name == '__PAGEZERO' or vmsize == 0:
            continue
        size = (vmsize + 0xfff) & ~0xfff
        uc.mem_map(vmaddr, size)
        if filesize:
            uc.mem_write(vmaddr, d[fileoff:fileoff+filesize])
        if name == '__TEXT':
            text_base = vmaddr
    uc.mem_map(STACK_BASE, STACK_SIZE)

    state = {'brk': MMAP_BASE, 'exit': None, 'out': [], 'err': []}

    # Optional read-only progress snapshots. Offsets come from the exact
    # compiled runtime's diagnostic layout, never from inferred GC state.
    progress_file = os.environ.get('KLASSIC_MACHO_PROGRESS_FILE')
    layout_file = os.environ.get('KLASSIC_MACHO_PROGRESS_LAYOUT')
    if progress_file and layout_file:
        with open(layout_file) as source:
            progress_layout = json.load(source)
        next_progress = [0.0]
        def progress_snapshot():
            now = time.monotonic()
            if now < next_progress[0]:
                return
            next_progress[0] = now + 10
            record = {'wall_monotonic': now}
            try:
                owner = primary_owner(uc)
                for name, offset in progress_layout.items():
                    base = owner if offset & (1 << 63) else MMAP_BASE
                    record[name] = struct.unpack('<Q', uc.mem_read(base + (offset & ((1 << 63)-1)), 8))[0]
                record['owner_state'] = struct.unpack('<I', uc.mem_read(owner + 4, 4))[0]
                registration = struct.unpack('<Q', uc.mem_read(owner + 128, 8))[0]
                segment = struct.unpack('<Q', uc.mem_read(registration, 8))[0]
                record['root_cells_used'] = struct.unpack('<Q', uc.mem_read(segment + 32, 8))[0]
            except Exception as error:
                record['not_initialized'] = type(error).__name__
            with open(progress_file, 'a') as output:
                output.write(json.dumps(record) + '\n')
        state['progress_callback'] = progress_snapshot

    # Resolve the emitted eager pointer bindings exactly as dyld would. This
    # host model implements the monotonic clock import only; other called
    # imports fail explicitly rather than pretending to execute native APIs.
    stubs = 0x600000000
    uc.mem_map(stubs, 0x1000)
    uc.mem_write(stubs, struct.pack('<I', 0xd65f03c0) * 1024)
    apis = {}
    pos, symbol, segment, offset = 0, None, None, 0
    while pos < len(bindings):
        op = bindings[pos]
        pos += 1
        kind, imm = op & 0xf0, op & 15
        if op == 0:
            break
        if kind in (0x10, 0x50):
            assert imm == 1, f'unsupported bind ordinal/type {op:x}'
        elif kind == 0x40:
            end = bindings.index(0, pos)
            symbol = bindings[pos:end].decode('ascii')
            pos = end + 1
        elif kind == 0x70:
            segment, offset, shift = imm, 0, 0
            while True:
                byte = bindings[pos]
                pos += 1
                offset |= (byte & 127) << shift
                shift += 7
                if byte < 128:
                    break
        elif op == 0x90:
            assert symbol and segment is not None and segment < len(segs)
            address = stubs + 4 * len(apis)
            assert address < stubs + 0x1000
            apis[address] = symbol
            uc.mem_write(segs[segment][1] + offset, struct.pack('<Q', address))
            offset += 8
        else:
            raise ValueError(f'unsupported dyld binding opcode {op:x}')
    root_layout = json.loads(os.environ['KLASSIC_MACHO_ROOTCELL_LAYOUT']) if os.environ.get('KLASSIC_MACHO_ROOTCELL_LAYOUT') else None
    state['rootcell_samples'] = []
    def sample_rootcells(data, expected_state=2):
        ctx = primary_owner(uc)
        assert struct.unpack('<II', uc.mem_read(ctx, 8)) == (4, expected_state), 'root observation in unexpected owner state'
        read = lambda address: struct.unpack('<Q', uc.mem_read(address, 8))[0]
        registration = read(ctx + root_layout['context_roots'])
        seen, used_total, live_total, frame_live = set(), 0, 0, 0
        while registration:
            assert registration not in seen, 'cyclic owner root inventory'
            seen.add(registration)
            assert read(registration + root_layout['registration_context']) == ctx
            segment = read(registration + root_layout['registration_segment'])
            cells = read(segment + root_layout['segment_cells'])
            used = read(segment + root_layout['segment_used'])
            assert used <= read(segment + root_layout['segment_capacity'])
            for index in range(used):
                cell = cells + index * root_layout['cell_bytes']
                value = read(cell + root_layout['cell_value'])
                if value:
                    assert read(cell + root_layout['cell_active']) == 1, 'inactive live frame cell'
                    assert value >> 16 and (value >> 16) % 16 == 0, 'non-reference in precise frame root'
                    live_total += 1
                    frame_live += index != 0
            used_total += used
            registration = read(registration + root_layout['registration_next'])
        state['rootcell_samples'].append({'data': data.hex(), 'used': used_total, 'live': live_total, 'frame_live': frame_live})
    require_copyout = bool(os.environ.get('KLASSIC_MACHO_REQUIRE_NATIVE_COPYOUT'))
    require_console = bool(os.environ.get('KLASSIC_MACHO_REQUIRE_NATIVE_CONSOLE'))
    state['console_writes'] = 0
    state['console_diagnostic'] = False
    require_file_io = bool(os.environ.get('KLASSIC_MACHO_REQUIRE_NATIVE_FILE_IO'))
    require_filesystem = bool(os.environ.get('KLASSIC_MACHO_REQUIRE_NATIVE_FILESYSTEM'))
    state['native_filesystem_calls'] = 0
    state['native_file_calls'] = 0
    state['reserved_ranges'] = []
    state['file_buffers'] = set()
    state['copyout_writes'] = 0
    state['initial_mapping_end'] = 0
    state['monotonic_ns'] = 1_000_000_000
    workers = None
    if '_pthread_create' in apis.values():
        from macho_worker_model import WorkerModel
        workers = WorkerModel(uc, state)
        for name, address, _, fileoff, filesize in segs:
            if name == '__TEXT':
                workers.install_yields(address + offset for offset in range(0, filesize - 3, 4)
                    if d[fileoff + offset:fileoff + offset + 4] == b'\x3f\x20\x03\xd5')

    def imported(uc, address, size, user):
        symbol = apis.get(address)
        if symbol is None:
            return
        if workers is not None and workers.call(symbol):
            uc.reg_write(UC_ARM64_REG_PC, uc.reg_read(UC_ARM64_REG_LR))
            return
        if symbol == '_clock_gettime':
            clock = uc.reg_read(UC_ARM64_REG_X0)
            target = uc.reg_read(UC_ARM64_REG_X1)
            if clock != 6:  # Darwin CLOCK_MONOTONIC
                state['err'].append(b'[emu] unexpected monotonic clock id\n')
                state['exit'] = 127
                uc.emu_stop()
                return
            ns = state['monotonic_ns']
            state['monotonic_ns'] += 1000
            uc.mem_write(target, struct.pack('<qq', ns // 1_000_000_000,
                                             ns % 1_000_000_000))
            uc.reg_write(UC_ARM64_REG_X0, 0)  # libc int status
        else:
            state['err'].append(f'[emu] unhandled import {symbol}\n'.encode())
            state['exit'] = 127
            uc.emu_stop()

    uc.hook_add(UC_HOOK_CODE, imported, begin=stubs, end=stubs + 0xfff)

    def svc(uc, intno, user):
        num = uc.reg_read(UC_ARM64_REG_X16)
        nzcv = uc.reg_read(UC_ARM64_REG_NZCV)
        def ok(value):
            uc.reg_write(UC_ARM64_REG_X0, value & 0xffffffffffffffff)
            uc.reg_write(UC_ARM64_REG_NZCV, nzcv & ~(1 << 29))   # carry clear
        def fail(errno):
            uc.reg_write(UC_ARM64_REG_X0, errno)
            uc.reg_write(UC_ARM64_REG_NZCV, nzcv | (1 << 29))    # carry set
        def native_pointer(buf, n):
            assert not any(lo <= buf < hi or n and buf < lo < buf + n
                           for lo, hi in state['reserved_ranges']), 'managed filesystem operand'
        fs_operations = {SYS_ACCESS: 'access', SYS_UNLINK: 'unlink', SYS_MKDIR: 'mkdir',
                         SYS_RMDIR: 'rmdir', SYS_RENAME: 'rename', SYS_FSTATAT64: 'stat',
                         SYS_GETCWD: 'getcwd'}
        if require_filesystem and num in fs_operations:
            assert struct.unpack('<II', uc.mem_read(primary_owner(uc), 8)) == (4, 2), 'filesystem outside BlockedNative'
            assert workers is not None and workers.current == 0
            assert not any(v == 0 for v in workers.mutexes.values()), 'filesystem holds GC mutex'
            state['native_filesystem_calls'] += 1
            if num == SYS_GETCWD:
                native_pointer(uc.reg_read(UC_ARM64_REG_X0), uc.reg_read(UC_ARM64_REG_X1))
            else:
                registers = [UC_ARM64_REG_X1] if num == SYS_FSTATAT64 else [UC_ARM64_REG_X0]
                if num == SYS_RENAME:
                    registers.append(UC_ARM64_REG_X1)
                for register in registers:
                    ptr = uc.reg_read(register)
                    native_pointer(ptr, len(cstring(uc, ptr).encode()) + 1)
                if num == SYS_FSTATAT64:
                    native_pointer(uc.reg_read(UC_ARM64_REG_X2), 144)
            if os.environ.get('KLASSIC_MACHO_FS_FAULT') == fs_operations[num]:
                fail(5)
                return
        file_call = (num in (SYS_OPEN, SYS_READ, SYS_CLOSE)
                     or num == SYS_WRITE and uc.reg_read(UC_ARM64_REG_X0) not in (1, 2))
        if require_file_io and file_call:
            assert struct.unpack('<II', uc.mem_read(primary_owner(uc), 8)) == (4, 2), 'file I/O outside BlockedNative'
            assert workers is not None and workers.current == 0
            assert not any(v == 0 for v in workers.mutexes.values()), 'native file call holds GC mutex'
            state['native_file_calls'] += 1
            if num == SYS_OPEN:
                ptr = uc.reg_read(UC_ARM64_REG_X0)
                native_pointer(ptr, len(cstring(uc, ptr).encode()) + 1)
            operation = {SYS_OPEN: 'open', SYS_READ: 'read', SYS_WRITE: 'write', SYS_CLOSE: 'close'}[num]
            if os.environ.get('KLASSIC_MACHO_FILE_FAULT') == operation:
                fail(5)
                return

            if num in (SYS_READ, SYS_WRITE):
                buf = uc.reg_read(UC_ARM64_REG_X1)
                n = uc.reg_read(UC_ARM64_REG_X2)
                assert not any(lo <= buf < hi or n and buf < lo < buf + n
                               for lo, hi in state['reserved_ranges']), 'managed file buffer'
                if MMAP_BASE <= buf < STACK_BASE:
                    state['file_buffers'].add(buf)
        if num == SYS_EXIT:
            state['exit'] = uc.reg_read(UC_ARM64_REG_X0) & 0xff
            if root_layout and state['exit'] == 0:
                sample_rootcells(b'<exit>', 0)
            uc.emu_stop()
        elif num == SYS_WRITE:
            fd = uc.reg_read(UC_ARM64_REG_X0)
            buf = uc.reg_read(UC_ARM64_REG_X1)
            n = uc.reg_read(UC_ARM64_REG_X2)
            if require_copyout and n and MMAP_BASE <= buf < STACK_BASE:
                # Directed generational fixture: its first mmap is Runtime.
                # Every dynamic stdout/stderr string must use native storage
                # in that mapping, with the owner published BlockedNative.
                abi, owner_state = struct.unpack('<II', uc.mem_read(primary_owner(uc), 8))
                if not (abi == 4 and owner_state == 2 and n <= 4096
                        and buf + n <= state['initial_mapping_end']):
                    state['err'].append(b'[emu] invalid native copyout boundary\n')
                    state['exit'] = 95
                    uc.emu_stop()
                    return
                state['copyout_writes'] += 1
            data = bytes(uc.mem_read(buf, n)) if n else b''
            if root_layout and fd == 1:
                sample_rootcells(data)
            if require_console and fd in (1, 2):
                if fd == 2 and data.startswith(b'gc[generational]:'):
                    state['console_diagnostic'] = True
                if not state['console_diagnostic']:
                    abi, owner_state = struct.unpack('<II', uc.mem_read(primary_owner(uc), 8))
                    assert abi == 4 and owner_state == 2, 'console write outside Native'
                    assert not any(buf < hi and buf + n > lo for lo, hi in state['reserved_ranges']), 'managed console buffer'
                    state['console_writes'] += 1
            if fd in (1, 2):
                (state['out'] if fd == 1 else state['err']).append(data)
                ok(n)
                if fd == 1 and workers is not None:
                    workers.idle_owner()
            else:
                try:
                    ok(os.write(fd, data))
                except OSError as e:
                    fail(e.errno)
        elif num == SYS_MMAP:
            length = uc.reg_read(UC_ARM64_REG_X1)
            prot = uc.reg_read(UC_ARM64_REG_X2)
            if not length or prot & ~7:
                fail(22)
            else:
                length = (length + 0x3fff) & ~0x3fff
                addr = state['brk']
                try:
                    uc.mem_map(addr, length, prot)
                    state['brk'] = addr + length
                    if prot == 0:
                        state['reserved_ranges'].append((addr, addr + length))
                    if addr == MMAP_BASE:
                        state['initial_mapping_end'] = addr + length
                    ok(addr)
                except UcError:
                    fail(12)
        elif num == SYS_MPROTECT:
            addr = uc.reg_read(UC_ARM64_REG_X0)
            length = (uc.reg_read(UC_ARM64_REG_X1) + 0x3fff) & ~0x3fff
            prot = uc.reg_read(UC_ARM64_REG_X2)
            if addr & 0x3fff or prot & ~7:
                fail(22)
            else:
                try:
                    if length:
                        uc.mem_protect(addr, length, prot)
                        if workers is not None and prot == 0:
                            workers.counts['decommits'] += 1
                    ok(0)
                except UcError:
                    fail(12)
        elif num == SYS_MADVISE:
            addr = uc.reg_read(UC_ARM64_REG_X0)
            length = uc.reg_read(UC_ARM64_REG_X1)
            advice = uc.reg_read(UC_ARM64_REG_X2)
            if addr & 0x3fff or advice != 5:
                fail(22)
            else:
                try:
                    if length:
                        uc.mem_read(addr, 1)
                        uc.mem_read(addr + length - 1, 1)
                    # MADV_FREE can retain bytes. Page cleanup, rather than
                    # this emulator, must make a later reuse safe.
                    ok(0)
                except UcError:
                    fail(12)
        elif num == SYS_MUNMAP:
            addr = uc.reg_read(UC_ARM64_REG_X0)
            length = (uc.reg_read(UC_ARM64_REG_X1) + 0x3fff) & ~0x3fff
            if addr & 0x3fff or not length:
                fail(22)
            elif os.environ.get('KLASSIC_MACHO_METADATA_RELEASE_FAULT') == '1':
                fail(5)
            else:
                try:
                    uc.mem_read(addr, 1)
                    uc.mem_read(addr + length - 1, 1)
                    uc.mem_unmap(addr, length)
                    state['file_buffers'] = {b for b in state['file_buffers'] if not addr <= b < addr + length}
                    ok(0)
                except UcError:
                    fail(22)
        elif num == SYS_GETPID:
            ok(4242)
        elif num == SYS_GETTIMEOFDAY:
            tv = uc.reg_read(UC_ARM64_REG_X0)
            if tv:
                uc.mem_write(tv, struct.pack('<qq', NOW_SECONDS, 0))
            ok(0)
        elif num == SYS_GETCWD:
            buf = uc.reg_read(UC_ARM64_REG_X0)
            cwd = os.getcwd().encode() + b'\0'
            uc.mem_write(buf, cwd)
            ok(len(cwd))
        elif num == SYS_OPEN:
            path = cstring(uc, uc.reg_read(UC_ARM64_REG_X0))
            raw = uc.reg_read(UC_ARM64_REG_X1)
            flags = os.O_RDONLY
            for bit, mapped in DARWIN_O.items():
                if raw & bit:
                    flags |= mapped
            try:
                ok(os.open(path, flags, 0o644))
            except OSError as e:
                fail(e.errno)
        elif num == SYS_CLOSE:
            try:
                os.close(uc.reg_read(UC_ARM64_REG_X0))
                ok(0)
            except OSError as e:
                fail(e.errno)
        elif num == SYS_READ:
            fd = uc.reg_read(UC_ARM64_REG_X0)
            buf = uc.reg_read(UC_ARM64_REG_X1)
            n = uc.reg_read(UC_ARM64_REG_X2)
            try:
                chunk = os.read(fd, min(n, int(os.environ.get("KLASSIC_MACHO_STDIN_CHUNK", n))) if fd == 0 else n)
                if chunk:
                    uc.mem_write(buf, chunk)
                ok(len(chunk))
                if workers is not None:
                    workers.block_native_read()
            except OSError as e:
                fail(e.errno)
        elif num in (SYS_UNLINK, SYS_RMDIR, SYS_MKDIR, SYS_ACCESS):
            path = cstring(uc, uc.reg_read(UC_ARM64_REG_X0))
            try:
                if num == SYS_UNLINK:
                    os.unlink(path)
                elif num == SYS_RMDIR:
                    os.rmdir(path)
                elif num == SYS_MKDIR:
                    os.mkdir(path, 0o755)
                else:
                    os.stat(path)
                ok(0)
            except OSError as e:
                fail(e.errno)
        elif num == SYS_RENAME:
            src = cstring(uc, uc.reg_read(UC_ARM64_REG_X0))
            dst = cstring(uc, uc.reg_read(UC_ARM64_REG_X1))
            try:
                os.rename(src, dst)
                ok(0)
            except OSError as e:
                fail(e.errno)
        elif num == SYS_FSTATAT64:
            path = cstring(uc, uc.reg_read(UC_ARM64_REG_X1))
            buf = uc.reg_read(UC_ARM64_REG_X2)
            try:
                st = os.stat(path)
            except OSError as e:
                fail(e.errno)
            else:
                # Only st_mode is read back, as a halfword at offset 4.
                uc.mem_write(buf, b'\0' * 144)
                uc.mem_write(buf + 4, struct.pack('<H', st.st_mode & 0xffff))
                ok(0)
        else:
            state['err'].append(f'[emu] unhandled syscall {num}\n'.encode())
            fail(45)

    uc.hook_add(UC_HOOK_INTR, svc)
    if trace:
        def code(uc, address, size, user):
            print(f'{address:#x}', file=sys.stderr)
        uc.hook_add(UC_HOOK_CODE, code)

    # Darwin LC_MAIN entry: x0=argc, x1=argv, x2=envp, x3=apple, with the
    # three vectors laid out end to end, each NULL-terminated.
    argv = argv or [b'/prog']
    sp = STACK_BASE + STACK_SIZE - 0x1000
    def push_string(text):
        nonlocal sp
        sp -= len(text) + 1
        uc.mem_write(sp, text + b'\0')
        return sp
    argv_ptrs = [push_string(a) for a in argv]
    env_ptrs = [push_string(e) for e in ENVIRON]
    apple_ptrs = [push_string(argv[0])]
    sp &= ~0xf
    vector = (argv_ptrs + [0]) + (env_ptrs + [0]) + (apple_ptrs + [0])
    sp -= 8 * len(vector)
    sp &= ~0xf
    uc.mem_write(sp, b''.join(struct.pack('<Q', p) for p in vector))
    argv_addr = sp
    env_addr = argv_addr + 8 * (len(argv_ptrs) + 1)
    apple_addr = env_addr + 8 * (len(env_ptrs) + 1)
    sp = (sp - 0x100) & ~0xf
    uc.reg_write(UC_ARM64_REG_SP, sp)
    uc.reg_write(UC_ARM64_REG_X0, len(argv_ptrs))
    uc.reg_write(UC_ARM64_REG_X1, argv_addr)
    uc.reg_write(UC_ARM64_REG_X2, env_addr)
    uc.reg_write(UC_ARM64_REG_X3, apple_addr)

    try:
        if workers is None:
            uc.emu_start(text_base + entryoff, 0, 0, 0)
        else:
            workers.run(text_base + entryoff)
    except AssertionError:
        # A directed boundary rejection deliberately stops before worker join;
        # preserve its diagnostic instead of masking it with lifecycle checks.
        if not (require_copyout and state['exit'] == 95):
            raise
    except UcError as e:
        pc = uc.reg_read(UC_ARM64_REG_PC)
        state['err'].append(f'[emu] {e} at pc={pc:#x}\n'.encode())
        if workers is not None:
            registers = [hex(uc.reg_read(UC_ARM64_REG_X0+i)) for i in range(13)]
            state['err'].append(('worker-error: tid='+str(workers.current)+' regs='+str(registers)
                +' history='+str([(t, hex(p)) for t,p in workers.history])+'\n').encode())
        state['exit'] = 139
    if root_layout and state['exit'] == 0:
        assert state['rootcell_samples'], 'no frame root observations'
        for sample in state['rootcell_samples']:
            state['err'].append(('frame-root-model: data=' + sample['data'] + ' used=' + str(sample['used']) + ' live=' + str(sample['live']) + ' frame_live=' + str(sample['frame_live']) + '\n').encode())
    if require_copyout and state['exit'] == 0:
        owner_state = struct.unpack('<I', uc.mem_read(primary_owner(uc) + 4, 4))[0]
        if not state['copyout_writes'] or owner_state != 0:
            state['err'].append(b'[emu] unbalanced native copyout boundary\n')
            state['exit'] = 95
        else:
            state['err'].append(('native-copyout-model: writes=' + str(state['copyout_writes']) + '\n').encode())
    if require_file_io or require_filesystem:
        assert struct.unpack('<II', uc.mem_read(primary_owner(uc), 8)) == (4, 0), 'file I/O did not restore Running'
    if require_console and state['exit'] == 0:
        assert state['console_writes'] > 0
        assert struct.unpack('<II', uc.mem_read(primary_owner(uc), 8)) == (4, 0)
        state['err'].append(('native-console-model: writes=' + str(state['console_writes']) + '\n').encode())
    if os.environ.get('KLASSIC_MACHO_REQUIRE_READ_PROGRESS') and state['exit'] == 0:
        assert workers is not None and workers.counts['native_read_blocks'] == workers.counts['native_read_progress'] == 1
    if require_filesystem and state['exit'] == 0:
        assert state['native_filesystem_calls'] > 0
        state['err'].append(('native-filesystem-model: calls=' + str(state['native_filesystem_calls']) + '\n').encode())
    if require_file_io and state['exit'] == 0:
        assert state['native_file_calls'] > 0 and not state['file_buffers'], 'native file buffers retained'
        state['err'].append(('native-file-model: calls=' + str(state['native_file_calls']) + '\n').encode())
    sys.stdout.buffer.write(b''.join(state['out']))
    sys.stdout.buffer.flush()
    sys.stderr.buffer.write(b''.join(state['err']))
    sys.stderr.buffer.flush()
    return state['exit'] if state['exit'] is not None else 0

if __name__ == '__main__':
    args = [a for a in sys.argv[1:] if a != '--trace']
    sys.exit(run(args[0], [a.encode() for a in args] or None, '--trace' in sys.argv))
