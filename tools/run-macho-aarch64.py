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

Requires `pip install unicorn`. It is a development aid, not part of the
test suite: CI still runs the real thing on real arm64 hardware, which is
the only authority. Known gaps -- syscalls outside the list below, and any
program whose answer depends on the host being macOS."""
import struct, sys, os
from unicorn import *
from unicorn.arm64_const import *

STACK_BASE = 0x0000_7000_0000_0000
STACK_SIZE = 8 * 1024 * 1024
MMAP_BASE  = 0x0000_2000_0000_0000

SYS_EXIT, SYS_READ, SYS_WRITE, SYS_OPEN, SYS_CLOSE = 1, 3, 4, 5, 6
SYS_UNLINK, SYS_GETPID, SYS_GETTIMEOFDAY = 10, 20, 116
SYS_MMAP, SYS_MUNMAP, SYS_ACCESS, SYS_RENAME = 197, 73, 33, 128
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
    segs, entry = [], None
    off = 32
    for _ in range(ncmds):
        cmd, cmdsize = struct.unpack_from('<2I', d, off)
        if cmd == 0x19:  # LC_SEGMENT_64
            name = d[off+8:off+24].rstrip(b'\0').decode()
            vmaddr, vmsize, fileoff, filesize = struct.unpack_from('<4Q', d, off+24)
            segs.append((name, vmaddr, vmsize, fileoff, filesize))
        elif cmd == 0x80000028:  # LC_MAIN
            entry = struct.unpack_from('<Q', d, off+8)[0]
        off += cmdsize
    return d, segs, entry

def run(path, argv=None, trace=False):
    d, segs, entryoff = load(path)
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

    def svc(uc, intno, user):
        num = uc.reg_read(UC_ARM64_REG_X16)
        nzcv = uc.reg_read(UC_ARM64_REG_NZCV)
        def ok(value):
            uc.reg_write(UC_ARM64_REG_X0, value & 0xffffffffffffffff)
            uc.reg_write(UC_ARM64_REG_NZCV, nzcv & ~(1 << 29))   # carry clear
        def fail(errno):
            uc.reg_write(UC_ARM64_REG_X0, errno)
            uc.reg_write(UC_ARM64_REG_NZCV, nzcv | (1 << 29))    # carry set
        if num == SYS_EXIT:
            state['exit'] = uc.reg_read(UC_ARM64_REG_X0) & 0xff
            uc.emu_stop()
        elif num == SYS_WRITE:
            fd = uc.reg_read(UC_ARM64_REG_X0)
            buf = uc.reg_read(UC_ARM64_REG_X1)
            n = uc.reg_read(UC_ARM64_REG_X2)
            data = bytes(uc.mem_read(buf, n)) if n else b''
            if fd in (1, 2):
                (state['out'] if fd == 1 else state['err']).append(data)
                ok(n)
            else:
                try:
                    ok(os.write(fd, data))
                except OSError as e:
                    fail(e.errno)
        elif num == SYS_MMAP:
            length = uc.reg_read(UC_ARM64_REG_X1)
            length = (length + 0xffff) & ~0xffff
            addr = state['brk']
            uc.mem_map(addr, length)
            state['brk'] = addr + length
            ok(addr)
        elif num == SYS_MUNMAP:
            ok(0)
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
                chunk = os.read(fd, n)
                if chunk:
                    uc.mem_write(buf, chunk)
                ok(len(chunk))
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
        uc.emu_start(text_base + entryoff, 0, 0, 0)
    except UcError as e:
        pc = uc.reg_read(UC_ARM64_REG_PC)
        state['err'].append(f'[emu] {e} at pc={pc:#x}\n'.encode())
        state['exit'] = 139
    sys.stdout.buffer.write(b''.join(state['out']))
    sys.stdout.buffer.flush()
    sys.stderr.buffer.write(b''.join(state['err']))
    sys.stderr.buffer.flush()
    return state['exit'] if state['exit'] is not None else 0

if __name__ == '__main__':
    args = [a for a in sys.argv[1:] if a != '--trace']
    sys.exit(run(args[0], [a.encode() for a in args] or None, '--trace' in sys.argv))
