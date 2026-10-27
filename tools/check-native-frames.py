#!/usr/bin/env python3
"""Execute exported normal-call frame instructions with Unicorn 2.1.4.

Generate cases with KLASSIC_NATIVE_FRAME_FIXTURES=<dir> cargo test -p
klassic-native native_frames_execute_from_c, then pass that directory here.
Native C calls remain a separate test; this checks every saved register.
"""
import pathlib
import struct
import sys
from unicorn import Uc, UC_ARCH_ARM64, UC_ARCH_X86, UC_MODE_64, UC_MODE_ARM
from unicorn import arm64_const as arm, x86_const as x86

BASE, STOP, STACK, PROBE = 0x100000, 0x200000, 0x400000, 0x600000
RESULT, LEAF, DONE = 0x123456789abcdef0, 0x4c454146, 0x444f4e45


def execute(code, arch, entry, leaf, is_leaf):
    arm64 = arch == "arm64"
    uc = Uc(UC_ARCH_ARM64 if arm64 else UC_ARCH_X86,
            UC_MODE_ARM if arm64 else UC_MODE_64)
    uc.mem_map(BASE, (len(code) + 4095) // 4096 * 4096)
    uc.mem_write(BASE, code)
    uc.mem_map(STOP, 4096)
    uc.mem_map(STACK, 0x100000)
    uc.mem_map(PROBE, 4096)
    expected = {}
    if arm64:
        uc.reg_write(arm.UC_ARM64_REG_CPACR_EL1, 3 << 20)
        sp = STACK + 0xff000
        uc.reg_write(arm.UC_ARM64_REG_SP, sp)
        uc.reg_write(arm.UC_ARM64_REG_LR, STOP)
        for n in range(19, 30):
            expected[getattr(arm, f"UC_ARM64_REG_X{n}")] = 0x11110000 + n * 0x101
        for n in range(8, 16):
            expected[getattr(arm, f"UC_ARM64_REG_Q{n}")] = ((0xaabb0000 + n) << 64) | (0x11220000 + n)
        for reg, value in expected.items():
            uc.reg_write(reg, value)
        uc.reg_write(arm.UC_ARM64_REG_X0, PROBE)
        result_reg, stack_reg = arm.UC_ARM64_REG_X0, arm.UC_ARM64_REG_SP
    else:
        sp = STACK + 0xff000 - 8
        uc.reg_write(x86.UC_X86_REG_RSP, sp)
        uc.mem_write(sp, struct.pack("<Q", STOP))
        regs = ["RBX", "RBP", "R12", "R13", "R14", "R15"]
        if arch == "win64":
            regs += ["RDI", "RSI"]
        for n, name in enumerate(regs):
            expected[getattr(x86, f"UC_X86_REG_{name}")] = 0x11110000 + n * 0x101
        if arch == "win64":
            for n in range(6, 16):
                expected[getattr(x86, f"UC_X86_REG_XMM{n}")] = ((0xaabb0000 + n) << 64) | (0x11220000 + n)
        for reg, value in expected.items():
            uc.reg_write(reg, value)
        uc.reg_write(x86.UC_X86_REG_RCX if arch == "win64" else x86.UC_X86_REG_RDI, PROBE)
        result_reg, stack_reg = x86.UC_X86_REG_RAX, x86.UC_X86_REG_RSP
    uc.emu_start(BASE + (leaf if is_leaf else entry), STOP, count=20000)
    assert uc.reg_read(result_reg) == (LEAF if is_leaf else RESULT)
    assert uc.reg_read(stack_reg) == sp + (0 if arm64 else 8)
    for reg, value in expected.items():
        assert uc.reg_read(reg) == value, (arch, reg, hex(value), hex(uc.reg_read(reg)))
    if arm64:
        assert uc.reg_read(arm.UC_ARM64_REG_LR) == STOP
    if not is_leaf:
        argument, body_sp, address, touched = struct.unpack("<4Q", uc.mem_read(PROBE, 32))
        assert argument == PROBE and body_sp % 16 == 0
        assert address == BASE + leaf and touched == DONE


def main(directory):
    count = 0
    for line in (directory / "cases.tsv").read_text().splitlines():
        name, arch, locals_, entry, leaf = line.split("\t")
        code = (directory / f"{name}.bin").read_bytes()
        for is_leaf in [False, True]:
            try:
                execute(code, arch, int(entry), int(leaf), is_leaf)
            except Exception as exc:
                raise RuntimeError(f"{name}, leaf={is_leaf}: {exc}") from exc
        count += 1
    assert count == 24, count
    print(f"native frame instruction execution: {count} cases / {count * 2} entry-leaf calls PASS")


if __name__ == "__main__":
    main(pathlib.Path(sys.argv[1]))
