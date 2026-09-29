#!/usr/bin/env python3
"""Rebuild the conservative native-GC access inventory (no third-party modules).

This is an audit index, not a proof of barrier coverage. It deliberately includes
stack/static accesses and all output calls: classifying those as heap accesses
without following value provenance would hide the actual review obligation.
"""
import argparse
import hashlib
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
# Discover the entire native/runtime source tree, including inline and standalone
# test fixtures. An allowlist silently lost new production helpers during the
# migration. Extra candidates are intentional; provenance review decides which
# accesses belong to generated programs, runtime metadata or test-only owners.
def source_files():
    roots = (
        ('crates/klassic-native/src', {'.rs'}),
        ('crates/klassic-runtime/src', {'.rs'}),
        ('runtime/gc', {'.c', '.h'}),
    )
    return sorted(str(path.relative_to(ROOT))
                  for directory, suffixes in roots
                  for path in (ROOT / directory).rglob('*')
                  if path.is_file() and path.suffix in suffixes)


METHOD = re.compile(r'\.([A-Za-z_][A-Za-z_0-9]*)\s*\(')
FUNCTION = re.compile(r'\bfn\s+(\w+)|^(?:static\s+)?(?:inline\s+)?(?:void|int|uint\w*|MutatorContext|Mutator)\s*\**\s*(\w+)\s*\(')


def categories(line, is_c):
    result = set()
    for method in METHOD.findall(line):
        if method.startswith(('load_', 'ldr', 'ldur', 'movzx_byte', 'ldp')):
            result.add('load')
        if method.startswith(('store_', 'str_', 'strb', 'stur', 'mov_byte_', 'stp')):
            result.add('store')
        if method.startswith('atomic_') or method.startswith('lock_cmpxchg'):
            result.add('atomic-memory')
            if method in ('atomic_load_acquire', 'atomic_load_acquire_u32', 'atomic_compare_exchange', 'atomic_compare_exchange_u32', 'atomic_fetch_or', 'lock_cmpxchg', 'lock_cmpxchg_u32'):
                result.add('load')
            if method in ('atomic_store_release', 'atomic_store_release_u32', 'atomic_compare_exchange', 'atomic_compare_exchange_u32', 'atomic_fetch_or', 'lock_cmpxchg', 'lock_cmpxchg_u32'):
                result.add('store')
        if method in ('rep_movsb', 'emit_cstring_copy'):
            result.update(('load', 'store'))
        if method in ('call_label', 'call_reg', 'call_import', 'bl', 'blr', 'branch'):
            result.add('call')
        if method in ('bytes', 'byte', 'word', 'emit32', 'insn'):
            result.add('raw-instruction')
        if method.startswith(('data_label_', 'reserve_data_', 'intern_')):
            result.add('static-data')
        if method in ('syscall', 'svc_0x80'):
            result.add('blocking-boundary')
    if re.search(r'Box::leak|into_boxed_|kstr_from_owned|gc_alloc|alloc_object|alloc_raw_string|box_scalar|klassic_rt_.*(?:concat|substring|_str)|\b(?:malloc|calloc|realloc|mmap|free|munmap)\(', line):
        result.add('allocation')
    if re.search(r'shadow_|root_|rooted|push_frame|pop_frame|allocate_.*slot|pop_scope', line):
        result.add('root')
    if re.search(r'barrier|gc_read|gc_write|gc_load|colour_pointer', line):
        result.add('barrier')
    if re.search(r'poll|safepoint|handshake|register_thread|unregister_thread', line):
        result.add('safepoint')
    if re.search(r'print!|println!|pthread_|plat_write|plat_exit|plat_mmap|call_import|klassic_rt_print|\b(?:read|write|fread|fwrite|sleep|nanosleep)\(', line):
        result.add('blocking-boundary')
    if re.search(r'(?:load|store) (?:atomic |ptr |i\d+)', line):
        result.add('llvm-memory')
    if is_c and re.search(r'\*|\[|->|__atomic_|memcpy|memset', line):
        result.add('c-memory')
    if re.search(r'from_raw_parts|copy_nonoverlapping|write_all|stdout\.lock', line):
        result.add('native-resource')
    if re.search(r'\bb\.(?:set|branch|pointer|value)\(', line):
        result.add('emitted-metadata-expression')
    # Keep every emitted LLVM/C statement indexed; helper calls may allocate
    # or block even without an explicit heap instruction in the emitter.
    if re.search(r'\.emit\(|\.line\(|klassic_rt_', line):
        result.add('emitted-statement')
    return sorted(result)


def inventory():
    records = {}
    for relative in source_files():
        data = (ROOT / relative).read_text()
        owner = '<module>'
        sites = []
        for number, raw in enumerate(data.splitlines(), 1):
            line = raw.strip()
            if not line or line.startswith(('//', '/*', '* ', '*/', '#include')):
                continue
            declaration = FUNCTION.search(line)
            if declaration:
                owner = next(group for group in declaration.groups() if group)
                # Single-line helpers can access memory on the declaration line.
            kinds = categories(line, relative.endswith(('.c', '.h')))
            if kinds:
                sites.append({'line': number, 'function': owner, 'kinds': kinds, 'source': line})
        functions = {}
        for site in sites:
            by_kind = functions.setdefault(site['function'], {})
            for kind in site['kinds']:
                by_kind.setdefault(kind, []).append(site['line'])
        records[relative] = {'sha256': hashlib.sha256(data.encode()).hexdigest(),
                             'site_count': len(sites), 'functions': functions}
    return json.dumps({'schema': 1, 'scope': 'conservative candidate sites; manual provenance review required',
                       'files': records}, indent=2) + '\n'


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    target = ROOT / 'docs/gc/access-inventory.json'
    rendered = inventory()
    if args.check:
        if not target.exists() or target.read_text() != rendered:
            raise SystemExit('GC access inventory is stale; run tools/gc_access_inventory.py')
    else:
        target.write_text(rendered)
    parsed = json.loads(rendered)
    print(f"GC inventory: {sum(f['site_count'] for f in parsed['files'].values())} candidate sites across {len(parsed['files'])} files")
