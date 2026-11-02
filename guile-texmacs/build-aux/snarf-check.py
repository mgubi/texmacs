#!/usr/bin/env python3
# snarfcheck.py <libguile dir> [--fix]: the SCM_* definitions of libguile
# whose registration (the SCM_SNARF_INIT part of the macro, which guile-snarf
# used to put in the .x files) is missing from the file; with --fix, the
# registrations are added at the end of the block of pasted registrations,
# under the preprocessor conditions of their definitions.
import re, sys, os

MACROS = {
 # name: (args, init template, registered key)
 'SCM_DEFINE': (['FNAME','PRIMNAME','REQ','OPT','VAR','ARGLIST','DOC'],
   'scm_c_define_gsubr (s_{FNAME}, {REQ}, {OPT}, {VAR}, (SCM (*)()) {FNAME}); ;', r'\(s_{FNAME}\s*,'),
 'SCM_PRIMITIVE_GENERIC': (['FNAME','PRIMNAME','REQ','OPT','VAR','ARGLIST','DOC'],
   'g_{FNAME} = SCM_PACK (0); scm_c_define_gsubr_with_generic (s_{FNAME}, {REQ}, {OPT}, {VAR}, (SCM (*)()) {FNAME}, &g_{FNAME}); ;', r'\(s_{FNAME}\s*,'),
 'SCM_DEFINE1': (['FNAME','PRIMNAME','TYPE','ARGLIST','DOC'],
   'scm_c_define_subr (s_{FNAME}, {TYPE}, {FNAME}); ;', r'\(s_{FNAME}\s*,'),
 'SCM_PRIMITIVE_GENERIC_1': (['FNAME','PRIMNAME','TYPE','ARGLIST','DOC'],
   'g_{FNAME} = SCM_PACK (0); scm_c_define_subr_with_generic (s_{FNAME}, {TYPE}, {FNAME}, &g_{FNAME}); ;', r'\(s_{FNAME}\s*,'),
 'SCM_PROC': (['RANAME','STR','REQ','OPT','VAR','CFN'],
   'scm_c_define_gsubr ({RANAME}, {REQ}, {OPT}, {VAR}, (SCM (*)()) {CFN});', r'\({RANAME}\s*,'),
 'SCM_REGISTER_PROC': (['RANAME','STR','REQ','OPT','VAR','CFN'],
   'scm_c_define_gsubr ({RANAME}, {REQ}, {OPT}, {VAR}, (SCM (*)()) {CFN}); ;', r'\({RANAME}\s*,'),
 'SCM_GPROC': (['RANAME','STR','REQ','OPT','VAR','CFN','GF'],
   '{GF} = SCM_PACK (0); scm_c_define_gsubr_with_generic ({RANAME}, {REQ}, {OPT}, {VAR}, (SCM (*)()) {CFN}, &{GF});', r'\({RANAME}\s*,'),
 'SCM_PROC1': (['RANAME','STR','TYPE','CFN'],
   'scm_c_define_subr ({RANAME}, {TYPE}, (SCM (*)()) {CFN});', r'\({RANAME}\s*,'),
 'SCM_GPROC1': (['RANAME','STR','TYPE','CFN','GF'],
   '{GF} = SCM_PACK (0); scm_c_define_subr_with_generic ({RANAME}, {TYPE}, (SCM (*)()) {CFN}, &{GF});', r'\({RANAME}\s*,'),
 'SCM_SYNTAX': (['RANAME','STR','TYPE','CFN'],
   'scm_make_synt ({RANAME}, {TYPE}, {CFN});', r'scm_make_synt\s*\(\s*{RANAME}\s*,'),
 'SCM_SYMBOL': (['c_name','scheme_name'],
   '{c_name} = scm_permanent_object (scm_from_locale_symbol ({scheme_name}));', r'\b{c_name}\s*=\s*scm_permanent_object'),
 'SCM_GLOBAL_SYMBOL': (['c_name','scheme_name'],
   '{c_name} = scm_permanent_object (scm_from_locale_symbol ({scheme_name}));', r'\b{c_name}\s*=\s*scm_permanent_object'),
 'SCM_KEYWORD': (['c_name','scheme_name'],
   '{c_name} = scm_permanent_object (scm_from_locale_keyword ({scheme_name}));', r'\b{c_name}\s*=\s*scm_permanent_object'),
 'SCM_GLOBAL_KEYWORD': (['c_name','scheme_name'],
   '{c_name} = scm_permanent_object (scm_from_locale_keyword ({scheme_name}));', r'\b{c_name}\s*=\s*scm_permanent_object'),
 'SCM_VARIABLE': (['c_name','scheme_name'],
   '{c_name} = scm_permanent_object (scm_c_define ({scheme_name}, SCM_BOOL_F));', r'\b{c_name}\s*=\s*scm_permanent_object'),
 'SCM_GLOBAL_VARIABLE': (['c_name','scheme_name'],
   '{c_name} = scm_permanent_object (scm_c_define ({scheme_name}, SCM_BOOL_F));', r'\b{c_name}\s*=\s*scm_permanent_object'),
 'SCM_VARIABLE_INIT': (['c_name','scheme_name','init_val'],
   '{c_name} = scm_permanent_object (scm_c_define ({scheme_name}, {init_val}));', r'\b{c_name}\s*=\s*scm_permanent_object'),
 'SCM_GLOBAL_VARIABLE_INIT': (['c_name','scheme_name','init_val'],
   '{c_name} = scm_permanent_object (scm_c_define ({scheme_name}, {init_val}));', r'\b{c_name}\s*=\s*scm_permanent_object'),
}

def split_args(s, i):
    # s[i] == '(' : the top-level arguments and the index after ')'
    depth, args, cur, j, instr = 0, [], '', i, None
    while j < len(s):
        c = s[j]
        if instr:
            cur += c
            if c == '\\': cur += s[j+1]; j += 2; continue
            if c == instr: instr = None
        elif c in '"\'': instr = c; cur += c
        elif c == '(':
            depth += 1
            if depth > 1: cur += c
        elif c == ')':
            depth -= 1
            if depth == 0: args.append(cur.strip()); return args, j+1
            cur += c
        elif c == ',' and depth == 1: args.append(cur.strip()); cur = ''
        else: cur += c
        j += 1
    raise ValueError('unbalanced')

def blank_comments(s):
    # comments replaced by spaces (newlines kept), so that a commented-out
    # definition is not taken for one; strings are kept as they are
    out, i, n = [], 0, len(s)
    while i < n:
        if s.startswith('/*', i):
            j = s.find('*/', i+2); j = n if j < 0 else j+2
            out.append(''.join(c if c == '\n' else ' ' for c in s[i:j])); i = j
        elif s.startswith('//', i):
            j = s.find('\n', i); j = n if j < 0 else j
            out.append(' ' * (j-i)); i = j
        elif s[i] in '"\'':
            q, j = s[i], i+1
            while j < n and s[j] != q:
                j += 2 if s[j] == '\\' else 1
            out.append(s[i:j+1]); i = j+1
        else:
            out.append(s[i]); i += 1
    return ''.join(out)

def analyze(path):
    src = blank_comments(open(path, encoding='latin-1', newline='').read())
    lines = src.split('\n')
    # the preprocessor conditions at each line
    stack, conds = [], []
    for ln in lines:
        t = ln.strip()
        conds.append(list(stack))
        m = re.match(r'#\s*(if|ifdef|ifndef|elif|else|endif)\b(.*)', t)
        if not m: continue
        kw, rest = m.group(1), re.sub(r'/\*.*?\*/|//.*', '', m.group(2)).strip()
        if kw == 'if': stack.append(['(%s)' % rest])
        elif kw == 'ifdef': stack.append(['defined(%s)' % rest])
        elif kw == 'ifndef': stack.append(['!defined(%s)' % rest])
        elif kw == 'elif':
            prev = stack[-1]; stack[-1] = ['!(%s)' % ' && '.join(prev)] + (['(%s)' % rest])
        elif kw == 'else':
            prev = stack[-1]; stack[-1] = ['!(%s)' % ' && '.join(prev)]
        elif kw == 'endif': stack.pop()
    def cond_at(pos):
        ln = src.count('\n', 0, pos)
        return [c for frame in conds[ln] for c in frame]
    out = []
    for m in re.finditer(r'^(SCM_[A-Z0-9_]+)\s*\(', src, re.M):
        name = m.group(1)
        if name not in MACROS: continue
        if '#define' in src[src.rfind('\n', 0, m.start())+1:m.start()]: continue
        argn, tmpl, key = MACROS[name]
        args, _ = split_args(src, m.end()-1)
        if len(args) < len(argn): continue
        d = dict(zip(argn, args))
        keyre = key.format(**{k: re.escape(v) for k, v in d.items()})
        if re.search(keyre, src): continue
        out.append((name, d, tmpl.format(**d), cond_at(m.start())))
    return src, out

def fix(path, src, missing):
    # after the last pasted registration of the file (a line which only
    # registers, as guile-snarf made them)
    regs = [m for m in re.finditer(r'^\s*(scm_c_define_gsubr|scm_c_define_subr|scm_make_synt|g_\w+ = SCM_PACK|\w+ = scm_permanent_object)\b[^\n]*;[ \t;]*\r?\n', src, re.M)]
    if not regs: return None
    at = regs[-1].end()
    # out of the conditions which the last registrations are in: past the
    # #endif (and blank lines) which follow them
    while True:
        m = re.match(r'[ \t]*(#\s*endif[^\n]*)?\r?\n', src[at:])
        if not m or (not m.group(1) and not re.match(r'[ \t]*(#\s*endif|\r?\n)', src[at+m.end():])):
            if m and m.group(1): at += m.end()
            break
        at += m.end()
    block = ['  /* registrations which the snarfing of a Windows build left out',
             '     (the .x files of guile-snarf, pasted in this file), under the',
             '     conditions of their definitions */']
    for name, d, init, cond in missing:
        if cond: block.append('#if ' + ' && '.join(cond))
        block.append('  ' + init)
        if cond: block.append('#endif')
    eol = '\r\n' if '\r\n' in src else '\n'
    return src[:at] + eol.join(block) + eol + src[at:]

if __name__ == '__main__':
    d = sys.argv[1]; dofix = '--fix' in sys.argv
    total = 0
    for f in sorted(os.listdir(d)):
        if not f.endswith('.c') or f.endswith('.i.c'): continue  # .i.c: templates, included
        p = os.path.join(d, f)
        src, missing = analyze(p)
        if not missing: continue
        total += len(missing)
        print('== %s: %d missing' % (f, len(missing)))
        for name, dd, init, cond in missing:
            print('   %-22s %-40s %s' % (name, list(dd.values())[0][:40], ' && '.join(cond)[:110]))
        if dofix:
            new = fix(p, open(p, encoding='latin-1', newline='').read(), missing)
            if new is None: print('   !! no registration block found, not fixed')
            else: open(p, 'w', encoding='latin-1', newline='').write(new)
    print('total missing:', total)
