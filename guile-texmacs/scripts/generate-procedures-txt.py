import os, glob, re

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
c_files = (
    sorted(glob.glob(os.path.join(root, 'libguile', '*.c'))) +
    sorted(glob.glob(os.path.join(root, 'srfi', '*.c'))) +
    sorted(glob.glob(os.path.join(root, 'guile-readline', '*.c')))
)

str_lit = r'"(?:[^"\\]|\\.)*"'

# Macro patterns
p_define = re.compile(
    r'(SCM_DEFINE|SCM_PRIMITIVE_GENERIC|SCM_DEFINE_PUBLIC)\s*'
    r'\(\s*([a-zA-Z0-9_]+)\s*,\s*(' + str_lit + r')\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*(\([^\)]*\))\s*,\s*((?:' + str_lit + r'\s*)+)\)',
    re.MULTILINE
)

p_define1 = re.compile(
    r'(SCM_DEFINE1|SCM_PRIMITIVE_GENERIC_1)\s*'
    r'\(\s*([a-zA-Z0-9_]+)\s*,\s*(' + str_lit + r')\s*,\s*([^,]+)\s*,\s*(\([^\)]*\))\s*,\s*((?:' + str_lit + r'\s*)+)\)',
    re.MULTILINE
)

p_register = re.compile(
    r'SCM_REGISTER_PROC\s*\(\s*([a-zA-Z0-9_]+)\s*,\s*(' + str_lit + r')\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([a-zA-Z0-9_]+)\s*\)',
    re.MULTILINE
)

def clean_arglist(arglist_str):
    raw = arglist_str.strip()
    if raw.startswith('(') and raw.endswith(')'):
        raw = raw[1:-1].strip()
    if not raw or raw == 'void':
        return []
    parts = [p.strip() for p in raw.split(',') if p.strip()]
    args = []
    for p in parts:
        # e.g. "SCM foo", "SCM *bar", "int x"
        tokens = p.split()
        if tokens:
            arg_name = tokens[-1].lstrip('*')
            args.append(arg_name)
    return args

def clean_doc(doc_str):
    # Convert texinfo markup to clean readable text
    doc = doc_str
    # @var{foo} -> FOO
    doc = re.sub(r'@var\{([^}]+)\}', lambda m: m.group(1).upper(), doc)
    # @code{foo} -> `foo'
    doc = re.sub(r'@code\{([^}]+)\}', r"`\1'", doc)
    # @emph{foo} -> foo
    doc = re.sub(r'@emph\{([^}]+)\}', r'\1', doc)
    # @samp{foo} -> `foo'
    doc = re.sub(r'@samp\{([^}]+)\}', r"`\1'", doc)
    # @@ -> @
    doc = doc.replace('@@', '@')
    return doc

entries = {}

for c in c_files:
    with open(c, 'r', encoding='latin1') as f:
        txt = f.read()
    
    for m in p_define.finditer(txt):
        macro, cname, sname_raw, req, opt, var, arglist_raw, doc_raw = m.groups()
        sname = eval(sname_raw)
        doc_pieces = re.findall(str_lit, doc_raw)
        doc = ''.join(eval(p) for p in doc_pieces)
        args = clean_arglist(arglist_raw)
        entries[sname] = {
            'sname': sname,
            'cname': cname,
            'args': args,
            'doc': clean_doc(doc)
        }

    for m in p_define1.finditer(txt):
        macro, cname, sname_raw, type_, arglist_raw, doc_raw = m.groups()
        sname = eval(sname_raw)
        doc_pieces = re.findall(str_lit, doc_raw)
        doc = ''.join(eval(p) for p in doc_pieces)
        args = clean_arglist(arglist_raw)
        entries[sname] = {
            'sname': sname,
            'cname': cname,
            'args': args,
            'doc': clean_doc(doc)
        }

    for m in p_register.finditer(txt):
        raname, sname_raw, req, opt, var, cname = m.groups()
        sname = eval(sname_raw)
        if sname not in entries:
            entries[sname] = {
                'sname': sname,
                'cname': cname,
                'args': [],
                'doc': f'Implemented by the C function `{cname}\'.'
            }

out_path = os.path.join(root, 'libguile', 'guile-procedures.txt')
with open(out_path, 'w', encoding='utf-8', newline='\n') as f:
    f.write('This is guile-procedures.txt for Guile.\n\n')
    for sname in sorted(entries.keys()):
        info = entries[sname]
        arg_str = ' '.join(info['args'])
        args_display = f" {arg_str}" if arg_str else ""
        c_args = ', '.join(info['args'])
        c_sig = f"{info['cname']} ({c_args})"
        
        f.write('\x0c\n')
        f.write(f"{sname}\n\n")
        f.write(f" - Scheme Procedure: {sname}{args_display}\n")
        f.write(f" - C Function: {c_sig}\n")
        doc_lines = info['doc'].splitlines()
        for line in doc_lines:
            f.write(f"     {line}\n" if line.strip() else "\n")
        f.write('\n')

print(f"Successfully generated {out_path} with {len(entries)} documented procedures.")
