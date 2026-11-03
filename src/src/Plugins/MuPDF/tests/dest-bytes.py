# Every link to a place in the document must name it in exactly the bytes
# of a key of the /Dests name tree: a reader looks a name up by its bytes.
# (MuPDF decodes both before comparing and so forgives a difference; most
# readers do not.) Also: every /URI is 7-bit ASCII. Run on a PDF which
# `mutool clean -d` has decompressed. Prints "ok", or what is wrong.
import re, sys
d= open (sys.argv[1], 'rb').read ()
def pdf_string (tok):
    if tok.startswith (b'<'):
        return bytes.fromhex (tok[1:-1].decode ())
    body= tok[1:-1]; out= bytearray (); i= 0
    esc= {b'n': b'\n', b'r': b'\r', b't': b'\t', b'b': b'\b', b'f': b'\f'}
    while i < len (body):
        c= body[i:i+1]
        if c == b'\\':
            n= body[i+1:i+2]
            if n in esc: out += esc[n]; i += 2
            elif n.isdigit ():
                m= re.match (rb'[0-7]{1,3}', body[i+1:]); out.append (int (m.group (0), 8)); i += 1 + len (m.group (0))
            else: out += n; i += 2
        else: out += c; i += 1
    return bytes (out)
STR= rb'(<[0-9A-Fa-f\s]*>|\((?:\\.|[^\\)])*\))'
bad= []
i= d.find (b'/Dests')
keys= set ()
if i >= 0:
    j= d.find (b'/Names', i)
    arr= d[j:] if j >= 0 else b''   # the loop below stops at its end
    # the keys are the strings at the top level of the /Names array
    depth= 0; pos= arr.find (b'[') + 1
    while pos < len (arr):
        c= arr[pos:pos+1]
        if c == b'[': depth += 1; pos += 1
        elif c == b']':
            if depth == 0: break
            depth -= 1; pos += 1
        elif depth == 0 and c in (b'(', b'<') and arr[pos:pos+2] != b'<<':
            m= re.match (STR, arr[pos:], re.S); keys.add (pdf_string (m.group (0))); pos += len (m.group (0))
        else: pos += 1
for m in re.finditer (rb'/Dest\s*' + STR, d, re.S):
    b= pdf_string (m.group (1))
    if b not in keys: bad.append ('the link to %r names no key of the name tree' % b)
for m in re.finditer (rb'/URI\s*' + STR, d, re.S):
    b= pdf_string (m.group (1))
    if any (x >= 128 or x <= 32 for x in b): bad.append ('/URI %r is not plain ASCII' % b)
print ('; '.join (bad) if bad else 'ok (%d keys)' % len (keys))
