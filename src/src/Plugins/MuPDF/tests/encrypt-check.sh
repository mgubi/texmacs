#!/bin/sh
#
# A PDF with a password (mupdf_pdf_renderer.cpp, pdf_encryption; File ->
# Export -> Pdf with password):
#
#   src/Plugins/MuPDF/tests/encrypt-check.sh <out dir> <home dir>
#
# structure.tm is exported with a password to open it, an owner password
# and the permissions print and copy. Checked: no reader opens it without
# a password (MuPDF, Ghostscript); both passwords open it; it is AES with
# a key of 256 bits; exactly print and copy are allowed. Prints "ok" lines
# and "FAIL" lines.

set -u
OUT=$1; HOMEDIR=$2
BIN=TeXmacs/bin/texmacs.bin
pdf="$OUT/encrypted.pdf"
rm -f "$pdf"
TEXMACS_HOME_PATH="$HOMEDIR" TEXMACS_PDF_MUPDF=1 \
TEXMACS_PDF_USER_PASSWORD=open-me TEXMACS_PDF_OWNER_PASSWORD=owner-pw \
TEXMACS_PDF_PERMISSIONS=print,copy \
  "$BIN" -c src/Plugins/MuPDF/tests/structure.tm "$pdf" -q > "$pdf.log" 2>&1 &
p=$!; i=0
while [ $i -lt 60 ]; do [ -s "$pdf" ] && sleep 2 && break; sleep 1; i=$((i+1)); done
kill -9 $p 2>/dev/null; wait $p 2>/dev/null
if [ ! -s "$pdf" ]; then echo "  FAIL  encrypt: nothing was written"; exit 0; fi
if mutool info "$pdf" > /dev/null 2>&1; then
  echo "  FAIL  encrypt: MuPDF opens it without a password"
elif gs -q -dNOPAUSE -dBATCH -sDEVICE=nullpage "$pdf" 2>&1 | grep -q "requires a password"; then
  echo "  ok    encrypt: no reader opens it without the password"
else
  echo "  FAIL  encrypt: Ghostscript opens it without a password"
fi
if mutool info -p open-me "$pdf" > /dev/null 2>&1 && mutool info -p owner-pw "$pdf" > /dev/null 2>&1; then
  echo "  ok    encrypt: the password and the owner password open it"
else
  echo "  FAIL  encrypt: a password does not open it"
fi
enc=$(mutool show -p owner-pw "$pdf" trailer/Encrypt 2>/dev/null)
v=$(printf '%s' "$enc" | sed -n 's|.*/V \([0-9]*\).*|\1|p' | head -1)
perm=$(printf '%s' "$enc" | sed -n 's|.*/P \(-*[0-9]*\).*|\1|p' | head -1)
if [ "$v" = 5 ] && printf '%s' "$enc" | grep -q AESV3; then
  echo "  ok    encrypt: AES, a key of 256 bits"
else
  echo "  FAIL  encrypt: not AES-256 (V $v)"
fi
allowed=$(python3 -c "
p= int ('${perm:-0}')
bits= {2: 'print', 3: 'modify', 4: 'copy', 5: 'annotate', 8: 'form', 9: 'accessibility', 10: 'assemble', 11: 'print-hq'}
print (','.join (n for b, n in sorted (bits.items ()) if p & (1 << b)))")
if [ "$allowed" = "print,copy" ]; then
  echo "  ok    encrypt: print and copy allowed, nothing else"
else
  echo "  FAIL  encrypt: allowed: $allowed"
fi
