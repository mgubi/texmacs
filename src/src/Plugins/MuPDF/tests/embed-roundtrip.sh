#!/bin/sh
#
# The round trip of "Pdf with embedded document" (tm-print.scm,
# file-menu.scm, mupdf_attachments.cpp):
#
#   src/Plugins/MuPDF/tests/embed-roundtrip.sh <out dir> <home dir>
#
# A document with an image next to it is exported with its source embedded,
# then taken back out with the source folder gone. Checked: the open
# document keeps its paths; mutool extract finds the document first and the
# image byte for byte (a reader which is not ours); the import brings both
# back, from the PDF alone; a file next to the PDF with the name of the
# document is left alone. Prints "ok" lines and "FAIL" lines.

set -u
OUT=$1; HOMEDIR=$2
BIN=TeXmacs/bin/texmacs.bin
D="$OUT/embed"
rm -rf "${D:?}"; mkdir -p "$D/src" "$D/out" "$D/extract"
cp TeXmacs/misc/images/The_Jolly_Writer.png "$D/src/pic.png"
cat > "$D/src/doc.tm" <<'TM'
<TeXmacs|2.1.5>

<style|<tuple|generic>>

<\body>
  A document to embed, with a picture next to it: <image|pic.png|3cm|||>

  The end. Été.
</body>

<initial|<\collection>
</collection>>
TM
echo "DECOY" > "$D/out/doc.tm"
cat > "$D/export.scm" <<SCM
(define (image-path)
  (with t (tree-search (buffer-tree) (lambda (x) (tree-is? x 'image)))
    (if (null? t) "none" (tree->string (tree-ref (car t) 0)))))
(delayed (:pause 2000) (load-buffer (system->url "$D/src/doc.tm")))
(delayed (:pause 7000)
  (wrapped-print-to-pdf-embeded-with-tm (system->url "$D/out/doc.pdf"))
  (display* "open document image: " (image-path) "\n")
  (display* "exported\n"))
SCM
cat > "$D/import.scm" <<SCM
(define (image-path)
  (with t (tree-search (buffer-tree) (lambda (x) (tree-is? x 'image)))
    (if (null? t) "none" (tree->string (tree-ref (car t) 0)))))
(delayed (:pause 2000)
  ((module-ref (resolve-module '(texmacs menus file-menu))
               'wrapped-import-pdf-embeded-with-tm)
   (system->url "$D/out/doc.pdf")))
(delayed (:pause 6000)
  (display* "imported image: " (image-path) "\n")
  (display* "imported\n"))
SCM

run () { # <scm> <log> <marker>
  TEXMACS_HOME_PATH="$HOMEDIR" TEXMACS_PDF_MUPDF=1 "$BIN" -x "(load \"$1\")" > "$2" 2>&1 &
  p=$!; i=0
  while [ $i -lt 60 ]; do grep -q "$3" "$2" 2>/dev/null && sleep 2 && break; sleep 1; i=$((i+1)); done
  kill -9 $p 2>/dev/null; wait $p 2>/dev/null
}

run "$D/export.scm" "$D/export.log" exported
if [ ! -s "$D/out/doc.pdf" ]; then echo "  FAIL  embed: nothing was exported"; exit 1; fi
if grep -q "open document image: pic.png" "$D/export.log"; then
  echo "  ok    embed: the open document keeps its paths"
else
  echo "  FAIL  embed: the export changed the open document ($(grep 'open document image' "$D/export.log"))"
fi
# a reader which is not ours: the document first, the image byte for byte
(cd "$D/extract" && mutool extract "$D/out/doc.pdf" > "$D/extract.log" 2>&1)
first=$(grep -o "file-[0-9]*\.[a-z]* ([^)]*)" "$D/extract.log" | head -1)
png=$(ls "$D/extract"/file-*.png 2>/dev/null | head -1)
case "$first" in
  *"(doc.tm)") if [ -n "$png" ] && cmp -s "$png" "$D/src/pic.png"; then
                 echo "  ok    embed: mutool extract finds doc.tm first, and the image as it was"
               else echo "  FAIL  embed: mutool extract does not give the image back"; fi ;;
  *) echo "  FAIL  embed: the first embedded file is not the document ($first)" ;;
esac
# the import, with the source folder gone
mv "$D/src" "$D/src-gone"
run "$D/import.scm" "$D/import.log" imported
mv "$D/src-gone" "$D/src"
img=$(sed -n 's/^imported image: //p' "$D/import.log")
if [ -n "$img" ] && [ "$img" != none ] && [ -f "$img" ] && cmp -s "$img" "$D/src/pic.png"; then
  echo "  ok    embed: the import brings the document and its image back"
else
  echo "  FAIL  embed: the import did not bring the image back ($img)"
fi
if [ "$(cat "$D/out/doc.tm")" = DECOY ]; then
  echo "  ok    embed: the file next to the PDF is left alone"
else
  echo "  FAIL  embed: the import overwrote the file next to the PDF"
fi
