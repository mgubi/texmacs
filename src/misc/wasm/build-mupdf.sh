#!/bin/sh
# Build MuPDF for the browser build, in build-wasm (after . misc/wasm/emenv.sh):
#
#   sh misc/wasm/build-mupdf.sh [version]
#
# The slim build (build/wasm/slim) is the one linked: MuPDF's own fonts are
# left out but the standard 14 (TeXmacs has its fonts; the others made 37 MB
# of the 59 of texmacs.wasm), and its readers of documents but PDF, SVG and
# the images, its JavaScript, its writers of docx/odt, its OCR, barcodes and
# hyphenation. texmacs.wasm is 22 MB then (5 MB compressed with brotli).
set -e
V=${1:-1.28.5}
cd build-wasm
[ -d mupdf-$V-source ] || {
  curl -LO https://mupdf.com/downloads/archive/mupdf-$V-source.tar.gz
  tar xzf mupdf-$V-source.tar.gz
}
cd mupdf-$V-source
# the fixes of TeXmacs (misc/wasm/mupdf-*.patch), applied once:
#   mupdf-subset-cff.patch  the subroutines of a CFF font are executed within
#                           the charstrings which call them, when the fonts
#                           are subset (the Fira fonts were embedded whole)
for p in ../../misc/wasm/mupdf-*.patch; do
  s=.applied-$(basename "$p")
  [ -f "$s" ] || { patch -p1 < "$p" && touch "$s"; }
done
F="-DTOFU -DTOFU_CJK -DTOFU_SIL -DTOFU_EMOJI -DTOFU_HISTORIC -DTOFU_SYMBOL \
 -DFZ_ENABLE_XPS=0 -DFZ_ENABLE_CBZ=0 -DFZ_ENABLE_HTML=0 -DFZ_ENABLE_FB2=0 \
 -DFZ_ENABLE_MOBI=0 -DFZ_ENABLE_EPUB=0 -DFZ_ENABLE_OFFICE=0 -DFZ_ENABLE_TXT=0 \
 -DFZ_ENABLE_MD=0 -DFZ_ENABLE_HTML_ENGINE=0 -DFZ_ENABLE_OCR_OUTPUT=0 \
 -DFZ_ENABLE_DOCX_OUTPUT=0 -DFZ_ENABLE_ODT_OUTPUT=0 -DFZ_ENABLE_BROTLI=0 \
 -DFZ_ENABLE_JS=0 -DFZ_ENABLE_BARCODE=0 -DFZ_ENABLE_HYPHEN=0"
emmake make -j8 OS=wasm build=release OUT=build/wasm/slim XCFLAGS="$F" brotli=no libs
