###############################################################################
# MODULE     : Makefile (WebAssembly)
# DESCRIPTION: Build of TeXmacs for the browser, with Emscripten
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# From the top of the source tree:
#
#   . misc/wasm/emenv.sh build-wasm
#   make -C build-wasm -f ../misc/wasm/Makefile -j8
#
# The build directory holds the objects and the output; MuPDF is expected in
# it, built with its own "make OS=wasm build=release libs" (see
# docs/wasm/README.md). The sources are those of misc/wasm/sources.txt (see
# list-sources.sh), the configuration misc/wasm/config.h.

TM       := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../..)
S        := $(TM)/src
W        := $(TM)/misc/wasm
MUPDF    ?= $(CURDIR)/mupdf-1.28.5-source
# the slim build of MuPDF (misc/wasm/build-mupdf.sh): no fonts of its own
# but the standard 14, no document formats but PDF, SVG and the images
MUPDF_LIB ?= $(MUPDF)/build/wasm/slim
OBJ      := obj
OUT      := out

SOURCES  := $(shell cat $(W)/sources.txt)
OBJECTS  := $(patsubst %,$(OBJ)/%.o,$(basename $(SOURCES)))

INCLUDE_DIRS := System System/Boot System/Classes System/Files System/Link \
  System/Misc System/Language Kernel/Abstractions Kernel/Containers \
  Kernel/Types Data/Convert Data/Drd Data/History Data/Observers \
  Data/Document Data/String Data/Tmfs Data/Tree Data/Parser Scheme \
  Graphics/Bitmap_fonts Graphics/Fonts Graphics/Gui Graphics/Mathematics \
  Graphics/Renderer Graphics/Handwriting Graphics/Types Graphics/Pictures \
  Graphics/Spacial Graphics/Colors Plugins Plugins/Unix \
  Style/Memorizer Style/Environment Style/Evaluate \
  Typeset Typeset/Bridge Typeset/Concat Typeset/Page \
  Edit Edit/Editor Edit/Interface Edit/Modify \
  Texmacs Texmacs/Data Plugins/Pdf/LibAesgm Plugins/Gnutls

# the model of exceptions and of setjmp/longjmp must be that of MuPDF
EH       := -fwasm-exceptions -sSUPPORT_LONGJMP=wasm
CPPFLAGS := -I$(W) -include $(W)/config.h $(patsubst %,-I$(S)/%,$(INCLUDE_DIRS)) \
  -I$(TM)/TeXmacs/include -I$(MUPDF)/include \
  -I$(MUPDF)/thirdparty/freetype/include -sUSE_SDL=3
OPT      ?= -O2
CFLAGS   := $(OPT) $(EH) -w
CXXFLAGS := $(OPT) $(EH) -w -std=c++17

LDFLAGS  := $(OPT) $(EH) -sUSE_SDL=3 -sALLOW_MEMORY_GROWTH=1 \
  -sINITIAL_MEMORY=268435456 -sSTACK_SIZE=16777216 \
  -sEXIT_RUNTIME=0 -sFORCE_FILESYSTEM=1
LIBS     := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a -lm
# a new MuPDF (build-mupdf.sh, its patches) links the program again
LIBDEPS  := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a

all: $(OUT)/texmacs.js

# the page: the files of TeXmacs/ go in packages (misc/wasm/package.py),
# loaded by the page (packages.js): the boot package before TeXmacs starts,
# the others in the background; brotli copies are written for the servers
# which serve them (misc/wasm/serve.mjs does)
web: $(OUT)/web/texmacs.html $(OUT)/web/texmacs-files.json

$(OUT)/web/texmacs.html: $(OBJECTS) $(LIBDEPS) $(W)/progress.js $(W)/packages.js $(W)/web-pre.js $(W)/files.js $(W)/frame.js $(W)/clipboard.js $(W)/print.js $(W)/shell.html
	@mkdir -p $(OUT)/web
	em++ $(LDFLAGS) -lidbfs.js --pre-js $(W)/progress.js --pre-js $(W)/packages.js --pre-js $(W)/web-pre.js \
	  --pre-js $(W)/files.js --pre-js $(W)/frame.js --pre-js $(W)/clipboard.js --pre-js $(W)/print.js --shell-file $(W)/shell.html \
	  $(OBJECTS) $(LIBS) -o $@
	perl -pi -e "s/\@TM_WASM_SIZE\@/$$(wc -c < $(OUT)/web/texmacs.wasm | tr -d ' ')/" $@
	-command -v brotli > /dev/null && brotli -f -q 9 -k $(OUT)/web/texmacs.wasm $(OUT)/web/texmacs.js

# made at every build (package.py only writes the packages which changed)
$(OUT)/web/texmacs-files.json: FORCE | $(TM)/TeXmacs/plugins
	@mkdir -p $(OUT)/web
	python3 $(W)/package.py $(TM)/TeXmacs $(OUT)/web $(W)/boot-files.txt
	-command -v brotli > /dev/null && for f in $(OUT)/web/tm-*.pack; do \
	  [ -f $$f.br ] || brotli -q 9 -k $$f; done; \
	  brotli -f -q 9 -k $(OUT)/web/texmacs-files.json

FORCE:

# a variant for node, which sees the host file system as it is (NODERAWFS)
# and takes its environment: to test the build headless, see
# docs/wasm/README.md
node: $(OUT)/node/texmacs.js $(TM)/TeXmacs/SVNREV $(TM)/TeXmacs/plugins

# the version TeXmacs checks its files against (test_texmacs_path): made by
# configure, which a checkout of the sources (the CI) has not run; the node
# build reads the files of the source tree. That of a build with configure
# is left as it is
$(TM)/TeXmacs/SVNREV:
	sed -n 's/^#define ALTERNATIVE_VERSION "\(.*\)"/\1/p' $(W)/config.h > $@

# the plugins, which the build with configure copies from plugins/ (make at
# the top): a checkout of the sources has none in TeXmacs/ (and a lazy
# format of a plugin, looked for at the first conversion, then stopped it)
$(TM)/TeXmacs/plugins:
	cp -R $(TM)/plugins $@

$(OUT)/node/texmacs.js: $(OBJECTS) $(LIBDEPS) $(W)/node-pre.js
	@mkdir -p $(OUT)/node
	em++ $(LDFLAGS) -sNODERAWFS=1 -sENVIRONMENT=node --pre-js $(W)/node-pre.js \
	  $(OBJECTS) $(LIBS) -o $@

$(OUT)/texmacs.js: $(OBJECTS)
	@mkdir -p $(OUT)
	em++ $(LDFLAGS) $(OBJECTS) $(LIBS) -o $@

$(OBJ)/Plugins/Vue/%.o: $(S)/Plugins/Vue/%.cpp
	@mkdir -p $(dir $@)
	em++ $(CPPFLAGS) $(CXXFLAGS) -std=c++20 -c $< -o $@

$(OBJ)/%.o: $(S)/%.cpp
	@mkdir -p $(dir $@)
	em++ $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

$(OBJ)/%.o: $(S)/%.c
	@mkdir -p $(dir $@)
	emcc $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ) $(OUT)

.PHONY: all web node clean
