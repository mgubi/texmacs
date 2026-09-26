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
MUPDF_LIB = $(MUPDF)/build/wasm/release
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

all: $(OUT)/texmacs.js

# the page: TeXmacs/ is packaged at /texmacs, without what the browser
# cannot use: the programs (bin/ and those of the plugins, which need
# processes; their Scheme code is kept, the formats refer to it) and the
# documentation of the plugins
PACKAGE  := --preload-file $(TM)/TeXmacs@/texmacs \
  --exclude-file '*/TeXmacs/bin' --exclude-file '*/TeXmacs/plugins/*/bin' \
  --exclude-file '*/TeXmacs/plugins/*/doc' \
  --exclude-file '*/misc/images/windows' --exclude-file '*.DS_Store'

web: $(OUT)/web/texmacs.html

$(OUT)/web/texmacs.html: $(OBJECTS) $(W)/web-pre.js $(W)/shell.html
	@mkdir -p $(OUT)/web
	em++ $(LDFLAGS) -lidbfs.js --pre-js $(W)/web-pre.js --shell-file $(W)/shell.html \
	  $(PACKAGE) $(OBJECTS) $(LIBS) -o $@

# a variant for node, which sees the host file system as it is (NODERAWFS)
# and takes its environment: to test the build headless, see
# docs/wasm/README.md
node: $(OUT)/node/texmacs.js

$(OUT)/node/texmacs.js: $(OBJECTS) $(W)/node-pre.js
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
