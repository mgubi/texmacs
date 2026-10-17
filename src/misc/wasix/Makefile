###############################################################################
# MODULE     : Makefile (WASIX)
# DESCRIPTION: Build of TeXmacs for WASIX (wasixcc), run by Wasmer
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# From the top of the source tree:
#
#   sh misc/wasix/build-deps.sh build-wasix       # MuPDF, SDL3, Hunspell
#   make -C build-wasix -f ../misc/wasix/Makefile -j8
#   wasmer run --dir=. build-wasix/out/texmacs.wasm -- -headless ...
#
# The sources are those of the Emscripten build (misc/wasm/sources.txt), as
# is the configuration (misc/wasm/config.h); see docs/wasix/README.md.

TM       := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../..)
S        := $(TM)/src
W        := $(TM)/misc/wasm
X        := $(TM)/misc/wasix
MUPDF    ?= $(CURDIR)/mupdf-1.28.5-source
MUPDF_LIB ?= $(MUPDF)/build/wasix/slim
SDL3     ?= $(CURDIR)/sdl3
OBJ      := obj
OUT      := out

# wasixcc (https://github.com/wasix-org/wasixcc), with the exceptions of
# WebAssembly (C++ exceptions, and setjmp/longjmp for MuPDF): Wasmer and the
# browsers run them
WASIXCC  ?= $(HOME)/.wasixcc/bin
export PATH := $(WASIXCC):$(PATH)
export WASIXCC_WASM_EXCEPTIONS := yes
CC       := $(WASIXCC)/wasixcc
CXX      := $(WASIXCC)/wasix++

# (and the stream of the display, which only WASIX has: vue_stream.hpp)
SOURCES  := $(shell cat $(W)/sources.txt) Plugins/Vue/vue_stream.cpp
OBJECTS  := $(patsubst %,$(OBJ)/%.o,$(basename $(SOURCES)))
HUNSPELL := hunspell-1.7.2
HUNSPELL_SRC := affentry affixmgr csutil filemgr hashmgr hunspell hunzip \
  phonet replist suggestmgr
HUNSPELL_OBJECTS := $(patsubst %,$(OBJ)/hunspell/%.o,$(HUNSPELL_SRC))
OBJECTS  += $(HUNSPELL_OBJECTS)

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

# tm_configure.hpp of misc/wasix first, config.h of misc/wasm (wasm32)
CPPFLAGS := -I$(X) -I$(X)/include -I$(W) -include $(W)/config.h \
  -include $(X)/include/wasix-compat.h \
  $(patsubst %,-I$(S)/%,$(INCLUDE_DIRS)) \
  -I$(TM)/TeXmacs/include -I$(MUPDF)/include \
  -I$(MUPDF)/thirdparty/freetype/include -I$(SDL3)/include \
  -I$(CURDIR)/$(HUNSPELL)/src -DHUNSPELL_STATIC
OPT      ?= -O2
CFLAGS   := $(OPT) -w
CXXFLAGS := $(OPT) -w -std=c++17
DEPFLAGS := -MMD -MP

# the stack of the main thread (1 MB by default): S7 and the typesetter
# recurse deeply
LDFLAGS  := $(OPT) -Wl,-z,stack-size=16777216
LIBS     := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a \
  $(SDL3)/lib/libSDL3.a -lm
LIBDEPS  := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a \
  $(SDL3)/lib/libSDL3.a

all: $(OUT)/texmacs.wasm $(TM)/TeXmacs/SVNREV plugins-sync

$(OUT)/texmacs.wasm: $(OBJECTS) $(LIBDEPS)
	@mkdir -p $(OUT)
	$(CXX) $(LDFLAGS) $(OBJECTS) $(LIBS) -o $@

# the page (misc/wasix/web): the program, its files (pack-fs.py: those of
# the boot of TeXmacs, its Scheme code and the Type 1 fonts) and the Wasmer
# SDK, npm installed in sdk/ (SDK_VERSION) and copied to out/web/sdk;
# served with the headers which the SDK needs by misc/wasix/serve.mjs
SDK_VERSION ?= 0.19.1
SDK := $(CURDIR)/sdk/node_modules/@wasmer/sdk
web: $(OUT)/web/texmacs.wasm $(OUT)/web/texmacs-fs.json $(OUT)/web/sdk/dist/index.js \
  $(OUT)/web/index.html $(OUT)/web/texmacs-wasix.js

$(OUT)/web/texmacs.wasm: $(OUT)/texmacs.wasm
	@mkdir -p $(OUT)/web
	cp $< $@

$(OUT)/web/texmacs-fs.json: FORCE $(TM)/TeXmacs/SVNREV plugins-sync
	@mkdir -p $(OUT)/web
	python3 $(X)/pack-fs.py $(TM)/TeXmacs $(W)/boot-files.txt $(OUT)/web/texmacs-fs

$(SDK)/dist/index.js:
	mkdir -p sdk && cd sdk && ([ -f package.json ] || npm init -y > /dev/null) && \
	  npm install @wasmer/sdk@$(SDK_VERSION)

$(OUT)/web/sdk/dist/index.js: $(SDK)/dist/index.js
	rm -rf $(OUT)/web/sdk; mkdir -p $(OUT)/web/sdk
	cp -R $(SDK)/dist $(SDK)/pkg $(SDK)/package.json $(SDK)/LICENSE* $(OUT)/web/sdk/ 2> /dev/null || \
	  cp -R $(SDK)/dist $(SDK)/pkg $(SDK)/package.json $(OUT)/web/sdk/

$(OUT)/web/%: $(X)/web/%
	@mkdir -p $(OUT)/web
	cp $< $@

$(TM)/TeXmacs/SVNREV:
	sed -n 's/^#define ALTERNATIVE_VERSION "\(.*\)"/\1/p' $(W)/config.h > $@

plugins-sync: FORCE
	@mkdir -p $(TM)/TeXmacs/plugins
	@rsync -a $(TM)/plugins/ $(TM)/TeXmacs/plugins/

FORCE:

$(OBJ)/Plugins/Vue/%.o: $(S)/Plugins/Vue/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -std=c++20 -c $< -o $@

$(HUNSPELL)/src/hunspell/hunspell.hxx:
	sh $(W)/get-hunspell.sh $(CURDIR)

$(HUNSPELL)/src/hunspell/%.cxx: $(HUNSPELL)/src/hunspell/hunspell.hxx ;

$(HUNSPELL_OBJECTS): $(OBJ)/hunspell/%.o: $(HUNSPELL)/src/hunspell/%.cxx
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -DHUNSPELL_STATIC -c $< -o $@

$(OBJ)/Plugins/Ispell/ispell_hunspell.o: $(HUNSPELL)/src/hunspell/hunspell.hxx

$(OBJ)/%.o: $(S)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

$(OBJ)/%.o: $(S)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(DEPFLAGS) $(CFLAGS) -c $< -o $@

-include $(OBJECTS:.o=.d)

clean:
	rm -rf $(OBJ) $(OUT)

.PHONY: all web clean plugins-sync
