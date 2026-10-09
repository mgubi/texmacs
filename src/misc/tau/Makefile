###############################################################################
# MODULE     : Makefile (Tau)
# DESCRIPTION: Build of Tau, the editor without a GUI, with Emscripten
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# From the top of the source tree:
#
#   . misc/wasm/emenv.sh build-tau
#   make -C build-tau -f ../misc/tau/Makefile -j8
#   make -C build-tau -f ../misc/tau/Makefile check
#
# See docs/tau-design.md. The build directory holds the objects and the
# output; MUPDF points to the sources of MuPDF built for WebAssembly
# (misc/wasm/build-mupdf.sh). The sources are those of misc/tau/sources.txt,
# the configuration misc/tau/config.h.

TM       := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../..)
S        := $(TM)/src
W        := $(TM)/misc/tau
MUPDF    ?= $(CURDIR)/mupdf-1.28.5-source
MUPDF_LIB ?= $(MUPDF)/build/wasm/slim
OBJ      := obj
OUT      := out

SOURCES  := $(shell grep -v '^\#' $(W)/sources.txt)
OBJECTS  := $(patsubst %,$(OBJ)/%.o,$(basename $(SOURCES)))
NODE     ?= $(shell command -v node)

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
  Texmacs Texmacs/Data Plugins/Gnutls

# the model of exceptions and of setjmp/longjmp must be that of MuPDF
EH       := -fwasm-exceptions -sSUPPORT_LONGJMP=wasm
CPPFLAGS := -I$(W) -include $(W)/config.h -I$(S) $(patsubst %,-I$(S)/%,$(INCLUDE_DIRS)) \
  -I$(TM)/TeXmacs/include -I$(MUPDF)/include \
  -I$(MUPDF)/thirdparty/freetype/include
OPT      ?= -O2
CFLAGS   := $(OPT) $(EH) -w
CXXFLAGS := $(OPT) $(EH) -w -std=c++17
DEPFLAGS := -MMD -MP

LDFLAGS  := $(OPT) $(EH) -sALLOW_MEMORY_GROWTH=1 \
  -sINITIAL_MEMORY=268435456 -sSTACK_SIZE=16777216 \
  -sEXIT_RUNTIME=0 -sFORCE_FILESYSTEM=1 -Wl,--error-limit=0
LIBS     := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a -lm
LIBDEPS  := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a

all: node

# for node, which sees the host file system as it is (NODERAWFS) and takes
# its environment: the core alone, for the tests
node: $(OUT)/node/tau.js $(TM)/TeXmacs/SVNREV plugins-sync

# the version TeXmacs checks its files against (test_texmacs_path): made by
# configure, which is not run here
$(TM)/TeXmacs/SVNREV:
	sed -n 's/^#define ALTERNATIVE_VERSION "\(.*\)"/\1/p' $(W)/config.h > $@

# the plugins, which the build with configure copies from plugins/: the
# lazy format of a plugin, looked for at the first conversion, stops it
# when the plugin is not there
plugins-sync: FORCE
	@mkdir -p $(TM)/TeXmacs/plugins
	@if command -v rsync > /dev/null; then \
	  rsync -a $(TM)/plugins/ $(TM)/TeXmacs/plugins/; \
	else cp -Rp $(TM)/plugins/. $(TM)/TeXmacs/plugins/; fi

$(OUT)/node/tau.js: $(OBJECTS) $(LIBDEPS) $(TM)/misc/wasm/node-pre.js $(W)/sources.txt
	@mkdir -p $(OUT)/node
	em++ $(LDFLAGS) -sNODERAWFS=1 -sENVIRONMENT=node --pre-js $(TM)/misc/wasm/node-pre.js \
	  $(OBJECTS) $(LIBS) -o $@

# for the page: the core for a worker (tau.js, tau.wasm), the files of
# TeXmacs in packages (misc/wasm/package.py and packages.js, as the browser
# build of TeXmacs made them) and the page itself (misc/tau/web)
WEB_FILES := index.html tau.mjs chrome.mjs tau-worker.js
web: $(OUT)/web/tau.js web-packages $(patsubst %,$(OUT)/web/%,$(WEB_FILES))

$(OUT)/web/tau.js: $(OBJECTS) $(LIBDEPS) $(TM)/misc/wasm/packages.js $(W)/web/tau-pre.js $(W)/sources.txt
	@mkdir -p $(OUT)/web
	em++ $(LDFLAGS) -sMODULARIZE=1 -sEXPORT_NAME=tauCore -sENVIRONMENT=worker \
	  --pre-js $(TM)/misc/wasm/packages.js --pre-js $(W)/web/tau-pre.js \
	  -sEXPORTED_RUNTIME_METHODS=ccall,UTF8ToString,HEAPU8,FS,ENV \
	  -sEXPORTED_FUNCTIONS=_main,_malloc,_free \
	  $(OBJECTS) $(LIBS) -o $@

web-packages: $(TM)/TeXmacs/SVNREV plugins-sync FORCE
	@mkdir -p $(OUT)/web
	python3 $(TM)/misc/wasm/package.py $(TM)/TeXmacs $(OUT)/web $(TM)/misc/wasm/boot-files.txt

$(OUT)/web/%: $(W)/web/%
	@mkdir -p $(OUT)/web
	cp $< $@

# a server of static files for the page, on this machine
serve: web
	$(NODE) $(TM)/misc/wasm/serve.mjs $(OUT)/web $(PORT)
PORT ?= 8080

$(OBJ)/%.o: $(S)/%.cpp
	@mkdir -p $(dir $@)
	em++ $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

$(OBJ)/%.o: $(S)/%.c
	@mkdir -p $(dir $@)
	emcc $(CPPFLAGS) $(DEPFLAGS) $(CFLAGS) -c $< -o $@

-include $(OBJECTS:.o=.d)

# the flags of the compilation, kept in obj/.flags: an object is compiled
# again when they change
FLAGS_FILE := $(OBJ)/.flags
$(FLAGS_FILE): FORCE
	@mkdir -p $(OBJ); echo '$(CPPFLAGS) $(CXXFLAGS) $(CFLAGS) $(EH)' > $@.new; \
	  if cmp -s $@.new $@; then rm -f $@.new; else mv $@.new $@; fi
$(OBJECTS): $(FLAGS_FILE)

# the core alone under node: a document of the examples is typeset and
# written as a PDF, with a home directory of its own
CHECK_DOC ?= $(TM)/TeXmacs/examples/texts/accent-test.tm
check: node
	@home=$$(mktemp -d); \
	  TEXMACS_PATH="$(TM)/TeXmacs" TEXMACS_HOME_PATH="$$home/.TeXmacs" HOME="$$home" \
	    $(NODE) $(OUT)/node/tau.js -headless -c $(CHECK_DOC) $$home/check.pdf -q \
	    > $$home/check.log 2>&1; \
	  if head -c 5 $$home/check.pdf 2> /dev/null | grep -q '%PDF'; then \
	    echo "check: $(notdir $(CHECK_DOC)) -> PDF of $$(wc -c < $$home/check.pdf | tr -d ' ') bytes"; \
	  else echo "check: FAILED, see $$home/check.log"; exit 1; fi

FORCE:

clean:
	rm -rf $(OBJ) $(OUT)

.PHONY: all node web web-packages serve check clean plugins-sync
