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
# node, as the shell finds it: make looks the command up itself, and takes
# the first "node" on the PATH even when it is a directory, as the one of
# emsdk, which the CI puts on the PATH ("node: Permission denied")
NODE     ?= $(shell command -v node)
OBJECTS  := $(patsubst %,$(OBJ)/%.o,$(basename $(SOURCES)))
# the spell checker, Hunspell, in the program (src/Plugins/Ispell/
# ispell_hunspell.cpp), from the sources fetched by get-hunspell.sh
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

# the model of exceptions and of setjmp/longjmp must be that of MuPDF
EH       := -fwasm-exceptions -sSUPPORT_LONGJMP=wasm
CPPFLAGS := -I$(W) -include $(W)/config.h $(patsubst %,-I$(S)/%,$(INCLUDE_DIRS)) \
  -I$(TM)/TeXmacs/include -I$(MUPDF)/include \
  -I$(MUPDF)/thirdparty/freetype/include -sUSE_SDL=3 \
  -I$(CURDIR)/$(HUNSPELL)/src -DHUNSPELL_STATIC
OPT      ?= -O2
CFLAGS   := $(OPT) $(EH) -w
CXXFLAGS := $(OPT) $(EH) -w -std=c++17

# the GPU renderer of the Vue port (src/Plugins/Vue/vue_gpu.cpp), which the
# page uses unless ?gpu=0 or the browser has no WebGL2: ThorVG, by default
# the one of the build directory (build-wasm/thorvg/wasm), which is built
# when it is not there (misc/thorvg/build-thorvg.sh, which fetches ThorVG:
# git, and python3 for meson); THORVG=<dir> one installed elsewhere by
# build-thorvg.sh (<prefix>/wasm); THORVG=none a build with MuPDF only
THORVG   ?= $(CURDIR)/thorvg/wasm
ifeq ($(THORVG),none)
override THORVG :=
endif
THORVG_LIB :=
ifneq ($(THORVG),)
CPPFLAGS += -DUSE_THORVG -I$(THORVG)/include/thorvg-1
THORVG_LIB := $(THORVG)/lib/libthorvg-1.a
endif

# the headers each object includes (obj/....d): a change of a header, of
# config.h (-include) or of those of MuPDF compiles again the objects which
# include it, else an object built against the old layout of a class gives
# "indirect call signature mismatch" at startup
DEPFLAGS := -MMD -MP

LDFLAGS  := $(OPT) $(EH) -sUSE_SDL=3 -sALLOW_MEMORY_GROWTH=1 \
  -sINITIAL_MEMORY=268435456 -sSTACK_SIZE=16777216 \
  -sEXIT_RUNTIME=0 -sFORCE_FILESYSTEM=1
LIBS     := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a -lm
ifneq ($(THORVG),)
LIBS     += $(THORVG)/lib/libthorvg-1.a
LDFLAGS  += -sMAX_WEBGL_VERSION=2 -sMIN_WEBGL_VERSION=2
endif
# a new MuPDF (build-mupdf.sh, its patches) links the program again
LIBDEPS  := $(MUPDF_LIB)/libmupdf.a $(MUPDF_LIB)/libmupdf-third.a $(THORVG_LIB)

all: $(OUT)/texmacs.js

# the page: the files of TeXmacs/ go in packages (misc/wasm/package.py),
# loaded by the page (packages.js): the boot package before TeXmacs starts,
# the others in the background; brotli copies are written for the servers
# which serve them (misc/wasm/serve.mjs does)
# the icon of the page (the tab of the browser, its bookmarks, the home
# screen of a phone): the logo of TeXmacs Vue (misc/icons/vue-logo),
# declared in shell.html
WEB_ICONS := $(foreach n,16 32 48 64 128 192,$(OUT)/web/texmacs-vue-$(n).png) $(OUT)/web/favicon.ico

web: $(OUT)/web/texmacs.html $(OUT)/web/texmacs-files.json $(WEB_ICONS) tikzjax asymptote python r

# the TeX of the TikZ plugin (plugins/tikz, src/docs/wasm/tikzjax.md):
# TikZJax, fetched by get-tikzjax.sh, without its fonts and the part of its
# pages, and the worker of the plugin, in out/web/tikzjax/
TIKZJAX := tikzjax-1.6.0
tikzjax: $(OUT)/web/tikzjax/tm-tikz.js $(OUT)/web/tikzjax/tables.js

# the tables of the worker: the codes of dvi2html, the encodings of TeXmacs
$(OUT)/web/tikzjax/tables.js: $(W)/tikzjax-tables.mjs $(TIKZJAX)/dist/run-tex.js $(wildcard $(TM)/TeXmacs/fonts/enc/*.enc)
	@mkdir -p $(OUT)/web/tikzjax
	$(NODE) $(W)/tikzjax-tables.mjs $(TIKZJAX)/dist/run-tex.js $(TM)/TeXmacs/fonts/enc $@ $(TIKZJAX:tikzjax-%=%)

$(TIKZJAX)/dist/run-tex.js:
	sh $(W)/get-tikzjax.sh $(CURDIR)

$(OUT)/web/tikzjax/tm-tikz.js: $(TM)/plugins/tikz/web/tm-tikz.js $(TIKZJAX)/dist/run-tex.js $(wildcard $(W)/tikzjax-extra/*.tex)
	@mkdir -p $(OUT)/web/tikzjax
	cp $(TIKZJAX)/dist/run-tex.js $(TIKZJAX)/dist/tex.wasm.gz $(TIKZJAX)/dist/core.dump.gz $(TIKZJAX)/LICENSE $(OUT)/web/tikzjax/
	rm -rf $(OUT)/web/tikzjax/tex_files; cp -R $(TIKZJAX)/dist/tex_files $(OUT)/web/tikzjax/
	# the libraries of pgfplots which TikZJax lacks (misc/wasm/tikzjax-extra)
	for f in $(W)/tikzjax-extra/*.tex; do gzip -9 -n -c $$f > $(OUT)/web/tikzjax/tex_files/`basename $$f`.gz; done
	cp $(TM)/plugins/tikz/web/tm-tikz.js $@

# the program of the Asymptote plugin (plugins/asymptote, docs/wasm/asymptote.md):
# Asymptote-web, Asymptote in WebAssembly, fetched by get-asymptote.sh, and
# the worker of the plugin, in out/web/asymptote/; version.js tells the
# versions to the worker (the one of Asymptote is that of the package)
ASYMPTOTE := asymptote-web-0.3.3
ASYMPTOTE_VERSION := 3.15
ASY_FILES := asymptote.js asymptote.wasm asy.data asymptote-web.js utils.js
asymptote: $(OUT)/web/asymptote/asymptote.wasm $(OUT)/web/asymptote/tm-asy.mjs $(OUT)/web/asymptote/version.js

$(ASYMPTOTE)/dist/asymptote.wasm:
	sh $(W)/get-asymptote.sh $(CURDIR)

$(OUT)/web/asymptote/version.js: $(ASYMPTOTE)/dist/asymptote.wasm
	@mkdir -p $(OUT)/web/asymptote
	printf 'export const ASYMPTOTE_VERSION = "%s";\nexport const ASYMPTOTE_WEB_VERSION = "%s";\n' \
	  $(ASYMPTOTE_VERSION) $(ASYMPTOTE:asymptote-web-%=%) > $@

# Asymptote-web, with brotli copies of its large files for the servers
$(OUT)/web/asymptote/asymptote.wasm: $(ASYMPTOTE)/dist/asymptote.wasm
	@mkdir -p $(OUT)/web/asymptote
	cd $(ASYMPTOTE)/dist && cp $(ASY_FILES) $(CURDIR)/$(OUT)/web/asymptote/
	-command -v brotli > /dev/null && cd $(OUT)/web/asymptote && \
	  for f in asymptote.wasm asy.data asymptote.js; do brotli -f -q 9 -k $$f; done
	touch $@

$(OUT)/web/asymptote/tm-asy.mjs: $(TM)/plugins/asymptote/web/tm-asy.mjs
	@mkdir -p $(OUT)/web/asymptote
	cp $< $@

# the worker of the Python plugin (plugins/python), in out/web/python/: it
# loads Python in WebAssembly (Pyodide) from the CDN of jsDelivr
python: $(OUT)/web/python/tm-python.mjs

$(OUT)/web/python/tm-python.mjs: $(TM)/plugins/python/web/tm-python.mjs
	@mkdir -p $(OUT)/web/python
	cp $< $@

# the worker of the R plugin (plugins/r), in out/web/r/: it loads R in
# WebAssembly (webR) from webr.r-wasm.org
r: $(OUT)/web/r/tm-r.mjs

$(OUT)/web/r/tm-r.mjs: $(TM)/plugins/r/web/tm-r.mjs
	@mkdir -p $(OUT)/web/r
	cp $< $@

$(OUT)/web/texmacs-vue-%.png: $(TM)/TeXmacs/misc/images/texmacs-vue-%.png
	@mkdir -p $(OUT)/web
	cp $< $@

$(OUT)/web/favicon.ico: $(TM)/TeXmacs/misc/images/texmacs-vue.ico
	@mkdir -p $(OUT)/web
	cp $< $@

$(OUT)/web/texmacs.html: $(OBJECTS) $(LIBDEPS) $(W)/progress.js $(W)/packages.js $(W)/web-pre.js $(W)/files.js $(W)/frame.js $(W)/clipboard.js $(W)/print.js $(W)/workers.js $(W)/javascript.js $(W)/ime.js $(W)/wallet.js $(W)/shell.html
	@mkdir -p $(OUT)/web
	em++ $(LDFLAGS) -lidbfs.js --pre-js $(W)/progress.js --pre-js $(W)/packages.js --pre-js $(W)/web-pre.js \
	  --pre-js $(W)/files.js --pre-js $(W)/frame.js --pre-js $(W)/clipboard.js --pre-js $(W)/print.js --pre-js $(W)/workers.js --pre-js $(W)/javascript.js --pre-js $(W)/ime.js --pre-js $(W)/wallet.js --shell-file $(W)/shell.html \
	  $(OBJECTS) $(LIBS) -o $@
	perl -pi -e "s/\@TM_WASM_SIZE\@/$$(wc -c < $(OUT)/web/texmacs.wasm | tr -d ' ')/" $@
	perl -pi -e "s/\@TM_VERSION\@/$$(sed -n 's/^#define TEXMACS_VERSION \"\(.*\)\"/\1/p' $(W)/tm_configure.hpp)/" $@
	cp $@ $(OUT)/web/index.html
	-command -v brotli > /dev/null && brotli -f -q 9 -k $(OUT)/web/texmacs.wasm $(OUT)/web/texmacs.js
	gzip -9 -n -k -f $(OUT)/web/texmacs.wasm

# made at every build (package.py only writes the packages which changed)
$(OUT)/web/texmacs-files.json: FORCE plugins-sync
	@mkdir -p $(OUT)/web
	python3 $(W)/package.py $(TM)/TeXmacs $(OUT)/web $(W)/boot-files.txt
	-command -v brotli > /dev/null && for f in $(OUT)/web/tm-*.pack $(OUT)/web/tm-font-*; do \
	  case $$f in *.br) continue;; esac; \
	  [ -f $$f.br ] || brotli -q 9 -k $$f; done; \
	  brotli -f -q 9 -k $(OUT)/web/texmacs-files.json

FORCE:

# a variant for node, which sees the host file system as it is (NODERAWFS)
# and takes its environment: to test the build headless, see
# docs/wasm/README.md
node: $(OUT)/node/texmacs.js $(TM)/TeXmacs/SVNREV plugins-sync

# the version TeXmacs checks its files against (test_texmacs_path): made by
# configure, which a checkout of the sources (the CI) has not run; the node
# build reads the files of the source tree. That of a build with configure
# is left as it is
$(TM)/TeXmacs/SVNREV:
	sed -n 's/^#define ALTERNATIVE_VERSION "\(.*\)"/\1/p' $(W)/config.h > $@

# the plugins, which the build with configure copies from plugins/ (make at
# the top): a checkout of the sources has none in TeXmacs/ (and a lazy
# format of a plugin, looked for at the first conversion, then stopped it).
# Copied again at each build, so that a change of a plugin is packaged (only
# the files which changed, with their dates, by rsync)
plugins-sync: FORCE
	@mkdir -p $(TM)/TeXmacs/plugins
	@if command -v rsync > /dev/null; then \
	  rsync -a $(TM)/plugins/ $(TM)/TeXmacs/plugins/; \
	else cp -Rp $(TM)/plugins/. $(TM)/TeXmacs/plugins/; fi

$(OUT)/node/texmacs.js: $(OBJECTS) $(LIBDEPS) $(W)/node-pre.js
	@mkdir -p $(OUT)/node
	em++ $(LDFLAGS) -sNODERAWFS=1 -sENVIRONMENT=node --pre-js $(W)/node-pre.js \
	  $(OBJECTS) $(LIBS) -o $@

$(OUT)/texmacs.js: $(OBJECTS)
	@mkdir -p $(OUT)
	em++ $(LDFLAGS) $(OBJECTS) $(LIBS) -o $@

$(OBJ)/Plugins/Vue/%.o: $(S)/Plugins/Vue/%.cpp
	@mkdir -p $(dir $@)
	em++ $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -std=c++20 -c $< -o $@

$(HUNSPELL)/src/hunspell/hunspell.hxx:
	sh $(W)/get-hunspell.sh $(CURDIR)

# (the sources come with the header)
$(HUNSPELL)/src/hunspell/%.cxx: $(HUNSPELL)/src/hunspell/hunspell.hxx ;

$(HUNSPELL_OBJECTS): $(OBJ)/hunspell/%.o: $(HUNSPELL)/src/hunspell/%.cxx
	@mkdir -p $(dir $@)
	em++ $(CXXFLAGS) -DHUNSPELL_STATIC -c $< -o $@

$(OBJ)/Plugins/Ispell/ispell_hunspell.o: $(HUNSPELL)/src/hunspell/hunspell.hxx

$(OBJ)/%.o: $(S)/%.cpp
	@mkdir -p $(dir $@)
	em++ $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

$(OBJ)/%.o: $(S)/%.c
	@mkdir -p $(dir $@)
	emcc $(CPPFLAGS) $(DEPFLAGS) $(CFLAGS) -c $< -o $@

-include $(OBJECTS:.o=.d)

# ThorVG, built in the build directory when it is the default one and is
# not there yet; the objects need its header. Without the jobs of make in
# its environment: ninja joins them, which stopped a make -j ("read jobs
# pipe: Resource temporarily unavailable")
ifneq ($(THORVG_LIB),)
$(THORVG_LIB):
	@if [ "$(THORVG)" = "$(CURDIR)/thorvg/wasm" ]; then \
	  env -u MAKEFLAGS -u MFLAGS -u MAKELEVEL \
	    sh $(TM)/misc/thorvg/build-thorvg.sh $(CURDIR)/thorvg wasm; \
	else echo "no ThorVG in $(THORVG) (THORVG=none for a build without it)"; exit 1; fi
$(OBJECTS): | $(THORVG_LIB)
endif

# the flags of the compilation, kept in obj/.flags: an object is compiled
# again when they change (ThorVG on or off, say), which make does not see
# by itself (an object of a build without ThorVG was linked with ThorVG)
FLAGS_FILE := $(OBJ)/.flags
$(FLAGS_FILE): FORCE
	@mkdir -p $(OBJ); echo '$(CPPFLAGS) $(CXXFLAGS) $(CFLAGS) $(EH)' > $@.new; \
	  if cmp -s $@.new $@; then rm -f $@.new; else mv $@.new $@; fi
$(OBJECTS): $(FLAGS_FILE)

clean:
	rm -rf $(OBJ) $(OUT)

.PHONY: all web node clean tikzjax asymptote python r plugins-sync
