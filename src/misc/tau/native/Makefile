###############################################################################
# MODULE     : Makefile (Tau, native)
# DESCRIPTION: The core of Tau as a native program, and a shell for it
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# A trial (docs/tau-design.md, "On the desktop"): the same core as the one
# of the page, built for the machine, which talks to another process over
# pipes (src/Tau/tau_native.cpp), and a window of SDL which is that other
# process (tau_shell.cpp). From the top of the source tree:
#
#   mkdir -p build-tau-native
#   make -C build-tau-native -f ../misc/tau/native/Makefile -j8
#   build-tau-native/tau_shell
#
# MuPDF, FreeType and SDL3 are those of the system (PREFIX, Homebrew here).

TM       := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../../..)
S        := $(TM)/src
W        := $(TM)/misc/tau
N        := $(W)/native
PREFIX   ?= /opt/homebrew
OBJ      := obj

SOURCES  := $(shell grep -v '^\#' $(W)/sources.txt) Tau/tau_native.cpp
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
  Texmacs Texmacs/Data Plugins/Gnutls

CPPFLAGS := -I$(N) -include $(N)/config.h -I$(S) $(patsubst %,-I$(S)/%,$(INCLUDE_DIRS)) \
  -I$(TM)/TeXmacs/include -I$(PREFIX)/include -I$(PREFIX)/include/freetype2
OPT      ?= -O2
CFLAGS   := $(OPT) -w
CXXFLAGS := $(OPT) -w -std=c++17
DEPFLAGS := -MMD -MP
LIBS     := -L$(PREFIX)/lib -lmupdf -lmupdf-third -lfreetype -liconv -lz -lm

all: tau_core tau_shell

tau_core: $(OBJECTS)
	$(CXX) $(OPT) $(OBJECTS) $(LIBS) -o $@

tau_shell: $(N)/tau_shell.cpp
	$(CXX) $(OPT) -std=c++17 -I$(PREFIX)/include $< -L$(PREFIX)/lib -lSDL3 -o $@

$(OBJ)/%.o: $(S)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(DEPFLAGS) -c $< -o $@

$(OBJ)/%.o: $(S)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DEPFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ) tau_core tau_shell

-include $(OBJECTS:.o=.d)
.PHONY: all clean
