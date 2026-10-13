###############################################################################
# MODULE     : Makefile (Tau, native)
# DESCRIPTION: The desktop program of Tau: Vue, with the core in a process
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# The desktop program of Tau (docs/tau-design.md, "On the desktop"): Vue,
# the GUI of TeXmacs on SDL, with the rest of TeXmacs as usual and the
# switch TAU_SHELL (config.h), for the interface; the editor is tau_core
# (misc/tau/native/Makefile), which it starts. From the top of the tree:
#
#   mkdir -p build-tau-shell
#   make -C build-tau-shell -f ../misc/tau/shell/Makefile -j8
#   build-tau-shell/tau_shell
#
# The shell finds the files of TeXmacs (../TeXmacs from where it is) and
# the core (../build-tau-native/tau_core, which this Makefile builds too)
# by itself, and keeps its settings in ~/.TauTeXmacs; TEXMACS_PATH,
# TAU_CORE and TEXMACS_HOME_PATH say otherwise.
#
# MuPDF, FreeType and SDL3 are those of the system (PREFIX, Homebrew here).

TM       := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../../..)
S        := $(TM)/src
W        := $(TM)/misc/tau
N        := $(W)/shell
PREFIX   ?= /opt/homebrew
OBJ      := obj

SOURCES  := $(filter-out Tau/%,$(shell grep -v "^\#" $(W)/sources.txt)) Plugins/Vue/vue_gui.cpp Plugins/Vue/vue_widget.cpp Plugins/Vue/vue_gpu.cpp Plugins/Vue/clay.c Tau/tau_shell.cpp
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
CXXFLAGS := $(OPT) -w -std=c++20
DEPFLAGS := -MMD -MP
LIBS     := -L$(PREFIX)/lib -lmupdf -lmupdf-third -lfreetype -liconv -lz -lm -lSDL3 -lSDL3_ttf -framework Cocoa

all: tau_shell core

# the core, which the shell starts (it looks for it in ../build-tau-native)
core:
	@mkdir -p ../build-tau-native
	$(MAKE) -C ../build-tau-native -f $(W)/native/Makefile tau_core

tau_shell: $(OBJECTS)
	$(CXX) $(OPT) $(OBJECTS) $(LIBS) -o $@

$(OBJ)/%.o: $(S)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(DEPFLAGS) -c $< -o $@

$(OBJ)/%.o: $(S)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DEPFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ) tau_shell

-include $(OBJECTS:.o=.d)
.PHONY: all clean core
