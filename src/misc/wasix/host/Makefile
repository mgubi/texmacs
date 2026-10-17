# The desktop host of the WASIX build (texmacs-wasix.c), a native program
# with SDL3 (pkg-config sdl3):
#
#   make -C misc/wasix/host OUT=$PWD/build-wasix/out
#   build-wasix/out/texmacs-wasix -tm $PWD/TeXmacs -wasm build-wasix/out/texmacs.wasm

OUT ?= .
CC  ?= cc

$(OUT)/texmacs-wasix: texmacs-wasix.c
	$(CC) -O2 -Wall -o $@ $< $$(pkg-config --cflags --libs sdl3) -lpthread
