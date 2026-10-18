# TeXmacs on WASIX (branch `wip_wasix`)

An experiment: the WebAssembly port of `maxs_texmacs` (Vue GUI, S7, MuPDF),
built with [wasixcc](https://github.com/wasix-org/wasixcc) for
[WASIX](https://wasix.org) instead of Emscripten, to run

- on the desktop with the [Wasmer](https://wasmer.io) runtime (`wasmer run`),
- in the browser with the Wasmer JavaScript SDK (`@wasmer/sdk`).

The Emscripten build (`misc/wasm`, [`../wasm/README.md`](../wasm/README.md))
stays as it is; the WASIX build shares its list of sources and its
configuration (`misc/wasm/sources.txt`, `config.h`).

## Why WASIX

What Emscripten cannot give and WASIX can: processes (`posix_spawn`, pipes:
the plugins could run their programs, built for WASIX, as on the desktop),
threads, TCP sockets (on the desktop; in the browser over a WebSocket proxy),
a POSIX file system mounted from the host (Wasmer's `--volume`). Not
available: `fork` (in the configuration with the exceptions of WebAssembly),
`waitid`, `flock`, a display.

## Toolchain

    cargo install wasixcc
    ~/.cargo/bin/wasixccenv install-executables ~/.wasixcc/bin
    ~/.cargo/bin/wasixccenv download-all     # LLVM, sysroots, binaryen
    brew install wasmer                      # 7.5, Cranelift

The build uses the sysroot with the exceptions of WebAssembly (exnref):
`WASIXCC_WASM_EXCEPTIONS=yes`, needed for the C++ exceptions of TeXmacs and
the setjmp/longjmp of MuPDF and S7 (Wasmer 7.5 with Cranelift runs them).

## Building

    sh misc/wasix/build-deps.sh build-wasix          # MuPDF, SDL3, Hunspell
    make -C build-wasix -f ../misc/wasix/Makefile -j8

- MuPDF 1.28.5, the slim build of the browser build, with its patches;
  built by its own Makefile with `CC=wasixcc` (`OS=wasix`), as it is.
- SDL3 3.4.2 with its dummy video driver only, built as for Linux (SDL's
  CMake knows no WASI; `misc/wasix/wasix-toolchain.cmake`,
  `-DWASIX_SYSTEM_NAME=Linux`), with `misc/wasix/include` for what the
  sysroot lacks (`dlfcn.h`, the priorities of threads).
- TeXmacs: `misc/wasix/Makefile`, `out/texmacs.wasm` (31 MB, `-O2`).

## Running on the desktop

    wasmer run --volume $PWD/TeXmacs:/texmacs --volume $DIR:/work --cwd /work \
      --env TEXMACS_PATH=/texmacs --env TEXMACS_HOME_PATH=/work/home \
      --env HOME=/work/home build-wasix/out/texmacs.wasm -- \
      -headless -c /work/in.tm /work/out.pdf -q

turns a document into a PDF (the MuPDF writer) in about 7 s, most of it the
compilation of the module by Cranelift.

## The interface: a stream of pixels and events

WASIX has no display. With `TEXMACS_DISPLAY=stream`
(`src/Plugins/Vue/vue_stream.cpp`, the protocol at its top), TeXmacs runs
in single-window mode in a window of SDL's dummy video driver; after each
redraw it sends the tiles (64x64, those of a row joined) which changed on
its standard output, and it takes the pointer, the keyboard, the focus and
the size of the display from its standard input, as SDL events (the
standard input is polled when SDL's queue is empty, and wakes the loop).
What TeXmacs prints goes to the standard error, from the start of `main`.
A host shows the frames:

- on the desktop, `texmacs-wasix` (`misc/wasix/host`, native, SDL3), which
  runs `wasmer run` itself:

      make -C misc/wasix/host OUT=$PWD/build-wasix/out
      build-wasix/out/texmacs-wasix -tm $PWD/TeXmacs -wasm build-wasix/out/texmacs.wasm

  (the TeXmacs tree as `/texmacs`, `~/.TeXmacs-wasix` as the home, the
  current directory as `/work`);
- in the browser, the page `index.html` (below);
- for the tests, `misc/wasix/stream-client.mjs` under node
  (`test/stream-wasmer.mjs`: wasmer; `test/stream-sdk.mjs`: the SDK).

## Running in the browser

    make -C build-wasix -f ../misc/wasix/Makefile web
    node misc/wasix/serve.mjs                 # http://localhost:8094/

The page (`misc/wasix/web/index.html`, `texmacs-gui.js`) runs TeXmacs in
the Wasmer SDK (`@wasmer/sdk` 0.19.1, npm installed in `build-wasix/sdk`
and copied with the page): the program and its files make one package of
the SDK (`packages.create`), spawned in a sandbox with its standard input
and output piped; the page draws the rectangles in a canvas and sends the
events of the canvas (the keys as SDL scancodes and keycodes, US layout,
and their text). The SDK runs WASIX in Web Workers sharing the memory of
the program: the page must be cross-origin isolated (`serve.mjs` sends COOP
and COEP).

The files (`pack-fs.py`, `texmacs-fs.bin` and its index): the whole tree of
`TEXMACS_PATH` but the binaries of the plugins, 155 MB (with fewer files,
the icons were not found).

`pdf.html` is the headless use: the document of a text area typeset to PDF
by a TeXmacs started for it (0.9 to 1.2 s a run in Chrome). Under node:
`node misc/wasix/sdk-run.mjs build-wasix in.tm out.pdf`.

Tests in headless Chrome 154 with the puppeteer-core and Chrome for
Testing of the Emscripten build (`<tools>`, see `misc/wasm/browser-run.mjs`):
`node misc/wasix/test/browser-gui.mjs <tools> <out dir>` (the File menu, a
new document, text and a formula typed with the keyboard of the page; the
first frame 7.5 s after the page is opened, from a local server) and
`node misc/wasix/test/browser-pdf.mjs <tools>`.

## WebGPU

WASIX gives a program no GPU, the Wasmer SDK no way to add imports, and
Wasmer runs no components (so no `wasi:webgpu` of wasi-gfx either). The
WebGPU of WASIX goes over the stream instead:

- `misc/wasix/webgpu/webgpu_wire.cpp` implements `webgpu.h` (that of
  wgpu-native 29, `webgpu-native/webgpu-headers` 673658bc, fetched by
  `build-deps.sh`): each call becomes `[method, target, arguments, result]`
  in messages GPU (6) of the stream. The program numbers the objects and
  keeps what it may ask of them (sizes, formats); the adapter and the device
  are those of the host. Nothing comes back, so the calls flow one way (as
  Chrome sends a page's calls to its GPU process). Not done: mapping buffers
  (reading the GPU), errors back to the program.
- `misc/wasix/web/webgpu-host.js` makes the calls on the WebGPU of the page
  (the strings of the enumerated values made from `webgpu.h` by
  `gen-enums.mjs`). The program draws into a texture of its own, copied
  into the canvas when it presents (a frame comes in several tasks of the
  page, and the texture of a canvas is gone at the end of one).
- ThorVG's WebGPU engine builds for WASIX unchanged
  (`build-thorvg.sh <prefix> wasix`, against this `webgpu.h`).
- `src/Plugins/Vue/gpu_backend_wg.cpp`: a backend of the GPU renderer of
  the Vue port (beside OpenGL and Metal): the shaders of the GL backend in
  WGSL, the canvas RGBA8 as the targets, one render pass per run of draws
  into a target, the vertices and uniforms of a submission written at once,
  ThorVG for the shapes (its targets BGRA8, the format of its last blit).
  The page asks for it (`TEXMACS_VUE_GPU=webgpu` when it has WebGPU;
  `?gpu=0` for the pixels of MuPDF).
- The loop draws every window at every turn and presents a frame only when
  it differs from the last one: the calls of a frame are held
  (`wgpuWireHold`) and dropped when it is not presented and drew nothing
  but the screen; the shapes which ThorVG drew in a box are kept (by a hash
  of the shapes), so that the frames of an idle window draw no new shape.
  Idle: 0.15 MB in 5 s (the cursor) instead of 3.2 MB.

Tests (headless Chrome, which has WebGPU on Metal here):
`node misc/wasix/test/browser-webgpu.mjs <tools> <out> [hello-triangle.wasm|thorvg-shapes.wasm] [frames]`
(test programs: a triangle; ThorVG's shapes, gradients, strokes) and
`browser-gui.mjs` (TeXmacs: 59 frames, 49k calls, no WebGPU error; 15 MB
for the session against 13.5 MB of pixels, most of it the first uploads of
the atlas and the icons).

## Changes to the sources (all under `__wasi__`)

- `pipe_link`, `cmdline_link`: the programs of the plugins started by
  `posix_spawn` (`unix_spawn_shell` in `unix_sys_utils.cpp`), no `fork`.
- `unix_system_finished`: `waitpid` with `WNOHANG` instead of `waitid`.
- `unix_system.cpp`: `flock` (declared, not implemented) does nothing; the
  file systems of Wasmer give no permission bits, which `texmacs_stat` adds
  (otherwise TeXmacs found none of its files).
- `use_mupdf_pdf`: the MuPDF writer, as in the browser (no Ghostscript).
- `VUE_SDL_RENDERER` (SDL3_ttf) left out, as in the browser.
- The GPU renderer: `gpu_backend_wg.cpp` (WebGPU, chosen by
  `TEXMACS_VUE_GPU=webgpu`), the GL backend left out (`USE_THORVG` means
  ThorVG's WebGPU engine here); `begin_frame`/`skip_frame` of the backends
  (nothing for GL and Metal), `vue_gpu_skip` for a frame not presented.
- `misc/wasix/include/wasix-compat.h`, included before each source:
  `sigsetjmp`/`siglongjmp` as `setjmp`/`longjmp`, the only names LLVM turns
  into exceptions (the errors of S7 were uncaught).

## State

| | |
|---|---|
| desktop, headless (`wasmer run`) | converts documents to PDF |
| browser and node (`@wasmer/sdk`) | converts documents to PDF |
| GUI on the desktop (`texmacs-wasix`) | runs: menus, editing |
| GUI in the browser (`index.html`) | runs: menus, editing; drawn by the page's WebGPU (or pixels, `?gpu=0`) |

Not yet: a pixel density above 1 (blurry on HiDPI displays); compression
of the stream (raw pixels: 6 MB for the first frames at 1000x700; the
batches of WebGPU too); WebGPU on the desktop host (native wgpu or Dawn
making the calls); reading the GPU back (snapshots of the GPU renderer); the
home of the page in the browser's storage (it lives in the memory of the
sandbox); the clipboard, files from and to the page; the browser's
keyboard layouts beyond the text they type; the plugins' programs (WASIX
packages for them).
