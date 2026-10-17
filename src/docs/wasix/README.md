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

## Running in the browser

    make -C build-wasix -f ../misc/wasix/Makefile web
    node misc/wasix/serve.mjs                 # http://localhost:8094/

The page (`misc/wasix/web`) runs TeXmacs in the Wasmer SDK (`@wasmer/sdk`
0.19.1, npm installed in `build-wasix/sdk` and copied with the page):
the program and its files make one package of the SDK
(`packages.create`), each run a sandbox of its own, with the document of
the text area in `/workspace/in.tm` and the PDF read back from
`/workspace/out.pdf`. The SDK runs WASIX in Web Workers sharing the memory
of the program: the page must be cross-origin isolated (`serve.mjs` sends
COOP and COEP).

The files (`pack-fs.py`, `texmacs-fs.bin` and its index, 32 MB): the boot
list of the Emscripten build, the Scheme code, the styles and packages,
the metrics and the Type 1 fonts; not the TrueType and OpenType fonts (75
MB), nor the documentation.

In headless Chrome 154 (`node misc/wasix/test/browser-pdf.mjs <tools>`,
with the puppeteer-core and Chrome for Testing of the Emscripten build), the
page is ready in 0.9 s (local server) and a run, TeXmacs started and the
document typeset, takes 0.9 to 1.2 s. Under node, the same through the SDK:
`node misc/wasix/sdk-run.mjs build-wasix in.tm out.pdf`.

## Changes to the sources (all under `__wasi__`)

- `pipe_link`, `cmdline_link`: the programs of the plugins started by
  `posix_spawn` (`unix_spawn_shell` in `unix_sys_utils.cpp`), no `fork`.
- `unix_system_finished`: `waitpid` with `WNOHANG` instead of `waitid`.
- `unix_system.cpp`: `flock` (declared, not implemented) does nothing; the
  file systems of Wasmer give no permission bits, which `texmacs_stat` adds
  (otherwise TeXmacs found none of its files).
- `use_mupdf_pdf`: the MuPDF writer, as in the browser (no Ghostscript).
- `VUE_SDL_RENDERER` (SDL3_ttf) left out, as in the browser.
- `misc/wasix/include/wasix-compat.h`, included before each source:
  `sigsetjmp`/`siglongjmp` as `setjmp`/`longjmp`, the only names LLVM turns
  into exceptions (the errors of S7 were uncaught).

## State

| | |
|---|---|
| desktop, headless (`wasmer run`) | converts documents to PDF |
| browser and node (`@wasmer/sdk`) | converts documents to PDF |
| GUI | not yet: WASIX has no display |

Known: in the SDK (not under `wasmer run`) TeXmacs warns 44 times "cannot
load picture {}" (an icon it looks for and does not find).
