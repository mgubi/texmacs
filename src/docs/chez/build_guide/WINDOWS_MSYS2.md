# Building TeXmacs (Chez backend) on Windows with MSYS2

> **Status: builds and runs.** Verified on Windows x86_64 with MSYS2 UCRT64,
> Qt 6.12.0 and Chez Scheme 10.4.1 built from source with `--machine=ta6nt`.
> Phases A–E are confirmed end to end, including the CMake-driven Scheme module
> build (`BUILD_CHEZ_MODULES`) and the optional `texmacs.boot` AOT image.
> Phase F is a concrete plan, not yet an automated target.

Target: MinGW-w64 / UCRT64. Native MSVC is **not** a viable target — MSYS2's
Qt 6 is MinGW-ABI, so the C++ ABI forces the whole build into one ecosystem,
and this repository's `WIN32` CMake branches already assume MinGW.

---

## Phase A — install packages

Use an **UCRT64** shell (`C:\msys64\ucrt64.exe`), not the MSYS shell; the MSYS
shell puts the wrong compiler first in `PATH`.

```bash
pacman -Syu          # may ask you to close and reopen the terminal, then rerun
pacman -S --needed base-devel \
  mingw-w64-ucrt-x86_64-toolchain \
  mingw-w64-ucrt-x86_64-cmake mingw-w64-ucrt-x86_64-ninja \
  mingw-w64-ucrt-x86_64-qt6-base mingw-w64-ucrt-x86_64-qt6-svg \
  mingw-w64-ucrt-x86_64-qt6-5compat \
  mingw-w64-ucrt-x86_64-freetype mingw-w64-ucrt-x86_64-libjpeg-turbo \
  mingw-w64-ucrt-x86_64-libpng mingw-w64-ucrt-x86_64-gmp \
  mingw-w64-ucrt-x86_64-gnutls mingw-w64-ucrt-x86_64-sqlite3 \
  mingw-w64-ucrt-x86_64-zlib mingw-w64-ucrt-x86_64-lz4 \
  mingw-w64-ucrt-x86_64-ncurses mingw-w64-ucrt-x86_64-dlfcn \
  rsync
```

The last two are the non-obvious ones:

| Package | Version | Why |
|---|---|---|
| `mingw-w64-ucrt-x86_64-ncurses` | 6.6-4 | `-lncurses`, needed by Chez's `expeditor.o` |
| `mingw-w64-ucrt-x86_64-dlfcn` | 1.4.2-1 | `-ldl` |
| `mingw-w64-ucrt-x86_64-qt6-5compat` | 6.12.0-1 | `Qt6::Core5Compat`, required by `CMakeLists.txt:670` |
| `rsync` | MSYS2 package | used by `packages/windows/make-bundle.sh` to stage the data tree |

`ntldd` (`mingw-w64-ucrt-x86_64-ntldd`) is needed by `copydll.sh`, and Inno
Setup 6/7 provides `ISCC.exe` for the installer; both are only required for
Phase F packaging, not for the A–E build.

MSYS2's Qt is **6.12.0**, the same version the macOS build is validated
against, so there is no version drift on this platform.

---

## Phase B — build Chez Scheme (the gate)

```bash
mkdir -p ~/build && cd ~/build
curl -LO https://github.com/cisco/ChezScheme/releases/download/v10.4.1/csv10.4.1.tar.gz
tar xf csv10.4.1.tar.gz && cd csv10.4.1
./configure --threads --machine=ta6nt
make -j"$(nproc)"
```

`ta6nt` is the x86_64 Windows machine type.

**Checkpoint.** The directory holding the boot images and kernel is
`ta6nt/boot/ta6nt/` — note the machine name appears twice, once for the build
directory and once for the machine. It must contain:

```text
libkernel.a  scheme.h  petite.boot  scheme.boot
```

If `make` produces `libkernel.lib` instead of `libkernel.a`, you have landed on
the MSVC build path. That is still usable — MinGW can link it, because Chez's
embedding API is plain C — but confirm the boot images are present.

If `configure` or `make` fails outright, fall back to the official MSVC build
(`cd wininstall && make` under a Visual Studio prompt) and point `CHEZ_ROOT` at
its output directory.

---

## Phase C — configure and build TeXmacs

```bash
cd /c/path/to/texmacs        # the wip-chez checkout
cmake -S . -B build-win -G Ninja \
  -DTEXMACS_GUI=Qt6 -DSCHEME_IMPL=chez \
  -DCHEZ_ROOT="$(cygpath -m ~/build/csv10.4.1/ta6nt/boot)" \
  -DCHEZ_MACHINE=ta6nt \
  -DCHEZ_SCHEME_EXECUTABLE="$(cygpath -m ~/build/csv10.4.1/ta6nt/bin/ta6nt/scheme.exe)" \
  -DCMAKE_BUILD_TYPE=Release
cmake --build build-win -j"$(nproc)"
```

`cmake --build` now performs **both** stages: it links `texmacs.exe` and then
runs the Scheme module target (`CMakeLists.txt:1018-1075`), which transpiles and
compiles all 453 modules into `build-win/boot/` and finishes with
`Failures: 0`. A C++-only build is still possible with
`-DBUILD_CHEZ_MODULES=OFF`.

| Option | Default | Effect |
|---|---|---|
| `BUILD_CHEZ_MODULES` | `ON` | Adds the `--compile-modules` custom command plus an `ALL` target (`texmacs-chez-modules`, or `texmacs-chez-boot` when the boot image is enabled). |
| `CHEZ_SCHEME_EXECUTABLE` | auto-detected | Path to a Chez `scheme` executable. When found, the build also assembles `build-win/boot/texmacs.boot` (native VFASL image, ~17 MB) via `src/Scheme/Chez/make-boot.scm`. Otherwise CMake prints `Chez Scheme executable not found; skipping texmacs.boot generation` and only produces loose `.so` modules. |

The Windows source build puts the executable at
`ta6nt/bin/ta6nt/scheme.exe` (the boot images stay in `ta6nt/boot/ta6nt/`).
Pass it explicitly so the fallback `find_program` cannot pick up an unrelated
`scheme` from `PATH`. `make-boot.scm` uses its built-in
`*startup-boot-modules*` list, so `boot-order-full.scm` is not required unless
`TEXMACS_BOOT_FULL=1`.

On Windows the CMake target is `texmacs`, **not** `TeXmacs` (see
`src/CMakeLists.txt:5-11`). The new CMake block resolves it through
`TEXMACS_CHEZ_EXE_TARGET` (`TeXmacs` on Apple, `texmacs` on WIN32, `texmacs.bin`
elsewhere). Hard-coding `TeXmacs` makes generation fail with
`No target "TeXmacs"`.

`cygpath -m` converts `/c/...` into `C:/...`, which CMake handles more
reliably on Windows. `CHEZ_ROOT` must be the directory that **contains** the
machine directory, because `CHEZ_MACHINE` is appended to it. The Windows source
build puts the kernel under `ta6nt/boot/ta6nt/`, so `CHEZ_ROOT` is
`~/build/csv10.4.1/ta6nt/boot`: `CHEZ_ROOT` + `ta6nt` resolves to
`ta6nt/boot/ta6nt/libkernel.a`. Pointing it at plain `~/build/csv10.4.1/ta6nt`
yields the doubled `ta6nt/ta6nt/libkernel.a` and CMake aborts with
"Chez Scheme kernel library not found".

Unlike macOS, no `QT_ADDITIONAL_PACKAGES_PREFIX_PATH` is needed — MSYS2
installs every Qt 6 module into a single prefix.

---

## Phase D — full Scheme recompile (optional)

Phase C already compiled the modules. Use the manual invocation when you want a
clean rebuild of every module — the incremental compiler trusts mtimes, and
cached `.so` files carry FASL descriptors that can mismatch after C++ or
compiler changes — or when debugging the compiler itself:

```bash
cd /c/path/to/texmacs
SCHEMEHEAPDIRS="$(cygpath -m ~/build/csv10.4.1/ta6nt/boot/ta6nt)" \
HOME="$PWD/fakehome" \
TEXMACS_PATH="$PWD/TeXmacs" \
TEXMACS_BOOT_PATH="$PWD/build-win/boot" \
TEXMACS_HOME_PATH="$PWD/build-win/.compile-home" \
./build-win/src/texmacs.exe -recompile-modules
```

For a quick incremental check use `-compile-modules` instead. On Windows Chez
loads shared objects through `LoadLibrary`, which accepts the `.so` suffix for
a PE image, so the Unix suffix hardcoded in `boot-chez.scm` and
`compile-texmacs.scm` works in practice.

`TEXMACS_BOOT_PATH` is what tells the module compiler where to write.
`compile-texmacs.scm` used to hardcode `build-texmacos/boot`, so a tree named
`build-win` silently produced modules in the wrong place; it now honours the
variable, mirroring how `boot-chez.scm` resolves modules at runtime.

**Assignment order matters.** `SCHEMEHEAPDIRS` must be written *before*
`HOME`, because the shell expands `$HOME` using the value in effect before any
of these assignments apply. Put `HOME="$PWD/fakehome"` first and
`SCHEMEHEAPDIRS="$HOME/..."` resolves to `<repo>/fakehome/chez/...`, giving:

```text
cannot open boot file .../fakehome/chez/lib/csv10.4.1/ta6nt/petite.boot
```

Use absolute paths (or a variable set beforehand) for anything derived from
`$HOME`.

---

## Phase E — run

```bash
SCHEMEHEAPDIRS="$(cygpath -m ~/build/csv10.4.1/ta6nt/boot/ta6nt)" \
HOME="$PWD/fakehome" \
TEXMACS_PATH="$PWD/TeXmacs" \
TEXMACS_BOOT_PATH="$PWD/build-win/boot" \
./build-win/src/texmacs.exe
```

Same assignment-order rule as Phase D: `SCHEMEHEAPDIRS` before `HOME`, or it
resolves inside `fakehome` and the boot fails.

For a non-interactive smoke test (no window), use `-H -x`:

```bash
SCHEMEHEAPDIRS="$(cygpath -m ~/build/csv10.4.1/ta6nt/boot/ta6nt)" \
HOME="$PWD/fakehome" \
TEXMACS_PATH="$PWD/TeXmacs" \
TEXMACS_BOOT_PATH="$PWD/build-win/boot" \
./build-win/src/texmacs.exe -H \
  -x '(begin (display "BOOT_OK: ") (display (texmacs-version)) (newline) (quit-TeXmacs))'
```

Expected output includes `BOOT_OK: 2.1.5`. If `build-win/boot/texmacs.boot`
was generated, startup logs `Chez Scheme: booting from boot file ...` and loads
the single image instead of the 453 loose `.so` files. Set `TEXMACS_NO_BOOT=1`
to force the loose-module path for comparison.

`TEXMACS_BOOT_PATH` is mandatory. Without it the runtime loads raw `.scm`
sources instead of the compiled modules, producing `invalid context for
definition` from `boot-chez.scm` followed by `variable current-buffer is not
bound` from `init-buffer.scm`. The window may still open, so check the log.

Binary name by platform (`src/CMakeLists.txt:5-11`):

| Platform | Name | Location |
|---|---|---|
| macOS | `TeXmacs` | `build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs` |
| Windows | `texmacs` | `build-win/src/texmacs.exe` |
| Linux | `texmacs.bin` | `build-texmacos/src/texmacs.bin` |

---

## Phase F — plan for a distributable package

Phases A–E produce a binary that runs **from the build tree with environment
variables set**. That is not a bundle. There is no automated CMake packaging
target yet, but three bodies of prior art cover almost all of it:

| Source | What to reuse | Caveat |
|---|---|---|
| `packages/macos/make-bundle.sh` (this tree) | Overall shape: preflight, versioned staging dir that is never deleted, slim vs `--full-progs` payload, `SVNREV`, deploy Qt before copying the `.so` modules, transitive dependency closure, Chez heap discovery from `CMakeCache.txt`, clean-room smoke test | macOS-only mechanics (`macdeployqt`, `otool`, `install_name_tool`, `codesign`, `.app`/`.dmg`) need Windows equivalents |
| [`slowphil/mingw-w64-texmacs`](https://github.com/slowphil/mingw-w64-texmacs) | `PKGBUILD:package()` is a working Windows bundle: `dlls_for_exes()` transitive DLL closure, Qt plugin set (`generic`, `imageformats`, `platforms/qwindows.dll`), `gswin32c.exe` → `gs.exe` rename, Hunspell dictionaries, Inno Setup via `ISCC.exe`, and a 7z SFX installer | Guile 1.8 / Qt4-5 / autotools / MinGW32 — not Chez/Qt6/CMake. Reuse the bundling skeleton, not its build steps |
| [`slowphil/texmacs-win-builder`](https://github.com/slowphil/texmacs-win-builder) | `build-tm.sh` + `pack_texmacs-win-sdk-installer.sh`: one-command SDK → build → installer, with a modified 7z SFX | Same dated toolchain; useful as a CI/one-click wrapper model |

`packages/windows/copydll.sh` in this tree is the modern `ntldd -R` replacement
for `dlls_for_exes()` from the PKGBUILD, so keep using it.

The port now exists as **`packages/windows/make-bundle.sh`** — run it from an
UCRT64 bash shell:

```bash
./packages/windows/make-bundle.sh --verify
```

Flags: `--build-dir` (default `build-win`), `--version`, `--full-progs`,
`--no-qt`, `--no-installer`, `--iscc /path/to/ISCC.exe`, and `--verify`
(headless smoke test of the staged tree). It stages into
`../distr/TeXmacs-chez-<version>/`, renders `TeXmacs.iss.in`, compiles the
installer with `ISCC.exe`, and refuses to overwrite an existing stage.
The steps below describe what the script does.

### Step 1 — preflight and stage the tree

Mirror `make-bundle.sh:62-105`: assert `<build>/src/texmacs.exe`, a non-empty
`<build>/boot/`, `TeXmacs/SVNREV`, and (in slim mode) `<build>/boot/texmacs.boot`.
Use a **versioned** output dir such as `../distr/TeXmacs-chez-$VERSION/` and
refuse to overwrite an existing one — the macOS script never deletes anything.

```bash
STAGE=../distr/TeXmacs-chez-$VERSION
mkdir -p "$STAGE/bin"
cp build-win/src/texmacs.exe "$STAGE/bin/"
```

For the data tree, rsync the directories the macOS script lists at
`make-bundle.sh:221-225` — `fonts styles texts langs misc doc examples packages
include` — plus `progs/` and `plugins/`, with
`--exclude '.git*' --exclude '*.o' --exclude '*.so'`. `TeXmacs/SVNREV` is generated by CMake
at configure time and is part of the data tree; startup refuses the
installation without it ("does not contain a valid TeXmacs installation").

### Step 2 — choose the Scheme payload

Both modes are verified to boot; port the macOS script's `--full-progs` switch
(`make-bundle.sh:190-269`):

- **Slim / AOT** (default): ship `texmacs.boot` (~17 MB), `module-deps.scm` and
  `init-texmacs-boot-file.so`, plus only the sources still read from disk —
  `init-texmacs-boot-file.scm`, `init-buffer.scm`, `tm-mode.el`, `progs/forms`,
  `progs/client/forms`, `progs/server`. Get the resident-module exclusion list
  from the compiler itself (`make-bundle.sh:260`):
  ```bash
  scheme.exe --script src/Scheme/Chez/make-boot.scm --print-startup-excludes
  ```
  and feed it to rsync as an exclusion file — never copy the resident modules
  in the first place:
  ```bash
  scheme.exe --script src/Scheme/Chez/make-boot.scm --print-startup-excludes \
    > excludes.txt
  rsync -a --prune-empty-dirs \
        --exclude-from=excludes.txt \
        --exclude '*-test.so' --exclude '/check/' --exclude '/utils/test/' \
        --exclude 'texmacs-user.so' \
        --include '*/' --include '*.so' --include 'texmacs.boot' \
        --include 'module-deps.scm' --include 'init-texmacs-boot-file.so' \
        --exclude '*' \
        "$SRC_BOOT/" "$STAGE/boot/"
  ```
  Startup logs `Chez Scheme: booting from boot file ...`.
- **Full** (`--full-progs`): copy all 453 `.so` modules and the
  compiler/linter scripts from `src/Scheme/Chez`, and set `TEXMACS_NO_BOOT=1`
  in the launcher. This is the fallback if the AOT image misbehaves on a target
  machine.

### Step 3 — Chez heap images

```bash
mkdir -p "$STAGE/chez"
cp ~/build/csv10.4.1/ta6nt/boot/ta6nt/{petite.boot,scheme.boot} "$STAGE/chez/"
```

These are architecture-bound (`ta6nt` here). Note the path: on Windows the
artifacts live under `ta6nt/boot/ta6nt/`, not the macOS-style `boot/ta6nt/`.
Lift the heap-discovery logic from `make-bundle.sh:299-360`: read `CHEZ_ROOT`
and `CHEZ_MACHINE` from `build-win/CMakeCache.txt`, probe
`$CHEZ_ROOT/$CHEZ_MACHINE`, `$CHEZ_ROOT/boot/$CHEZ_MACHINE` and `$CHEZ_ROOT`,
then fall back to the `bin/ta6nt` directory beside the `scheme.exe` you passed
as `CHEZ_SCHEME_EXECUTABLE`. Fail preflight if neither `petite.boot` nor
`scheme.boot` is found — the bundle cannot start without them.

### Step 4 — Qt runtime

```bash
windeployqt --release --no-translations "$STAGE/bin/texmacs.exe"
```

This is the Windows `macdeployqt` analogue. In the actual script, run it right
after staging the executable — i.e. before Step 2 copies the Scheme `.so`
modules — so the deployer does not scan hundreds of Chez FASL files
(`make-bundle.sh:142-188`). Copy the Qt
plugins that `PKGBUILD:package()` lists — `generic`, `imageformats` — plus
`platforms/qwindows.dll`; without `qwindows.dll` the GUI exits silently.

### Step 5 — MinGW DLL closure

```bash
pacman -S mingw-w64-ucrt-x86_64-ntldd   # once
packages/windows/copydll.sh "$STAGE/bin/texmacs.exe"
```

`copydll.sh` walks `ntldd -R` recursively and copies only libraries under the
MinGW prefix, leaving system DLLs alone. It is the successor of
`dlls_for_exes()` in `PKGBUILD:package()` (which did the same via `objdump -p`).
`windeployqt` does **not** copy `libgcc_s_seh-1.dll`, `libstdc++-6.dll` or
`libwinpthread-1.dll`. Run the closure pass on any extra executables you adopt
from slowphil's bundle (`gswin32c.exe`/`gs.exe`, `pdftocairo.exe`,
`rsvg-convert.exe`, `magick.exe`, `hunspell.exe`, `wget.exe`), and copy their
Qt plugins and Hunspell dictionaries as that PKGBUILD does if those features
are wanted. None of them are needed for the basic Chez/Qt6 build.

### Step 6 — launcher (required on Windows)

The `__env`-style env vars set by an installer are only seen by processes
started *after* Explorer refreshes its environment (log off/on, or restart
Explorer). To make the installed build launch correctly immediately, the
Start-menu/desktop shortcuts point at `TeXmacs-chez.bat` rather than
`texmacs.exe` directly: the batch file sets `TEXMACS_PATH`,
`SCHEMEHEAPDIRS`, `TEXMACS_BOOT_PATH` and `TEXMACS_HOME_PATH` in the launched
process itself. The `.iss` registry values are still written for other
launchers. Config is isolated from an existing `%APPDATA%\TeXmacs`:

```bat
@echo off
set SCHEMEHEAPDIRS=%~dp0..\chez
set TEXMACS_PATH=%~dp0..
set TEXMACS_BOOT_PATH=%~dp0..\boot
start "" "%~dp0texmacs.exe" %*
```

Adjust the relative paths to the final layout. The long-term fix is a small
`WIN32` arm in that `#if` pointing at `<exe>/chez` or `<exe>/../share/chez`;
after that the launcher only needs `TEXMACS_PATH` / `TEXMACS_BOOT_PATH`, and
the bundle behaves like the macOS `.app`, which needs no wrapper at all
(`make-bundle.sh:23-27`).

### Step 7 — installer

Two working routes, both demonstrated by slowphil:

- **Inno Setup**: retarget `packages/windows/TeXmacs.iss.in` from the autotools
  `distr/` layout to the CMake tree, then run `ISCC.exe`. This is what
  `PKGBUILD:package()` ends with.
- **7z SFX (portable single-file installer)**: concatenate `7zSD.sfx`, a
  `;!@Install@!UTF-8!` config block and the 7z archive, as
  `PKGBUILD:package()` and `texmacs-win-builder/pack_texmacs-win-sdk-installer.sh`
  do. `texmacs-win-builder/build-tm.sh` shows how to wrap build + install into
  a one-click SDK.

MSIX: `packages/msix/*.in` manifests are driven by the autotools
`WINDOWS_APPX_*` targets and need the same retargeting.
Icons/banners: `packages/windows/resource.rc.in`, `TeXmacs.ico`,
`TeXmacs-large.bmp`, `TeXmacs-small.bmp`, `topbanner.bmp` are usable as-is.

### Step 8 — verify from a clean environment

Mirror the macOS script's final instructions (`make-bundle.sh:543-554`):
launch with `env -u TEXMACS_PATH -u TEXMACS_BOOT_PATH -u SCHEMEHEAPDIRS -u TEXMACS_HOME_PATH` (on
Windows, unset them outside the launcher so the launcher is the only thing
setting them). Check version 2.1.5, no `LOAD-ERROR` / `Exception` in the log,
menus and fonts render. With the AOT payload, confirm the "booting from boot
file" log line. Inspect the final binaries with `ntldd -R` to make sure no DLL
resolves back into the MSYS2 prefix.

### Bundle contents checklist

| Component | Source | Notes |
|---|---|---|
| `texmacs.exe` | `build-win/src/texmacs.exe` | |
| Qt DLLs + plugins | `windeployqt` | needs `platforms/qwindows.dll` or it exits silently |
| MinGW runtime | `libgcc_s_seh-1.dll`, `libstdc++-6.dll`, `libwinpthread-1.dll` | not copied by `windeployqt` |
| Data tree | `TeXmacs/{progs,fonts,styles,doc,plugins}` | plus generated `SVNREV` |
| Scheme payload | `build-win/boot/` (453 `.so` + `module-deps.scm`) or `texmacs.boot` | AOT image ~17 MB; `boot-order-full.scm` not needed |
| Chez heap images | `ta6nt/boot/ta6nt/{petite.boot,scheme.boot}` | architecture-bound, `ta6nt` only |
| Launcher | `TeXmacs-chez.bat` | sets env vars until WIN32 auto-detect lands |
| Config | `%APPDATA%\TeXmacs-chez` | isolated from the classic `%APPDATA%\TeXmacs` |

## Troubleshooting

| Error | Cause | Fix |
|---|---|---|
| `cannot find -ldl` | dlfcn not installed | `pacman -S mingw-w64-ucrt-x86_64-dlfcn` |
| `Chez Scheme: bootstrap file not found` for a file that exists | `url("C:/…")` treats the drive colon as a protocol separator and builds the relative url `C/msys64/…` | fixed: `url_system()` instead of `url()` in `chez_tm.cpp` (four call sites: boot-file env, `TEXMACS_PATH/boot` probe, `texmacs.boot` probe, bootstrap existence check) |
| CMake generate error `No target "TeXmacs"` | new Scheme block hard-coded the Apple target name; WIN32 target is `texmacs` | use `TEXMACS_CHEZ_EXE_TARGET` in `CMakeLists.txt` |
| `Chez Scheme executable not found; skipping texmacs.boot generation` | no `scheme.exe` found by `find_program` | pass `-DCHEZ_SCHEME_EXECUTABLE="$(cygpath -m ~/build/csv10.4.1/ta6nt/bin/ta6nt/scheme.exe)"`; loose `.so` modules still work without it |
| `variable getlogin is not bound` while loading `gpg-base`/`gpg-widgets` | Chez has no `getlogin`; `db-users.scm` calls it on every platform except macOS | fixed: shim in `kernel/boot/chez-compat.scm` |
| `cannot rename "…compiler-db.fasl.tmp" … file exists` | Windows `rename` refuses to replace an existing file; POSIX `rename(2)` does | fixed: unlink the destination first in `compiler-db.scm` |
| `cannot find -lncurses` | ncurses not installed | `pacman -S mingw-w64-ucrt-x86_64-ncurses` |
| `cannot find -lpthread` | wrong shell | use the UCRT64 shell, not MSYS |
| `cannot find -lm` | MinGW folds libm into the CRT | drop `m` from `CMakeLists.txt:883` |
| `Chez Scheme kernel library not found` | `CHEZ_ROOT`/`CHEZ_MACHINE` wrong | must be `ta6nt`, and `libkernel.*` must exist |
| Stray console window | subsystem not set to GUI | check `WIN32_EXECUTABLE` on the target |
| Installed build crashes / exits silently with a fresh env | stale `%APPDATA%\TeXmacs-chez` cache written by an earlier broken launch | rename `%APPDATA%\TeXmacs-chez` to `TeXmacs-chez.bak` and relaunch; TeXmacs regenerates it |

---

## Verification checklist

1. `ta6nt/boot/ta6nt/` has `libkernel.a`, `scheme.h`, `petite.boot`, `scheme.boot`.
2. `cmake --build build-win` links without unresolved symbols.
3. The build's `texmacs-chez-modules` target reports `Failures: 0` (453 modules),
   or `texmacs-chez-boot` reports `Created AOT boot image ... texmacs.boot`.
4. Headless `-H -x` smoke test prints `BOOT_OK: 2.1.5` and
   `(top-level-bound? 'current-buffer)` = `#t`.
5. No `LOAD-ERROR` / `Exception` lines in the log.

Expected pre-existing failure, present on every platform: `buffer-export`
returns `Error: bad format or data`. Tracked in `TODO_CHEZ.md`, unrelated to
the platform.
