# Building & Packaging TeXmacs (Chez Backend) on macOS

Verified on macOS arm64 (Apple Silicon) with Homebrew **Chez Scheme 10.4.1** and **Qt 6.12.0**.

---

## 1. Prerequisites

```bash
brew install cmake ninja pkg-config qt chezscheme freetype jpeg libpng gnutls gmp
```

| Component | Homebrew Path |
|---|---|
| Chez Scheme 10.4.1 | `/opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1/tarm64osx` |
| Qt 6.12.0 | `/opt/homebrew/opt/qtbase`, `/opt/homebrew/opt/qt5compat`, `/opt/homebrew/opt/qtsvg` |

---

## 2. Configure & Build

```bash
cd texmacs
cmake -S . -B build-texmacos -G Ninja \
  -DTEXMACS_GUI=Qt6 \
  -DSCHEME_IMPL=chez \
  -DCHEZ_ROOT=/opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1 \
  -DCHEZ_MACHINE=tarm64osx \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_PREFIX_PATH="/opt/homebrew/opt/qtbase;/opt/homebrew/opt/qt5compat;/opt/homebrew/opt/qtsvg" \
  -DQT_ADDITIONAL_PACKAGES_PREFIX_PATH="/opt/homebrew/opt/qtbase;/opt/homebrew/opt/qt5compat;/opt/homebrew/opt/qtsvg"
cmake --build build-texmacos -j 10
```

**Common configure pitfalls:**
- **Chez defaults:** `CMakeLists.txt` defaults `CHEZ_ROOT` to an older pre-release path and `CHEZ_MACHINE` to `tarm64le`. Always override both (`tarm64osx` on Apple Silicon).
- **Qt6 components:** `Qt6Config.cmake` lives in `qtbase`, while `Svg` and `Core5Compat` are separate kegs. `QT_ADDITIONAL_PACKAGES_PREFIX_PATH` must list all three.
- **Scheme modules:** `BUILD_CHEZ_MODULES=ON` (default) compiles all 453 `.so` modules and generates `build-texmacos/boot/texmacs.boot`. Pass `-DBUILD_CHEZ_MODULES=OFF` to build only the C++ binary.

---

## 3. Run from the Build Tree

```bash
SCHEMEHEAPDIRS="/opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1/tarm64osx" \
HOME="$PWD/fakehome" \
TEXMACS_PATH="$PWD/TeXmacs" \
TEXMACS_BOOT_PATH="$PWD/build-texmacos/boot" \
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs
```

- `SCHEMEHEAPDIRS`: Points Chez to `petite.boot` and `scheme.boot` matching the linked `libkernel.a`.
- `HOME`: Using `fakehome` keeps your real home directory clean during testing.
- `TEXMACS_BOOT_PATH`: Required when running from the build tree so TeXmacs finds `texmacs.boot`, `.so` modules, and `module-deps.scm`.
- **Clean Scheme rebuild:** Pass `-recompile-modules` to rebuild all 453 `.so` modules (~23–30 s).

---

## 4. Packaging (`.app` and `.dmg`)

Use `packages/macos/make-bundle.sh` after building `build-texmacos`:

```bash
cd texmacs
./packages/macos/make-bundle.sh --bundle-qt --dmg
```

This produces:
- `dist/TeXmacs-chez-<version>/TeXmacs-chez.app` (self-contained, with Qt frameworks and Homebrew dylibs bundled)
- `dist/TeXmacs-chez-<version>.dmg`

The bundled `.app` embeds `progs/`, `boot/` (`texmacs.boot` and `.so` modules), and Chez boot files under `Contents/Resources/`, so it runs without setting `TEXMACS_PATH`, `TEXMACS_BOOT_PATH`, or `SCHEMEHEAPDIRS`.

### `make-bundle.sh` Options

| Flag | Effect |
|---|---|
| `--bundle-qt` | Copy Qt frameworks and Homebrew dylibs into the bundle (zero external Homebrew refs) |
| `--dmg` | Also create `dist/TeXmacs-chez-<version>.dmg` |
| `--full-progs` | Include all `.scm` sources, all `.so` modules (`TEXMACS_NO_BOOT=1`), and compiler/linter scripts instead of the default slim fast-boot bundle |
| `--sign "<id>"` | Code-sign with the given Developer ID identity |
| `--version X.Y.Z` | Override bundle version (defaults to `Info.plist` / `CMakeLists.txt`) |
| `--build-dir <path>` | Override build directory (defaults to `build-texmacos`) |
| `--chez-heap <dir>` | Override Chez heap directory containing `petite.boot` and `scheme.boot` |

> [!NOTE]
> `make-bundle.sh` never deletes an existing output directory. If `dist/TeXmacs-chez-<version>` already exists, move or remove it before re-running (or pass `--version`).
