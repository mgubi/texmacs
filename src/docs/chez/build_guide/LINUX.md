# Building TeXmacs (Chez Backend) on Linux

Verified on **Ubuntu 26.04.1 arm64** with **Qt 6.10.2** and **Chez Scheme 10.4.1**.

---

## 1. Install System Packages

```bash
sudo apt install -y build-essential cmake ninja-build pkg-config git curl \
  qt6-base-dev qt6-base-dev-tools libqt6svg6-dev qt6-5compat-dev \
  libfreetype-dev libjpeg-dev libpng-dev libgmp-dev libgnutls28-dev \
  libsqlite3-dev zlib1g-dev liblz4-dev libncurses-dev
```

`libncurses-dev` is required so the linker finds the unversioned `libncurses.so`.

---

## 2. Install or Build Chez Scheme

### Option A: Build Chez Scheme 10.4.1 from Source (Verified)

Distro Chez packages do not ship `liblz4.a` / `libz.a` alongside `libkernel.a`, so building from source with `--threads` is the verified path:

```bash
curl -LO https://github.com/cisco/ChezScheme/releases/download/v10.4.1/csv10.4.1.tar.gz
tar xf csv10.4.1.tar.gz && cd csv10.4.1
./configure --installprefix="$HOME/chez" --threads --installschemename=chez
make -j"$(nproc)" && make install
```

### Option B: Install via `apt` (Reference Only — Not Fully Tested)

You can also try installing Chez Scheme from Ubuntu's repositories (`sudo apt install -y chezscheme chezscheme-dev`), and point `-DCHEZ_ROOT` at the distro's `csv*` directory (e.g. `/usr/lib/csv*`) when configuring CMake. This was previously tested on an older Qt 5 build and has not been re-verified on the current Qt 6 / Chez 10.4.1 tree (you may need to adjust library paths if `CMakeLists.txt` expects `liblz4.a`/`libz.a` inside `CHEZ_ROOT`).

---

## 3. Configure, Build & Run

```bash
cd texmacs
MACH=$( [ "$(uname -m)" = "aarch64" ] && echo tarm64le || echo ta6le )
cmake -S . -B build-texmacos -G Ninja \
  -DTEXMACS_GUI=Qt6 \
  -DSCHEME_IMPL=chez \
  -DCHEZ_ROOT="$HOME/chez/lib/csv10.4.1" \
  -DCHEZ_MACHINE="$MACH" \
  -DCMAKE_BUILD_TYPE=Release
cmake --build build-texmacos -j"$(nproc)"
```

- Linux does not need `CMAKE_PREFIX_PATH` or `QT_ADDITIONAL_PACKAGES_PREFIX_PATH`.
- `cmake --build` compiles all 453 `.so` modules and generates `texmacs.boot` (when `chez` is on `PATH` or `-DCHEZ_SCHEME_EXECUTABLE=...` is set).
- `CMakeLists.txt` generates `TeXmacs/SVNREV` at configure time. On older trees, create it manually if missing (`grep -o 'ALTERNATIVE_VERSION "[^"]*"' CMakeLists.txt | cut -d'"' -f2 > TeXmacs/SVNREV`).

Run the binary (`build-texmacos/src/texmacs.bin`):

```bash
SCHEMEHEAPDIRS="$HOME/chez/lib/csv10.4.1/$MACH" \
HOME="$PWD/fakehome" \
TEXMACS_PATH="$PWD/TeXmacs" \
TEXMACS_BOOT_PATH="$PWD/build-texmacos/boot" \
./build-texmacos/src/texmacs.bin
```

> [!IMPORTANT]
> Put `SCHEMEHEAPDIRS=...` **before** `HOME="$PWD/fakehome"` so `$HOME` expands to your real home directory when locating `~/chez`.
