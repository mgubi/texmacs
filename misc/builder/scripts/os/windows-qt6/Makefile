all: local/lib/libltdl.a local/lib/libiconv.a local/lib/libpng16.a local/lib/libfreetype.a local/lib/libgmp.a local/bin/qmake local/bin/aspell.exe ghostscript gnutls/lib/.libs/libgnutls.a local/lib/libresvg.a texmacs

.PHONY: texmacs

libtool/Makefile:
	cd libtool; ./configure --prefix=$$WORKING_DIR/local --disable-silent-rules --enable-static --disable-shared --with-gnu-ld=yes

libtool/libltdl/.libs/libltdl.a: libtool/Makefile
	cd libtool; $(MAKE)

local/lib/libltdl.a: libtool/libltdl/.libs/libltdl.a
	cd libtool; $(MAKE) install
	if [ -f "local/lib/libltdl.a" ]; then echo "libltdl.a has been installed"; else echo "error on libltdl"; exit; fi;

libiconv/Makefile:
	cd libiconv; ./configure --enable-static --disable-shared --prefix=$$WORKING_DIR/local

libiconv/lib/.libs/libiconv.a: libiconv/Makefile
	cd libiconv; $(MAKE)

local/lib/libiconv.a: libiconv/lib/.libs/libiconv.a
	cd libiconv; $(MAKE) install

libpng/Makefile: local/lib/libltdl.a
	cd libpng; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes

libpng/.libs/libpng16.a: libpng/Makefile
	cd libpng; $(MAKE)

local/lib/libpng16.a: libpng/.libs/libpng16.a
	cd libpng; $(MAKE) install
	if [ -f "local/lib/libpng16.a" ]; then echo "libpng16.a has been installed"; else echo "error on libpng"; exit; fi;

freetype/build/Makefile: local/lib/libltdl.a local/lib/libpng16.a
	cd freetype; mkdir build; cd build; cmake -DCMAKE_POLICY_VERSION_MINIMUM=3.5 -DCMAKE_INSTALL_PREFIX=$$WORKING_DIR/local -DFT_DISABLE_BZIP2=ON -DFT_DISABLE_BROTLI=ON -DFT_DISABLE_HARFBUZZ=ON -DCMAKE_OSX_DEPLOYMENT_TARGET=$$OSX_TARGET ..

freetype/build/libfreetype.a: freetype/build/Makefile
	cd freetype/build; cmake --build .

local/lib/libfreetype.a: freetype/build/libfreetype.a
	cd freetype/build; cmake --build . --target install
	if [ -f "local/lib/libfreetype.a" ]; then echo "libfreetype.a has been installed"; else echo "error on libfreetype"; exit; fi;

gmp/build/Makefile: local/lib/libltdl.a
	cd gmp; mkdir -p build; cd build; CC=clang CXX=clang++ ../configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes --enable-assembly=no

gmp/build/.libs/libgmp.a: gmp/build/Makefile
	cd gmp/build; CC=clang CXX=clang++ $(MAKE)

local/lib/libgmp.a: gmp/build/.libs/libgmp.a
	cd gmp/build; CC=clang CXX=clang++ $(MAKE) install
	if [ -f "local/lib/libgmp.a" ]; then echo "libgmp.a has been installed"; else echo "error on libgmp"; exit; fi;

nettle/Makefile: local/lib/libltdl.a local/lib/libgmp.a
	cd nettle; ./configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared

nettle/lib/.libs/libnettle.a: nettle/Makefile
	cd nettle; $(MAKE)

local/lib/libnettle.a: nettle/lib/.libs/libnettle.a
	cd nettle; $(MAKE) install
	if [ -f "local/lib/libnettle.a" ]; then echo "libnettle.a has been installed"; else echo "error on libnettle"; exit; fi;

gnutls/Makefile: local/lib/libltdl.a local/lib/libgmp.a local/lib/libnettle.a
	cp -r patched/gnutls/* gnutls/
	cd gnutls; ./configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared --disable-hardware-acceleration --disable-doc

gnutls/lib/.libs/libgnutls.a: gnutls/Makefile
	cd gnutls; $(MAKE)

local/lib/libgnutls.a: gnutls/lib/.libs/libgnutls.a
	cd gnutls; $(MAKE) install
	if [ -f "local/lib/libgnutls.a" ]; then echo "libgnutls.a has been installed"; else echo "error on libgnutls"; exit; fi;

local/bin/qmake:
#	cp patched/QtPkgConfigHelpers.cmake qt/qtbase/cmake/QtPkgConfigHelpers.cmake 
	-mkdir qt-build
	cd qt-build; ../qt/configure -opensource -confirm-license -prefix $$WORKING_DIR/local -release -pkg-config -no-warnings-are-errors -submodules qtbase,qtsvg
	cd qt-build; cmake --build . --parallel
	cd qt-build; cmake --build . --target install --parallel

ghostscript/Makefile: local/bin/qmake local/lib/libfreetype.a
	chmod +x ghostscript/configure
	chmod +x ghostscript/base/instcopy
	cd ghostscript; ./configure --prefix=$$WORKING_DIR_WIN/local --disable-compile-inits --without-x --without-tesseract

ghostscript: ghostscript/Makefile
	cd ghostscript; make; make install
	cp local/bin/gs.exe local/windows/bin

aspell/Makefile:
#	cp patched/file_util.cpp aspell/common/file_util.cpp
#	cp patched/ostream.hpp aspell/common/ostream.hpp
#	cp patched/language.cpp aspell/modules/speller/default/language.cpp
#	cp patched/vector_hash-t.hpp aspell/modules/speller/default/vector_hash-t.hpp
	cd aspell; ./configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared

aspell/aspell.exe: aspell/Makefile
	cd aspell; $(MAKE)

local/bin/aspell.exe: aspell/aspell.exe
	cd aspell; $(MAKE) install
	python3 download_dictionaries.py

local/lib/libresvg.a:
	cd resvg && cargo build --release --manifest-path crates/c-api/Cargo.toml
	cp resvg/crates/c-api/resvg.h local/include/
	cp resvg/target/release/libresvg.a local/lib/
	if [ -f "local/lib/libresvg.a" ]; then echo "libresvg.a has been installed"; else echo "error on libresvg"; exit 1; fi;

texmacs:
	cd texmacs/src; autoreconf; CFLAGS="$$CFLAGS -std=c99" PKG_CONFIG_PATH=$$WORKING_DIR/local/lib/pkgconfig ./configure --with-texmacs-builder=$$WORKING_DIR/local --prefix=$$WORKING_DIR/local --with-guile=embedded18 --with-qt-find-method=pkgconfig --with-gnutls=yes; mingw32-make; mingw32-make WINDOWS_APPX
