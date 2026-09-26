all: local/lib/libcrypt.so local/lib/libltdl.a local/lib/libicui18n.so local/lib/libintl.a local/lib/libpng16.a local/lib/libfreetype.a local/lib/libgmp.a local/bin/qmake ghostscript local/lib/libgnutls.a local/usr/optional/exec.so texmacs

.PHONY: texmacs

# --- Libxcrypt ---
libxcrypt/configure:
	cd libxcrypt; ./autogen.sh

libxcrypt/Makefile: libxcrypt/configure
	cd libxcrypt; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

libxcrypt/libcrypt.so: libxcrypt/Makefile
	cd libxcrypt; make

local/lib/libcrypt.so: libxcrypt/libcrypt.so
	cd libxcrypt; make install

# --- Libtool ---
libtool/Makefile:
	cd libtool; ./configure --prefix=$$WORKING_DIR/local --disable-silent-rules --enable-shared --disable-static --with-gnu-ld=yes

libtool/libltdl/.libs/libltdl.a: libtool/Makefile
	cd libtool; $(MAKE)

local/lib/libltdl.a: libtool/libltdl/.libs/libltdl.a
	cd libtool; $(MAKE) install
	if [ -f "local/lib/libltdl.a" ]; then echo "libltdl.a has been installed"; else echo "error on libltdl"; exit; fi;

# --- ICU ---
icu/source/Makefile: local/lib/libltdl.a
	cd icu/source; ./configure --prefix=$$WORKING_DIR/local

icu/source/lib/libicui18n.so: icu/source/Makefile
	cd icu/source; $(MAKE)

local/lib/libicui18n.so: icu/source/lib/libicui18n.so
	cd icu/source; $(MAKE) install

# --- Gettext ---
gettext/Makefile:
	cd gettext; ./configure --enable-shared --disable-static --prefix=$$WORKING_DIR/local

gettext/gettext-runtime/intl/.libs/libintl.a: gettext/Makefile
	cd gettext; make

local/lib/libintl.a: gettext/gettext-runtime/intl/.libs/libintl.a
	cd gettext; make install

# --- Libpng ---
libpng/Makefile: local/lib/libltdl.a
	cd libpng; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-shared --disable-static --with-gnu-ld=yes

libpng/.libs/libpng16.a: libpng/Makefile
	cd libpng; $(MAKE)

local/lib/libpng16.a: libpng/.libs/libpng16.a
	cd libpng; $(MAKE) install
	if [ -f "local/lib/libpng16.a" ]; then echo "libpng16.a has been installed"; else echo "error on libpng"; exit; fi;

# --- Freetype ---
freetype/build/Makefile: local/lib/libltdl.a local/lib/libpng16.a
	cd freetype; mkdir -p build; cd build; cmake -G"Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$$WORKING_DIR/local -DFT_DISABLE_BZIP2=ON -DFT_DISABLE_BROTLI=ON -DFT_DISABLE_HARFBUZZ=ON ..

freetype/build/libfreetype.a: freetype/build/Makefile
	cd freetype/build; cmake --build .

local/lib/libfreetype.a: freetype/build/libfreetype.a
	cd freetype/build; cmake --build . --target install
	if [ -f "local/lib/libfreetype.a" ]; then echo "libfreetype.a has been installed"; else echo "error on libfreetype"; exit; fi;

# --- GMP ---
gmp/build/Makefile: local/lib/libltdl.a
	cd gmp; mkdir -p build; cd build; ../configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static --with-gnu-ld=yes --enable-assembly=no

gmp/build/.libs/libgmp.a: gmp/build/Makefile
	cd gmp/build; $(MAKE)

local/lib/libgmp.a: gmp/build/.libs/libgmp.a
	cd gmp/build; $(MAKE) install

# --- Nettle ---
nettle/Makefile: local/lib/libltdl.a local/lib/libgmp.a
	cd nettle; ./configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared

nettle/lib/.libs/libnettle.a: nettle/Makefile
	cd nettle; $(MAKE)

local/lib/libnettle.a: nettle/lib/.libs/libnettle.a
	cd nettle; $(MAKE) install
	if [ -f "local/lib/libnettle.a" ]; then echo "libnettle.a has been installed"; else echo "error on libnettle"; exit; fi;

# --- GnuTLS ---
gnutls/Makefile: local/lib/libltdl.a local/lib/libgmp.a local/lib/libnettle.a
	cd gnutls; ./configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-included-libtasn1 --with-included-unistring --without-p11-kit --disable-guile --without-brotli

gnutls/lib/.libs/libgnutls.a: gnutls/Makefile
	cd gnutls; $(MAKE)

local/lib/libgnutls.a: gnutls/lib/.libs/libgnutls.a
	cd gnutls; $(MAKE) install
	if [ -f "local/lib/libgnutls.a" ]; then echo "libgnutls.a has been installed"; else echo "error on libgnutls"; exit; fi;

# --- Guile ---
guile/Makefile: local/lib/libltdl.a local/lib/libgmp.a
	cd guile; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static --disable-networking --disable-nls

guile/libguile/.libs/libguile.a: guile/Makefile
	cd guile; $(MAKE)

local/lib/libguile.a: guile/libguile/.libs/libguile.a
	cd guile; $(MAKE) install

# --- XCB Proto ---
xcbproto/configure:
	cd xcbproto; ./autogen.sh

xcbproto/Makefile: xcbproto/configure
	cd xcbproto; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

xcbproto/xcb-proto.pc: xcbproto/Makefile
	cd xcbproto; make

local/share/pkgconfig/xcb-proto.pc: xcbproto/xcb-proto.pc
	cd xcbproto; make install

# --- Libxcb ---
libxcb/Makefile: local/share/pkgconfig/xcb-proto.pc freetype/build/libfreetype.a
	cd libxcb; ./configure --prefix=$$WORKING_DIR/local --without-doxygen --enable-shared --disable-static

libxcb/src/.libs/libxcb.a: libxcb/Makefile
	cd libxcb; make

local/lib/libxcb.a: libxcb/src/.libs/libxcb.a
	cd libxcb; make install

# --- LibXML2 ---
libxml2/configure:
	cd libxml2; ./autogen.sh --prefix=$$WORKING_DIR/local --enable-shared --disable-static

libxml2/Makefile: libxml2/configure
	cd libxml2; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

libxml2/.libs/libxml2.a: libxml2/Makefile
	cd libxml2; make

local/lib/libxml2.a: libxml2/.libs/libxml2.a
	cd libxml2; make install

# --- Wayland ---
local/lib/libwayland-client.so:
	cd wayland; meson setup --prefix=$$WORKING_DIR/local -Ddefault_library=shared -Ddocumentation=false build
	cd wayland; meson compile -C build
	cd wayland; meson install -C build
	-cp -ra $$WORKING_DIR/local/lib/x86_64-linux-gnu/* $$WORKING_DIR/local/lib

local/share/pkgconfig/wayland-protocols.pc: local/lib/libwayland-client.so
	cd wayland-protocols; meson setup --prefix=$$WORKING_DIR/local -Ddefault_library=shared build
	cd wayland-protocols; meson compile -C build
	cd wayland-protocols; meson install -C build
	-cp -ra $$WORKING_DIR/local/lib/x86_64-linux-gnu/* $$WORKING_DIR/local/lib

# --- XCB Utilities ---

# 1. xcb-util (Core)
xcb-util/Makefile: local/lib/libxcb.a
	cd xcb-util; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

local/lib/libxcb-util.so: xcb-util/Makefile
	cd xcb-util; make install

# 2. xcb-util-keysyms
xcb-util-keysyms/Makefile: local/lib/libxcb.a
	cd xcb-util-keysyms; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

local/lib/libxcb-keysyms.so: xcb-util-keysyms/Makefile
	cd xcb-util-keysyms; make install

# 3. xcb-util-wm (ICCCM)
xcb-util-wm/Makefile: local/lib/libxcb.a
	cd xcb-util-wm; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

local/lib/libxcb-icccm.so: xcb-util-wm/Makefile
	cd xcb-util-wm; make install

# 4. xcb-util-renderutil
xcb-util-renderutil/Makefile: local/lib/libxcb.a
	cd xcb-util-renderutil; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

local/lib/libxcb-render-util.so: xcb-util-renderutil/Makefile
	cd xcb-util-renderutil; make install

# 5. xcb-util-image (Depends on xcb-util core)
xcb-util-image/Makefile: local/lib/libxcb-util.so
	cd xcb-util-image; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

local/lib/libxcb-image.so: xcb-util-image/Makefile
	cd xcb-util-image; make install

# 6. xcb-util-cursor (Depends on image and renderutil)
xcb-util-cursor/Makefile: local/lib/libxcb-image.so local/lib/libxcb-render-util.so
	cd xcb-util-cursor; ./configure --prefix=$$WORKING_DIR/local --enable-shared --disable-static

local/lib/libxcb-cursor.so: xcb-util-cursor/Makefile
	cd xcb-util-cursor; make install

# --- Libxkbcommon ---
local/lib/libxkbcommon.a: local/lib/libxml2.a local/lib/libwayland-client.so local/share/pkgconfig/wayland-protocols.pc local/lib/libxcb.a
	cd libxkbcommon; meson setup --prefix=$$WORKING_DIR/local -Ddefault_library=shared -Denable-x11=true -Denable-wayland=true build
	cd libxkbcommon; meson compile -C build
	cd libxkbcommon; meson install -C build
	-cp -ra $$WORKING_DIR/local/lib/x86_64-linux-gnu/* $$WORKING_DIR/local/lib

# --- Fontconfig ---
local/lib/libfontconfig.a: local/lib/libxkbcommon.a
	cd fontconfig; meson setup --prefix=$$WORKING_DIR/local -Ddefault_library=shared build
	cd fontconfig; meson compile -C build
	cd fontconfig; meson install -C build
	-cp -ra $$WORKING_DIR/local/lib/x86_64-linux-gnu/* $$WORKING_DIR/local/lib

# --- Qt 6 ---
local/bin/qmake: local/lib/libfontconfig.a local/lib/libxcb-cursor.so local/lib/libxcb-icccm.so local/lib/libxcb-keysyms.so
	-mkdir qt-build
	cd qt-build; ../qt/configure \
		-opensource -pkg-config -confirm-license -prefix $$WORKING_DIR/local -release -nomake tests -nomake examples \
		-skip qt3d \
		-skip qtactiveqt \
		-skip qtcharts \
		-skip qtcoap \
		-skip qtdatavis3d \
		-skip qtdeclarative \
		-skip qtdoc \
		-skip qtgamepad \
		-skip qtgraphicaleffects \
		-skip qtimageformats \
		-skip qtlocation \
		-skip qtlottie \
		-skip qtmultimedia \
		-skip qtnetworkauth \
		-skip qtopcua \
		-skip qtpositioning \
		-skip qtquick3d \
		-skip qtquickcontrols \
		-skip qtquickcontrols2 \
		-skip qtremoteobjects \
		-skip qtscript \
		-skip qtscxml \
		-skip qtsensors \
		-skip qtserialbus \
		-skip qtserialport \
		-skip qtspeech \
		-skip qtvirtualkeyboard \
		-skip qtwebchannel \
		-skip qtwebengine \
		-skip qtwebsockets \
		-skip qtwebview \
		-skip qtxmlpatterns \
		-skip qtgraphs \
		-skip qtmqtt \
		-skip qtquick3dphysics \
		-skip qtquickeffectmaker \
		-skip qtquicktimeline \
		-no-warnings-are-errors -- -G Ninja
	cd qt-build; cmake --build . --parallel
	cd qt-build; cmake --build . --target install --parallel

# --- Ghostscript ---
ghostscript/Makefile: local/bin/qmake local/lib/libfreetype.a
	chmod +x ghostscript/configure
	chmod +x ghostscript/base/instcopy
	cd ghostscript; LIBS=$$LDFLAGS CFLAGS="$$CFLAGS -Wno-implicit-int -Wno-implicit-function-declaration -D__CRT__NO_INLINE" CPPFLAGS="$$CPPFLAGS -DOPJ_EXPORTS" ./configure --prefix=$$WORKING_DIR/local --without-x --with-drivers=FILES --disable-cups --disable-gtk --without-tesseract --with-libiconv=gnu --disable-threading --disable-byteswap-h --disable-fontconfig --disable-dbus --with-libtiff

ghostscript: ghostscript/Makefile
	cd ghostscript; $(MAKE) -j1; $(MAKE) install

# --- Exec.so ---
local/usr/optional/exec.so:
	cd execso; $(MAKE)
	mkdir -p $$WORKING_DIR/local/usr/optional
	cp execso/exec.so $$WORKING_DIR/local/usr/optional

# --- Texmacs ---
texmacs:
	-rm -rf texmacs/distr
	mkdir texmacs/distr
	cd texmacs/src; autoreconf; PKG_CONFIG_PATH=$$WORKING_DIR/local/lib/pkgconfig TMREPO=$$WORKING_DIR/local ./configure --with-tmrepo=$$WORKING_DIR/local --with-gnutls=yes; $(MAKE) APPIMAGE
