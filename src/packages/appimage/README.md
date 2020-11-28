


wget -c https://github.com/$(wget -q https://github.com/probonopd/go-appimage/releases -O - | grep "appimagetool-.*-x86_64.AppImage" | head -n 1 | cut -d '"' -f 2)
chmod +x appimagetool-*.AppImage
./appimagetool-*.AppImage -s deploy appdir/usr/share/applications/*.desktop # Bundle EVERYTHING
# or 
./appimagetool-*.AppImage deploy appdir/usr/share/applications/*.desktop # Bundle everything expect what comes with the base system
# and
VERSION=1.0 ./appimagetool-*.AppImage ./Some.AppDir # turn AppDir into AppImage


./appimagetool -s deploy 