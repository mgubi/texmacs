#!/bin/bash
# The installer of TeXmacs Upsilon for Windows (MSYS2 shell, in src/, after
# make WINDOWS_BUNDLE): Inno Setup with packages/windows/TeXmacs-Upsilon.iss
# makes $GITHUB_WORKSPACE/texmacs-upsilon-<gui>-x86_64-setup.exe from
# ../distr/TeXmacs-Windows, which is then
#   1. installed for the user, without questions;
#   2. run with TEXMACS_PATH set to another tree, as the installer of the
#      stock TeXmacs leaves it: TeXmacs Upsilon must use its own and run a
#      command given with -x;
#   2b. its Ghostscript and Aspell are those of the package (#662);
#   3. uninstalled: its folder must be gone, and it must have written no
#      TEXMACS_PATH into the environment of the user.
#
#   .github/scripts/windows-installer.sh qt|vue

set -e
gui="$1"
case "$gui" in
  qt)  Gui=Qt;  name="TeXmacs Upsilon" ;;
  vue) Gui=Vue; name="TeXmacs Upsilon Vue" ;;
  *) echo "usage: $0 qt|vue" >&2; exit 1 ;;
esac
out="texmacs-upsilon-$gui-x86_64-setup"
ws=$(cygpath -u "$GITHUB_WORKSPACE")
# (the options of Windows programs, /VERYSILENT..., are not paths)
export MSYS2_ARG_CONV_EXCL='*'

iscc="/c/Program Files (x86)/Inno Setup 6/ISCC.exe"
if [ ! -x "$iscc" ]; then
  /c/ProgramData/chocolatey/bin/choco.exe install innosetup -y --no-progress
fi
version=$(sed -n 's/^tm_devel_version *= *//p' Makefile | head -1)
"$iscc" /Q "/DGui=$Gui" "/DVersion=${version:-2.1.5}" \
  "/DBundle=$(cygpath -wa ../distr/TeXmacs-Windows)" \
  "/DOutputDir=$(cygpath -wa "$ws")" "/DOutputName=$out" \
  "$(cygpath -wa packages/windows/TeXmacs-Upsilon.iss)"
ls -l "$ws/$out.exe"

echo "== 1. installed for the user"
"$ws/$out.exe" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /CURRENTUSER \
  "/LOG=$(cygpath -wa "$ws")\\install-$gui.log"
app="$(cygpath -u "$LOCALAPPDATA")/Programs/$name"
[ -f "$app/bin/texmacs.exe" ] || { echo "error: no $app/bin/texmacs.exe"; exit 1; }

echo "== 2. run, with the TEXMACS_PATH of another TeXmacs"
home=$(mktemp -d)
whome=$(cygpath -m "$home")
TEXMACS_HOME_PATH="$whome" TEXMACS_PATH='C:\Program Files\TeXmacs' \
  timeout 300 "$app/bin/texmacs.exe" \
  -x "(begin (string-save \"ok\" \"$whome/ok\") (quit-TeXmacs))" \
  > "$home/log.txt" 2>&1 || true
tail -20 "$home/log.txt"
[ -f "$home/ok" ] || { echo "error: the installed TeXmacs did not run the command"; exit 1; }

echo "== 2b. Ghostscript and Aspell of the installed program, with no gs of the system"
packages/bundled-tools/check.sh -a -r "$app" \
  -v "$(sed -n 's/^GS_VERSION=//p' packages/build-gs.sh)" -- "$app/bin/texmacs.exe"

echo "== 3. uninstalled"
"$app/unins000.exe" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART
# (the uninstaller runs a copy of itself and returns at once)
for i in $(seq 1 60); do [ -e "$app" ] || break; sleep 2; done
[ ! -e "$app" ] || { echo "error: $app is still there"; ls -la "$app"; exit 1; }
if reg query 'HKCU\Environment' /v TEXMACS_PATH > /dev/null 2>&1; then
  echo "error: TEXMACS_PATH is in the environment of the user"; exit 1
fi
echo "== the installer of $name is fine"
