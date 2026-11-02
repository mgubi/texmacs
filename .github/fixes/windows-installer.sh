#!/bin/bash
# The installer of TeXmacs Fixes for Windows (MSYS2 shell, in src/, after
# make WINDOWS_BUNDLE): Inno Setup with .github/fixes/TeXmacs-Fixes.iss
# makes $GITHUB_WORKSPACE/texmacs-fixes-x86_64-setup.exe from
# ../distr/TeXmacs-Windows, which is then
#   1. installed for the user, without questions;
#   2. run with TEXMACS_PATH set to another tree, as the installer of the
#      stock TeXmacs leaves it: TeXmacs Fixes must use its own and run a
#      command given with -x;
#   3. uninstalled: its folder must be gone, and it must have written no
#      TEXMACS_PATH into the environment of the user.
#
#   ../.github/fixes/windows-installer.sh

set -e
name="TeXmacs Fixes"
out="texmacs-fixes-x86_64-setup"
ws=$(cygpath -u "$GITHUB_WORKSPACE")
# (the options of Windows programs, /VERYSILENT..., are not paths)
export MSYS2_ARG_CONV_EXCL='*'

iscc="/c/Program Files (x86)/Inno Setup 6/ISCC.exe"
if [ ! -x "$iscc" ]; then
  /c/ProgramData/chocolatey/bin/choco.exe install innosetup -y --no-progress
fi
version=$(sed -n 's/^tm_devel_version *= *//p' Makefile | head -1)
"$iscc" /Q "/DVersion=${version:-2.1.5}" \
  "/DBundle=$(cygpath -wa ../distr/TeXmacs-Windows)" \
  "/DPkg=$(cygpath -wa packages/windows)" \
  "/DOutputDir=$(cygpath -wa "$ws")" "/DOutputName=$out" \
  "$(cygpath -wa ../.github/fixes/TeXmacs-Fixes.iss)"
ls -l "$ws/$out.exe"

echo "== 1. installed for the user"
"$ws/$out.exe" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /CURRENTUSER \
  "/LOG=$(cygpath -wa "$ws")\\install.log"
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

echo "== 3. uninstalled"
"$app/unins000.exe" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART
# (the uninstaller runs a copy of itself and returns at once)
for i in $(seq 1 60); do [ -e "$app" ] || break; sleep 2; done
[ ! -e "$app" ] || { echo "error: $app is still there"; ls -la "$app"; exit 1; }
if reg query 'HKCU\Environment' /v TEXMACS_PATH > /dev/null 2>&1; then
  echo "error: TEXMACS_PATH is in the environment of the user"; exit 1
fi
echo "== the installer of $name is fine"
