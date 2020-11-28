#!/bin/sh

DISTRIBUTOR_ID=`lsb_release -a 2>/dev/null |grep "Distributor ID"`
if [ ! "${DISTRIBUTOR_ID#*Ubuntu}" = "$DISTRIBUTOR_ID" ] ; then
  export QT_X11_NO_NATIVE_MENUBAR=1
fi

if [ -z $APPDIR ]; then APPDIR=$(readlink -f $(dirname "$0")); export APPDIR; fi

prefix="$APPDIR"/usr/
exec_prefix="${prefix}"
datarootdir="${prefix}/share"
datadir="${datarootdir}"

TEXMACS_PATH=${datarootdir}/TeXmacs
export TEXMACS_PATH
TEXMACS_BIN_PATH=${exec_prefix}
export TEXMACS_BIN_PATH

export PATH="$TEXMACS_BIN_PATH/bin:$APPDIR"/usr/bin/:"$PATH"

exec texmacs.bin "$@" < /dev/null

