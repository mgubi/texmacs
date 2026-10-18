#!/bin/bash
# Denis RAUX LIX 2016
# copy the dll files of $1
#
# we prefer to use ntldd which gives accurate result on windows10
# on old environment only ldd is available
#
typeset mingwbase=$(cygpath -m /).$'*'
if test -x "$1"
then
	typeset dest=$(dirname $1)
# with for we have one parameter per loop
# windows backslash need to be converted to slash in order being able be compared
	if command -v ntldd > /dev/null
	then
	ntldd -R $1 |while read -r lib sign  full trash
	do [[ ! -x $dest/$lib && ${full//\\/\/} =~ ${mingwbase} ]] && { cp $full $dest || exit $?; }
	done
	else
# (MSYS2 no longer has ntldd for MINGW64; the ldd of its runtime lists the
# dlls too, recursively, with paths of MSYS2 which cygpath makes Windows ones)
	ldd $1 |while read -r lib sign  full trash
	do [[ "$full" == /* ]] || continue
	full=$(cygpath -m "$full")
	[[ ! -x $dest/$lib && $full =~ ${mingwbase} ]] && { cp $full $dest || exit $?; }
	done
	fi
fi
exit 0
