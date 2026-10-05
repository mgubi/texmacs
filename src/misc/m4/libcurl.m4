
#--------------------------------------------------------------------
#
# MODULE      : libcurl.m4
# DESCRIPTION : TeXmacs configuration options for libcurl
# COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

# The HTTP requests of the builds without Qt (the AI engines, the posts of
# Scheme) are made by libcurl when it is there (a library of the system on
# macOS and in the Linux distributions), else by the curl program. On by
# default when found; --without-libcurl turns it off.

AC_DEFUN([LC_LIBCURL],[
  AC_ARG_WITH(libcurl,
    AS_HELP_STRING([--with-libcurl@<:@=ARG@:>@],
      [make the HTTP requests with libcurl [ARG=yes when found]]),
    with_libcurl=$withval, with_libcurl="auto")

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_libcurl" = "no"; then
    AC_MSG_RESULT([disabling libcurl support])
  else
    if command -v curl-config > /dev/null 2>&1; then
      CPPFLAGS=`curl-config --cflags`
      LIBS=`curl-config --libs`
    else
      CPPFLAGS=`pkg-config --cflags libcurl 2> /dev/null`
      LIBS=`pkg-config --libs libcurl 2> /dev/null`
      if test "$LIBS" = ""; then LIBS="-lcurl"; fi
    fi
    AC_MSG_CHECKING(for libcurl)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <curl/curl.h>
]], [[
    CURLM* m= curl_multi_init ();
    curl_multi_cleanup (m);
]])],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_LIBCURL, 1, [Make the HTTP requests with libcurl])
      LIBCURL_CPPFLAGS="$CPPFLAGS"
      LIBCURL_LDFLAGS="$LIBS"
    ],[
      AC_MSG_RESULT(no)
      if test "$with_libcurl" = "yes"; then
        AC_MSG_ERROR([libcurl was asked for but cannot be linked])
      fi
    ])
  fi
  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST([LIBCURL])
])
