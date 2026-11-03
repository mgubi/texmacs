# ThorVG for the GPU renderer of the Vue port (src/Plugins/Vue/vue_gpu.cpp).
# The Vue port uses it whenever it is available: without --with-thorvg it
# is looked for where pkg-config says (thorvg-1), in the prefix and in
# /usr/local, /opt/homebrew and /usr; --with-thorvg=<prefix> names where
# misc/thorvg/build-thorvg.sh installed it (<prefix>/include/thorvg-1/
# thorvg.h, <prefix>/lib/libthorvg-1.a, or a shared library), and
# --with-thorvg=no draws with MuPDF. A ThorVG is taken only if a program
# making its OpenGL canvas links: the packaged ones (Homebrew) usually have
# the software engine only.

# LC_THORVG_TRY(prefix): sets tm_thorvg_cflags and tm_thorvg_ldflags when a
# ThorVG with its OpenGL engine is in the prefix, else empties them
AC_DEFUN([LC_THORVG_TRY],[
  tm_thorvg_cflags=""
  tm_thorvg_ldflags=""
  tm_p="$1"
  if test -n "$tm_p" -a -f "$tm_p/include/thorvg-1/thorvg.h" ; then
    tm_cflags="-I$tm_p/include/thorvg-1 -DGL_SILENCE_DEPRECATION"
    if test -f "$tm_p/lib/libthorvg-1.a" ; then
      tm_cflags="$tm_cflags -DTVG_STATIC"
      tm_ldflags="$tm_p/lib/libthorvg-1.a"
    else
      tm_ldflags="-L$tm_p/lib -lthorvg-1"
    fi
    case "${host}" in
      *darwin*) tm_ldflags="$tm_ldflags -framework OpenGL" ;;
      *mingw* | *cygwin* | *msys*) tm_ldflags="$tm_ldflags -lopengl32" ;;
      *) tm_ldflags="$tm_ldflags -lGL -lpthread" ;;
    esac
    AC_LANG_PUSH([C++])
    tm_save_CXXFLAGS="$CXXFLAGS"
    tm_save_LIBS="$LIBS"
    CXXFLAGS="$CXXFLAGS -std=c++17 $tm_cflags"
    LIBS="$tm_ldflags $LIBS"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <thorvg.h>]],
                                    [[return tvg::GlCanvas::gen () == nullptr;]])],
                   [tm_thorvg_cflags="$tm_cflags"; tm_thorvg_ldflags="$tm_ldflags"])
    CXXFLAGS="$tm_save_CXXFLAGS"
    LIBS="$tm_save_LIBS"
    AC_LANG_POP([C++])
  fi
])

AC_DEFUN([LC_THORVG],[
  AC_ARG_WITH(thorvg,
  AS_HELP_STRING([--with-thorvg@<:@=ARG@:>@],
  [the GPU renderer of the Vue port, with ThorVG installed in ARG (default:
   found by itself; no: draw with MuPDF)]))

  THORVG_CFLAGS=""
  THORVG_LDFLAGS=""
  if test "$with_thorvg" = "no" ; then
    AC_MSG_RESULT([disabling the GPU renderer (--with-thorvg=no)])
  else
    if test "$with_thorvg" = "" -o "$with_thorvg" = "yes" ; then
      tm_places=""
      if test -n "$PKG_CONFIG" -o -n "`which pkg-config 2>/dev/null`" ; then
        tm_places="`${PKG_CONFIG:-pkg-config} --variable=prefix thorvg-1 2>/dev/null`"
      fi
      if test "$prefix" != "NONE" ; then tm_places="$tm_places $prefix" ; fi
      tm_places="$tm_places /usr/local /opt/homebrew /usr"
    else
      tm_places="$with_thorvg"
    fi
    AC_MSG_CHECKING([for ThorVG with its OpenGL engine])
    tm_found=""
    for tm_place in $tm_places ; do
      LC_THORVG_TRY([$tm_place])
      if test -n "$tm_thorvg_ldflags" ; then tm_found="$tm_place" ; break ; fi
    done
    if test -n "$tm_found" ; then
      AC_MSG_RESULT([in $tm_found])
      AC_MSG_RESULT([enabling the GPU renderer with ThorVG in $tm_found])
      AC_DEFINE(USE_THORVG, 1, [Use ThorVG and OpenGL for the Vue renderer])
      THORVG_CFLAGS="$tm_thorvg_cflags"
      THORVG_LDFLAGS="$tm_thorvg_ldflags"
    else
      AC_MSG_RESULT([no])
      if test "$with_thorvg" = "" ; then
        AC_MSG_WARN([no ThorVG with its OpenGL engine: the Vue port draws with MuPDF (build ThorVG with misc/thorvg/build-thorvg.sh <prefix>, then --with-thorvg=<prefix>)])
      else
        AC_MSG_ERROR([no ThorVG with its OpenGL engine in $tm_places (see misc/thorvg/build-thorvg.sh)])
      fi
    fi
  fi
])
