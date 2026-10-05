# ThorVG for the GPU renderer of the Vue port (src/Plugins/Vue/vue_gpu.cpp):
# --with-thorvg=<prefix>, where misc/thorvg/build-thorvg.sh installed it
# (<prefix>/include/thorvg-1/thorvg.h, <prefix>/lib/libthorvg-1.a)
AC_DEFUN([LC_THORVG],[
  AC_ARG_WITH(thorvg,
  AS_HELP_STRING([--with-thorvg@<:@=ARG@:>@],
  [with the GPU renderer of the Vue port, ThorVG installed in ARG [ARG=no]]))

  THORVG_CFLAGS=""
  THORVG_LDFLAGS=""
  if test "$with_thorvg" = "no" -o "$with_thorvg" = "" ; then
      AC_MSG_RESULT([disabling the GPU renderer (no ThorVG)])
  else
      if test -f "$with_thorvg/include/thorvg-1/thorvg.h" -a \
              -f "$with_thorvg/lib/libthorvg-1.a" ; then
        AC_MSG_RESULT([enabling the GPU renderer with ThorVG in $with_thorvg])
        AC_DEFINE(USE_THORVG, 1, [Use ThorVG and OpenGL for the Vue renderer])
        THORVG_CFLAGS="-I$with_thorvg/include/thorvg-1 -DGL_SILENCE_DEPRECATION -DTVG_STATIC"
        THORVG_LDFLAGS="$with_thorvg/lib/libthorvg-1.a"
        case "${host}" in
          *darwin*) THORVG_LDFLAGS="$THORVG_LDFLAGS -framework OpenGL" ;;
          *mingw* | *cygwin* | *msys*) THORVG_LDFLAGS="$THORVG_LDFLAGS -lopengl32" ;;
          *) THORVG_LDFLAGS="$THORVG_LDFLAGS -lGL" ;;
        esac
      else
        AC_MSG_ERROR([no ThorVG in $with_thorvg (see misc/thorvg/build-thorvg.sh)])
      fi
  fi
])
