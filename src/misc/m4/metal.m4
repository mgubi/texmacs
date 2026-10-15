# Metal for the GPU renderer of the Vue port on macOS
# (src/Plugins/Vue/gpu_backend_metal.mm): on by default there,
# --with-metal=no leaves only the OpenGL backend (with ThorVG)
AC_DEFUN([LC_METAL],[
  AC_ARG_WITH(metal,
  AS_HELP_STRING([--with-metal@<:@=ARG@:>@],
  [the Metal backend of the GPU renderer of the Vue port, on macOS [ARG=yes]]))

  METAL_LDFLAGS=""
  case "${host}" in
    *darwin*)
      if test "$with_metal" = "no" ; then
        AC_MSG_RESULT([disabling the Metal backend of the GPU renderer])
      else
        AC_MSG_RESULT([enabling the Metal backend of the GPU renderer])
        AC_DEFINE(USE_METAL, 1, [Use Metal for the GPU renderer of the Vue port])
        METAL_LDFLAGS="-framework Metal -framework QuartzCore -framework CoreGraphics"
      fi
      ;;
  esac
])
