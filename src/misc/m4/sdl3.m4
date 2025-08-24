AC_DEFUN([LC_SDL3],[
  AC_ARG_WITH(sdl3,
  AS_HELP_STRING([--with-sdl3@<:@=ARG@:>@],
  [with sdl3 support [ARG=no]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_sdl3" = "no" -o "$with_sdl3" = "" ; then
      AC_MSG_RESULT([disabling sdl3 support])
  else
      CPPFLAGS=`pkg-config --cflags sdl3`
      LIBS=`pkg-config --libs sdl3`
      AC_CHECK_HEADER(SDL3/SDL.h,
      AC_MSG_CHECKING(for sdl3)
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <SDL3/SDL.h>
  ]], [[
SDL_Init(SDL_INIT_VIDEO);
  ]])],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_SDL3, 1, [Use SDL3 library])
      SDL3_CPPFLAGS="$CPPFLAGS"
      SDL3_LDFLAGS="$LIBS"
  ],[
      AC_MSG_RESULT(no)]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST(SDL3)
])
