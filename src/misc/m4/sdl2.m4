AC_DEFUN([LC_SDL2],[
  AC_ARG_WITH(sdl2,
  AS_HELP_STRING([--with-sdl2@<:@=ARG@:>@],
  [with sdl2 support [ARG=no]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_sdl2" = "no" -o "$with_sdl2" = "" ; then
      AC_MSG_RESULT([disabling sdl2 support])
  else
      CPPFLAGS=`pkg-config --cflags sdl2`
      LIBS=`pkg-config --libs sdl2`
      AC_CHECK_HEADER(SDL.h,
      AC_MSG_CHECKING(for sdl2)
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <SDL.h>
  ]], [[
SDL_Init(SDL_INIT_VIDEO);
  ]])],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_SDL2, 1, [Use SDL2 library])
      SDL2_CPPFLAGS="$CPPFLAGS"
      SDL2_LDFLAGS="$LIBS"
  ],[
      AC_MSG_RESULT(no)]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST(SDL2)
])
