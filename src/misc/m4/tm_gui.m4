
#--------------------------------------------------------------------
#
# MODULE      : tm_gui.m4
# DESCRIPTION : GUI selection
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_GUI],[

  CONFIG_X11=""
  CONFIG_COCOA=""
  CONFIG_GUI="X11"
  CONFIG_QTPIPES="no"

  AC_ARG_WITH(gui,[  --with-gui=GUI   GUI type selector: qt (default), qtwk, x11, aqua, sdl, vue],
            gui_selector="$withval", gui_selector="qt")

  case "$gui_selector" in
      qt | qtwk)
         LC_WITH_QT
         if test x"$at_cv_qt_build" = xko; then 
            AC_MSG_ERROR([cannot find Qt!])
         else
            if test x"$gui_selector" = xqt; then
               AC_MSG_RESULT([enabling Qt port])
               CONFIG_GUI="QT"
            else 
               AC_MSG_RESULT([enabling Qt port with Widkit])
               CONFIG_GUI="QTWK"
            fi
            if test x"$CONFIG_OS" = xMACOS; then
               # on Mac we rely on some ObjC code contained in 
               # src/Plugins/MacOS    
               CONFIG_MACOS="MacOS"
            fi
            # Qt Plugins list
            if test $QT_MAJOR -ge 5 
            then QT_PLUGINS_LIST="imageformats,platforms"
            else QT_PLUGINS_LIST="accessible,imageformats"
            fi
         fi
         CONFIG_QTPIPES="yes"
         ;;
      x11)
         AC_MSG_RESULT([enabling X11 port])
         LC_X_HEADERS
         AC_PATH_X
         AC_PATH_XTRA
         ;;
      cocoa)
         AC_MSG_RESULT([enabling experimental Cocoa port])
         COCOA_CFLAGS=""
         COCOA_LDFLAGS="-framework Cocoa"
         CONFIG_GUI="COCOA"
         ;;
      sdl) 
         AC_MSG_RESULT([enabling experimental SDL port])
         LC_SDL3
         AC_MSG_RESULT([SDL3_CFLAGS=$SDL3_CFLAGS])
         AC_MSG_RESULT([SDL3_LDFLAGS=$SDL3_LDFLAGS])
         AC_MSG_RESULT([SDL3_LIBS=$SDL3_LIBS])
         SDL_CFLAGS="$SDL3_CFLAGS"
         SDL_LDFLAGS="$SDL3_LDFLAGS"
         SDL_LIBS="$SDL3_LIBS"
         CONFIG_GUI="SDL"
         ;;
      vue) 
         AC_MSG_RESULT([enabling experimental Vue/SDL immediate mode GUI])
         LC_SDL3
         AC_MSG_RESULT([SDL3_CFLAGS=$SDL3_CFLAGS])
         AC_MSG_RESULT([SDL3_LDFLAGS=$SDL3_LDFLAGS])
         AC_MSG_RESULT([SDL3_LIBS=$SDL3_LIBS])
         VUE_CFLAGS="-std=c++20 $SDL3_CFLAGS"
         VUE_LDFLAGS="$SDL3_LDFLAGS"
         VUE_LIBS="$SDL3_LIBS"
         CONFIG_GUI="VUE"
         ;;
      *)
         AC_MSG_ERROR([bad option --with-gui=$gui_selector])
         ;;
  esac

  # Qt Pipes
  AC_ARG_ENABLE(qtpipes,
  [  --enable-qtpipes        replace UNIX pipes by Qt pipes],
      [], [enable_qtpipes=$CONFIG_QTPIPES])

  case "$enable_qtpipes" in
      yes)
         case "$CONFIG_GUI" in
            QT | QTWK )
               AC_DEFINE(QTPIPES, 1, [Enabling Qt pipes])
               AC_MSG_RESULT([enabling Qt pipes])
               ;;
            *)
               AC_MSG_ERROR([QT not enabled!])
               ;;
         esac
         ;;
      no)
         if test x"$CONFIG_GUI" = xQT; then
            AC_MSG_RESULT([disabling Qt pipes])
         fi
         ;;
      *)
         AC_MSG_ERROR([bad option --enable-qtpipes=$enable_qtpipes])
         ;;
  esac

  case "$CONFIG_GUI" in
      X11)
         CONFIG_X11="X11 Widkit"
         if test "x${CONFIG_GS}" != "xGhostscript"; then
           CONFIG_X11="$CONFIG_X11 Ghostscript"
         fi
         CONFIG_GUI_DEFINE="X11TEXMACS"
         AC_DEFINE(X11TEXMACS, 1, [Use standard X11 port])
         ;;
      COCOA)
         CONFIG_COCOA="Cocoa"
         CONFIG_GUI_DEFINE="AQUATEXMACS"
         AC_DEFINE(AQUATEXMACS, 1, [Enable experimental Cocoa port])
         ;;
      QT)
         CONFIG_QT="Qt"
         CONFIG_GUI_DEFINE="QTTEXMACS"
         AC_DEFINE(QTTEXMACS, 1, [Enable standard Qt port])
         ;;
      QTWK)
         CONFIG_QT="Qtwk Widkit"
         # HACK!
         CONFIG_GUI_DEFINE="QTWKTEXMACS -DQTTEXMACS" 
         AC_DEFINE(QTTEXMACS, 1, [Enable Qt port])
         AC_DEFINE(QTWKTEXMACS, 1, [Enable experimental Qt port with Widkit])
         ;;
      SDL)
         CONFIG_SDL="SDL Widkit"
         CONFIG_GUI_DEFINE="SDLTEXMACS"
         AC_DEFINE(SDLTEXMACS, 1, [Enable experimental SDL port])
         ;;
      VUE)
         CONFIG_VUE="Vue"
         CONFIG_GUI_DEFINE="VUETEXMACS"
         AC_DEFINE(VUETEXMACS, 1, [Enable experimental Vue port])
         ;;
  esac

  AC_SUBST(CONFIG_X11)
  AC_SUBST(CONFIG_COCOA)
  AC_SUBST(CONFIG_QT)
  AC_SUBST(CONFIG_SDL)
  AC_SUBST(CONFIG_VUE)
  AC_SUBST(CONFIG_GUI)
  AC_SUBST(CONFIG_GUI_DEFINE)

  AC_SUBST(QT_FRAMEWORKS_PATH)  
  AC_SUBST(QT_PLUGINS_PATH)
  AC_SUBST(QT_PLUGINS_LIST)

  AC_SUBST(COCOA_CFLAGS)
  AC_SUBST(COCOA_LDFLAGS)

  AC_SUBST(SDL_CFLAGS)
  AC_SUBST(SDL_LDFLAGS)

  AC_SUBST(VUE_CFLAGS)
  AC_SUBST(VUE_LDFLAGS)
])
