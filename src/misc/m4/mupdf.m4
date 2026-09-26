AC_DEFUN([LC_MUPDF], [
  AC_ARG_WITH([mupdf],
    [AS_HELP_STRING([--with-mupdf=DIR],
      [Path to MuPDF installation prefix (optional)])],
    [MUPDF_DIR=$withval],
    [MUPDF_DIR=""]
  )


  if test "x$MUPDF_DIR" != "x"; then
    MUPDF_CFLAGS="-I$MUPDF_DIR/include"
    MUPDF_LIBS="-L$MUPDF_DIR/lib -lmupdf -lmupdf-third"
  else
    MUPDF_CFLAGS=""
    MUPDF_LIBS="-lmupdf -lmupdf-third"
  fi

  CPPFLAGS_SAVED="$CPPFLAGS"
  LDFLAGS_SAVED="$LDFLAGS"
  LIBS_SAVED="$LIBS"

  CPPFLAGS="$CPPFLAGS $MUPDF_CFLAGS"
  LDFLAGS="$LDFLAGS $MUPDF_LIBS"
  LIBS="$LIBS $MUPDF_LIBS"
  
  AC_CHECK_HEADER(mupdf/pdf.h)
  AC_MSG_CHECKING([for usable MuPDF])
  AC_LINK_IFELSE([
    AC_LANG_PROGRAM([
      #include <mupdf/fitz.h>
    ], [
      fz_context *ctx = fz_new_context(NULL, NULL, FZ_STORE_DEFAULT);
      fz_drop_context(ctx);
    ])
  ], [
    AC_MSG_RESULT(yes)
    mupdf_usable=yes
    MUPDF_CPPFLAGS="$CPPFLAGS"
    MUPDF_LDFLAGS="$LDFLAGS"
    if test "$with_mupdf" = "linked" ; then
      MUPDF_LIBS="$LIBS"
    fi
  ], [
    AC_MSG_RESULT(no)
    mupdf_usable=no])

  CPPFLAGS="$CPPFLAGS_SAVED"
  LDFLAGS="$LDFLAGS_SAVED"
  LIBS="$LIBS_SAVED"

  LC_SUBST(MUPDF)
])

# Whether MuPDF is used depends on the GUI (TM_GUI must have run): the Qt,
# SDL and Vue ports draw their pictures with it (MUPDF_RENDERER), while the
# X11 and Cocoa ports have pictures of their own, which would clash with
# those of MuPDF at link time.
AC_DEFUN([TM_MUPDF_FOR_GUI], [
  CONFIG_MUPDF=""
  case "$CONFIG_GUI" in
    X11 | COCOA)
      if test "$mupdf_usable" = yes; then
        if test "x$with_mupdf" != x -a "x$with_mupdf" != xno; then
          AC_MSG_ERROR([--with-gui=$gui_selector cannot draw with MuPDF: configure it without --with-mupdf])
        fi
        AC_MSG_RESULT([not using MuPDF with --with-gui=$gui_selector])
      fi
      MUPDF_CFLAGS=""
      MUPDF_LIBS=""
      MUPDF_CPPFLAGS=""
      MUPDF_LDFLAGS=""
      ;;
    *)
      if test "$mupdf_usable" = yes; then
        AC_DEFINE(USE_MUPDF, 1, [Use MuPDF library])
        if test "$with_mupdf" = "linked" ; then
          AC_DEFINE(LINKED_MUPDF, 1, [Link MuPDF library with TeXmacs])
        fi
        AC_DEFINE(MUPDF_RENDERER, 1, [Enabling native MuPDF backend])
        CONFIG_MUPDF="MuPdf"
      else
        case "$CONFIG_GUI" in
          SDL | VUE)
            AC_MSG_ERROR([--with-gui=$gui_selector needs MuPDF: use --with-mupdf=DIR]) ;;
        esac
      fi
      ;;
  esac
  AC_SUBST(CONFIG_MUPDF)
])
