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
    AC_DEFINE(USE_MUPDF, 1, [Use MuPDF library])
    MUPDF_CPPFLAGS="$CPPFLAGS"
    MUPDF_LDFLAGS="$LDFLAGS"
    if test "$with_mupdf" = "linked" ; then
      MUPDF_LIBS="$LIBS"
      AC_DEFINE(LINKED_MUPDF, 1, [Link MuPDF library with TeXmacs])
    fi
    AC_DEFINE(MUPDF_RENDERER, 1, [Enabling native MuPDF backend])
    CONFIG_MUPDF="MuPdf"
    AC_SUBST(CONFIG_MUPDF)
  ], [
    AC_MSG_RESULT(no)])

  CPPFLAGS="$CPPFLAGS_SAVED"
  LDFLAGS="$LDFLAGS_SAVED"
  LIBS="$LIBS_SAVED"

  LC_SUBST(MUPDF)
])
