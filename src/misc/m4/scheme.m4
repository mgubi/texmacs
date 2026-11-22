
#--------------------------------------------------------------------
#
# MODULE      : scheme.m4
# DESCRIPTION : Selection of the Scheme interpreter (S7, Guile, femtolisp or Chez)
# COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_SCHEME],[
  AC_ARG_WITH(scheme,
    AS_HELP_STRING([--with-scheme@<:@=s7|guile|femtolisp|chez@:>@],
                   [Scheme interpreter to use @<:@default=s7@:>@]),
    [], [with_scheme=s7])
  case "$with_scheme" in
    (s7 | S7)
      AC_MSG_NOTICE([using the S7 Scheme interpreter (vendored in src/Scheme/S7)])
      AC_DEFINE([USE_S7], [1], [Use the S7 Scheme interpreter])
      SCHEME_DIR=S7
      ;;
    (guile | Guile)
      AC_MSG_NOTICE([using the Guile Scheme interpreter])
      AC_DEFINE([USE_GUILE], [1], [Use the Guile Scheme interpreter])
      SCHEME_DIR=Guile
      LC_GUILE
      ;;
    (femtolisp | Femtolisp)
      AC_MSG_NOTICE([using the femtolisp interpreter (vendored in src/Scheme/Femtolisp)])
      AC_DEFINE([USE_FEMTOLISP], [1], [Use the femtolisp interpreter])
      SCHEME_DIR=Femtolisp
      ;;
    (chez | Chez)
      AC_MSG_NOTICE([using Chez Scheme])
      LC_CHEZ
      AC_DEFINE([USE_CHEZ], [1], [Use Chez Scheme])
      SCHEME_DIR=Chez
      ;;
    (*)
      AC_MSG_ERROR([unknown Scheme interpreter '$with_scheme', use s7, guile, femtolisp or chez])
      ;;
  esac
  AC_SUBST(SCHEME_DIR)
])

#--------------------------------------------------------------------
# Chez Scheme is linked statically from the "machine directory" of an
# installation, which holds scheme.h, libkernel.a (with liblz4.a and
# libz.a) and the heap images petite.boot and scheme.boot, e.g.
# /opt/homebrew/lib/csv10.4.1/tarm64osx.  --with-chez=DIR gives it;
# otherwise it is derived from the chez (or scheme) executable.
#--------------------------------------------------------------------

AC_DEFUN([LC_CHEZ],[
  AC_ARG_WITH(chez,
    AS_HELP_STRING([--with-chez@<:@=DIR@:>@],
                   [machine directory of Chez Scheme (with scheme.h and libkernel.a)]),
    [], [with_chez=auto])
  CHEZ_DIR=""
  if test "$with_chez" != "auto" -a "$with_chez" != "yes"; then
    CHEZ_DIR="$with_chez"
  else
    AC_PATH_PROGS([CHEZ_BIN], [chez scheme chezscheme])
    if test -n "$CHEZ_BIN"; then
      chez_machine=`echo '(display (machine-type))' | "$CHEZ_BIN" -q 2>/dev/null`
      chez_version=`echo '(call-with-values scheme-version-number (lambda (a b c) (printf "~a.~a.~a" a b c)))' | "$CHEZ_BIN" -q 2>/dev/null`
      chez_prefix=`dirname "$CHEZ_BIN"`/..
      for d in "$chez_prefix/lib/csv$chez_version/$chez_machine" \
               "$chez_prefix/lib/csv$chez_version-pre-release.1/$chez_machine"; do
        if test -f "$d/scheme.h"; then CHEZ_DIR=`cd "$d" && pwd`; break; fi
      done
    fi
  fi
  AC_MSG_CHECKING([for the Chez Scheme machine directory])
  if test -z "$CHEZ_DIR" -o ! -f "$CHEZ_DIR/scheme.h" -o ! -f "$CHEZ_DIR/libkernel.a"; then
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([Chez Scheme not found: give its machine directory with --with-chez=DIR])
  fi
  AC_MSG_RESULT([$CHEZ_DIR])
  CPPFLAGS="$CPPFLAGS -I$CHEZ_DIR"
  CHEZ_LIBS="$CHEZ_DIR/libkernel.a"
  for l in lz4 z; do
    if test -f "$CHEZ_DIR/lib$l.a"; then CHEZ_LIBS="$CHEZ_LIBS $CHEZ_DIR/lib$l.a"
    else CHEZ_LIBS="$CHEZ_LIBS -l$l"; fi
  done
  case "$host_os" in
    (darwin*) CHEZ_LIBS="$CHEZ_LIBS -liconv -lncurses" ;;
    (mingw*)  CHEZ_LIBS="$CHEZ_LIBS -lole32 -lrpcrt4 -luuid" ;;
    (*)       CHEZ_LIBS="$CHEZ_LIBS -lncurses -ldl -lm" ;;
  esac
  CHEZ_LIBS="$CHEZ_LIBS -lpthread"
  AC_SUBST(CHEZ_LIBS)
  AC_SUBST(CHEZ_DIR)
  AC_DEFINE_UNQUOTED([CHEZ_BOOT_DIR], ["$CHEZ_DIR"],
                     [Default directory of the Chez Scheme heap images])
  if test -z "$CHEZ_BIN"; then
    for b in chez scheme; do
      if test -x "$CHEZ_DIR/$b"; then CHEZ_BIN="$CHEZ_DIR/$b"; break; fi
    done
  fi
  AC_SUBST(CHEZ_BIN)
])
