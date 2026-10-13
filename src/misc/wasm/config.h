/******************************************************************************
* MODULE     : config.h (WebAssembly)
* DESCRIPTION: The configuration of the browser build, in place of the one
*              configure writes (it cannot run the programs it compiles when
*              cross compiling). wasm32: pointers and long are 4 bytes.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#define ALIGNOF_VOID_P 4
#define ALTERNATIVE_VERSION "2.1."
#define DEBUG_ASSERT 1
#define HAVE_FILE 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_INTPTR_T 1
#define HAVE_INTTYPES_H 1
#define HAVE_SNPRINTF 1
#define HAVE_STDINT_H 1
#define HAVE_STDIO_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_TIME_T 1
#define HAVE_UNISTD_H 1
#define LINKED_FREETYPE 1
#define MAX_FAST 260
#define MUPDF_RENDERER 1
#define PACKAGE_BUGREPORT ""
#define PACKAGE_NAME ""
#define PACKAGE_STRING ""
#define PACKAGE_TARNAME ""
#define PACKAGE_URL ""
#define PACKAGE_VERSION ""
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_LONG_LONG 8
#define SIZEOF_SHORT 2
#define SIZEOF_VOID_P 4
#define STDC_HEADERS 1
#define TEXMACS_REVISION "wasm"
#define USE_FREETYPE 3
#define USE_ICONV 1
#define USE_MUPDF 1
// the Scheme interpreter: S7, or femtolisp with SCHEME=femtolisp (Makefile)
#ifndef USE_FEMTOLISP
#define USE_S7 1
#endif
#define USE_SDL3 1
// the spell checker in the program (src/Plugins/Ispell/ispell_hunspell.cpp)
#define USE_HUNSPELL 1
#define VUETEXMACS 1
#define WORD_LENGTH 4
#define WORD_LENGTH_INC 3
#define WORD_MASK 0xfffffffc
