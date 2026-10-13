/******************************************************************************
* MODULE     : config.h (Tau)
* DESCRIPTION: The configuration of the native build of the core of Tau
*              (misc/tau/native/Makefile), after the one of the build for
*              WebAssembly (misc/tau/config.h): pointers and long are 8 bytes.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#define ALIGNOF_VOID_P 8
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
#define MAX_FAST 264
#define MUPDF_RENDERER 1
#define PACKAGE_BUGREPORT ""
#define PACKAGE_NAME ""
#define PACKAGE_STRING ""
#define PACKAGE_TARNAME ""
#define PACKAGE_URL ""
#define PACKAGE_VERSION ""
#define SIZEOF_INT 4
#define SIZEOF_LONG 8
#define SIZEOF_LONG_LONG 8
#define SIZEOF_SHORT 2
#define SIZEOF_VOID_P 8
#define STDC_HEADERS 1
#define TEXMACS_REVISION "native"
#define USE_FREETYPE 3
#define USE_ICONV 1
#define USE_MUPDF 1
#define USE_S7 1

#define VUETEXMACS 1
#define TAU_SHELL 1
#define WORD_LENGTH 8
#define WORD_LENGTH_INC 7
#define WORD_MASK 0xfffffffffffffff8

// The machine: Vue behaves as each system expects (on a Mac the system
// glides after a swipe and Vue must not, option composes characters, the
// wheel zooms with command...)
#ifdef __APPLE__
#define OS_MACOS 1
#else
#define OS_GNU_LINUX 1
#endif
