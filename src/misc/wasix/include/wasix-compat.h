/******************************************************************************
* MODULE     : wasix-compat.h (WASIX)
* DESCRIPTION: Included before each source of TeXmacs in the WASIX build
*              (misc/wasix/Makefile, -include)
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WASIX_COMPAT_H
#define WASIX_COMPAT_H

// setjmp and longjmp are made into exceptions of WebAssembly by LLVM, which
// knows them by these names only: sigsetjmp, a function of the libc of
// WASIX, made a longjmp which nothing caught (the errors of S7). The libc
// of Emscripten has the same macros; there are no signal masks to restore
#include <setjmp.h>
#undef sigsetjmp
#undef siglongjmp
#define sigjmp_buf jmp_buf
#define sigsetjmp(env, save) setjmp (env)
#define siglongjmp(env, val) longjmp (env, val)

#endif
