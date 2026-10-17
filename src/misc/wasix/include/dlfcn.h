/******************************************************************************
* MODULE     : dlfcn.h (WASIX)
* DESCRIPTION: dlopen and co for the static build of WASIX, whose sysroot
*              (the one without PIC) has no dynamic loading: nothing is ever
*              loaded, as on a system where the library is missing
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WASIX_DLFCN_H
#define WASIX_DLFCN_H

#define RTLD_LAZY   1
#define RTLD_NOW    2
#define RTLD_GLOBAL 256
#define RTLD_LOCAL  0
#define RTLD_NOLOAD 4
#define RTLD_DEFAULT ((void*) 0)

static inline void* dlopen (const char* f, int m) { (void) f; (void) m; return 0; }
static inline void* dlsym (void* h, const char* s) { (void) h; (void) s; return 0; }
static inline int dlclose (void* h) { (void) h; return 0; }
static inline char* dlerror (void) { return (char*) "no dynamic loading (WASIX)"; }

#endif
