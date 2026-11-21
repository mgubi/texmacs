/******************************************************************************
* MODULE     : stack_guard.cpp
* DESCRIPTION: How much of the stack of the thread is left
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "stack_guard.hpp"
#include "config.h"
#include <stdint.h>
#include <stddef.h>

char* (*scheme_stack_low) () = NULL;

static inline long
scheme_remaining (char* here, long n) {
  // the bytes left before the limit of the Scheme interpreter, if it is
  // nearer than @n (or @n is unknown, -1)
  char* s= scheme_stack_low == NULL ? (char*) NULL : scheme_stack_low ();
  if (s == NULL) return n;
  long m= here > s ? (long) (here - s) : 0L;
  return (n < 0 || m < n) ? m : n;
}

#if defined (__EMSCRIPTEN__)
#  include <emscripten/stack.h>
long
stack_remaining () {
  return (long) emscripten_stack_get_free ();
}
#elif defined (OS_MINGW)
#  include <windows.h>
long
stack_remaining () {
  static thread_local char* low= NULL;
  static thread_local bool  init= false;
  char here;
  if (!init) {
    init= true;
    // (the function exists from Windows 8 on, and the headers declare it only
    // for _WIN32_WINNT >= 0x0602: it is looked for at run time; without it, the
    // depth of the stack is not known)
    typedef void (WINAPI *limits_fn) (PULONG_PTR, PULONG_PTR);
    HMODULE k= GetModuleHandleA ("kernel32.dll");
    limits_fn f= k == NULL ? NULL : (limits_fn) GetProcAddress (k, "GetCurrentThreadStackLimits");
    if (f != NULL) {
      ULONG_PTR l= 0, h= 0;
      f (&l, &h);
      low= (char*) l;
    }
  }
  return scheme_remaining (&here, low == NULL ? -1L : (long) (&here - low));
}
#else
#  include <pthread.h>
#  include <sys/resource.h>
#  if defined (__FreeBSD__) || defined (__DragonFly__) || defined (__OpenBSD__)
#    include <pthread_np.h>
#  endif
#  if defined (__OpenBSD__)
#    include <signal.h>
#  endif
long
stack_remaining () {
  // (the low end of the stack of the thread is asked once for each thread)
  static thread_local char* low= NULL;
  static thread_local bool  init= false;
  char here;
  if (!init) {
    init= true;
#  if defined (__APPLE__)
    pthread_t t= pthread_self ();
    char* high= (char*) pthread_get_stackaddr_np (t);
    size_t size= pthread_get_stacksize_np (t);
    // (for the main thread, the system says 64 MB, the hard limit; the stack
    // is as large as the soft limit, 8 MB by default)
    if (pthread_main_np () != 0) {
      struct rlimit rl;
      if (getrlimit (RLIMIT_STACK, &rl) == 0 && rl.rlim_cur != RLIM_INFINITY &&
          (size_t) rl.rlim_cur < size)
        size= (size_t) rl.rlim_cur;
    }
    low= high - size;
#  elif defined (__linux__) || defined (__NetBSD__) || defined (__GLIBC__)
    pthread_attr_t a;
    if (pthread_getattr_np (pthread_self (), &a) == 0) {
      void* addr= NULL; size_t size= 0;
      if (pthread_attr_getstack (&a, &addr, &size) == 0) low= (char*) addr;
      pthread_attr_destroy (&a);
    }
#  elif defined (__FreeBSD__) || defined (__DragonFly__)
    pthread_attr_t a;
    pthread_attr_init (&a);
    if (pthread_attr_get_np (pthread_self (), &a) == 0) {
      void* addr= NULL; size_t size= 0;
      if (pthread_attr_getstack (&a, &addr, &size) == 0) low= (char*) addr;
    }
    pthread_attr_destroy (&a);
#  elif defined (__OpenBSD__)
    stack_t ss;
    if (pthread_stackseg_np (pthread_self (), &ss) == 0)
      low= (char*) ss.ss_sp - ss.ss_size;
#  endif
    // (on other systems the depth of the stack is not known: no check)
  }
  return scheme_remaining (&here, low == NULL ? -1L : (long) (&here - low));
}
#endif

int
tree_max_depth () {
  long left= stack_remaining ();
  if (left < 0) left= 8L * 1024L * 1024L;
  return (int) (left / 4096);
}
