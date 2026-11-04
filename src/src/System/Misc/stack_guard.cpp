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
    ULONG_PTR l= 0, h= 0;
    GetCurrentThreadStackLimits (&l, &h);
    low= (char*) l;
  }
  if (low == NULL) return -1;
  return (long) (&here - low);
}
#else
#  include <pthread.h>
#  include <sys/resource.h>
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
#  else
    pthread_attr_t a;
    if (pthread_getattr_np (pthread_self (), &a) == 0) {
      void* addr= NULL; size_t size= 0;
      if (pthread_attr_getstack (&a, &addr, &size) == 0) low= (char*) addr;
      pthread_attr_destroy (&a);
    }
#  endif
  }
  if (low == NULL) return -1;
  return (long) (&here - low);
}
#endif
