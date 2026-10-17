/******************************************************************************
* MODULE     : sdl-compat.h (WASIX)
* DESCRIPTION: What SDL3 calls of POSIX and the sysroot of WASIX lacks: the
*              priorities of threads (one priority for all), included before
*              each source of SDL by build-deps.sh (-include)
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WASIX_SDL_COMPAT_H
#define WASIX_SDL_COMPAT_H

// (first: SDL's own sources define it, which a header included before
// them must as well, else pthread_setname_np is not declared)
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include <pthread.h>
#include <sched.h>

static inline int
pthread_getschedparam (pthread_t t, int* policy, struct sched_param* p) {
  (void) t; if (policy) *policy= 0; if (p) p->sched_priority= 0; return 0; }
static inline int
pthread_setschedparam (pthread_t t, int policy, const struct sched_param* p) {
  (void) t; (void) policy; (void) p; return 0; }
static inline int sched_get_priority_min (int policy) { (void) policy; return 0; }
static inline int sched_get_priority_max (int policy) { (void) policy; return 0; }

#endif
