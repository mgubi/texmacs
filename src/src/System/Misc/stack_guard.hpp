/******************************************************************************
* MODULE     : stack_guard.hpp
* DESCRIPTION: How much of the stack of the thread is left
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef STACK_GUARD_H
#define STACK_GUARD_H

// The bytes left on the stack of the calling thread, or -1 when the system
// does not tell. A macro that calls itself (the evaluator and the typesetter
// expand it by recursion) must give an error box before the stack is full, not
// a crash: the recursions of macros stop when less than STACK_MARGIN is left,
// whatever the size of the stack (8 MB on the main thread of macOS and Linux,
// 16 MB with the CMake build for Windows, less in other threads) and the size
// of the frames (about 8 KB for each level of macro in a paragraph: with an
// 8 MB stack, a recursion of 800 levels is typeset; before, one of 1500 was a
// crash).
long stack_remaining ();

#define STACK_MARGIN (1024L * 1024L)

inline bool
stack_nearly_full () {
  long n= stack_remaining ();
  return n >= 0 && n < STACK_MARGIN;
}

#endif // STACK_GUARD_H
