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

// The depth of the trees which the editor can take: the bridges, the
// typesetter and the paths recurse on the levels of a tree, with up to about
// 2 KB of stack for each (a <with> in a paragraph); 4 KB are counted, of the
// stack which is left (or of 8 MB, if the system does not tell), about 2000
// levels on the main thread. The readers of .tm files (fromtm.cpp) and the
// conversions of Scheme lists to trees (glue.cpp) cut what is deeper, with an
// error "nesting too deep" in its place (#813: deeper, the recursions of C++
// on the tree crashed)
int tree_max_depth ();

inline bool
stack_nearly_full () {
  long n= stack_remaining ();
  return n >= 0 && n < STACK_MARGIN;
}

// The Scheme interpreter may have a limit of its own for the stack it shares
// with C++ (Guile 1.8 stops with "Stack overflow" when the stack is deeper
// than its option stack, counted from where Guile started). It then sets this
// hook, which gives the lowest address that a frame may reach for the
// interpreter in the calling thread, or NULL when it has no limit there:
// stack_remaining counts up to it, if it comes first, so that the guards stop
// a recursion of C++ while the Scheme called from it (a message, a macro of
// Scheme) can still run (#813: on Linux the stack is 16 MB, set in main, but
// Guile stopped at 8 MB, in the message of the reader of a deep file)
extern char* (*scheme_stack_low) ();

#endif // STACK_GUARD_H
