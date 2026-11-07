
/******************************************************************************
* MODULE     : build_revision.cpp
* DESCRIPTION: the revision of the sources a TeXmacs was built from (#753)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// This is the only unit which knows the commit: git_revision.hpp is made by
// the build (misc/scripts/git-revision.sh with make, cmake/GitRevision.cmake
// with CMake) at each run and rewritten only when the commit changed, so a
// new commit rebuilds this file and relinks, nothing else. config.h, with
// TEXMACS_REVISION, is not touched.

#include "build_revision.hpp"

#if defined(__has_include)
#if __has_include("git_revision.hpp")
#include "git_revision.hpp"
#endif
#endif

#ifndef TM_GIT_REVISION
#define TM_GIT_REVISION ""
#endif

string
get_build_revision () {
  string git (TM_GIT_REVISION);
  if (N(git) != 0) return git;
  return TEXMACS_REVISION;
}
