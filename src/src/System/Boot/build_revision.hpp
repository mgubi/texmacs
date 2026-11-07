
/******************************************************************************
* MODULE     : build_revision.hpp
* DESCRIPTION: the revision of the sources a TeXmacs was built from
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BUILD_REVISION_H
#define BUILD_REVISION_H
#include "string.hpp"

// "ebf1221c79 2026-11-01T00:41:00Z" (the commit of the git checkout and its
// time) when the build knew it, else TEXMACS_REVISION ("Custom ...", #753)
string get_build_revision ();

#endif // defined BUILD_REVISION_H
