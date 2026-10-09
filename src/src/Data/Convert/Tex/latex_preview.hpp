
/******************************************************************************
* MODULE     : latex_preview.hpp
* DESCRIPTION: Pictures of pieces of LaTeX which are not converted
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* TeXmacs made them with the LaTeX of the system and Ghostscript (the plugin
* LaTeX_Preview). Tau has neither: no picture is made, and the importer
* keeps the source of what it does not convert.
******************************************************************************/

#ifndef LATEX_PREVIEW_H
#define LATEX_PREVIEW_H
#include "tree.hpp"
#include "array.hpp"

inline array<tree> latex_preview (string s, tree t) {
  (void) s; (void) t; return array<tree> (); }
inline void set_latex_command (string cmd) { (void) cmd; }

#endif // defined LATEX_PREVIEW_H
