
/******************************************************************************
* MODULE     : html_math.hpp
* DESCRIPTION: static formulas for the HTML export, as SVG
* COPYRIGHT  : (C) 2026 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HTML_MATH_H
#define HTML_MATH_H

#include "renderer.hpp"
#include "tree.hpp"

// A renderer which records what a box draws. Formulas are typeset by the
// normal typesetter, in whatever fonts the document selects, and redrawn on
// this renderer; em is the size in SI of one em of the surrounding HTML text.
renderer html_math_renderer (SI em);

// The SXML of the formula drawn on ren, whose logical extents are given, as
// (h:svg ...); an empty tuple when it drew something SVG cannot represent
// here (a picture), or without FreeType.
scheme_tree html_math_svg (renderer ren, SI x1, SI y1, SI x2, SI y2);

// The definitions of the glyphs and clips used since the previous call, as a
// hidden (h:svg ...) to put once in the document; an empty tuple when none.
scheme_tree html_math_defs ();

#endif // HTML_MATH_H
