
/******************************************************************************
* MODULE     : tex_unicode.hpp
* DESCRIPTION: the Unicode characters of the glyphs of the TeX math fonts
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEX_UNICODE_H
#define TEX_UNICODE_H
#include "string.hpp"
#include "array.hpp"
#include "url.hpp"

// The Unicode characters of the glyphs of a TeX math font (cmmi10, cmex10,
// msbm10...), by position, for the text layer of a PDF: an empty table for
// the other fonts, and -1 at a position whose symbol has no single character.
// The larger variants of a symbol (in cmex) stand for the symbol.
array<int> tex_math_unicode (string fontname);

// The Unicode character of the glyph at position ch of a TeX font in the
// Cork encoding (the EC fonts), or in T2A (the Cyrillic LH fonts)
int tex_glyph_unicode (int ch, bool t2a);

// Whether the font file u is a Cyrillic TeX font in the T2A encoding
bool is_t2a_font_file (url u);

#endif // TEX_UNICODE_H
