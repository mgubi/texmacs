
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

// The characters of the codes of a font of glyphs in the given encoding
// (font_glyphs_rep::encoding: the .enc files of TeXmacs joined by "+",
// cork, t2a, ot1, ot1-tt, adobe), by position, for the text layer of a PDF;
// -1 at a code of no single character, the larger variants of a symbol
// (after the TeX metric, cmex10.tfm) standing for the symbol. An empty
// table for no encoding, and for the text encodings (cork, ot1, adobe)
// unless text holds, a reader then going by the names of the glyphs.
array<int> tex_encoding_table (string encoding, string metric, bool text);

// The Unicode character of the glyph at position ch of a TeX font in the
// Cork encoding (the EC fonts), or in T2A (the Cyrillic LH fonts)
int tex_glyph_unicode (int ch, bool t2a);

// The character of code c of a font of glyphs in the given encoding (its
// table, Unicode, the double-struck forms of bbb:...), for a tagged PDF;
// U+FFFD for a code of no character, of no encoding, and for a control
int tex_encoding_char (string encoding, string metric, int c);

#endif // TEX_UNICODE_H
