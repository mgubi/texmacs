
/******************************************************************************
* MODULE     : pdf_font_subst.hpp
* DESCRIPTION: fonts to embed for the fonts which a PDF picture names only
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
* A PDF picture (a plot) often names its fonts (Times-Roman, Helvetica,
* Arial) without embedding them, and leaves them to the reader. A tagged
* PDF (PDF/UA) wants every font embedded: the PDF writers then embed a font
* which TeXmacs ships instead (docs/pdf-tagging.md), and this says which,
* and what goes with it: the glyph of each code, its width, its character.
*
******************************************************************************/

#ifndef PDF_FONT_SUBST_H
#define PDF_FONT_SUBST_H
#include "url.hpp"
#include "array.hpp"

struct pdf_font_subst {
  url    file;            // the font program to embed (OpenType, CFF)
  string ps_name;         // its PostScript name (/BaseFont, /FontName)
  int    flags;           // of the font descriptor
  int    bbox[4];         // in thousandths of an em
  int    italic, ascent, descent, cap_height, stem_v;
  int    first, last;     // the codes which have a glyph
  array<int>    widths;   // by code, from first to last (thousandths)
  array<int>    unicode;  // by code (0..255): its character, -1 for none
  array<string> glyph;    // by code: the name of its glyph, "" for no
                          // code, ".notdef" for a code of no glyph
  int    missing;         // the number of the codes of no glyph
  int    missing_width;   // the width of .notdef (MissingWidth)
  array<int>    gids;     // the glyphs of the codes in the program, with 0,
                          // in order and once each
};

// The names of the glyphs of the 256 codes of a simple font: its encoding
// (StandardEncoding, WinAnsiEncoding, MacRomanEncoding, or "" for the
// font's own: that of Symbol for Symbol, Standard otherwise), then its
// differences (diffs[c] != "")
array<string> pdf_encoding_names (string base_font, string encoding,
                                  array<string> diffs);

// The font to embed for the font base_font, whose codes are the glyphs of
// these names: false when TeXmacs has none (ZapfDingbats)
bool pdf_font_substitute (string base_font, array<string> names,
                          pdf_font_subst& r);

// The bare CFF program of an OpenType font (its table CFF), which a PDF
// embeds as /FontFile3 /Type1C and its writers can subset; "" if none
string pdf_cff_program (url file);

// The character of a glyph name (of the standard encodings, uniXXXX,
// uXXXX, a symbol of TeXmacs, before a dot): -1 when unknown
int pdf_glyph_name_unicode (string name);

// The /ToUnicode CMap of the codes of a simple font
string pdf_simple_to_unicode (array<int> unicode);

#endif // PDF_FONT_SUBST_H
