
/******************************************************************************
* MODULE     : tt_tools.cpp
* DESCRIPTION: Direct access of True Type font (independent from FreeType)
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tt_tools.hpp"
#include "tt_file.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "analyze.hpp" // as_hexadecimal

#ifdef HAVE_STDINT_H
#include <stdint.h>
#define U8  uint8_t
#define U16 uint16_t
#define U32 uint32_t
#define F32 uint32_t
#define U64 uint64_t
#else
#define U8  unsigned char
#define U16 unsigned short
#define U32 unsigned int
#define U64 unsigned long long int
#endif

string strip_suffix (string name);

/******************************************************************************
* Data access
******************************************************************************/

string
get_sub (string s, int i, int j) {
  i= min (max (i, 0), N(s));
  j= min (max (j, 0), N(s));
  return s (i, j);
}

string
get_tag (string s, int i) {
  return get_sub (s, i, i+4);
}

U8
get_U8 (string s, int i) {
  return *((U8*) ((void*) (&s[i])));
}

U16
get_U16 (string s, int i) {
  return (((U16) get_U8 (s, i)) << 8) + get_U8 (s, i+1);
}

U32
get_U32 (string s, int i) {
  return (((U32) get_U16 (s, i)) << 16) + get_U16 (s, i+2);
}

F32
get_F32 (string s, int i) {
  return (F32) get_U32 (s, i);
}

string
pack_U32 (U32 i) {
  string r;
  r << ((char) ( i >> 24));
  r << ((char) ((i >> 16) & 255));
  r << ((char) ((i >>  8) & 255));
  r << ((char) ( i        & 255));
  return r;
}

/******************************************************************************
* True Type font collections
******************************************************************************/

bool
tt_is_collection (string tt) {
  return get_tag (tt, 0) == "ttcf";
}

int
tt_nr_fonts (string tt) {
  if (!tt_is_collection (tt)) return 1;
  else return get_U32 (tt, 8);
}

int
tt_header_index (string tt, int i) {
  ASSERT (i >= 0 && i < tt_nr_fonts (tt), "index out of range");
  if (!tt_is_collection (tt)) return 0;
  else return get_U32 (tt, 12 + 4*i);
}

/******************************************************************************
* Font table management
******************************************************************************/

bool
tt_correct_version (string tt, int i) {
  int h= tt_header_index (tt, i);
  return
    get_F32 (tt, h) == 0x00010000 ||
    get_tag (tt, h) == "OTTO" ||
    get_tag (tt, h) == "true" ||
    get_tag (tt, h) == "typ1";
}

int
tt_nr_tables (string tt, int i) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  int h= tt_header_index (tt, i);
  return get_U16 (tt, h + 4);
}

string
tt_table_tag (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  return get_tag (tt, h + 12 + 16 * k);
}

U32
tt_table_start (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  return get_U32 (tt, h + 20 + 16 * k);
}

string
tt_table (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  int start= get_U32 (tt, h + 20 + 16 * k);
  int len  = get_U32 (tt, h + 24 + 16 * k);
  return get_sub (tt, start, start + len);
}

string
tt_table (string tt, int i, string tag) {
  for (int k=0; k<tt_nr_tables (tt, i); k++)
    if (tt_table_tag (tt, i, k) == tag)
      return tt_table (tt, i, k);
  return "";
}

/******************************************************************************
* Extracting a subfont
******************************************************************************/

string
tt_extract_subfont (string tt, int i) {
  ASSERT (i >= 0 && i < tt_nr_fonts (tt), "index out of range");
  if (!tt_is_collection (tt)) return tt;
  string r;
  int h= tt_header_index (tt, i);
  r << get_sub (tt, h, h + 12);
  int nr_tabs= tt_nr_tables (tt, i);
  int offset= 12 + 16 * nr_tabs;
  for (int k=0; k < nr_tabs; k++) {
    int taboff= h + 12 + 16 * k;
    r << get_sub (tt, taboff, taboff + 8);
    r << pack_U32 (offset);
    r << get_sub (tt, taboff + 12, taboff + 16);
    int len= get_U32 (tt, taboff + 12);
    offset += (((len + 3) >> 2) << 2);
  }
  for (int k=0; k < nr_tabs; k++) {
    int taboff= h + 12 + 16 * k;
    int start= get_U32 (tt, taboff + 8);
    int len  = get_U32 (tt, taboff + 12);
    int plen = (((len + 3) >> 2) << 2);
    r << get_sub (tt, start, start + plen);
  }
  return r;
}

/******************************************************************************
* Name table
******************************************************************************/

int
name_format (string nt) {
  return get_U16 (nt, 0);
}

int
name_nr_records (string nt) {
  return get_U16 (nt, 2);
}

int
name_storage_offset (string nt) {
  return get_U16 (nt, 4);
}

string
name_record (string nt, int i) {
  ASSERT (i >= 0 && i < name_nr_records (nt), "index out of range");
  return get_sub (nt, 6 + 12*i, 18 + 12*i);
}

int
name_record_platform_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 0);
}

int
name_record_encoding_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 2);
}

int
name_record_language_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 4);
}

int
name_record_name_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 6);
}

string
name_record_string (string nt, int i) {
  int off= name_storage_offset (nt);
  string rec= name_record (nt, i);
  int start= get_U16 (rec, 10) + off;
  int len= get_U16 (rec, 8);
  return get_sub (nt, start, start + len);
}

string
filter_english (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (s[i] >= ' ' && (((unsigned int) s[i]) <= 127))
      r << s[i];
  return r;
}

string
name_record_english_string (string nt, int name_id) {
  for (int i=0; i < name_nr_records (nt); i++)
    if (name_record_name_id (nt, i) == name_id) {
      if ((name_record_platform_id (nt, i) == 1 &&
           name_record_language_id (nt, i) == 0) ||
          (name_record_platform_id (nt, i) == 3 &&
           name_record_language_id (nt, i) == 0x0409))
        return filter_english (name_record_string (nt, i));
    }
  return "";
}

string
name_record_family (string nt) {
  return name_record_english_string (nt, 1);
}

string
name_record_shape (string nt) {
  return name_record_english_string (nt, 2);
}


/******************************************************************************
* OpenType MATH table
******************************************************************************/

static void
parse_construction (string tt, unsigned int construction_offset,
                    array<unsigned int> variants,
                    array<int> adv,
                    array<unsigned int> assembly  ) {
  unsigned int glyphAssemblyOffset = get_U16 (tt, construction_offset);
  unsigned int variantCount = get_U16 (tt, construction_offset + 2);
  if (variantCount > 0) { // could be zero
    for (unsigned int j=0; j<variantCount; j++) {
      unsigned int mathGlyphVariantGlyph = get_U16 (tt, construction_offset + 4 + 4*j);
      int mathGlyphVariantAdvanceMeasurement = get_U16 (tt, construction_offset + 6 + 4*j);
      variants << mathGlyphVariantGlyph;
      adv << mathGlyphVariantAdvanceMeasurement;
    }
  }
  
  // read glyph assembly instructions
  if (glyphAssemblyOffset > 0) {
    assembly << (unsigned int) get_U16 (tt, construction_offset + glyphAssemblyOffset + 0); // italicsCorrection
    assembly << (unsigned int) get_U16 (tt, construction_offset + glyphAssemblyOffset + 2); // italicsCorrection
    unsigned int partCount = get_U16 (tt, construction_offset + glyphAssemblyOffset + 4);
    assembly << partCount; // partCount
    int pos = 6;
    for (unsigned int j=0; j< partCount; j++ ) {
      for (int k=0; k< 5; k++, pos += 2 ) {
        // part record
        assembly << (unsigned int) get_U16 (tt, construction_offset + glyphAssemblyOffset + pos );
      }
    }
  }
}

static void
parse_variants (string tt, int var_offset,
                int coverage_offset, int construction_offsets,
                hashmap<unsigned int, array<unsigned int> > glyph_variants,
                hashmap<unsigned int, array<int> > glyph_variants_adv,
                hashmap<unsigned int, array<unsigned int> > glyph_assembly ) {
  int glyphCoverageFormat = get_U16 (tt, var_offset + coverage_offset);
  if (glyphCoverageFormat == 1) {
    unsigned int glyphCount = get_U16 (tt, var_offset + coverage_offset + 2);
    for (unsigned int coverage_index=0; coverage_index< glyphCount; coverage_index++) {
      unsigned int glyph = get_U16 (tt, var_offset + coverage_offset + 4 + 2*coverage_index);
      unsigned int constructionOffset = get_U16 (tt, var_offset + construction_offsets + 2*coverage_index);
      array<unsigned int> variants;
      array<int> adv;
      array<unsigned int> assembly;
      parse_construction (tt, var_offset + constructionOffset,
                          variants, adv, assembly);
      if (N(variants)>0) {
        glyph_variants(glyph)= variants;
        glyph_variants_adv(glyph)= adv;
      }
      if (N(assembly)>0) {
        glyph_assembly(glyph)= assembly;
      }
    }
  } else if (glyphCoverageFormat == 2) {
    unsigned int rangeCount = get_U16 (tt, var_offset + coverage_offset + 2);
    unsigned int coverage_index= 0;
    for (unsigned int i=0; i<rangeCount; i++) {
      unsigned int startGlyphID = get_U16 (tt, var_offset + coverage_offset + 4 + 6*i);
      unsigned int endGlyphID = get_U16 (tt, var_offset + coverage_offset + 6 + 6*i);
      //unsigned int startCoverageIndex = get_U16 (tt, var_offset + coverage_offset + 8 + 6*i);
      for(unsigned int glyph=startGlyphID; glyph<=endGlyphID; glyph++, coverage_index++) {
        unsigned int constructionOffset = get_U16 (tt, var_offset + construction_offsets + 2*coverage_index);
        array<unsigned int> variants;
        array<int> adv;
        array<unsigned int> assembly;
        parse_construction (tt, var_offset + constructionOffset,
                            variants, adv, assembly);
        if (N(variants)>0) {
          glyph_variants(glyph)= variants;
          glyph_variants_adv(glyph)= adv;
        }
        if (N(assembly)>0) {
          glyph_assembly(glyph)= assembly;
        }
      }
    }
  } else {
    cout << "parse_mathtable : glyphCoverageFormat " << glyphCoverageFormat << " not supported." << LF;
  }
}

// parse the OpenType MATH table.
// buf is the full font file
// tt is a buffer which contains the full table.
ot_mathtable
parse_mathtable (string buf) {
  if ((N(buf) == 0) || (!tt_correct_version(buf, 0))) return ot_mathtable();
  //TODO: what if multiple fonts?
  string tt= tt_table (buf, 0, "MATH");
  if (N(tt) == 0) return ot_mathtable();

  ot_mathtable table (tm_new<ot_mathtable_rep>());
  
  // header
  table->majorVersion = get_U16 (tt, 0);
  table->minorVersion = get_U16 (tt, 2);
  int mathConstantsOffset = get_U16 (tt, 4);
  int mathGlyphInfoOffset = get_U16 (tt, 6);
  int mathVariantsOffset = get_U16 (tt, 8);

  if ((table->majorVersion != 1) || (table->minorVersion != 0))
    return ot_mathtable();
  
  // math constants
  for (int i=0; i<otmath_constants_end; i++) {
    table->params[i] = get_U16 (tt, mathConstantsOffset + 2*i);
  }
 
  // math glyph info
  int mathItalicsCorrectionInfoOffset = mathGlyphInfoOffset + get_U16 (tt, mathGlyphInfoOffset + 0);
  int mathTopAccentAttachmentOffset = mathGlyphInfoOffset + get_U16 (tt, mathGlyphInfoOffset + 2);
  //int extendedShapeCoverageOffset = mathGlyphInfoOffset + get_U16 (tt, mathGlyphInfoOffset + 4);
  //int mathKernInfoOffset = mathGlyphInfoOffset + get_U16 (tt, mathGlyphInfoOffset + 6);

  // italics corrections
  {
    int italicsCorrectionCoverageOffset = mathItalicsCorrectionInfoOffset + get_U16 (tt, mathItalicsCorrectionInfoOffset + 0);
    //int italicsCorrectionCount = get_U16 (tt, mathItalicsCorrectionInfoOffset + 2);
    int glyphCoverageFormat = get_U16 (tt, italicsCorrectionCoverageOffset + 0);
    if (glyphCoverageFormat == 1) {
      unsigned int glyphCount = get_U16 (tt, italicsCorrectionCoverageOffset + 2);
      for (unsigned int coverage_index=0; coverage_index< glyphCount; coverage_index++) {
        unsigned int glyph = get_U16 (tt, italicsCorrectionCoverageOffset + 4 + 2*coverage_index);
        table->italics_correction_value (glyph) = get_U16 (tt, mathItalicsCorrectionInfoOffset + 4 + 4*coverage_index);
        table->italics_correction_devoff (glyph) = get_U16 (tt, mathItalicsCorrectionInfoOffset + 6 + 4*coverage_index);
      }
    } else if (glyphCoverageFormat == 2) {
      unsigned int rangeCount = get_U16 (tt, italicsCorrectionCoverageOffset + 2);
      unsigned int coverage_index= 0;
      for (unsigned int i=0; i<rangeCount; i++) {
        unsigned int startGlyphID = get_U16 (tt, italicsCorrectionCoverageOffset + 4 + 6*i);
        unsigned int endGlyphID = get_U16 (tt, italicsCorrectionCoverageOffset + 6 + 6*i);
        //unsigned int startCoverageIndex = get_U16 (tt, var_offset + coverage_offset + 8 + 6*i);
        for(unsigned int glyph = startGlyphID; glyph<=endGlyphID; glyph++, coverage_index++) {
          table->italics_correction_value (glyph) = get_U16 (tt, mathItalicsCorrectionInfoOffset + 4 + 4*coverage_index);
          table->italics_correction_devoff (glyph) = get_U16 (tt, mathItalicsCorrectionInfoOffset + 6 + 4*coverage_index);
        }
      }
    } else {
      cout << "parse_mathtable (italics table) : glyphCoverageFormat " << glyphCoverageFormat << " not supported." << LF;
    }
  }
  
  // top accent attachment offsets
  {
    int topAccentCoverageOffset = mathTopAccentAttachmentOffset + get_U16 (tt, mathTopAccentAttachmentOffset + 0);
    //int topAccentAttachmentCount = get_U16 (tt, mathTopAccentAttachmentOffset + 2);
    int glyphCoverageFormat = get_U16 (tt, topAccentCoverageOffset + 0);
    if (glyphCoverageFormat == 1) {
      unsigned int glyphCount = get_U16 (tt, topAccentCoverageOffset + 2);
      for (unsigned int coverage_index=0; coverage_index< glyphCount; coverage_index++) {
        unsigned int glyph = get_U16 (tt, topAccentCoverageOffset + 4 + 2*coverage_index);
        table->top_accent_value (glyph) = get_U16 (tt, mathTopAccentAttachmentOffset + 4 + 4*coverage_index);
        table->top_accent_devoff (glyph) = get_U16 (tt, mathTopAccentAttachmentOffset + 6 + 4*coverage_index);
      }
    } else if (glyphCoverageFormat == 2) {
      unsigned int rangeCount = get_U16 (tt, topAccentCoverageOffset + 2);
      unsigned int coverage_index= 0;
      for (unsigned int i=0; i<rangeCount; i++) {
        unsigned int startGlyphID = get_U16 (tt, topAccentCoverageOffset + 4 + 6*i);
        unsigned int endGlyphID = get_U16 (tt, topAccentCoverageOffset + 6 + 6*i);
        //unsigned int startCoverageIndex = get_U16 (tt, var_offset + coverage_offset + 8 + 6*i);
        for(unsigned int glyph = startGlyphID; glyph<=endGlyphID; glyph++, coverage_index++) {
          table->top_accent_value (glyph) = get_U16 (tt, mathTopAccentAttachmentOffset + 4 + 4*coverage_index);
          table->top_accent_devoff (glyph) = get_U16 (tt, mathTopAccentAttachmentOffset + 6 + 4*coverage_index);
        }
      }
    } else {
      cout << "parse_mathtable (top attchm table) : glyphCoverageFormat " << glyphCoverageFormat << " not supported." << LF;
    }
  }
  
  // math variants
  table->minConnectorOverlap = get_U16 (tt, mathVariantsOffset + 0);
  int vertGlyphCoverageOffset = get_U16 (tt, mathVariantsOffset + 2);
  int horizGlyphCoverageOffset = get_U16 (tt, mathVariantsOffset + 4);
  int vertGlyphCount = get_U16 (tt, mathVariantsOffset + 6);
  //int horizGlyphCount = get_U16 (tt, mathVariantsOffset + 8);
  
  // (tt-dump "/Users/mgubi/t/svn-src/TeXmacs/fonts/truetype/texgyre/texgyrepagella-math.otf")
  
  // parse vertical variants
  parse_variants (tt, mathVariantsOffset,
                  vertGlyphCoverageOffset, 10,
                  table->ver_glyph_variants, table->ver_glyph_variants_adv,
                  table->ver_glyph_assembly);

  // parse horizontal variants
  parse_variants (tt, mathVariantsOffset,
                  horizGlyphCoverageOffset, 10 + 2*vertGlyphCount,
                  table->hor_glyph_variants, table->hor_glyph_variants_adv,
                  table->hor_glyph_assembly);

  return table;
}

array<string>
as_hexadecimal (array<unsigned int> a) {
  array<string> as = array<string> ();
  for (int i=0; i<N(a); i++) {
    as << as_hexadecimal (a [i]);
  }
  return as;
}

void
dump_mathtable (ot_mathtable table) {
  if (is_nil(table)) return;
  
  cout << "MATH table " << table->majorVersion << " "
       << table->minorVersion << LF;
  {
    cout << "Vertical variants" << LF;
    iterator<unsigned int> it= iterate (table->ver_glyph_variants);
    while (it->busy())
    {
      int glyph= it->next();
      cout << "glyph: " << as_hexadecimal (glyph) << " variants ("
           << N(table->ver_glyph_variants[glyph])
           << ") : " << as_hexadecimal (table->ver_glyph_variants[glyph]) << LF;
    }
    cout << "Vertical assembly" << LF;
    it= iterate (table->ver_glyph_assembly);
    while (it->busy())
    {
      int glyph= it->next();
      cout << "glyph: " << as_hexadecimal (glyph) << " assembly ("
           << N(table->ver_glyph_assembly[glyph])
           << ") : " << as_hexadecimal (table->ver_glyph_assembly[glyph]) << LF;
    }
  }
  {
    cout << "Horizontal variants" << LF;
    iterator<unsigned int> it= iterate (table->hor_glyph_variants);
    while (it->busy())
    {
      int glyph= it->next();
      cout << "glyph: " << as_hexadecimal (glyph) << " variants ("
           << N(table->hor_glyph_variants[glyph])
           << ") : " << as_hexadecimal (table->hor_glyph_variants[glyph]) << LF;
    }
    cout << "Horizontal assembly" << LF;
    it= iterate (table->hor_glyph_assembly);
    while (it->busy())
    {
      int glyph= it->next();
      cout << "glyph: " << as_hexadecimal (glyph) << " assembly ("
           << N(table->hor_glyph_assembly[glyph])
           << ") : " << as_hexadecimal (table->hor_glyph_assembly[glyph]) << LF;
    }

  }
}

/******************************************************************************
* Dump all font info to the screen
******************************************************************************/

void
tt_dump (string tt) {
  for (int i=0; i<tt_nr_fonts (tt); i++) {
    cout << HRULE << "Font " << i << LF << HRULE;

    cout << "Font tables:";
    for (int k=0; k<tt_nr_tables (tt, i); k++)
      cout << " " << tt_table_tag (tt, i, k);
    cout << LF;

    string nt= tt_table (tt, i, "name");
    cout << "Font family: " << name_record_family (nt) << "\n";
    cout << "Font shape: " << name_record_shape (nt) << "\n";
    cout << "Name table" << LF;
    for (int k=0; k<name_nr_records (nt); k++)
      cout << "  " << name_record_name_id (nt, k)
           << " [" << name_record_language_id (nt, k)
           << ", " << name_record_platform_id (nt, k) << "]"
           << " -> " << name_record_string (nt, k) << LF;
    
  }
  dump_mathtable (parse_mathtable (tt));
}

void
tt_dump (url u) {
  string tt;
  if (!load_string (u, tt, false))
    tt_dump (tt);
  else cout << "file not found";
}

/******************************************************************************
* Get the font family and available shapes
******************************************************************************/

void
move_to_shape (string& fam, string& shape, string what, string by) {
  int pos= search_forwards (" " * what, 0, fam);
  if (pos < 0) return;
  fam  = fam (0, pos) * fam (pos + N(what) + 1, N(fam));
  if (N(by) == 0) return;
  if (shape == "Regular") shape= by;
  else shape= by * " " * shape;
}

scheme_tree
tt_font_name (url u) {
  string tt;
  tree r (TUPLE);
  if (load_string (u, tt, false)) return r;
  for (int i=0; i < tt_nr_fonts (tt); i++) {
    if (!tt_correct_version (tt, i)) return tree (TUPLE);
    string nt = tt_table (tt, i, "name");
    string fam= name_record_family (nt);
    string sh = name_record_shape (nt);

    // Some basic normalization of family name
    move_to_shape (fam, sh, "Narrow", "Narrow");
    move_to_shape (fam, sh, "Condensed", "Condensed");
    move_to_shape (fam, sh, "Extended", "Extended");
    move_to_shape (fam, sh, "Wide", "Wide");
    move_to_shape (fam, sh, "Caption", "Caption");
    move_to_shape (fam, sh, "Semilight", "SemiLight");
    move_to_shape (fam, sh, "SemiLight", "SemiLight");
    move_to_shape (fam, sh, "Semi Light", "SemiLight");
    move_to_shape (fam, sh, "Ultralight", "Thin");
    move_to_shape (fam, sh, "UltraLight", "Thin");
    move_to_shape (fam, sh, "Ultra Light", "Thin");
    move_to_shape (fam, sh, "Light", "Light");
    move_to_shape (fam, sh, "Medium", "");
    move_to_shape (fam, sh, "Semibold", "SemiBold");
    move_to_shape (fam, sh, "SemiBold", "SemiBold");
    move_to_shape (fam, sh, "Semi Bold", "SemiBold");
    move_to_shape (fam, sh, "Demibold", "DemiBold");
    move_to_shape (fam, sh, "DemiBold", "DemiBold");
    move_to_shape (fam, sh, "Demi Bold", "DemiBold");
    move_to_shape (fam, sh, "Bold", "Bold");
    move_to_shape (fam, sh, "Extrabold", "ExtraBold");
    move_to_shape (fam, sh, "ExtraBold", "ExtraBold");
    move_to_shape (fam, sh, "Extra Bold", "ExtraBold");
    move_to_shape (fam, sh, "Heavy", "Heavy");
    move_to_shape (fam, sh, "Black", "Black");
    move_to_shape (fam, sh, "Italic", "Italic");
    move_to_shape (fam, sh, "Oblique", "Oblique");

    while (fam != "" && !is_alpha (fam[0])) fam= fam (1, N(fam));
    if (upcase_all (fam) == fam) fam= locase_all (fam);
    fam= upcase_first (fam);
    if (starts (fam, "STIX")) fam= "Stix" * fam (4, N(fam));
    // End normalization of family name
    
    r << tuple (fam, sh);
  }
  return r;
}

url
tt_unpack (string s) {
  if (!is_int (suffix (url (s)))) return url_none ();
  url dir= url ("$TEXMACS_HOME_PATH/fonts/unpacked");
  if (!exists (dir)) mkdir (dir);
  url name= dir * url (s * ".ttf");
  if (exists (name)) return name;
  //cout << "Extracting " << name << "\n";
  int i= as_int (suffix (url (s)));
  s= strip_suffix (s);
  url u= tt_font_find (s);
  if (is_none (u)) return url_none ();
  string ttc;
  if (load_string (u, ttc, false)) return url_none ();
  string tt= tt_extract_subfont (ttc, i);
  if (save_string (name, tt, false)) return url_none ();
  return name;
}
