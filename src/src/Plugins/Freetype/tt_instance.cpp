/******************************************************************************
* MODULE     : tt_instance.cpp
* DESCRIPTION: Static fonts for the named instances of variable fonts
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// A named instance of a variable font becomes an ordinary TrueType font,
// which the rest of TeXmacs (metrics, rendering, the OpenType tables, the
// PDF writer) handles like any other. FreeType applies the variations: it
// opens the instance and gives, glyph by glyph, the outline and the advance
// in font units. The glyf, loca and hmtx tables are written anew from
// them, head, hhea, maxp and OS/2 are adjusted, the tables of variations
// and of hinting are dropped (the outlines are no longer those the hints
// were written for), and the other tables are copied as they are. The
// values of GPOS and MATH remain those of the default instance.

#include "tt_tools.hpp"
#include "analyze.hpp"

#ifdef USE_FREETYPE
#include "free_type.hpp"

string tt_table (string tt, int i, string tag);
int tt_nr_tables (string tt, int i);
string tt_table_tag (string tt, int i, int k);
string tt_table (string tt, int i, int k);

static int
U16_at (string s, int i) {
  if (i + 2 > N(s)) return 0;
  return (((int) (unsigned char) s[i]) << 8) + ((int) (unsigned char) s[i+1]);
}

static void
put16 (string& s, int v) {
  s << ((char) ((v >> 8) & 255)) << ((char) (v & 255));
}

static void
put32 (string& s, unsigned int v) {
  s << ((char) ((v >> 24) & 255)) << ((char) ((v >> 16) & 255))
    << ((char) ((v >> 8) & 255)) << ((char) (v & 255));
}

static void
set16 (string& s, int i, int v) {
  if (i + 2 > N(s)) return;
  s[i]  = (char) ((v >> 8) & 255);
  s[i+1]= (char) (v & 255);
}

static void
set32 (string& s, int i, unsigned int v) {
  if (i + 4 > N(s)) return;
  s[i]  = (char) ((v >> 24) & 255);
  s[i+1]= (char) ((v >> 16) & 255);
  s[i+2]= (char) ((v >> 8) & 255);
  s[i+3]= (char) (v & 255);
}

static unsigned int
checksum (string s) {
  unsigned int sum= 0;
  int n= N(s);
  for (int i=0; i<n; i+=4) {
    unsigned int v= 0;
    for (int j=0; j<4; j++)
      v= (v << 8) + (i + j < n? (unsigned int) (unsigned char) s[i+j]: 0);
    sum += v;
  }
  return sum;
}

static bool
dropped_table (string tag) {
  return
    tag == "fvar" || tag == "gvar" || tag == "avar" || tag == "cvar" ||
    tag == "HVAR" || tag == "VVAR" || tag == "MVAR" || tag == "STAT" ||
    tag == "cvt " || tag == "fpgm" || tag == "prep" || tag == "hdmx" ||
    tag == "LTSH" || tag == "VDMX" || tag == "DSIG";
}

string
tt_make_instance (string tt, int k) {
  if (!tt_is_variable (tt, 0)) return "";
  if (ft_initialize ()) return "";
  FT_Face face;
  if (ft_new_memory_face (ft_library, (const FT_Byte*) &(tt[0]), N(tt),
                          ((FT_Long) k) << 16, &face))
    return "";
  int n= (int) face->num_glyphs;

  string glyf, hmtx;
  array<int> loca;
  int xmin_all= 32767, ymin_all= 32767, xmax_all= -32768, ymax_all= -32768;
  int max_points= 0, max_contours= 0, max_adv= 0;
  int min_lsb= 32767, min_rsb= 32767, max_extent= -32768;
  bool ok= true;
  for (int g=0; g<n && ok; g++) {
    loca << N(glyf);
    if (ft_load_glyph (face, g, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING |
                                FT_LOAD_NO_BITMAP)) {
      put16 (hmtx, 0); put16 (hmtx, 0);
      continue;
    }
    FT_GlyphSlot slot= face->glyph;
    int adv= (int) slot->metrics.horiAdvance;
    FT_Outline& o= slot->outline;
    if (slot->format != FT_GLYPH_FORMAT_OUTLINE || o.n_contours <= 0) {
      put16 (hmtx, adv); put16 (hmtx, 0);
      max_adv= max (max_adv, adv);
      continue;
    }
    int xmin= 32767, ymin= 32767, xmax= -32768, ymax= -32768;
    for (int p=0; p<o.n_points; p++) {
      if (FT_CURVE_TAG (o.tags[p]) == FT_CURVE_TAG_CUBIC) { ok= false; break; }
      int x= (int) o.points[p].x, y= (int) o.points[p].y;
      xmin= min (xmin, x); xmax= max (xmax, x);
      ymin= min (ymin, y); ymax= max (ymax, y);
    }
    if (!ok) break;
    string gl;
    put16 (gl, o.n_contours);
    put16 (gl, xmin); put16 (gl, ymin); put16 (gl, xmax); put16 (gl, ymax);
    for (int c=0; c<o.n_contours; c++) put16 (gl, o.contours[c]);
    put16 (gl, 0);                             // no instructions
    for (int p=0; p<o.n_points; p++)           // on curve or not, and
      gl << ((char) (FT_CURVE_TAG (o.tags[p]) == FT_CURVE_TAG_ON? 1: 0));
    int last= 0;                               // coordinates as 16-bit deltas
    for (int p=0; p<o.n_points; p++) {
      int x= (int) o.points[p].x; put16 (gl, x - last); last= x; }
    last= 0;
    for (int p=0; p<o.n_points; p++) {
      int y= (int) o.points[p].y; put16 (gl, y - last); last= y; }
    while ((N(gl) & 3) != 0) gl << '\0';
    glyf << gl;
    put16 (hmtx, adv); put16 (hmtx, xmin);
    xmin_all= min (xmin_all, xmin); xmax_all= max (xmax_all, xmax);
    ymin_all= min (ymin_all, ymin); ymax_all= max (ymax_all, ymax);
    max_points= max (max_points, (int) o.n_points);
    max_contours= max (max_contours, (int) o.n_contours);
    max_adv= max (max_adv, adv);
    min_lsb= min (min_lsb, xmin);
    min_rsb= min (min_rsb, adv - xmax);
    max_extent= max (max_extent, xmax);
  }
  loca << N(glyf);
  ft_done_face (face);
  if (!ok) return "";                          // cubic outlines (CFF2)
  if (xmin_all > xmax_all) xmin_all= ymin_all= xmax_all= ymax_all= 0;
  if (min_lsb == 32767) min_lsb= min_rsb= max_extent= 0;

  string loca_tab;
  for (int i=0; i<N(loca); i++) put32 (loca_tab, (unsigned int) loca[i]);

  // the tables of the static font, sorted by tag
  hashmap<string,string> tabs ("");
  array<string> tags;
  for (int t=0; t<tt_nr_tables (tt, 0); t++) {
    string tag= tt_table_tag (tt, 0, t);
    if (dropped_table (tag)) continue;
    tabs (tag)= tt_table (tt, 0, t);
    tags << tag;
  }
  tabs ("glyf")= glyf;
  tabs ("loca")= loca_tab;
  tabs ("hmtx")= hmtx;

  string head= tabs ["head"];
  set32 (head, 8, 0);                          // checkSumAdjustment, below
  set16 (head, 36, xmin_all); set16 (head, 38, ymin_all);
  set16 (head, 40, xmax_all); set16 (head, 42, ymax_all);
  set16 (head, 50, 1);                         // long loca offsets
  tabs ("head")= head;

  string hhea= tabs ["hhea"];
  set16 (hhea, 10, max_adv);
  set16 (hhea, 12, min_lsb);
  set16 (hhea, 14, min_rsb);
  set16 (hhea, 16, max_extent);
  set16 (hhea, 34, n);                         // an advance for every glyph
  tabs ("hhea")= hhea;

  string maxp= tabs ["maxp"];
  if (N(maxp) >= 32) {
    set16 (maxp, 6, max_points);
    set16 (maxp, 8, max_contours);
    set16 (maxp, 10, 0);                       // no more composite glyphs
    set16 (maxp, 12, 0);
    set16 (maxp, 26, 0);                       // no instructions
    set16 (maxp, 28, 0);
    set16 (maxp, 30, 0);
  }
  tabs ("maxp")= maxp;

  string os2= tabs ["OS/2"];
  if (N(os2) >= 6) {
    string fv= tt_table (tt, 0, "fvar");
    int w= (int) (tt_instance_coordinate (fv, k, "wght", U16_at (os2, 4))
                  + 0.5);
    set16 (os2, 4, max (1, min (1000, w)));
  }
  tabs ("OS/2")= os2;

  for (int i=0; i<N(tags); i++)                // insertion sort of the tags
    for (int j=i; j>0 && tags[j] < tags[j-1]; j--) {
      string t= tags[j]; tags[j]= tags[j-1]; tags[j-1]= t; }

  int nr= N(tags), pow= 1, lg= 0;
  while (2 * pow <= nr) { pow *= 2; lg++; }
  string r;
  put32 (r, 0x00010000);
  put16 (r, nr); put16 (r, 16 * pow); put16 (r, lg); put16 (r, 16 * nr - 16 * pow);
  int offset= 12 + 16 * nr;
  for (int i=0; i<nr; i++) {
    string data= tabs [tags[i]];
    r << tags[i];
    put32 (r, checksum (data));
    put32 (r, offset);
    put32 (r, N(data));
    offset += ((N(data) + 3) >> 2) << 2;
  }
  int head_pos= -1;
  for (int i=0; i<nr; i++) {
    if (tags[i] == "head") head_pos= N(r);
    string data= tabs [tags[i]];
    r << data;
    while ((N(r) & 3) != 0) r << '\0';
  }
  if (head_pos >= 0) set32 (r, head_pos + 8, 0xB1B0AFBA - checksum (r));
  return r;
}

#else

string
tt_make_instance (string tt, int k) {
  (void) tt; (void) k;
  return "";
}

#endif // USE_FREETYPE
