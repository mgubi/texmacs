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
#include "hashmap.hpp"
#include <math.h>

#ifdef USE_FREETYPE
#include "free_type.hpp"

string tt_table (string tt, int i, string tag);
int tt_nr_tables (string tt, int i);
string tt_table_tag (string tt, int i, int k);
string tt_table (string tt, int i, int k);

static int
U16_at (string s, int i) {
  if (i < 0 || i + 2 > N(s)) return 0;
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
  if (i < 0 || i + 2 > N(s)) return;
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

/******************************************************************************
* The positioning values of an instance
******************************************************************************/

// The values of GPOS (kerning pairs, the anchors of marks) are those of the
// default instance; a value which varies has a Device table in the format
// VariationIndex, which points to deltas in the ItemVariationStore of GDEF,
// one for each region of the design space. The values are corrected in
// place: the tables keep their layout, and every field is corrected once,
// however many records share it.

static int
S16_at (string s, int i) {
  int v= U16_at (s, i);
  return v >= 32768? v - 65536: v;
}

static int
U32_at (string s, int i) {
  return (U16_at (s, i) << 16) + U16_at (s, i+2);
}

static double
F2DOT14_at (string s, int i) {
  return ((double) S16_at (s, i)) / 16384.0;
}

static double
region_scalar (string gd, int regions, int r, array<double> nc) {
  int axis_count  = U16_at (gd, regions);
  int region_count= U16_at (gd, regions + 2);
  if (r >= region_count) return 0.0;
  int pos= regions + 4 + r * axis_count * 6;
  double scalar= 1.0;
  for (int a=0; a<axis_count; a++, pos += 6) {
    double start= F2DOT14_at (gd, pos);
    double peak = F2DOT14_at (gd, pos + 2);
    double end  = F2DOT14_at (gd, pos + 4);
    double c    = (a < N(nc))? nc[a]: 0.0;
    if (start > peak || peak > end) continue;
    if (start < 0.0 && end > 0.0 && peak != 0.0) continue;
    if (peak == 0.0) continue;
    if (c < start || c > end) return 0.0;
    if (c == peak) continue;
    if (c < peak) scalar *= (c - start) / (peak - start);
    else scalar *= (end - c) / (end - peak);
  }
  return scalar;
}

static double
item_delta (string gd, int store, int outer, int inner, array<double> nc) {
  if (store <= 0 || store + 8 > N(gd) || U16_at (gd, store) != 1) return 0.0;
  int regions= store + U32_at (gd, store + 2);
  int count  = U16_at (gd, store + 6);
  if (outer >= count) return 0.0;
  int data= store + U32_at (gd, store + 8 + 4 * outer);
  int item_count= U16_at (gd, data);
  int word_count= U16_at (gd, data + 2);
  int ric       = U16_at (gd, data + 4);
  bool long_words= (word_count & 0x8000) != 0;
  word_count &= 0x7fff;
  if (inner >= item_count || word_count > ric) return 0.0;
  int row_size= long_words? 4 * word_count + 2 * (ric - word_count)
                          : 2 * word_count + (ric - word_count);
  int pos= data + 6 + 2 * ric + inner * row_size;
  if (pos + row_size > N(gd)) return 0.0;
  double sum= 0.0;
  for (int j=0; j<ric; j++) {
    int region= U16_at (gd, data + 6 + 2 * j);
    int delta;
    if (j < word_count) {
      if (long_words) { delta= U32_at (gd, pos); pos += 4; }
      else { delta= S16_at (gd, pos); pos += 2; }
    }
    else {
      if (long_words) { delta= S16_at (gd, pos); pos += 2; }
      else {
        delta= (pos < N(gd))? (int) (unsigned char) gd[pos]: 0; pos += 1;
        if (delta >= 128) delta -= 256;
      }
    }
    if (delta != 0) sum += delta * region_scalar (gd, regions, region, nc);
  }
  return sum;
}

struct gpos_variator {
  string g, gd;
  int store;
  array<double> nc;
  hashmap<int,bool> done;
  gpos_variator (string g2, string gd2, int store2, array<double> nc2):
    g (copy (g2)), gd (gd2), store (store2), nc (nc2), done (false) {}
  void device (int field, int dev);
  int  value_size (int format);
  void value_record (int rec, int format, int base);
  void anchor (int a);
  void subtable (int st, int type, int depth);
  void run ();
};

void
gpos_variator::device (int field, int dev) {
  // correct the value at field by the delta of the Device table at dev
  if (dev + 6 > N(g) || field + 2 > N(g) || done[field]) return;
  if (U16_at (g, dev + 4) != 0x8000) return;  // hinting deltas, for pixels
  int outer= U16_at (g, dev), inner= U16_at (g, dev + 2);
  double d= item_delta (gd, store, outer, inner, nc);
  int v= S16_at (g, field) + (int) floor (d + 0.5);
  set16 (g, field, v);
  done (field)= true;
}

int
gpos_variator::value_size (int format) {
  int n= 0;
  for (int b=0; b<8; b++) if ((format >> b) & 1) n++;
  return 2 * n;
}

void
gpos_variator::value_record (int rec, int format, int base) {
  // the value fields come first, then the offsets of their Device tables
  int field[4]= { -1, -1, -1, -1 };
  int pos= rec;
  for (int b=0; b<4; b++)
    if ((format >> b) & 1) { field[b]= pos; pos += 2; }
  for (int b=4; b<8; b++)
    if ((format >> b) & 1) {
      int off= U16_at (g, pos); pos += 2;
      if (off != 0 && field[b-4] >= 0) device (field[b-4], base + off);
    }
}

void
gpos_variator::anchor (int a) {
  if (a <= 0 || a + 10 > N(g) || U16_at (g, a) != 3) return;
  int xdev= U16_at (g, a + 6), ydev= U16_at (g, a + 8);
  if (xdev != 0) device (a + 2, a + xdev);
  if (ydev != 0) device (a + 4, a + ydev);
}

void
gpos_variator::subtable (int st, int type, int depth) {
  if (st <= 0 || st + 4 > N(g) || depth > 2) return;
  int fmt= U16_at (g, st);
  if (type == 9) {                             // extension
    if (fmt == 1) subtable (st + U32_at (g, st + 4), U16_at (g, st + 2),
                            depth + 1);
  }
  else if (type == 1) {                        // single adjustment
    int vf= U16_at (g, st + 4);
    if (fmt == 1) value_record (st + 6, vf, st);
    else if (fmt == 2) {
      int n= U16_at (g, st + 6), sz= value_size (vf);
      for (int i=0; i<n; i++) value_record (st + 8 + i * sz, vf, st);
    }
  }
  else if (type == 2) {                        // pair adjustment
    int vf1= U16_at (g, st + 4), vf2= U16_at (g, st + 6);
    int s1= value_size (vf1), s2= value_size (vf2);
    if (fmt == 1) {
      int n= U16_at (g, st + 8);
      for (int i=0; i<n; i++) {
        int ps= st + U16_at (g, st + 10 + 2 * i);
        int cnt= U16_at (g, ps);
        for (int j=0; j<cnt; j++) {
          // here the Device tables are relative to the PairSet
          int rec= ps + 2 + j * (2 + s1 + s2);
          value_record (rec + 2, vf1, ps);
          value_record (rec + 2 + s1, vf2, ps);
        }
      }
    }
    else if (fmt == 2) {
      int c1= U16_at (g, st + 12), c2= U16_at (g, st + 14);
      int rec= st + 16;
      for (int i=0; i < c1 * c2; i++, rec += s1 + s2) {
        value_record (rec, vf1, st);
        value_record (rec + s1, vf2, st);
      }
    }
  }
  else if (type == 3) {                        // cursive attachment
    int n= U16_at (g, st + 4);
    for (int i=0; i<n; i++) {
      int en= U16_at (g, st + 6 + 4 * i), ex= U16_at (g, st + 8 + 4 * i);
      if (en != 0) anchor (st + en);
      if (ex != 0) anchor (st + ex);
    }
  }
  else if (type == 4 || type == 5 || type == 6) {  // attachment of marks
    int classes= U16_at (g, st + 6);
    int marks= st + U16_at (g, st + 8);
    int n= U16_at (g, marks);
    for (int i=0; i<n; i++) {
      int off= U16_at (g, marks + 2 + 4 * i + 2);
      if (off != 0) anchor (marks + off);
    }
    int bases= st + U16_at (g, st + 10);
    int m= U16_at (g, bases);
    if (type == 5)
      for (int i=0; i<m; i++) {
        int lig= bases + U16_at (g, bases + 2 + 2 * i);
        int comps= U16_at (g, lig);
        for (int k=0; k < comps * classes; k++) {
          int off= U16_at (g, lig + 2 + 2 * k);
          if (off != 0) anchor (lig + off);
        }
      }
    else
      for (int k=0; k < m * classes; k++) {
        int off= U16_at (g, bases + 2 + 2 * k);
        if (off != 0) anchor (bases + off);
      }
  }
}

void
gpos_variator::run () {
  if (N(g) < 10) return;
  int ll= U16_at (g, 8);
  int n= U16_at (g, ll);
  for (int i=0; i<n; i++) {
    int lk= ll + U16_at (g, ll + 2 + 2 * i);
    int type= U16_at (g, lk), subs= U16_at (g, lk + 4);
    for (int j=0; j<subs; j++)
      subtable (lk + U16_at (g, lk + 6 + 2 * j), type, 0);
  }
}

string
tt_vary_gpos (string gpos, string gdef, array<double> nc) {
  // gpos with its values at the normalized coordinates nc
  if (N(gdef) < 18 || U16_at (gdef, 0) != 1 || U16_at (gdef, 2) < 3)
    return gpos;
  int store= U32_at (gdef, 14);
  if (store == 0) return gpos;
  gpos_variator v (gpos, gdef, store, nc);
  v.run ();
  return v.g;
}

// The static font of the named instance k (from 1) or, for k = 0, of the
// point with the given design coordinates (one for each axis, in the order
// of fvar)
static string
tt_make_static (string tt, int k, array<double> coords) {
  if (!tt_is_variable (tt, 0)) return "";
  if (ft_initialize ()) return "";
  string fv= tt_table (tt, 0, "fvar");
  if (k == 0 && ft_set_var_design_coordinates == NULL) return "";
  FT_Face face;
  if (ft_new_memory_face (ft_library, (const FT_Byte*) &(tt[0]), N(tt),
                          ((FT_Long) k) << 16, &face))
    return "";
  if (k == 0) {
    int na= N(coords);
    FT_Fixed* c= tm_new_array<FT_Fixed> (max (na, 1));
    for (int a=0; a<na; a++) c[a]= (FT_Fixed) (coords[a] * 65536.0 + 0.5);
    FT_Error err= ft_set_var_design_coordinates (face, na, c);
    tm_delete_array (c);
    if (err) { ft_done_face (face); return ""; }
  }
  array<double> nc;                           // normalized coordinates
  if (ft_get_var_blend_coordinates != NULL) {
    int na= N (tt_axis_tags (fv));
    FT_Fixed* c= tm_new_array<FT_Fixed> (max (na, 1));
    if (ft_get_var_blend_coordinates (face, na, c) == 0)
      for (int a=0; a<na; a++) nc << ((double) c[a]) / 65536.0;
    tm_delete_array (c);
  }
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
  if (N(nc) > 0 && tabs->contains ("GPOS") && tabs->contains ("GDEF"))
    tabs ("GPOS")= tt_vary_gpos (tabs ["GPOS"], tabs ["GDEF"], nc);
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
    double w0= U16_at (os2, 4);
    if (k > 0) w0= tt_instance_coordinate (fv, k, "wght", w0);
    else {
      array<string> tags= tt_axis_tags (fv);
      for (int a=0; a<N(tags) && a<N(coords); a++)
        if (tags[a] == "wght") w0= coords[a];
    }
    int w= (int) (w0 + 0.5);
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

string
tt_make_instance (string tt, int k) {
  return tt_make_static (tt, k, array<double> ());
}

string
tt_make_variation (string tt, array<double> coords) {
  return tt_make_static (tt, 0, coords);
}

#else

string
tt_make_instance (string tt, int k) {
  (void) tt; (void) k;
  return "";
}

string
tt_make_variation (string tt, array<double> coords) {
  (void) tt; (void) coords;
  return "";
}

#endif // USE_FREETYPE
