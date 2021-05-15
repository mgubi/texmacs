
/******************************************************************************
* MODULE     : rubber_unicode_font.cpp
* DESCRIPTION: Rubber unicode fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "converter.hpp"
#include "convert.hpp" // string_to_scheme_tree
#include "translator.hpp"
#include "./Freetype/tt_face.hpp"

#ifdef USE_FREETYPE

bool supports_big_operators (string res_name); // from poor_rubber.cpp

/******************************************************************************
* True Type fonts
******************************************************************************/

struct rubber_unicode_font_rep: font_rep {
  font base;
  bool big_flag;
  array<bool> initialized;
  array<font> subfn;
  bool big_sums;

  tt_face mathface; // additional data for math typesetting
  translator virt; // virtual glyph translator
  
  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;

  rubber_unicode_font_rep (string name, font base, tt_face face= tt_face ());
  font   get_font (int nr);
  int    search_font_sub (string s, string& rew);
  bool   search_font_sub_bis (string s, string& rew, int& nr);
  int    search_font_cached (string s, string& rew);
  font   search_font (string& s);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);

  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
  SI     get_wide_correction  (string s, int mode);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

rubber_unicode_font_rep::rubber_unicode_font_rep (string name, font base2, tt_face face2)
  : font_rep (name, base2), base (base2),
    big_flag (supports_big_operators (base2->res_name)),
    mathface (face2)
{
  this->copy_math_pars (base);
  big_sums= false;
  if (base->supports ("<sum>")) {
    metric ex;
    base->get_extents ("<sum>", ex);
    //cout << base->res_name << " -> "
    //<< ((double) (ex->y2-ex->y1)) / base->yx << LF;
    if ((((double) (ex->y2-ex->y1)) / base->yx) >= 1.55) big_sums= true;
  }
  for (int i=0; i<6; i++) {
    initialized << false;
    subfn << base;
  }
  if (!is_nil (mathface) && !is_nil(mathface->mathtable)) {
    // We have math informations
    big_flag= true;
    big_sums= true;
  }
  // we create an empty virtual translator which we populate lazily with
  // informations from the OpenType MATH table (if present)
  string vname= "opentype_virtual[" * res_name * "]";
  virt= tm_new<translator_rep> (vname);
  virt->virt_def= array<tree> ();
  virt->virt_def << tree(); // fill out the 0 glyph
}

font
rubber_unicode_font_rep::get_font (int nr) {
  ASSERT (nr < N(subfn), "wrong font number");
  if (initialized[nr]) return subfn[nr];
  initialized[nr]= true;
  switch (nr) {
  case 0:
    break;
  case 1:
    subfn[nr]= base->magnify (sqrt (0.5));
    break;
  case 2:
    subfn[nr]= base->magnify (sqrt (2.0));
    break;
  case 3:
    subfn[nr]= base->magnify (2.0);
    break;
  case 4:
    subfn[nr]= rubber_assemble_font (base);
  case 5:
    {
      // the virtual font uses the info from the translator
      int hdpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
      int vdpi= (72 * base->hpt + (PIXEL/2)) / PIXEL;
      subfn[nr]= virtual_font (base, virt->res_name, base->size, hdpi, vdpi, false);
    }
    break;
  }
  return subfn[nr];
}

/******************************************************************************
* Find the font
******************************************************************************/

int parse_variant (string s, string& r, string& rg) {
  int var=0;
  int start= search_forwards ("-", 0, s);
  int end= search_backwards ("-", N(s), s);
  if (end == start) { end=N(s)-1; var=0; }
  else {
    var= max (0, as_int (s (end+1, N(s))));
  }
  r= s (start+1, end);
  rg= s(0, start);
  return var;
}

string
normalized_cork_to_utf8 (string s) {
  // FIXME: these rewritings should be really implemented via an hashtable
  if (N(s) < 3) return s;
  string r= s;
  if (r == "<hat>") r= "<#302>";
  else if (r == "<tilde>") r= "<#303>";
  else if (r == "<check>") r= "<#30C>";
  else if (r == "<bar>") r= "<#305>";
  else if (r == "<vect>") r= "<#20D7>";
  else if (r == "<breve>") r= "<#306>";
  else if (r == "<invbreve>") r= "<#311>";
  else if (r == "<punderbrace>") r= "<#23DD>";
  else if (r == "<punderbrace*>") r= "<#23DD>";
  else if (r == "<underbrace>") r= "<#23DF>";
  else if (r == "<underbrace*>") r= "<#23DF>";
  else if (r == "<squnderbrace>") r= "<#23B5>";
  else if (r == "<squnderbrace*>") r= "<#23B5>";
  else if (r == "<poverbrace>") r= "<#23DC>";
  else if (r == "<poverbrace*>") r= "<#23DC>";
  else if (r == "<overbrace>") r= "<#23DE>";
  else if (r == "<overbrace*>") r= "<#23DE>";
  else if (r == "<sqoverbrace>") r= "<#23B4>";
  else if (r == "<sqoverbrace*>") r= "<#23B4>";
  return strict_cork_to_utf8 (r);
}

bool
rubber_unicode_font_rep::search_font_sub_bis (string s, string& rew, int& nr) {
// look into mathtable and fill the virtual font lazily
  string r; // root character
  string rg; // head
  int var= 0; // variant sequential number
  bool hor= false; // horizontal or vertical?
  rew= s;
  nr= 0;
  if (starts (s, "<big-")) {
    var= parse_variant (s, r, rg);
    if (var > 0) var= var-1; //FIXME: is this ok?
    hor= false;
  } else if (starts (s, "<large-") || starts (s, "<left-") ||
             starts (s, "<mid-")   || starts (s, "<right-")) {
    var= parse_variant (s, r, rg);
    hor= false;
  } else if (starts (s, "<wide-")) {
    var= parse_variant (s, r, rg);
    hor= true;
  } else {
    return false;
  }
  if (r == "") return false;
  string uu= (N(r)>1 ? normalized_cork_to_utf8 ("<" * r * ">") : r);
  int j= 0;
  unsigned int u= decode_from_utf8 (uu, j);
  unsigned int glyphid= ft_get_char_index (mathface->ft_face, u);
  if (glyphid == 0) return 0;
  hashmap<unsigned int, array<unsigned int> > v=
     (hor ? mathface->mathtable->hor_glyph_variants
          : mathface->mathtable->ver_glyph_variants);
  hashmap<unsigned int, array<unsigned int> > ass=
     (hor ? mathface->mathtable->hor_glyph_assembly
          : mathface->mathtable->ver_glyph_assembly);
  if (v->contains (glyphid)) {
    if (var < N(v (glyphid))) {
      unsigned int res=  v (glyphid)[var];
      rew= "<@" * as_hexadecimal (res) * ">";
      nr= 0; // base font
      cout << "returning opentype variant " << s << " -> " << rew << LF;
      return true;
    } else if (ass->contains (glyphid)) {
      // we have an assembly
      // normalize name
      if (r == "(") r= "lparen";
      else if (r == ")") r= "rparen";
      else if (r == "[") r= "lbracket";
      else if (r == "]") r= "rbracket";
      else if (r == "{") r= "lcurly";
      else if (r == "}") r= "rcurly";
      // make a generic symbol for the assembly (used as a key)
      string symbol= rg * "-" * r * "-" * as_string (var) * ">";
      if (!virt->dict->contains (symbol)) {
        // let us create a new virtual glyph from the assembly
        // TODO: use the placement info
        array<unsigned int> a= ass [glyphid];
        tree glyph;
        //unsigned int italics_correction_val= a[0];
        //unsigned int italics_correction_devoff= a[1];
        unsigned int part_count= a[2];
        string l;
        if (hor) {
          var -= N(mathface->mathtable->hor_glyph_variants [glyphid])-1;
          // horizontal assembly
          if (part_count == 3) {
            unsigned int ga= a[3], gb= a[3+5], gc= a[3+5*2];
            l= "(glue* #" * as_hexadecimal (0xc000000 + ga) * " \n\
                  (glue* (hor-take #" * as_hexadecimal (0xc000000 + gb) * " 0.5 " * as_string (var) * " 0.5)\n\
                     #" * as_hexadecimal (0xc000000 + gc) * " ))";
          } else if (part_count == 5) {
            unsigned int ga= a[3], gb= a[3+5], gc= a[3+5*2], gd= a[3+5*3], ge= a[3+5*4];
            l= "(glue* #" * as_hexadecimal (0xc000000 + ga) * " \n\
                  (glue* (hor-take #" * as_hexadecimal (0xc000000 + gb) * " 0.5 " * as_string (var) * " 0.25)\n\
                    (glue* #" * as_hexadecimal (0xc000000 + gc) * "\n\
                      (glue* (hor-take #" * as_hexadecimal (0xc000000 + gd) * " 0.5 " * as_string (var) * " 0.25)\n\
                         #" * as_hexadecimal (0xc000000 + ge) * " ))))";
          } else {
            cout << "rubber_unicode_font: part_count not supported :" << part_count << LF;
            return false;
          }
        } else {
          var -= N(mathface->mathtable->ver_glyph_variants [glyphid])-1;
          // vertical assembly
          if (part_count == 2) {
            unsigned int ga= a[3], gb= a[3+5];
            l= "(glue-above #" * as_hexadecimal (0xc000000 + ga) * " \n\
                  (glue-above (ver-take #" * as_hexadecimal (0xc000000 + gb) * " 0.5 " * as_string (var) * " 0.5)\n\
                     #" * as_hexadecimal (0xc000000 + gb) * " ))";
          } else if (part_count == 3) {
            unsigned int ga= a[3], gb= a[3+5], gc= a[3+5*2];
            l= "(glue-above #" * as_hexadecimal (0xc000000 + ga) * " \n\
                  (glue-above (ver-take #" * as_hexadecimal (0xc000000 + gb) * " 0.5 " * as_string (var) * " 0.5)\n\
                     #" * as_hexadecimal (0xc000000 + gc) * " ))";
          } else if (part_count == 5) {
            unsigned int ga= a[3], gb= a[3+5], gc= a[3+5*2], gd= a[3+5*3], ge= a[3+5*4];
            l= "(glue-above #" * as_hexadecimal (0xc000000 + ga) * " \n\
                  (glue-above (ver-take #" * as_hexadecimal (0xc000000 + gb) * " 0.5 " * as_string (var) * " 0.25)\n\
                    (glue-above #" * as_hexadecimal (0xc000000 + gc) * "\n\
                      (glue-above (ver-take #" * as_hexadecimal (0xc000000 + gd) * " 0.5 " * as_string (var) * " 0.25)\n\
                         #" * as_hexadecimal (0xc000000 + ge) * " ))))";
          } else {
            cout << "rubber_unicode_font: part_count not supported :" << part_count << LF;
            return false;
          }
        }
        cout << "virtual glyph " << N(virt->virt_def) << " for [" << symbol << "] = " << l << LF;
        glyph = string_to_scheme_tree (l);
        virt->dict (symbol)= N(virt->virt_def);
        virt->virt_def << glyph;
        if (initialized [5]) {
          // reset the virtual font to refresh it
          font::instances -> reset (subfn [5] -> res_name);
          initialized [5]= false;
        }
      }
      rew= symbol;
      nr= 5; // virtual font
      cout << "returning opentype rubber " << s << " -> " << rew << LF;
      return true;
    }
  }
  return false;
}


int
rubber_unicode_font_rep::search_font_sub (string s, string& rew) {
  if (starts (s, "<big-") && ends (s, "-1>")) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 0;
      return 2;
    }
  }
  if (starts (s, "<big-") && ends (s, "-2>")) {
    if (big_flag && base->supports (s)) {
      rew= s;
      return 0;
    }
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    if (big_flag && base->supports ("<big-" * r * "-1>")) {
      rew= "<big-" * r * "-1>";
      return 2;
    }
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 2;
      return 3;
    }
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      if (s[pos-1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") { rew= ""; return 0; }
      if ((r == "(" && base->supports ("<#239C>")) ||
          (r == ")" && base->supports ("<#239F>")) ||
          (r == "[" && base->supports ("<#23A2>")) ||
          (r == "]" && base->supports ("<#23A5>")) ||
          ((r == "{" || r == "}") && base->supports ("<#23AA>")) ||
          (r == "sqrt" && base->supports ("<#23B7>"))) {
        rew= s;
        return 4;
      }
      rew= r;
      if (N(rew) > 1) rew= "<" * rew * ">";
      if (ends (s, "-0>")) return 0;
      return 0;
    }
  }
  rew= s;
  return 0;
}

int
rubber_unicode_font_rep::search_font_cached (string s, string& rew) {
  if (mapper->contains (s)) {
    rew= rewriter[s];
    return mapper[s];
  }
  int nr= 0;
  if (!is_nil (mathface) && !is_nil (mathface->mathtable)
      && !search_font_sub_bis (s, rew, nr)) // we look in the MATH table
    nr= search_font_sub (s, rew);
  mapper(s)= nr;
  rewriter(s)= rew;
  cout << "search_font_cached " << s << " -> " << nr << ", " << rew << LF;
  return nr;
}

font
rubber_unicode_font_rep::search_font (string& s) {
  string rew;
  int nr= search_font_cached (s, rew);
  s= rew;
  return get_font (nr);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

// TODO: update this function
bool
rubber_unicode_font_rep::supports (string s) {
  if (starts (s, "<wide-")) return true; // FIXME: hack for developing, remove.
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    if (N(r) > 1) r= "<" * r * ">";
    return base->supports (r);
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      if (s[pos-1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") return true;
      if (r == "sqrt") return base->supports ("<#23B7>");
      if (N(r) > 1) r= "<" * r * ">";
      if (!base->supports (r)) return false;
      if (ends (s, "-0>")) return true;
      if (r == "(") return base->supports ("<#239C>");
      if (r == ")") return base->supports ("<#239F>");
      if (r == "[") return base->supports ("<#23A2>");
      if (r == "]") return base->supports ("<#23A5>");
      if (r == "{" || r == "}") return base->supports ("<#23AA>");
      return true;
    }
  }
  return base->supports (s);
}

void
rubber_unicode_font_rep::get_extents (string s, metric& ex) {
  font fn= search_font (s);
  fn->get_extents (s, ex);
}

void
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos) {
  if (s == "") return;
  string r= s;
  font fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos);
  else if (N(r) != 1) font_rep::get_xpositions (s, xpos);
  else {
    int i, n=N(s);
    for (i=1; i<n; i++) xpos[i]= 0;
    fn->get_xpositions (r, xpos+n-1);
  }
}

void
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  if (s == "") return;
  string r= s;
  font fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos, xk);
  else if (N(r) != 1) font_rep::get_xpositions (s, xpos, xk);
  else {
    int i, n=N(s);
    for (i=0; i<n; i++) xpos[i]= 0;
    fn->get_xpositions (r, xpos+n-1, xk);
  }
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y);
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y, xk);
}

//FIXME: move declaration elsewhere
font rubber_unicode_font (font base, tt_face face);

font
rubber_unicode_font_rep::magnify (double zoomx, double zoomy) {
  return rubber_unicode_font (base->magnify (zoomx, zoomy), mathface);
}

glyph
rubber_unicode_font_rep::get_glyph (string s) {
  font fn= search_font (s);
  return fn->get_glyph (s);
}

int
rubber_unicode_font_rep::index_glyph (string s, font_metric& fnm,
                                                font_glyphs& fng) {
  font fn= search_font (s);
  return fn->index_glyph (s, fnm, fng);
}

/******************************************************************************
* Metric properties
******************************************************************************/

double
rubber_unicode_font_rep::get_left_slope  (string s) {
  font fn= search_font (s);
  return fn->get_left_slope (s);
}

double
rubber_unicode_font_rep::get_right_slope (string s) {
  font fn= search_font (s);
  return fn->get_right_slope (s);
}

SI
rubber_unicode_font_rep::get_left_correction  (string s) {
  font fn= search_font (s);
  return fn->get_left_correction (s);
}

SI
rubber_unicode_font_rep::get_right_correction (string s) {
  font fn= search_font (s);
  return fn->get_right_correction (s);
}

SI
rubber_unicode_font_rep::get_lsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsub_correction (s);
}

SI
rubber_unicode_font_rep::get_lsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsup_correction (s);
}

SI
rubber_unicode_font_rep::get_rsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsub_correction (s);
}

SI
rubber_unicode_font_rep::get_rsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsup_correction (s);
}

SI
rubber_unicode_font_rep::get_wide_correction  (string s, int mode) {
  font fn= search_font (s);
  return fn->get_wide_correction (s, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base));
}

font
rubber_unicode_font (font base, tt_face face) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base, face));
}

#else

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

font
rubber_unicode_font (font base, tt_face face) {
  return rubber_unicode_font (base);
}
#endif
