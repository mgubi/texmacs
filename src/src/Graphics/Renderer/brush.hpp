
/******************************************************************************
* MODULE     : brush.hpp
* DESCRIPTION: brushes for painting
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BRUSH_H
#define BRUSH_H
#include "tree.hpp"
class url;

enum brush_kind {
  brush_none,
  brush_color,
  brush_pattern
};

class brush_rep: abstract_struct {
public:
  inline brush_rep () {}
  inline virtual ~brush_rep () {}

  virtual brush_kind get_type () = 0;
  virtual void* get_handle () = 0;

  virtual color get_color () = 0;
  virtual tree get_pattern () = 0;
  virtual url get_pattern_url ();
  virtual int get_alpha () = 0;

  friend class brush;
};

class brush {
ABSTRACT_NULL(brush);
  brush (bool b);
  brush (color c);
  brush (tree p, int a= 255);
  friend bool operator == (const brush& a, const brush& b);
};
ABSTRACT_NULL_CODE(brush);

bool operator == (const brush& a, const brush& b);
inline bool operator != (const brush& a, const brush& b) { return !(a == b); }

brush mix (brush b1, double a1, brush b2, double a2);

// The base color of a pattern (its optional 5th argument): the opaque color
// painted under the image, so that the transparent parts of the image show
// it and not what was there. False if there is none, or if the argument is
// not a color (none, empty...). The 4th argument is an effect, or, in old
// documents, a color which only stands for the image when patterns are off.
bool  get_pattern_base (tree pattern, color& c);

// The paper: the opaque color under the transparent parts of a pattern which
// makes the background of the page, when no base color was chosen for it.
// It is the color of the document background @bg (bg-color) if that is a
// plain color, the base color of @bg if it is a pattern which has one, and
// else the default of the environment variable bg-color.
color paper_color (brush bg);
string get_paper_color (tree bg);    // the same, for a tree, as #rrggbb

// On a screen, which keeps the previous paint and shows the grey squares of
// the device below transparent parts, clears the rectangle with the paper
// before the pattern @pat (the background of @ren) is painted, if it has no
// base color of its own (which clear_pattern paints).
class renderer_rep;
typedef renderer_rep* renderer;
void  clear_pattern_paper (renderer ren, brush pat, brush under,
                           SI x1, SI y1, SI x2, SI y2);

url   resolve_pattern (url im);
void  get_pattern_data (url& u, SI& w, SI& h, tree& eff, brush br, SI pixel);

#endif // defined BRUSH_H
