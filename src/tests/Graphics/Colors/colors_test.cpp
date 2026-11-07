
/******************************************************************************
* MODULE     : colors_test.cpp
* DESCRIPTION: tests of the reversed colors of the dark mode
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "colors.hpp"
#include "renderer.hpp"

static string
show (int r, int g, int b) {
  return "(" * as_string (r) * ", " * as_string (g) * ", " *
         as_string (b) * ")";
}

// a grey goes to the opposite grey
static void
test_reverse_grey () {
  for (int v=0; v<256; v++) {
    int r= v, g= v, b= v;
    reverse (r, g, b);
    CHECK_MSG (r == 255 - v && g == 255 - v && b == 255 - v,
               "grey " * as_string (v) * " goes to " * show (r, g, b));
  }
}

// a color one step away from a grey stays close to the opposite grey,
// and the components stay between 0 and 255
static void
test_reverse_near_grey () {
  int r= 254, g= 254, b= 253;
  reverse (r, g, b);
  CHECK_MSG (r <= 3 && g <= 3 && b <= 3,
             "(254, 254, 253) goes to " * show (r, g, b));
  for (int v=1; v<255; v++)
    for (int k=0; k<6; k++) {
      int c[3]= { v, v, v };
      c[k % 3] += (k < 3? 1: -1);
      r= c[0]; g= c[1]; b= c[2];
      reverse (r, g, b);
      int far= max (abs (r - (255 - v)), max (abs (g - (255 - v)),
                                              abs (b - (255 - v))));
      CHECK_MSG (far <= 3 && min (r, min (g, b)) >= 0 &&
                 max (r, max (g, b)) <= 255,
                 show (c[0], c[1], c[2]) * " goes to " * show (r, g, b));
    }
}

// reversing twice gives the color back, up to the rounding
static void
test_reverse_twice () {
  for (int r0=0; r0<256; r0+=15)
    for (int g0=0; g0<256; g0+=15)
      for (int b0=0; b0<256; b0+=15) {
        int r= r0, g= g0, b= b0;
        reverse (r, g, b);
        CHECK_MSG (min (r, min (g, b)) >= 0 && max (r, max (g, b)) <= 255,
                   show (r0, g0, b0) * " goes to " * show (r, g, b));
        reverse (r, g, b);
        int far= max (abs (r - r0), max (abs (g - g0), abs (b - b0)));
        CHECK_MSG (far <= 2, show (r0, g0, b0) * " comes back as " *
                             show (r, g, b));
      }
}

int
main () {
  RUN (test_reverse_grey);
  RUN (test_reverse_near_grey);
  RUN (test_reverse_twice);
  return test_report ();
}
