
/******************************************************************************
* MODULE     : point.cpp
* DESCRIPTION: points
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "point.hpp"

point
operator + (point p1, point p2) {
  int i, n= min (N(p1), N(p2));
  point r (n);
  for (i=0; i<n; i++)
    r[i]= p1[i] + p2[i];
  return r;
}

point
operator - (point p1, point p2) {
  int i, n= min (N(p1), N(p2));
  point r (n);
  for (i=0; i<n; i++)
    r[i]= p1[i] - p2[i];
  return r;
}

point
operator * (double x, point p) {
  int i, n= N(p);
  point r (n);
  for (i=0; i<n; i++)
    r[i]= x * p[i];
  return r;
}

point
operator / (point p, double x) {
  int i, n= N(p);
  point r (n);
  for (i=0; i<n; i++)
    r[i]= p[i] / x;
  return r;
}
