/* -*-objc-*-
   GSAutoLayoutProportionalManager.h

   Copyright (C) 2002 - 2008 Free Software Foundation, Inc.

   Author: Nicola Pero <nicola.pero@meta-innovation.com>
   Date: April 2002 - March 2008

   This file is part of GNUstep Renaissance

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; see the file COPYING.LIB.
   If not, write to the Free Software Foundation,
   59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _GNUstep_H_GSAutoLayoutProportionalManager
#define _GNUstep_H_GSAutoLayoutProportionalManager

#include "GSAutoLayoutManager.h"

/*
 * GSAutoLayoutProportionalManager objects are layout managers which
 * layout line parts and segments in proportion to a 'layout unit'.
 * By default, every line part is 1 unit long.  By changing the
 * 'proportion' of the line part, you can make it longer, or shorter;
 * eg, you can have a line part which is 2 unit long; then it will be
 * long exactly the double of all other line parts.
 * 
 * The layout unit must be big enough that all segments display their
 * `minimum length' when placed in the line parts.  All line parts
 * (and, consequently, segments) are always expanded as required no
 * matter what, since they need to keep their proportions with other
 * line parts.
 *
 * When content is placed inside a segment, its alignment is used
 * as usual to place the content inside the segment.
 *
 * All lines must contain the same number of layout units.  If the
 * segments in a line sum up to use less layout units than the
 * segments in another line, those layout units are left blank (the
 * idea being that you can then add an element at the end of one line,
 * but not the other ones ... the other ones will still be bigger, but
 * display blank there).
 */
@interface GSAutoLayoutProportionalManager : GSAutoLayoutManager
{
  float _minimumLayoutUnit;
  
  float _layoutUnit;
}

/*
 * The minimum layout is determined by determining the minimum layout
 * unit necessary to display all segments and line parts.  For each
 * segment, the minimum layout unit necessary to display that segment
 * is obtained by dividing the minimum length of the segment for the
 * number of layout units that it covers (obtained by summing the
 * proportion of all line parts that it spans).  For each line part,
 * the minimum layout unit necessary to display that line part is
 * obtained by dividing the minimum length of the line part for the
 * number of layout units that it covers (which is the proportion of
 * the segment).  The maximum of all these numbers is the minimum
 * layout unit needed to display everything comfortably.  Once the
 * minimum layout unit has been determined, the line parts are
 * easily laid out one after the other one by so that they take up
 * a size equal to their proportion times the layout unit.  Finally,
 * the segment minimum layout is done from the line part layout
 * as usual.
 */
- (BOOL) internalUpdateMinimumLayout;

/*
 * The layout is determined by computing the number of layout units in
 * a line (got by dividing the _minimumLength by the
 * _minimumLayoutUnit).  The _length is divided by this number to get
 * the _layoutUnit.  Layout is computed basing on this _layoutUnit -
 * all line parts have their layout computed by laying them out one
 * after the other one, and sizing them to cover `proportion' grid
 * units each.  Finally, the layout is propagated to segments as
 * usual.
 */
- (BOOL) internalUpdateLayout;

@end

#endif 
