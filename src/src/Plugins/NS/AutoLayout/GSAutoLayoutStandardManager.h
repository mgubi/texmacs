/* -*-objc-*-
   GSAutoLayoutStandardManager.h

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

#ifndef _GNUstep_H_GSAutoLayoutStandardManager
#define _GNUstep_H_GSAutoLayoutStandardManager

#include "GSAutoLayoutManager.h"

@class NSMutableArray;

/*
 * GSAutoLayoutStandardManager objects are layout managers which
 * layout segments one after the other.  Some segments are expanded,
 * other segments are not expanded.  The standard autolayout manager
 * guarantees that whenever there is a resize, all segments with
 * expand = YES are expanded, and that no segments with expand = NO
 * are expanded unless there is a sound reason to do so.  This
 * autolayout manager aligns segments in different lines, so that
 * segment 0 of one line is the same size of segment 0 of all other
 * lines.  By default, all 'span' attributes of all segments should be
 * 1.  If a 'span' attribute of a segment is different from 1, it is
 * interpreted as the number of line parts (columns if we are laying
 * out horizontally) that the segment takes up.
 */
@interface GSAutoLayoutStandardManager : GSAutoLayoutManager
{
  /* Cached number of line parts with expand set to YES.  Computed
   * during minimum layout.  */
  int _numberOfExpandingLineParts;
}

/*
 * The minimum layout is determined by 
 *
 * - finding how many line parts we have.
 *
 * - compute the length of each line part, which is the maximum length
 * of all segments contained in each line part (ignoring the special
 * case of segments with span != 1 for now), and whether each line
 * part expands or not (it expands if any of the segments displayed in
 * it expands).
 *
 * - for each segment which has a span != 1, if the sum of the length
 * of line parts it spans is less than the minimum size of the
 * segment, then enlarge all those line parts so that their sum get to
 * this minimum length (only enlarge line parts with expand = YES and
 * all of the same amount; if there is none with expand = YES, expand
 * all of an equal amount); if the segment has expand = YES and no
 * line part it spans is already marked as expand = YES, then mark
 * them all as expand = YES.
 *
 * - layout each line by laying out all segments in a line part to be
 * as big as that line part, and segments spanning multiple line parts
 * to be as big as the sum of those line parts.  All segments are one
 * after the other one.
 *
 * - the biggest line length is the line length.
 *
 * - layout the segment contents inside each segment according to the
 * padding for that segment, and the alignment of the segment contents.
 */
- (BOOL) internalUpdateMinimumLayout;

/*
 * The layout is determined by computing the difference between the
 * _length and the _minimumLength, and distributing the resulting
 * amount between the line parts with expand = YES, then performing
 * layout.
 */
- (BOOL) internalUpdateLayout;

@end

#endif 
