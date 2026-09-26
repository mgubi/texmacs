/* -*-objc-*-
   GSAutoLayoutManagerPrivate.h

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

#ifndef _GNUstep_H_GSAutoLayoutManagerPrivate
#define _GNUstep_H_GSAutoLayoutManagerPrivate


/* This is a separate header file than GSAutoLayoutManager because
 * these classes are to be used by GSAutoLayoutManager and its
 * subclasses, but not by end-user applications.  */
#include "GSAutoLayoutManager.h"

/* The ivars in these classes are public, because subclasses of
 * GSAutoLayoutManager need to access them.  The classes are trivial -
 * they contain nearly no code and we access all instance variables
 * directly from the GSAutoLayoutManager for performance reasons.
 * They are basically structs which can be stored in ObjC arrays
 * :-) */
@interface GSAutoLayoutManagerSegment : NSObject
{
  /* All the ivars are public, we set/read them directly.  */
@public

  /* The minimum length of the segment contents.  */
  float _minimumContentsLength;

  /* The bottom padding of the segment.  */
  float _bottomPadding;
  
  /* The top padding of the segment.  */
  float _topPadding;

  /* 0 if the segment should be expanded when possible, because that
   * would show additional information; > 0 if there is no additional
   * information to show, and so the segment looks only uglier when
   * expanded, and it's better not to expand it; in case > 0, then the
   * value can be 1, 2, 3, 4 meaning if in case the segment really
   * must be expanded, to expand it, or how to align the segment
   * contents inside the segment, if left, center, or right.  Because
   * 0 is determined by a functional reason, while > 0 by an
   * aesthetical reason, 0 should be strictly honoured, at the expense
   * of not always honouring > 0.
   */
  GSAutoLayoutAlignment _alignment;

  /* This number holds the line part in the line where the segment
   * starts.  Typically used to interact with other segments and
   * lines.  Please note that this information is regenerated each
   * time the minimum layout is done.  */
  int _linePart;

  /* This number holds the number of line parts in the line that the
   * segment takes up.  Typically used to interact with other segments
   * and lines.  */
  int _span;

  /* The layout of the segment once minimum layout is done.  */
  GSAutoLayoutSegmentLayout _minimumLayout;

  /* The layout of the segment once layout is done.  */
  GSAutoLayoutSegmentLayout _layout;

  /* The layout of the segment contents once layout is done.  This is
   * the final computation result which we serve to clients.  */
  GSAutoLayoutSegmentLayout _contentsLayout;
}
@end

/* Objects of the following class are used to store special
 * information that is set programmatically about some line parts.  */
@interface GSAutoLayoutManagerLinePartInformation : NSObject
{
  /* All the ivars are public, we set/read them directly.  */
@public

  /* The minimum length of the line part (no distinction between
   * paddings/content since a line part has no padding/content).  This
   * can be used to set a minimum size for rows or columns in grids.
   */
  float _minimumLength;
  
  /* This is the number of grid units that the line part takes up for
   * proportional layout managers; eg, a line part with _proportion=2
   * will have double the size of one with _proportion=1.  Ignored by
   * standard managers.  */
  float _proportion;

  /* This can be set programmatically to have the line part always
   * expand even if the views inside it are not set to.  */
  BOOL _alwaysExpands;

  /* This can be set programmatically to have the line part never
   * expand even if the views inside it are set to.  */
  BOOL _neverExpands;
}
@end

/* Objects of this class represent a line part that is used during
 * layout.  This information is collected/computed during
 * autolayout.  */
@interface GSAutoLayoutManagerLinePart : NSObject
{
  /* All the ivars are public, we set/read them directly.  */
@public

  /* Any special/hardcoded info that we might have on the line part
   * (stored here to avoid continuous lookups in the
   * _linePartInformation dictionary during autolayout); normally set
   * to nil.  */
  GSAutoLayoutManagerLinePartInformation *_info;

  /* If the line part expands (determined from the _alwaysExpands and
   * _neverExpands flags, and the alignment of the segments that are
   * displayed inside the line part).  */
  BOOL _expands;

  /* The proportion, used only by the proportional autolayout manager.  */
  float _proportion;

  /* The layout of the line part once minimum layout is done.  */
  GSAutoLayoutSegmentLayout _minimumLayout;

  /* The layout of the line part once layout is done.  */
  GSAutoLayoutSegmentLayout _layout;
}
@end

@class NSMutableArray;

@interface GSAutoLayoutManagerLine : NSObject
{
@public
  /* The forced length of the line, or < 0 if none.  */
  float _forcedLength;

  /* An array of GSAutoLayoutManagerSegment (or a subclass) objects.
   * Created/destroyed when the object is created/destroyed, but for
   * the rest managed directly by the GSAutoLayoutManager.  */
  NSMutableArray *_segments;
}
@end

#endif 
