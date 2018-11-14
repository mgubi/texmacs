/* -*-objc-*-
   GSAutoLayoutManager.m

   Copyright (C) 2002-2008 Free Software Foundation, Inc.

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
#include "AutoLayoutCommonInclude.h"
#include "GSAutoLayoutManager.h"
#include "GSAutoLayoutManagerPrivate.h"

#define min(X, Y)  ((X) < (Y) ? (X) : (Y))
#define max(X, Y)  ((X) < (Y) ? (Y) : (X))

NSString *GSAutoLayoutManagerChangedLayoutNotification = @"GSAutoLayoutManagerChangedLayoutNotification";


/* This class is just a place to store segment information.  */
@implementation GSAutoLayoutManagerSegment
- (id) init
{
  _minimumLayout.position = 0;
  _minimumLayout.length = 0;

  _layout.position = 0;
  _layout.length = 0;

  _contentsLayout.position = 0;
  _contentsLayout.length = 0;

  /* Set the span because this is essential to compute the line parts.
   * It must always be an integer > 0.
   */
  _span = 1;

  return self;
}

@end

/* This class is just a place to store line part information.  */
@implementation GSAutoLayoutManagerLinePartInformation
- (id) init
{
  return self;
}
@end

@implementation GSAutoLayoutManagerLinePart
- (id) initWithInfo: (GSAutoLayoutManagerLinePartInformation *)info
{
  _minimumLayout.position = 0;
  _minimumLayout.length = 0;

  _layout.position = 0;
  _layout.length = 0;

  ASSIGN (_info, info);

  return self;
}

- (void) dealloc
{
  RELEASE (_info);
  [super dealloc];
}
@end

/* This class contains a little more logic - allocates the segment
 * array, frees it.  */
@implementation GSAutoLayoutManagerLine

- (id) init
{
  _segments = [NSMutableArray new];
  _forcedLength = -1;
  return self;
}

- (void) dealloc
{
  RELEASE (_segments);
  [super dealloc];
}

@end

/* The main class.  */
@implementation GSAutoLayoutManager

- (id) init
{
  _lines = [NSMutableSet new];
  _linePartInformation = [NSMutableDictionary new];
  _lineParts = [NSMutableArray new];
  return self;
}

- (void) dealloc
{
  RELEASE (_lineParts);
  RELEASE (_linePartInformation);
  RELEASE (_lines);
  [super dealloc];
}

- (void) updateLayout
{
  if (_needsUpdateMinimumLayout)
    {
      if ([self internalUpdateMinimumLayout])
	{
	  _needsUpdateLayout = YES;
	}

      _needsUpdateMinimumLayout = NO;
    }
  
  if (_needsUpdateLayout)
    {
      /* First, compute the forced _length.  */
      NSEnumerator *e = [_lines objectEnumerator];
      GSAutoLayoutManagerLine *line;
      _length = -1;

      while ((line = [e nextObject]) != nil) 
	{
	  float forcedLength = line->_forcedLength;
	  if (forcedLength < 0)
	    {
	      /* no forced length for this line - ignore */
	    }
	  else
	    {
	      if (_length < 0)
		{
		  /* First forcedLength we find - use it as it is.  */
		  _length = forcedLength;
		}
	      else
		{
		  /* A new forcedLength - use it only if less than what
		   * we already have.  */
		  _length = min (forcedLength, _length);
		}
	    }
	}

      /* If there is no forced length, use _minimumLength.  */
      if (_length < 0)
	{
	  _length = _minimumLength;
	}

      /* Please note that it is possible that _length <
       * _minimumLength; in which case, in internalUpdateLayout, we
       * use the minimum layout.  */

      if ([self internalUpdateLayout])
	{
	  /* Post the notification that the layout changed.  Clients
	   * should observe this notification, and update themselves
	   * as a consequence of layout changes when they get this
	   * notification.  */
	  [[NSNotificationCenter defaultCenter]
	    postNotificationName: GSAutoLayoutManagerChangedLayoutNotification
	    object: self
	    userInfo: nil];
	}
      
      _needsUpdateLayout = NO;
    }
}

- (void) internalUpdateLineParts
{
  /* Determine the number of line parts.  */
  NSEnumerator *e = [_lines objectEnumerator];
  GSAutoLayoutManagerLine *line;
  int i, numberOfLineParts = 0;

  [_lineParts removeAllObjects];
  
  while ((line = [e nextObject]) != nil) 
    {
      int linePartCount = 0;
      int count = [line->_segments count];

      for (i = 0; i < count; i++)
	{
	  GSAutoLayoutManagerSegment *segment;
	  
	  segment = [line->_segments objectAtIndex: i];
	  segment->_linePart = linePartCount;
	  linePartCount += segment->_span;
	}
      numberOfLineParts = max(linePartCount, numberOfLineParts);
    }

  for (i = 0; i < numberOfLineParts; i++)
    {
      GSAutoLayoutManagerLinePart *linePart;
      GSAutoLayoutManagerLinePartInformation *linePartInfo;
      
      /* Store any special information that was set/hardcoded for that
       * specific line part.  */
      linePartInfo = [_linePartInformation objectForKey: [NSNumber numberWithInt: i]];
      linePart = [[GSAutoLayoutManagerLinePart alloc] initWithInfo: linePartInfo];
      [_lineParts addObject: linePart];
      RELEASE (linePart);
    }
}

- (void) internalUpdateSegmentsMinimumLayoutFromLineParts
{
  /* Iterate over all segments, and set their minimumLayout.  */
  NSEnumerator *e = [_lines objectEnumerator];
  GSAutoLayoutManagerLine *line;
  
  e = [_lines objectEnumerator];
  
  while ((line = [e nextObject]) != nil) 
    {
      int i, count = [line->_segments count];

      for (i = 0; i < count; i++)
	{
	  GSAutoLayoutManagerSegment *segment;
	  int j;

	  segment = [line->_segments objectAtIndex: i];
	  (segment->_minimumLayout).length = 0;
	  
	  for (j = 0; j < segment->_span; j++)
	    {
	      GSAutoLayoutManagerLinePart *linePart;
	      
	      linePart = [_lineParts objectAtIndex: segment->_linePart + j];

	      if (j == 0)
		{
		  (segment->_minimumLayout).position = (linePart->_minimumLayout).position;
		}
	      
	      (segment->_minimumLayout).length += (linePart->_minimumLayout).length;
	    }
	  
	  /* We do not need to layout segment contents inside the
	   * segment in the minimum layout.  The minimum layout is
	   * never used to actually draw anything on screen, so we can
	   * skip this operation.  When the actual layout is computed,
	   * then we will layout the segment contents inside the final
	   * layout; this will be stored in the _contentsLayout part
	   * of the segment.
	   */
	}
    }
}

- (void) internalUpdateSegmentsLayoutFromLineParts
{
  /* Iterate over all segments, and set their layout.  */
  NSEnumerator *e = [_lines objectEnumerator];
  GSAutoLayoutManagerLine *line;
  
  e = [_lines objectEnumerator];
  
  while ((line = [e nextObject]) != nil) 
    {
      int i, count = [line->_segments count];

      for (i = 0; i < count; i++)
	{
	  GSAutoLayoutManagerSegment *segment;
	  int j;

	  segment = [line->_segments objectAtIndex: i];
	  (segment->_layout).length = 0;
	  
	  for (j = 0; j < segment->_span; j++)
	    {
	      GSAutoLayoutManagerLinePart *linePart;
	      
	      linePart = [_lineParts objectAtIndex: segment->_linePart + j];

	      if (j == 0)
		{
		  (segment->_layout).position = (linePart->_layout).position;
		}
	      
	      (segment->_layout).length += (linePart->_layout).length;
	    }

	  /* Now place the segment contents inside the segment.  */

	  {
	    /* First, start with the segment, then remove the fixed
	     * paddings.  */
	    GSAutoLayoutSegmentLayout s = segment->_layout;
	    
	    /* Now, align the segment contents in the resulting space.  */
	    switch (segment->_alignment)
	      {
	      case GSAutoLayoutExpand:
	      case GSAutoLayoutWeakExpand:
		{
		  s.position += segment->_bottomPadding;
		  s.length -= segment->_bottomPadding + segment->_topPadding;
		  break;
		}
	      case GSAutoLayoutAlignBottom:
		{
		  s.position += segment->_bottomPadding;
		  s.length = segment->_minimumContentsLength;
		  break;
		}
	      case GSAutoLayoutAlignTop:
		{
		  s.position += s.length - segment->_topPadding - segment->_minimumContentsLength;
		  s.length = segment->_minimumContentsLength;
		  break;
		}
	      case GSAutoLayoutAlignCenter:
	      default:
		{
		  s.position += segment->_bottomPadding;
		  s.position += (s.length - segment->_bottomPadding - segment->_minimumContentsLength - segment->_topPadding) / 2;
		  s.length = segment->_minimumContentsLength;
		  break;
		}
	      }

	    /* Save the results of our computations.  */
	    segment->_contentsLayout = s;
	  }
	}
    }
}

- (BOOL) internalUpdateMinimumLayout
{
  /* Subclass responsibility.  */
  return NO;
}

- (BOOL) internalUpdateLayout
{
  /* Subclass responsibility.  */
  return NO;
}

- (id) addLine
{
  GSAutoLayoutManagerLine *line;

  line = [GSAutoLayoutManagerLine new];
  [_lines addObject: line];
  RELEASE (line);

  _needsUpdateMinimumLayout = YES;
  _needsUpdateLayout = YES;

  /* We are funny here ;-) ... we return the line itself as `an
   * identifier to identify that line which clients can use to
   * identify that line with the autolayout manager'.  This saves up
   * any lookup to find the line object.  Of course clients should
   * *NEVER* touch the line object we gave them - we might even change
   * our implementation and pass them something else - a real
   * identifier perhaps.  */
  return line;
}

- (void) removeLine: (id)line
{
  [_lines removeObject: line];
  _needsUpdateMinimumLayout = YES;
  _needsUpdateLayout = YES;
}

- (void) forceLength: (float)length
	      ofLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  if (l->_forcedLength != length)
    {
      _needsUpdateLayout = YES;
      l->_forcedLength = length;
    }
}

- (void) insertNewSegmentAtIndex: (int)segment
			  inLine: (id)line
{
  GSAutoLayoutManagerSegment *s;
  GSAutoLayoutManagerLine *l = line; 

  s = [GSAutoLayoutManagerSegment new];
  [l->_segments insertObject: s  atIndex: segment];
  RELEASE (s);

  _needsUpdateMinimumLayout = YES;
  _needsUpdateLayout = YES;
}

- (void) removeSegmentAtIndex: (int)segment
		       inLine: (id)line
{
  GSAutoLayoutManagerLine *l = line; 

  [l->_segments removeObjectAtIndex: segment];
  _needsUpdateMinimumLayout = YES;
  _needsUpdateLayout = YES;
}

- (unsigned int) segmentCountInLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  
  return [l->_segments count];
}

- (unsigned int) linePartCount
{
  return [_lineParts count];
}

- (unsigned int) linePartCountInLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  unsigned int linePartCount = 0;
  int i, count = [l->_segments count];

  for (i = 0; i < count; i++)
    {
      GSAutoLayoutManagerSegment *segment = [l->_segments objectAtIndex: i];
      linePartCount += segment->_span;
    }

  return linePartCount;
}

- (void) setMinimumLength: (float)min
		alignment: (GSAutoLayoutAlignment)flag
	    bottomPadding: (float)bottomPadding
	       topPadding: (float)topPadding
		     span: (int)span
	 ofSegmentAtIndex: (int)segment
		   inLine: (id)line;
{
  GSAutoLayoutManagerLine *l = line;
  GSAutoLayoutManagerSegment *s = [l->_segments objectAtIndex: segment];

  if (s->_minimumContentsLength != min)
    {
      s->_minimumContentsLength = min;
      _needsUpdateMinimumLayout = YES;
    }
  
  if (s->_alignment != flag)
    {
      s->_alignment = flag;
      _needsUpdateMinimumLayout = YES;
    }
  
  if (s->_bottomPadding != bottomPadding)
    {
      s->_bottomPadding = bottomPadding;
      _needsUpdateMinimumLayout = YES;
    }

  if (s->_topPadding != topPadding)
    {
      s->_topPadding = topPadding;
      _needsUpdateMinimumLayout = YES;
    }

  if (s->_span != span)
    {
      if (span > 0)
	{
	  s->_span = span;
	  _needsUpdateMinimumLayout = YES;
	}
      else
	{
	  NSLog (@"GSAutoLayoutManager: Warning, segment has non-positive span %d.  Ignored", 
		 span);
	}
    }
}

- (float) minimumLengthOfSegmentAtIndex: (int)segment
				 inLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  GSAutoLayoutManagerSegment *s = [l->_segments objectAtIndex: segment];

  return s->_minimumContentsLength;
}


- (GSAutoLayoutAlignment) alignmentOfSegmentAtIndex: (int)segment
					     inLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  GSAutoLayoutManagerSegment *s = [l->_segments objectAtIndex: segment];

  return s->_alignment;  
}

- (float) bottomPaddingOfSegmentAtIndex: (int)segment
				 inLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  GSAutoLayoutManagerSegment *s = [l->_segments objectAtIndex: segment];

  return s->_bottomPadding;  
}

- (float) topPaddingOfSegmentAtIndex: (int)segment
			      inLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  GSAutoLayoutManagerSegment *s = [l->_segments objectAtIndex: segment];

  return s->_topPadding;  
}

- (int) spanOfSegmentAtIndex: (int)segment
		      inLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  GSAutoLayoutManagerSegment *s = [l->_segments objectAtIndex: segment];

  return s->_span;
}

- (void) setMinimumLength: (float)min
	    alwaysExpands: (BOOL)alwaysExpands
	     neverExpands: (BOOL)neverExpands
	       proportion: (float)proportion
	ofLinePartAtIndex: (int)linePart
{
  GSAutoLayoutManagerLinePartInformation *info = [GSAutoLayoutManagerLinePartInformation new];

  info->_minimumLength = min;
  info->_alwaysExpands = alwaysExpands;
  info->_neverExpands = neverExpands;
  info->_proportion = proportion;

  [_linePartInformation setObject: info
			forKey: [NSNumber numberWithInt: linePart]];
  RELEASE (info);

  _needsUpdateMinimumLayout = YES;
}

- (float) proportionOfLinePartAtIndex: (int)linePart
{
  GSAutoLayoutManagerLinePartInformation *info;

  info = [_linePartInformation objectForKey: [NSNumber numberWithInt: linePart]];

  if (info == nil)
    {
      return 1.0;
    }
  else
    {
      return info->_proportion;
    }
}

- (float) minimumLengthOfLinePartAtIndex: (int)linePart
{
  GSAutoLayoutManagerLinePartInformation *info;

  info = [_linePartInformation objectForKey: [NSNumber numberWithInt: linePart]];

  if (info == nil)
    {
      return 0.0;
    }
  else
    {
      return info->_minimumLength;
    }
}

- (BOOL) alwaysExpandsOfLinePartAtIndex: (int)linePart
{
  GSAutoLayoutManagerLinePartInformation *info;

  info = [_linePartInformation objectForKey: [NSNumber numberWithInt: linePart]];

  if (info == nil)
    {
      return NO;
    }
  else
    {
      return info->_alwaysExpands;
    }
}

- (BOOL) neverExpandsOfLinePartAtIndex: (int)linePart
{
  GSAutoLayoutManagerLinePartInformation *info;

  info = [_linePartInformation objectForKey: [NSNumber numberWithInt: linePart]];

  if (info == nil)
    {
      return NO;
    }
  else
    {
      return info->_neverExpands;
    }
}

- (void) removeInformationOnLinePartAtIndex: (int)linePart
{
  [_linePartInformation removeObjectForKey: [NSNumber numberWithInt: linePart]];
}

- (float) lineLength
{
  return _length;
}


- (GSAutoLayoutSegmentLayout) layoutOfSegmentAtIndex: (int)segment
					      inLine: (id)line
{
  GSAutoLayoutManagerLine *l = line;
  GSAutoLayoutManagerSegment *s = [l->_segments objectAtIndex: segment];
  
  return s->_contentsLayout;
}

- (GSAutoLayoutSegmentLayout) layoutOfLinePartAtIndex: (int)linePart
{
  GSAutoLayoutManagerLinePart *l = [_lineParts objectAtIndex: linePart];

  return l->_layout;
}

- (float) minimumLineLength
{
  return _minimumLength;
}

@end
