/* -*-objc-*-
   GSAutoLayoutGrid.m

   Copyright (C) 2008 Free Software Foundation, Inc.

   Author: Nicola Pero <nicola.pero@meta-innovation.com>
   Date: March 2008

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
#include <AutoLayoutCommonInclude.h>

#include "GSAutoLayoutGrid.h"
#include "GSAutoLayoutManager.h"
#include "GSAutoLayoutStandardManager.h"
#include "GSAutoLayoutProportionalManager.h"
#include "GSAutoLayoutDefaults.h"

/* This is basically a struct which can be stored into a NSArray - all
 * ivars (except setting _view, which is set when the info is created
 * and destroyed when the info is destroyed) accessed directly.  */
@interface GSAutoLayoutGridViewInfo : NSObject
{
@public
  NSView *_view;

  /* The view minimum size.  When the view is first added, its size
   * is automatically used as the view minimum size.  You can change
   * the minimum size later on programmatically by specifying a new
   * minimum size, or by asking the autolayout view to update
   * itself, in which case the autolayout view examines all views,
   * and if any view has a size which is different from the size it
   * is supposed to have, the new size is used as the view's minimum
   * size.  */
  NSSize _minimumSize;

  /* Expand/Alignment in the horizontal direction.  */
  GSAutoLayoutAlignment _hAlignment;

  /* Expands/Alignment in the vertical direction.  */
  GSAutoLayoutAlignment _vAlignment;
    
  /* The horizontal paddings.  */
  float _bottomHPadding;
  float _topHPadding;
  
  /* The vertical paddings.  */
  float _bottomVPadding;
  float _topVPadding;

  /* The position of the view in the grid.  */
  int _rowPosition;
  int _columnPosition;

  /* For views spanning multiple columns or rows.  */
  int _columnSpan;
  int _rowSpan;

  /* The segment index with the row and column manager.  This is the
   * same as the _rowPosition unless there is a view with span set
   * before us, which shifts the segments indexes.
   */
  int _rowSegmentIndex;
  int _columnSegmentIndex;
}
- (id) initWithView: (NSView *)view;
@end

@implementation GSAutoLayoutGridViewInfo
- (id) initWithView: (NSView *)view
{
  ASSIGN (_view, view);
  return self;
}

- (void) dealloc
{
  RELEASE (_view);
  [super dealloc];
}
@end

@implementation GSAutoLayoutGrid
- (id) init
{
  GSAutoLayoutManager *manager;

  self = [super initWithFrame: NSZeroRect];
  /* Turn off traditional OpenStep subview autoresizing.  */
  [self setAutoresizesSubviews: NO];
  /* By default we are resizable in width and height ... in case we
   * are placed top-level in the window: we want to receive all
   * resizing of the window around us.  */
  [self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];

  _viewInfo = [NSMutableArray new];

  /* The column layout manager is by default a standard one, but could
   * be changed.  */
  manager = [GSAutoLayoutStandardManager new];
  [self setColumnAutoLayoutManager: manager];
  RELEASE (manager);

  /* The row layout manager is by default a standard one, but could be
   * changed.  */
  manager = [GSAutoLayoutStandardManager new];
  [self setRowAutoLayoutManager: manager];
  RELEASE (manager);

  return self;
}

- (void) dealloc
{
  RELEASE (_rowManager);
  RELEASE (_columnManager);
  RELEASE (_viewInfo);
  RELEASE (_rows);
  RELEASE (_columns);
  [super dealloc];
}

- (void) setRowGridType: (GSAutoLayoutBoxType)type
{
  if (type != [self rowGridType])
    {
      GSAutoLayoutManager *manager = nil;

      if (type == GSAutoLayoutProportionalBox)
	{
	  manager = [GSAutoLayoutProportionalManager new];
	}
      else
	{
	  manager = [GSAutoLayoutStandardManager new];
	}

      [self setRowAutoLayoutManager: manager];
      RELEASE (manager);
    }
}

- (GSAutoLayoutBoxType) rowGridType
{
  if ([_rowManager isKindOfClass: [GSAutoLayoutProportionalManager class]])
    {
      return GSAutoLayoutProportionalBox;
    }
  else
    {
      return GSAutoLayoutStandardBox;
    }
}

- (void) setColumnGridType: (GSAutoLayoutBoxType)type
{
  if (type != [self columnGridType])
    {
      GSAutoLayoutManager *manager = nil;

      if (type == GSAutoLayoutProportionalBox)
	{
	  manager = [GSAutoLayoutProportionalManager new];
	}
      else
	{
	  manager = [GSAutoLayoutStandardManager new];
	}

      [self setColumnAutoLayoutManager: manager];
      RELEASE (manager);
    }
}

- (GSAutoLayoutBoxType) columnGridType
{
  if ([_columnManager isKindOfClass: [GSAutoLayoutProportionalManager class]])
    {
      return GSAutoLayoutProportionalBox;
    }
  else
    {
      return GSAutoLayoutStandardBox;
    }
}

- (void) setRowAutoLayoutManager: (GSAutoLayoutManager *)aLayoutManager
{
  /* NB: this method currently only works if you call it when there
   * are no views in the grid.  TODO: Extend it.
   */
  ASSIGN (_rowManager, aLayoutManager);

  ASSIGN (_columns, [NSMutableArray new]);
  RELEASE (_columns);
  
  [[NSNotificationCenter defaultCenter] 
    addObserver: self
    selector: @selector(rowAutoLayoutManagerChangedLayout:)
    name: GSAutoLayoutManagerChangedLayoutNotification
    object: _rowManager];
}

- (GSAutoLayoutManager *)rowAutoLayoutManager
{
  return _rowManager;
}

- (void) setColumnAutoLayoutManager: (GSAutoLayoutManager *)aLayoutManager
{
  /* NB: this method currently only works if you call it when there
   * are no views in the grid.  TODO: Extend it.
   */
  ASSIGN (_columnManager, aLayoutManager);

  ASSIGN (_rows, [NSMutableArray new]);
  RELEASE (_rows);
  
  [[NSNotificationCenter defaultCenter] 
    addObserver: self
    selector: @selector(columnAutoLayoutManagerChangedLayout:)
    name: GSAutoLayoutManagerChangedLayoutNotification
    object: _columnManager];
}

- (GSAutoLayoutManager *)columnAutoLayoutManager
{
  return _columnManager;
}

- (void) addRow
{
  id newLine = [_columnManager addLine];

  {
    int i, count = [_columns count];
    for (i = 0; i < count; i++)
      {
	[_columnManager insertNewSegmentAtIndex: 0
		     inLine: newLine];
      }
  }

  [_rows addObject: newLine];

  /* Now we have a new row.  Add an empty segment at the end of every
   * column.  */
  {
    int i, count = [_columns count];
    for (i = 0; i < count; i++)
      {
	id line = [_columns objectAtIndex: i];
	[_rowManager insertNewSegmentAtIndex: 
		       [_rowManager segmentCountInLine: line]
		     inLine: line];
      }
  }

  /* Finally, just in case this is the first row we add, we make sure
   * we force the size of the row to be our frame size.  */
  [_columnManager forceLength: ([self frame]).size.width 
		  ofLine: [_rows objectAtIndex: 0]];
}

- (void) removeRow
{
  [_columnManager removeLine: [_rows lastObject]];
  [_rows removeLastObject];
  /* TODO: Remove all segments for that row in all lines of the row manager.  */
  /* TODO: Remove all views that were in that row.  */
}

- (unsigned int) numberOfRows
{
  return [_rows count];
}

- (void) addColumn
{
  id newLine = [_rowManager addLine];

  {
    int i, count = [_rows count];
    for (i = 0; i < count; i++)
      {
	[_rowManager insertNewSegmentAtIndex: 0
		     inLine: newLine];
      }
  }

  [_columns addObject: newLine];

  /* Now we have a new column.  Add an empty segment at the end of
   * every row.  */
  {
    int i, count = [_rows count];
    for (i = 0; i < count; i++)
      {
	id line = [_rows objectAtIndex: i];
	[_columnManager insertNewSegmentAtIndex: 
			  [_columnManager segmentCountInLine: line]
			inLine: line];
      }
  }

  /* Finally, just in case this is the first column we add, we make
   * sure we force the size of the column to be our frame size.  */
  [_rowManager forceLength: ([self frame]).size.height
		  ofLine: [_columns objectAtIndex: 0]];
}

- (void) removeColumn
{
  [_rowManager removeLine: [_columns lastObject]];
  [_columns removeLastObject];
  /* TODO: Remove all segments for that column in all lines of the column manager.  */
  /* TODO: Remove all views that were in that column.  */
}

- (unsigned int) numberOfColumns
{
  return [_columns count];
}

/* Private method to retrieve the info for a view.  */
- (GSAutoLayoutGridViewInfo *) infoForView: (NSView *)aView
{
  int i, count = [_viewInfo count];
  
  for (i = 0; i < count; i++)
    {
      GSAutoLayoutGridViewInfo *info = [_viewInfo objectAtIndex: i];
      
      if (info->_view == aView)
	{
	  return info;
	}
    }
  return nil;
}

/* Private methods to push layout info to layout managers.  */
- (void) pushViewInfoToAutoLayoutManagers: (GSAutoLayoutGridViewInfo *)info
{
  [_columnManager setMinimumLength: (info->_minimumSize).width
		  alignment: info->_hAlignment
		  bottomPadding: info->_bottomHPadding
		  topPadding: info->_topHPadding
		  span: info->_columnSpan
		  ofSegmentAtIndex: info->_columnSegmentIndex
		  inLine: [_rows objectAtIndex: info->_rowPosition]];

  /* We need to swap the paddings.  The reason is that later we flip
   * the results of the layout to draw views from top to bottom
   * instead of from bottom to top (and, when doing the flipping, we
   * don't want to use the paddings as we do not want to interfere
   * with the autolayout manager's job).  That operation will
   * implicitly swap the top and bottom padding of each view.  If we
   * swap them here before passing them to the autolayout manager,
   * we'll get the final correct result.
   */
  [_rowManager setMinimumLength: (info->_minimumSize).height
	       alignment: info->_vAlignment
	       bottomPadding: info->_topVPadding
	       topPadding: info->_bottomVPadding
	       span: info->_rowSpan
	       ofSegmentAtIndex: info->_rowSegmentIndex
	       inLine: [_columns objectAtIndex: info->_columnPosition]];
}

- (void) addView: (NSView *)aView
	   inRow: (int)row
	  column: (int)column
{
  GSAutoLayoutGridViewInfo *info;

  info = [[GSAutoLayoutGridViewInfo alloc] initWithView: aView];
  info->_minimumSize = [aView frame].size;
  info->_hAlignment = [aView autoLayoutDefaultHorizontalAlignment];
  info->_vAlignment = [aView autoLayoutDefaultVerticalAlignment];
  info->_bottomHPadding = [aView autoLayoutDefaultLeftPadding];
  info->_topHPadding = [aView autoLayoutDefaultRightPadding];
  info->_bottomVPadding = [aView autoLayoutDefaultBottomPadding];
  info->_topVPadding = [aView autoLayoutDefaultTopPadding];
  info->_rowPosition = row;
  info->_columnPosition = column;

  /* Compute the row and column segment index.  By default, if there
   * were no views with a 'span' set, it would be the same as the
   * rowPosition and columnPosition.  But if any view with a span set
   * was added to our left (or top), it would have eaten some of the
   * empty/dummy segments (see the 'setRowSpan:forView:' method for
   * more information).  We need to take them into account.
   */
  
  /* Start with the correct value if there are no views with span set,
   * ie if the table was still populated with exactly one row segment
   * and one column segment per cell.
   */
  info->_rowSegmentIndex = info->_rowPosition;
  info->_columnSegmentIndex = info->_columnPosition;

  /* Now iterate over all rows looking for span views that might have
   * eaten some of the empty/dummy segments on our left/top, and 
   * reduce the segment index appropriately.
   */
  {
    NSEnumerator *e = [_viewInfo objectEnumerator];
    GSAutoLayoutGridViewInfo *viewInfo;

    while ((viewInfo = [e nextObject]) != nil)
      {
	if (viewInfo->_rowPosition == row  &&  viewInfo->_columnPosition < column  &&  viewInfo->_columnSpan > 1)
	  {
	    info->_columnSegmentIndex -= viewInfo->_columnSpan - 1;
	  }
	if (viewInfo->_rowPosition < row  &&  viewInfo->_columnPosition == column  &&  viewInfo->_rowSpan > 1)
	  {
	    info->_rowSegmentIndex -= viewInfo->_rowSpan - 1;
	  }
      }
  }

  info->_rowSpan = 1;
  info->_columnSpan = 1;
  
  if (info->_hAlignment == GSAutoLayoutExpand)
    {
      _hExpand = YES;
    }
  if (info->_hAlignment == GSAutoLayoutWeakExpand)
    {
      _hWeakExpand = YES;
   }

  if (info->_vAlignment == GSAutoLayoutExpand)
    {
      _vExpand = YES;
    }
  if (info->_vAlignment == GSAutoLayoutWeakExpand)
    {
      _vWeakExpand = YES;
    }

  [_viewInfo addObject: info];
  RELEASE (info);
  [self addSubview: aView];

  [self pushViewInfoToAutoLayoutManagers: info];
}

- (void) removeView: (NSView *)aView
{
  /* TODO */
}

- (int) numberOfViewsInRow: (int)row
{
  /* FIXME: This is incorrect since we add segments for non-existing views.  */
  return [_columnManager segmentCountInLine: [_rows objectAtIndex: row]];
}

- (int) numberOfViewsInColumn: (int)column
{
  /* FIXME: This is incorrect since we add segments for non-existing views.  */
  return [_rowManager segmentCountInLine: [_columns objectAtIndex: column]];
}

/* Internal method which will get all the autolayout information from
 * the autolayout managers, and use it to resize all views.  This is
 * an expensive method and you should be calling it as little as
 * possible.  The method gets information from both the autolayout
 * managers in a single go.  */
- (void) updateViewsWithNewAutoLayout
{
  float newWidth, newHeight;

  newWidth = [_columnManager lineLength];
  newHeight = [_rowManager lineLength];

  [super setFrameSize: NSMakeSize (newWidth, newHeight)];
  
  {
    NSEnumerator *e = [_viewInfo objectEnumerator];
    GSAutoLayoutGridViewInfo *info;

    while ((info = [e nextObject]) != nil)
      {
	NSRect newFrame;
	id column = [_columns objectAtIndex: info->_columnPosition];
	id row = [_rows objectAtIndex: info->_rowPosition];
	GSAutoLayoutSegmentLayout rowSegment, columnSegment;

	rowSegment = [_rowManager layoutOfSegmentAtIndex: info->_rowSegmentIndex
				  inLine: column];
	columnSegment = [_columnManager layoutOfSegmentAtIndex: info->_columnSegmentIndex
					inLine: row];
	
	newFrame.origin.x = columnSegment.position;
	newFrame.origin.y = rowSegment.position;
	newFrame.size.width = columnSegment.length;
	newFrame.size.height = rowSegment.length;

	/* Flip them so the first view is displayed at the top, not at
	 * the bottom.  We ignore the paddings, which means the
	 * paddings get flipped too - but that's Ok because we gave
	 * them to the autolayout manager already flipped, so this
	 * additional flipping will put them back!  (TODO: Add support
	 * to the autolayout manager for doing all the flipping itself
	 * internally so we can get rid of this complication)
	 */
	newFrame.origin.y = newHeight - newFrame.origin.y - newFrame.size.height;
	
	[info->_view setFrame: newFrame];
      }
  }
}

- (void) updateLayout
{
  _performingLayoutUpdate = YES;

  /* TODO: we should be catching exceptions here, and making sure we
   * always set _performingLayoutUpdate back to NO at the end.  */

  [_rowManager updateLayout];
  [_columnManager updateLayout];

  [self updateViewsWithNewAutoLayout];

  _performingLayoutUpdate = NO;
}

- (void) rowAutoLayoutManagerChangedLayout: (NSNotification *)notification
{
  if (_performingLayoutUpdate)
    {
      return;
    }

  [self updateViewsWithNewAutoLayout];
}

- (void) columnAutoLayoutManagerChangedLayout: (NSNotification *)notification
{
  if (_performingLayoutUpdate)
    {
      return;
    }

  [self updateViewsWithNewAutoLayout];
}

- (int) numberOfViews
{
  return [_viewInfo count];
}

- (void) setFrame: (NSRect)frame
{
  if (NSEqualRects ([self frame], frame))
    {
      return;
    }

  [super setFrame: frame];

  if ([_columns count] > 0)
    {
      [_rowManager forceLength: frame.size.height 
		   ofLine: [_columns objectAtIndex: 0]];
    }
  
  if ([_rows count] > 0)
    {
      [_columnManager forceLength: frame.size.width 
		      ofLine: [_rows objectAtIndex: 0]];
    }

  [self updateLayout];
}

- (void) setFrameSize: (NSSize)size
{
  NSSize oldSize = [self frame].size;
  
  if (oldSize.width == size.width && oldSize.height == size.height)
    {
      return;
    }

  [super setFrameSize: size];

  if ([_columns count] > 0)
    {
      [_rowManager forceLength: size.height 
		   ofLine: [_columns objectAtIndex: 0]];
    }
  
  if ([_rows count] > 0)
    {
      [_columnManager forceLength: size.width 
		      ofLine: [_rows objectAtIndex: 0]];
    }

  [self updateLayout];
}

- (void) setMinimumSize: (NSSize)aSize  forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  info->_minimumSize = aSize;

  [self pushViewInfoToAutoLayoutManagers: info];
}

- (NSSize) minimumSizeForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_minimumSize;
}


- (void) setHorizontalAlignment: (GSAutoLayoutAlignment)flag  
			forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  info->_hAlignment = flag;

  /* Recompute the _hExpand and _hWeakExpand flags.  */
  {
    NSEnumerator *e = [_viewInfo objectEnumerator];
    GSAutoLayoutGridViewInfo *viewInfo;

    _hExpand = NO;
    _hWeakExpand = NO;

    while ((viewInfo = [e nextObject]) != nil)
      {
	if (viewInfo->_hAlignment == GSAutoLayoutExpand)
	  {
	    _hExpand = YES;
	  }
	if (viewInfo->_hAlignment == GSAutoLayoutWeakExpand)
	  {
	    _hWeakExpand = YES;
	  }
      }
  }

  [self pushViewInfoToAutoLayoutManagers: info];
}

- (GSAutoLayoutAlignment) horizontalAlignmentForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_hAlignment;
}


- (void) setVerticalAlignment: (GSAutoLayoutAlignment)flag  
		      forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  info->_vAlignment = flag;

  /* Recompute the _vExpand and _vWeakExpand flags.  */
  {
    NSEnumerator *e = [_viewInfo objectEnumerator];
    GSAutoLayoutGridViewInfo *viewInfo;

    _vExpand = NO;
    _vWeakExpand = NO;

    while ((viewInfo = [e nextObject]) != nil)
      {
	if (viewInfo->_vAlignment == GSAutoLayoutExpand)
	  {
	    _vExpand = YES;
	  }
	if (viewInfo->_vAlignment == GSAutoLayoutWeakExpand)
	  {
	    _vWeakExpand = YES;
	  }
      }
  }

  [self pushViewInfoToAutoLayoutManagers: info];
}

- (GSAutoLayoutAlignment) verticalAlignmentForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_vAlignment;
}

- (void) setLeftPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  info->_bottomHPadding = padding;

  [self pushViewInfoToAutoLayoutManagers: info];
} 

- (float) leftPaddingForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_bottomHPadding;
}

- (void) setRightPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  info->_topHPadding = padding;

  [self pushViewInfoToAutoLayoutManagers: info];
} 

- (float) rightPaddingForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_topHPadding;
}

- (void) setBottomPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  info->_bottomVPadding = padding;

  [self pushViewInfoToAutoLayoutManagers: info];
}

- (float) bottomPaddingForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_bottomVPadding;
}

- (void) setTopPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  info->_topVPadding = padding;

  [self pushViewInfoToAutoLayoutManagers: info];
}

- (float) topPaddingForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_topVPadding;
}

- (void) setRowSpan: (int)span  
	    forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  int spanIncrease = span - info->_rowSpan;

  /* TODO: First of all, check that there are enough dummy segments
   * following the view.  If the span is larger than the number of
   * dummy segments following the view we have, we raise an exception.
   */

  info->_rowSpan = span;

  /* Now we need to take care of something quite delicate - empty/dummy segments.
   * 
   * When you create an empty table, we fill it with empty segments.
   * In other words, we fill all the lines of the row manager and the
   * column manager with empty/dummy segments.  If you add a view with
   * rowSpan=1 and columnSpan=1 somewhere, the segments in that
   * row/column get replaced by the ones associated with the view.
   *
   * If we now increase the columnSpan of a view from 1 to 2, for
   * example, we need to remove the empty/dummy segment on its right;
   * this will also shift the columnSegmentIndex of all the segments
   * on its right.
   *
   * If we decrease the columnSpan of a view from 2 to 1, for example,
   * we need to add an empty/dummy segment on its bottom and increase
   * all the columnSegmentIndex of all the segments on its right by 1.
   *
   * Finally, there is the case of a view where both rowSpan and
   * columnSpan are 2.  In that case, we still only delete the
   * empty/dummy segment on its immediate right in its row line and
   * the one on its bottom in its column line - ie, it's not a special
   * case.  We leave all the other ones in place.  It's important to
   * understand that the final structure is slightly surprising.  If
   * you look at row lines, you would see:
   *
   *  -----
   *  |XXX|
   *  -----
   *  |o|o|
   *  -----
   *
   * where 'o' are the empty/dummy segments.  This is Ok because the
   * two 'o' segments are irrelevant in terms of layout.
   *
   * If you look at column lines, you would see:
   *
   *  -----
   *  |X|o|
   *  |X|-|
   *  |X|o|
   *  -----
   *
   * where 'o' are the empty/dummy segments.  This is again Ok because
   * the two 'o' segments are irrelevant in terms of layout.
   *
   * The relationship between rows and columns makes no sense if you think
   * of the 'o' as real, existing views.  But the layout works fine.
   */

  if (spanIncrease > 0)
    {
      /* If the span has increased, we need to remove an appropriate
       * number of empty/dummey segments.
       */
      int i;
      
      for (i = 0; i < spanIncrease; i++)
	{
	  [_rowManager removeSegmentAtIndex: (info->_rowSegmentIndex + 1)
		       inLine: [_columns objectAtIndex: info->_columnPosition]];
	}
    }
  else
    {
      /* If it has decreased, we add them.  */
      int i;
      
      for (i = 0; i < -spanIncrease; i++)
	{
	  [_rowManager insertNewSegmentAtIndex: (info->_rowSegmentIndex + 1)
		       inLine: [_columns objectAtIndex: info->_columnPosition]];
	}
    }
  
  /* Now we need to adjust the segment indexes to account for the fact
   * that we removed/added the empty/dummy segments.
   */
  if (spanIncrease != 0)
    {
      /* We reduce the segment index of all the views on our
       * right/bottom by spanIncrease.  Eg, if span is increasing from
       * 1 to 2, we need to subtract 1 by all these segment indexes
       * because we'll be removing an empty/dummy segment.  If the
       * increase is negative, it's the reverse.
       */
      NSEnumerator *e = [_viewInfo objectEnumerator];
      GSAutoLayoutGridViewInfo *viewInfo;
      
      while ((viewInfo = [e nextObject]) != nil)
	{
	  if (viewInfo->_columnPosition == info->_columnPosition
	      && viewInfo->_rowSegmentIndex > info->_rowSegmentIndex)
	    {
	      viewInfo->_rowSegmentIndex -= spanIncrease;
	    }
	}
    }
  
  [self pushViewInfoToAutoLayoutManagers: info];
}

- (int) rowSpanForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_rowSpan;
}

- (void) setColumnSpan: (int)span  
	       forView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  int spanIncrease = span - info->_columnSpan;

  /* TODO: First of all, check that there are enough dummy segments
   * following the view.  If the span is larger than the number of
   * dummy segments following the view we have, we raise an exception.
   */

  info->_columnSpan = span;

  if (spanIncrease > 0)
    {
      /* If the span has increased, we need to remove an appropriate
       * number of empty/dummey segments.
       */
      int i;
      
      for (i = 0; i < spanIncrease; i++)
	{
	  [_columnManager removeSegmentAtIndex: (info->_columnSegmentIndex + 1)
			  inLine: [_rows objectAtIndex: info->_rowPosition]];
	}
    }
  else
    {
      /* If it has decreased, we add them.  */
      int i;
      
      for (i = 0; i < -spanIncrease; i++)
	{
	  [_rowManager insertNewSegmentAtIndex: (info->_columnSegmentIndex + 1)
		       inLine: [_rows objectAtIndex: info->_rowPosition]];
	}
    }
  
  /* Now we need to adjust the segment indexes to account for the fact
   * that we removed/added the empty/dummy segments.
   */
  if (spanIncrease != 0)
    {
      /* We reduce the segment index of all the views on our
       * right/bottom by spanIncrease.  Eg, if span is increasing from
       * 1 to 2, we need to subtract 1 by all these segment indexes
       * because we'll be removing an empty/dummy segment.  If the
       * increase is negative, it's the reverse.
       */
      NSEnumerator *e = [_viewInfo objectEnumerator];
      GSAutoLayoutGridViewInfo *viewInfo;
      
      while ((viewInfo = [e nextObject]) != nil)
	{
	  if (viewInfo->_rowPosition == info->_rowPosition
	      && viewInfo->_columnSegmentIndex > info->_columnSegmentIndex)
	    {
	      viewInfo->_columnSegmentIndex -= spanIncrease;
	    }
	}
    }

  [self pushViewInfoToAutoLayoutManagers: info];
}

- (int) columnSpanForView: (NSView *)aView
{
  GSAutoLayoutGridViewInfo *info = [self infoForView: aView];
  return info->_columnSpan;
}

- (void) setProportion: (float)proportion  forRow: (int)row
{
  [_rowManager setMinimumLength: 0
	       alwaysExpands: NO
	       neverExpands: NO
	       proportion: proportion
	       ofLinePartAtIndex: row];
}

- (float) proportionForRow: (int)row
{
  return [_rowManager proportionOfLinePartAtIndex: row];
}

- (void) setProportion: (float)proportion  forColumn: (int)column
{
  [_columnManager setMinimumLength: 0
		  alwaysExpands: NO
		  neverExpands: NO
		  proportion: proportion
		  ofLinePartAtIndex: column];
}

- (float) proportionForColumn: (int)column
{
  return [_columnManager proportionOfLinePartAtIndex: column];
}


- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  if (_hExpand)
    {
      return GSAutoLayoutExpand;
    }
  else if (_hWeakExpand)
    {
      return GSAutoLayoutWeakExpand;
    }
  else
    {
      return GSAutoLayoutAlignCenter;
    }
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  if (_vExpand)
    {
      return GSAutoLayoutExpand;
    }
  else if (_vWeakExpand)
    {
      return GSAutoLayoutWeakExpand;
    }
  else
    {
      return GSAutoLayoutAlignCenter;
    }
}

- (float) autoLayoutDefaultLeftPadding
{
  return 0;
}

- (float) autoLayoutDefaultRightPadding
{
  return 0;
}

- (float) autoLayoutDefaultBottomPadding
{
  return 0;
}

- (float) autoLayoutDefaultTopPadding
{
  return 0;
}

- (void) sizeToFitContent
{
  [self setFrameSize: [self minimumSizeForContent]];
}

- (NSSize) minimumSizeForContent
{
  /* Get it from the autolayout managers.  */
  NSSize minimum;
  minimum.height = [_rowManager minimumLineLength];
  minimum.width = [_columnManager minimumLineLength];

  return minimum;
}

- (void) setDisplayAutoLayoutContainers: (BOOL)flag
{
  [super setDisplayAutoLayoutContainers: flag];
  _displayAutoLayoutContainers = flag;
  [self setNeedsDisplay: YES];
}

- (void) drawRect: (NSRect)exposedRect
{
  if (_displayAutoLayoutContainers)
    {
      /* Draw a red line around ourselves.  */
      NSRect bounds = [self bounds];

      [[NSColor redColor] set];
      NSFrameRect (bounds);

      /* Draw dotted red lines to display where we separate the
       * various boxes.  */
      
      /* First, go over all the columns, and draw a vertical line
       * between each column and the next one.  */
      {
	int i, count = [_columnManager linePartCount];

	for (i = 0; i < count; i++)
	  {
	    GSAutoLayoutSegmentLayout s;
	    
	    s = [_columnManager layoutOfLinePartAtIndex: i];
	    
	    if (i > 0)
	      {
		/* We draw a dashed line between each line part and
		 * the previous one.  */
		NSBezierPath *path;
		static const float dash[2] = { 1.0, 2.0 };
		
		path = [NSBezierPath bezierPath];
		[path setLineDash: dash  count: 2  phase: 0.0];
		[path moveToPoint: NSMakePoint (s.position, NSMinY (bounds))];
		[path lineToPoint: NSMakePoint (s.position, NSMaxY (bounds))];
		[path stroke];
	      }
	  }
      }

      /* Do the same for the rows.  */
      {
	int i, count = [_rowManager linePartCount];

	for (i = 0; i < count; i++)
	  {
	    GSAutoLayoutSegmentLayout s;
	    
	    s = [_rowManager layoutOfLinePartAtIndex: i];
	    
	    if (i > 0)
	      {
		/* We draw a dashed line between each line part and
		 * the previous one.  */
		NSBezierPath *path;
		static const float dash[2] = { 1.0, 2.0 };

		path = [NSBezierPath bezierPath];
		[path setLineDash: dash  count: 2  phase: 0.0];
		[path moveToPoint: NSMakePoint (NSMinX (bounds), NSMaxY (bounds) - s.position)];
		[path lineToPoint: NSMakePoint (NSMaxX (bounds), NSMaxY (bounds) - s.position)];
		[path stroke];
	      }
	  }
      }
    }
}

@end

