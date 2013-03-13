/* -*-objc-*-
   GSAutoLayoutHBox.m

   Copyright (C) 2002 Free Software Foundation, Inc.

   Author: Nicola Pero <n.pero@mi.flashnet.it>
   Date: May-November 2002

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

#include "GSAutoLayoutHBox.h"
#include "GSAutoLayoutManager.h"
#include "GSAutoLayoutStandardManager.h"
#include "GSAutoLayoutProportionalManager.h"
#include "GSAutoLayoutDefaults.h"

/* This is basically a struct which can be stored into a NSArray - all
 * ivars (except setting _view and _column, which are set when the
 * info is created and destroyed when the info is destroyed) accessed
 * directly.  */
@interface GSAutoLayoutHBoxViewInfo : NSObject
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

  /* For views that should look bigger (or smaller!) in proportional
   * autolayout managers.  */
  float _proportion;

  /* The autolayout _vManager id of this column.  */
  id _column;
}
@end

@implementation GSAutoLayoutHBoxViewInfo
- (id) initWithView: (NSView *)aView
	     column: (id)aColumn
{
  ASSIGN (_view, aView);
  ASSIGN (_column, aColumn);
  return self;
}

- (void) dealloc
{
  RELEASE (_column);
  RELEASE (_view);
  [super dealloc];
}
@end

@implementation GSAutoLayoutHBox
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

  /* The horizontal layout manager is by default a standard one,
   * but could be changed.  */
  manager = [GSAutoLayoutStandardManager new];
  [self setAutoLayoutManager: manager];
  RELEASE (manager);

  /* The vertical layout manager is always a standard layout manager
   * and can't be changed (pointless to change it anyway!) */
  _vManager = [GSAutoLayoutStandardManager new];
  
  [[NSNotificationCenter defaultCenter] 
    addObserver: self
    selector: @selector(autoLayoutManagerChangedVLayout:)
    name: GSAutoLayoutManagerChangedLayoutNotification
    object: _vManager];

  return self;
}

- (void) dealloc
{
  RELEASE (_hManager);
  RELEASE (_vManager);
  RELEASE (_viewInfo);
  RELEASE (_line);
  [super dealloc];
}

- (void) setBoxType: (GSAutoLayoutBoxType)type
{
  if (type != [self boxType])
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

      [self setAutoLayoutManager: manager];
      RELEASE (manager);
    }
}

- (GSAutoLayoutBoxType) boxType
{
  if ([_hManager isKindOfClass: [GSAutoLayoutProportionalManager class]])
    {
      return GSAutoLayoutProportionalBox;
    }
  else
    {
      return GSAutoLayoutStandardBox;
    }
}

- (void) setAutoLayoutManager: (GSAutoLayoutManager *)aLayoutManager
{
  /* NB: this method currently only works if you call it when
   * there are no views in the hbox.  TODO: Extend it.
   */
  ASSIGN (_hManager, aLayoutManager);

  _line = [_hManager addLine];
  RETAIN (_line);
  
  [[NSNotificationCenter defaultCenter] 
    addObserver: self
    selector: @selector(autoLayoutManagerChangedHLayout:)
    name: GSAutoLayoutManagerChangedLayoutNotification
    object: _hManager];
}

- (GSAutoLayoutManager *)autoLayoutManager
{
  return _hManager;
}

/* Private method to retrieve the info for a view.  */
- (GSAutoLayoutHBoxViewInfo *) infoForView: (NSView *)aView
{
  int i, count = [_viewInfo count];

  for (i = 0; i < count; i++)
    {
      GSAutoLayoutHBoxViewInfo *info = [_viewInfo objectAtIndex: i];

      if (info->_view == aView)
	{
	  return info;
	}
    }
  return nil;
}

/* Private methods to push layout info to layout managers.  */
- (void) pushToHManagerInfoForViewAtIndex: (int)i
{
  GSAutoLayoutHBoxViewInfo *info = [_viewInfo objectAtIndex: i];

  [_hManager setMinimumLength: (info->_minimumSize).width
	     alignment: info->_hAlignment
	     bottomPadding: info->_bottomHPadding
	     topPadding: info->_topHPadding
	     span: 1
	     ofSegmentAtIndex: i
	     inLine: _line];

  if (info->_proportion != 1)
    {
      [_hManager setMinimumLength: 0
		 alwaysExpands: NO
		 neverExpands: NO
		 proportion: info->_proportion
		 ofLinePartAtIndex: i];
    }
  else
    {
      [_hManager removeInformationOnLinePartAtIndex: i];
    }

  [_hManager updateLayout];
}

- (void) pushToVManagerInfoForViewAtIndex: (int)i
{
  GSAutoLayoutHBoxViewInfo *info = [_viewInfo objectAtIndex: i];

  [_vManager setMinimumLength: (info->_minimumSize).height
	     alignment: info->_vAlignment
	     bottomPadding: info->_bottomVPadding
	     topPadding: info->_topVPadding
	     span: 1
	     ofSegmentAtIndex: 0
	     inLine: info->_column];

  [_vManager updateLayout];
}

- (void) addView: (NSView *)aView
{
  int count = [_viewInfo count];
  GSAutoLayoutHBoxViewInfo *info;
  id column = [_vManager addLine];

  info = [[GSAutoLayoutHBoxViewInfo alloc] initWithView: aView  column: column];  
  info->_minimumSize = [aView frame].size;
  info->_hAlignment = [aView autoLayoutDefaultHorizontalAlignment];
  info->_vAlignment = [aView autoLayoutDefaultVerticalAlignment];
  info->_bottomHPadding = [aView autoLayoutDefaultLeftPadding];
  info->_topHPadding = [aView autoLayoutDefaultRightPadding];
  info->_bottomVPadding = [aView autoLayoutDefaultBottomPadding];
  info->_topVPadding = [aView autoLayoutDefaultTopPadding];
  info->_proportion = 1;

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
  
  /* First, vertical layout.  */
  [_vManager insertNewSegmentAtIndex: 0
	     inLine: column];
  
  [self pushToVManagerInfoForViewAtIndex: count];

  /* And then, horizontal layout.  */
  [_hManager insertNewSegmentAtIndex: count
	     inLine: _line];

  [self pushToHManagerInfoForViewAtIndex: count];
}

- (void) removeView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];

  [_vManager removeSegmentAtIndex: 0
	     inLine: info->_column];
  [_vManager removeLine: info->_column];

  [_hManager removeInformationOnLinePartAtIndex: index];

  [_hManager removeSegmentAtIndex: index
	     inLine: _line];

  [_viewInfo removeObject: info];

  /* Recompute the _vExpand, _hExpand, _vWeakExpand and _hWeakExpand
   * flags.  */
  {
    int i, count = [_viewInfo count];

    _vExpand = NO;
    _vWeakExpand = NO;
    _hExpand = NO;
    _hWeakExpand = NO;
    
    for (i = 0; i < count; i++)
      {
	info = [_viewInfo objectAtIndex: i];
	
	if (info->_vAlignment == GSAutoLayoutExpand)
	  {
	    _vExpand = YES;
	  }
	if (info->_vAlignment == GSAutoLayoutWeakExpand)
	  {
	    _vWeakExpand = YES;
	  }
	if (info->_hAlignment == GSAutoLayoutExpand)
	  {
	    _hExpand = YES;
	  }
	if (info->_hAlignment == GSAutoLayoutWeakExpand)
	  {
	    _hWeakExpand = YES;
	  }
      } 
  }
  
  /* Remove the view from our subviews.  */
  [aView removeFromSuperview];

  /* Update the layout.  */
  [_vManager updateLayout];
  [_hManager updateLayout];
}

- (void) autoLayoutManagerChangedVLayout: (NSNotification *)notification
{
  float newHeight;
  int i, count;

  if ([notification object] != _vManager)
    {
      return;
    }

  newHeight = [_vManager lineLength];

  [super setFrameSize: NSMakeSize (([self frame]).size.width, newHeight)];

  count = [_viewInfo count];

  for (i = 0; i < count; i++)
    {
      GSAutoLayoutSegmentLayout s;
      GSAutoLayoutHBoxViewInfo *info;
      NSRect newFrame;

      info = [_viewInfo objectAtIndex: i];

      s = [_vManager layoutOfSegmentAtIndex: 0  inLine: info->_column];

      newFrame = [info->_view frame];
      newFrame.origin.y = s.position;
      newFrame.size.height = s.length;

      [info->_view setFrame: newFrame];
    }
}


- (void) autoLayoutManagerChangedHLayout: (NSNotification *)notification
{
  float newWidth;
  int i, count;

  if ([notification object] != _hManager)
    {
      return;
    }
  
  newWidth = [_hManager lineLength];

  [super setFrameSize: NSMakeSize (newWidth, ([self frame].size).height)];

  count = [_viewInfo count];

  for (i = 0; i < count; i++)
    {
      GSAutoLayoutSegmentLayout s;
      GSAutoLayoutHBoxViewInfo *info;
      NSRect newFrame;

      info = [_viewInfo objectAtIndex: i];

      s = [_hManager layoutOfSegmentAtIndex: i  inLine: _line];

      newFrame = [info->_view frame];
      newFrame.origin.x = s.position;
      newFrame.size.width = s.length;

      [info->_view setFrame: newFrame];
    }
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
  
  if ([_viewInfo count] > 0)
    {
      GSAutoLayoutHBoxViewInfo *info;
      info = [_viewInfo objectAtIndex: 0];
      [_vManager forceLength: frame.size.height  ofLine: info->_column];
      [_vManager updateLayout];
    }
  else
    {
      /* ... ? ... we need to save the forced height somewhere ... but
       * how do you remove the forcing afterwards ? */
    }

  [_hManager forceLength: frame.size.width  ofLine: _line];
  [_hManager updateLayout];
}

- (void) setFrameSize: (NSSize)size
{
  NSSize oldSize = [self frame].size;
  
  if (oldSize.width == size.width && oldSize.height == size.height)
    {
      return;
    }

  [super setFrameSize: size];

  if ([_viewInfo count] > 0)
    {
      GSAutoLayoutHBoxViewInfo *info;
      info = [_viewInfo objectAtIndex: 0];
      [_vManager forceLength: size.height  ofLine: info->_column];
      [_vManager updateLayout];
    }
  else
    {
      /* ... ? ... we need to save the forced height somewhere ... but
       * how do you remove the forcing afterwards ? */
    }

  [_hManager forceLength: size.width  ofLine: _line];
  [_hManager updateLayout];
}

- (void) setMinimumSize: (NSSize)aSize  forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];
  
  info->_minimumSize = aSize;
  
  [self pushToHManagerInfoForViewAtIndex: index];
  [self pushToVManagerInfoForViewAtIndex: index];
}

- (NSSize) minimumSizeForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_minimumSize;
}


- (void) setHorizontalAlignment: (GSAutoLayoutAlignment)flag  
			forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];
  int i, count;

  info->_hAlignment = flag;

  /* Recompute the _hExpand and _hWeakExpand flags.  */
  _hExpand = NO;
  _hWeakExpand = NO;

  count = [_viewInfo count];

  for (i = 0; i < count; i++)
    {
      info = [_viewInfo objectAtIndex: i];
  
      if (info->_hAlignment == GSAutoLayoutExpand)
	{
	  _hExpand = YES;
	}
      if (info->_hAlignment == GSAutoLayoutWeakExpand)
	{
	  _hWeakExpand = YES;
	}
    }
  
  [self pushToHManagerInfoForViewAtIndex: index];
}

- (GSAutoLayoutAlignment) horizontalAlignmentForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_hAlignment;
}


- (void) setVerticalAlignment: (GSAutoLayoutAlignment)flag  
		      forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];
  int i, count;

  info->_vAlignment = flag;

  /* Recompute the _vExpand and _vWeakExpand flags.  */
  _vExpand = NO;
  _vWeakExpand = NO;

  count = [_viewInfo count];

  for (i = 0; i < count; i++)
    {
      info = [_viewInfo objectAtIndex: i];
  
      if (info->_vAlignment == GSAutoLayoutExpand)
	{
	  _vExpand = YES;
	}
      if (info->_vAlignment == GSAutoLayoutWeakExpand)
	{
	  _vWeakExpand = YES;
	}
    }

  [self pushToVManagerInfoForViewAtIndex: index];
}

- (GSAutoLayoutAlignment) verticalAlignmentForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_vAlignment;
}

- (void) setLeftPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];
  
  info->_bottomHPadding = padding;

  [self pushToHManagerInfoForViewAtIndex: index];
} 

- (float) leftPaddingForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_bottomHPadding;
}

- (void) setRightPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];
  
  info->_topHPadding = padding;
  
  [self pushToHManagerInfoForViewAtIndex: index];
} 

- (float) rightPaddingForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_topHPadding;
}

- (void) setBottomPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];

  info->_bottomVPadding = padding;

  [self pushToVManagerInfoForViewAtIndex: index];
}

- (float) bottomPaddingForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_bottomVPadding;
}

- (void) setTopPadding: (float)padding  forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];

  info->_topVPadding = padding;

  [self pushToVManagerInfoForViewAtIndex: index];
}

- (float) topPaddingForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_topVPadding;
}

- (void) setProportion: (float)proportion
	       forView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  int index = [_viewInfo indexOfObject: info];

  info->_proportion = proportion;
  [self pushToHManagerInfoForViewAtIndex: index];
}

- (float) proportionForView: (NSView *)aView
{
  GSAutoLayoutHBoxViewInfo *info = [self infoForView: aView];
  return info->_proportion;
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
  minimum.height = [_vManager minimumLineLength];
  minimum.width = [_hManager minimumLineLength];

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
       * various boxes.  We want to display the lines exactly at the
       * boundaries of the line parts, so we get the line part
       * boundaries from the autolayout manager.  */
      {
	int i, count = [_hManager linePartCount];

	for (i = 0; i < count; i++)
	  {
	    GSAutoLayoutSegmentLayout s;
	    
	    s = [_hManager layoutOfLinePartAtIndex: i];
	    
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
    }
}

@end

