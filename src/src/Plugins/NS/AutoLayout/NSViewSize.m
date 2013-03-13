/* -*-objc-*-
   NSViewSize.m

   Copyright (C) 2002-2010 Free Software Foundation, Inc.

   Author: Nicola Pero <n.pero@mi.flashnet.it>
   Date: March 2002, November 2002, May 2010

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

#include "NSViewSize.h"

@implementation NSView (sizeToContent)

- (void) sizeToFitContent
{
  /* In the general case of a general view, this is a no-op.  We have
   * no idea how to fit the size to the content; we trust that whoever
   * set up the view set up the width and height to be the right ones
   * for the content.  This has the additional benefit that
   * -minimuSizeForContent will return the current size of the view as
   * the minimum; ie, unless we know otherwise, we assume that the
   * view has been sized to display its content properly and we don't
   * want to shrink it.
   */
}

- (NSSize) minimumSizeForContent
{
  NSRect oldFrame;
  NSSize minimumSize;

  /* Save the oldFrame.  */
  oldFrame = [self frame];
  
  /* Resize the view to fit the contents ... this is the only
   * way available in the AppKit to get the minimum size.  */
  [self sizeToFitContent];

  /* Get the minimum size by reading the frame.  */
  minimumSize = [self frame].size;
  
  /* Restore the original frame.  */
  [self setFrame: oldFrame];
  
  return minimumSize;
}

@end

@implementation NSControl (sizeToContent)

- (void) sizeToFitContent
{
  [self sizeToFit];
}

@end

/* NSBox comments - make sure you realize that NSBox -sizeToFit calls
 * the contentView's -sizeToFit method.  If you want the NSBox
 * contentview to have a hardcoded frame which is not the minimum
 * size, you need to embed it in a [hv]box before putting it in
 * the NSBox, to make sure this hardcoded frame overrides the minimum
 * size.
 */
@implementation NSBox (sizeToContent)

- (void) sizeToFitContent
{
  [self sizeToFit];
  /*
   * Nicola: please note that -sizeToFit on Apple Mac OS X 10.4 seems
   * to be getting the results wrong, vertically, by a couple of
   * pixels. :-(
   *
   * Maybe we should be fixing that here ?  Ie, add +2 pixels vertically 
   * to the frame size ?
   */
}

@end

/* NSSplitView's sizeToContent makes the splitview just big enough
 * to display its subviews, plus the dividers.  
 *
 * NB: (not really relevant any longer, but for future memories of
 * past problems) the default implementation of setting a NSZeroSize
 * would not work because resizing a splitview resizes all subviews,
 * and resizing a splitview to a zero size, at least on GNUstep,
 * resizes all subviews to have zero size, losing all size
 * relationships between them ... it's an unrecoverable operation on
 * GNUstep.
 */
@implementation NSSplitView (sizeToContent)
- (void) sizeToFitContent
{
  NSSize newSize = NSZeroSize;
  NSArray *subviews = [self subviews];
  int i, count = [subviews count];
  float dividerThickness = [self dividerThickness];

  if (count == 0)
    {
      [self setFrameSize: newSize];
      return;
    }

  if ([self isVertical])
    {
      NSView *subview = [subviews objectAtIndex: 0];
      NSRect subviewRect = [subview frame];

      newSize.height = subviewRect.size.height;

      for (i = 0; i < count; i++)
	{
	  subview = [subviews objectAtIndex: i];
	  subviewRect = [subview frame];
	  
	  newSize.width += subviewRect.size.width;
	  if (subviewRect.size.height > newSize.height)
	    {
	      newSize.height = subviewRect.size.height;
	    }
	}
      
      newSize.width += dividerThickness * (count - 1);
    }
  else
    {
      NSView *subview = [subviews objectAtIndex: 0];
      NSRect subviewRect = [subview frame];

      newSize.width = subviewRect.size.width;

      for (i = 0; i < count; i++)
	{
	  subview = [subviews objectAtIndex: i];
	  subviewRect = [subview frame];
	  
	  newSize.height += subviewRect.size.height;
	  if (subviewRect.size.width > newSize.width)
	    {
	      newSize.width = subviewRect.size.width;
	    }
	}
      
      newSize.height += dividerThickness * (count - 1);
    }
  
  [self setFrameSize: newSize];
}
@end

/* NSTabView - this is quite a difficult one, because a NSTabView is
 * in fact an autolayout container, very much like NSBox, and very
 * much like NSBox, it's very basic.
 *
 * We recommend that you use a <hbox> or <vbox> (or <grid>) inside
 * each tab view items to make sure you have a proper autolayout
 * container that manages the contents.
 */
@implementation NSTabView (sizeToContent)

- (void) sizeToFitContent
{
  /* We assume here that NSTabView -minimumSize returns the minimum
   * size required by the tab view to display its tabs and paddings,
   * not considering the content, which we add separately.
   *
   * On Apple, -minimumSize does not seem to include the space
   * required to display borders.  So we'll add 10 pixels to account
   * for that.
   */
  NSSize minimumSize = [self minimumSize];
  NSSize newSize = NSZeroSize;
  NSArray *tabViewItems = [self tabViewItems];
  int i, count = [tabViewItems count];

  for (i = 0; i < count; i++)
    {
      NSTabViewItem *item = [tabViewItems objectAtIndex: i];
      NSRect subViewRect = [[item view] frame];

      if(newSize.width < subViewRect.size.width)
	{
	  newSize.width = subViewRect.size.width;
	}
      if(newSize.height < subViewRect.size.height)
        {
	  newSize.height = subViewRect.size.height;
	}
    }

  newSize.width  += minimumSize.width;
  newSize.height += minimumSize.height;

  /* On Apple Mac OS X 10.4, [NSTabView -minimumSize] seems to be
   * wrong by exactly 10 pixels - presumably because it doesn't include
   * the size of borders.  We always add them just in case GNUstep
   * decides to do the same.
   */
  newSize.height += 10;
  newSize.width += 10;

  [self setFrameSize: newSize];

#ifdef GNUSTEP
  /*
   * We select explicitly the first tab item. This ensure the correct
   * drawn of the content.
   */
  [self selectFirstTabViewItem: nil];
#endif
}
@end

@implementation NSTextField (sizeToContent)

/* We want text fields to get a reasonable size when empty.  */
- (void) sizeToFitContent
{
  NSString *stringValue = [self stringValue];
  
  if (stringValue == nil  ||  [stringValue length] == 0)
    {
      [self setStringValue: @"Nicola"];
      [self sizeToFit];
      [self setStringValue: @""];
    }
  else
    {
      [self sizeToFit];
    }
}

@end

@implementation NSMatrix (sizeToContent)
- (void) sizeToFitContent
{
  /* Unbelievable how it may be, -sizeToFit or -sizeToCells on Apple
   * don't seem to compute the cell size.  This is taken from code I
   * originally wrote for gnustep-gui.  */
  NSSize newSize = NSZeroSize;

  int numRows = [self numberOfRows];
  int numCols = [self numberOfColumns];
  int i, j;

  for (i = 0; i < numRows; i++)
    {
      for (j = 0; j < numCols; j++)
	{
	  NSCell *cell = [self cellAtRow: i  column: j];
	  if (cell != nil)
	    {
	      NSSize tempSize = [cell cellSize];
	      if (tempSize.width > newSize.width)
		{
		  newSize.width = tempSize.width;
		}
	      if (tempSize.height > newSize.height)
		{
		  newSize.height = tempSize.height;
		}
	    }
	}
    }
  
  [self setCellSize: newSize];
  [self sizeToCells];
}
@end

/* NSImageView does not support sizeToFit on Apple!  The following
 * hack only works after the image has just been set.  */
#ifndef GNUSTEP
@implementation NSImageView (sizeToContent)
- (void) sizeToFitContent
{
  [self setFrameSize: [[self image] size]];
}
@end
#endif

/* NSColorWell does not have a working sizeToFit; let's just implement
 * it to use a minimum size of 52x30, which is roughly Gorm's default
 * color well size.  */
@implementation NSColorWell (sizeToContent)

- (void) sizeToFitContent
{
  [self setFrameSize: [self minimumSizeForContent]];
}

- (NSSize) minimumSizeForContent
{
  return NSMakeSize (52, 30);
}

@end

/* NSSlider does not have sizeToFit; let's just implement it to use a
   minimum size of 83x16, which is roughly Gorm's default NSSlider
   size.  */
@implementation NSSlider (sizeToContent)

- (void) sizeToFitContent
{
  [self setFrameSize: [self minimumSizeForContent]];
}

- (NSSize) minimumSizeForContent
{
  return NSMakeSize (83, 16);
}

@end
