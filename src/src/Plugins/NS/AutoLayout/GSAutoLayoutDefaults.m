/* -*-objc-*-
   GSAutoLayoutDefaults.m

   Copyright (C) 2002 Free Software Foundation, Inc.

   Author: Nicola Pero <n.pero@mi.flashnet.it>
   Date: April 2002

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
#include "GSAutoLayoutDefaults.h"

@implementation NSView (GSAutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  return GSAutoLayoutAlignCenter;
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  return GSAutoLayoutAlignCenter;
}

- (float) autoLayoutDefaultLeftPadding
{
  /* Note that the Apple HIG seem to recommend 8 pixels to separate
   * standard controls.  Controls in a box will be separated by twice
   * this vertical padding, so if this is 4, then the control are
   * separated by 8 pixels by default. :-)
   */
  return 4;
}

- (float) autoLayoutDefaultRightPadding
{
  return 4;
}

- (float) autoLayoutDefaultBottomPadding
{
  return 4;
}

- (float) autoLayoutDefaultTopPadding
{
  return 4;
}

@end

#ifdef GNUSTEP
# include <AppKit/NSTextField.h>
#endif

@implementation NSTextField (AutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  if ([self isBezeled]  ||  [self isEditable])
    {
      return GSAutoLayoutExpand;
    }
  
  return GSAutoLayoutAlignCenter;
}

@end

#ifdef GNUSTEP
# include <AppKit/NSForm.h>
#endif

@implementation NSForm (AutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  return GSAutoLayoutExpand;
}

@end

#ifdef GNUSTEP
# include <AppKit/NSTextView.h>
#endif

@implementation NSTextView (AutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  return GSAutoLayoutExpand;
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  return GSAutoLayoutExpand;
}

@end

#ifdef GNUSTEP
# include <AppKit/NSScrollView.h>
#endif

@implementation NSScrollView (AutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  return GSAutoLayoutExpand;
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  return GSAutoLayoutExpand;
}

@end

#ifdef GNUSTEP
# include <AppKit/NSSplitView.h>
#endif

@implementation NSSplitView (AutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  return GSAutoLayoutExpand;
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  return GSAutoLayoutExpand;
}

@end

#ifdef GNUSTEP
# include <AppKit/NSBox.h>
#endif

@implementation NSBox (AutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  NSView *contentView = [self contentView];
  GSAutoLayoutAlignment flag;
  flag = [contentView autoLayoutDefaultHorizontalAlignment];

  if (flag == GSAutoLayoutExpand  ||  flag == GSAutoLayoutWeakExpand)
    {
      return flag;
    }

  return GSAutoLayoutAlignCenter;
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  NSView *contentView = [self contentView];
  GSAutoLayoutAlignment flag;
  flag = [contentView autoLayoutDefaultVerticalAlignment];

  if (flag == GSAutoLayoutExpand  ||  flag == GSAutoLayoutWeakExpand)
    {
      return flag;
    }

  return GSAutoLayoutAlignCenter;
}

@end

#ifndef GNUSTEP
/*
 * On Apple Mac OS X, push buttons when drawing leave around them a
 * lot of empty space.  Maybe the idea is that you put them one just
 * near the other one (with the frames technically touching), and the
 * correct empty space between them is implicitly drawn by the blank
 * space left inside its frame by each button ?  If so, it's
 * inconsistent with the rest of the framework, where objects don't
 * have implicit paddings and draw to the edges of their frames; it's
 * impossible to control comfortably button paddings programmatically,
 * and it's more trouble for us (and for anyone using the framework).
 *
 * Here we adjust the default padding to be 0 to account for this
 * problem.  With a padding of 0, buttons when laid out get spaced
 * exactly the native spacing used by other applications on the
 * platforms.  (Un)fortunately, not all buttons draw paddings in this
 * weird way.  We adjust only for push text buttons.
 */

@implementation NSButton (AutoLayoutDefaults)

- (float) autoLayoutDefaultLeftPadding
{
  /* Roughly, use 0 for push buttons, and 4 for the other ones.  
   * Empirically determined.  */
  if ([self isBordered] && [self bezelStyle] == NSRoundedBezelStyle)
    return 0;
  else
    return 4;
}

- (float) autoLayoutDefaultRightPadding
{
  /* Roughly, use 0 for push buttons, and 4 for the other ones.  
   * Empirically determined.  */
  if ([self isBordered] && [self bezelStyle] == NSRoundedBezelStyle)
    return 0;
  else
    return 4;
}

- (float) autoLayoutDefaultBottomPadding
{
  /* Roughly, use 1 for push buttons, and 4 for the other ones.
   * Empirically determined.  */
  if ([self isBordered] && [self bezelStyle] == NSRoundedBezelStyle)
    return 1;
  else
    return 4;
}

- (float) autoLayoutDefaultTopPadding
{
  /* Roughly, use 1 for push buttons, and 4 for the other ones.
   * Empirically determined.  */
  if ([self isBordered] && [self bezelStyle] == NSRoundedBezelStyle)
    return 1;
  else
    return 4;
}

@end
#endif

#ifdef GNUSTEP
# include <AppKit/NSTabView.h>
#endif

@implementation NSTabView (AutoLayoutDefaults)

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  NSArray *tabViewItems = [self tabViewItems];
  int i, count = [tabViewItems count];

  for (i = 0; i < count; i++)
    {
      NSTabViewItem *item = [tabViewItems objectAtIndex: i];
      NSView *subView = [item view];
      GSAutoLayoutAlignment flag;
      flag = [subView autoLayoutDefaultHorizontalAlignment];
      
      if (flag == GSAutoLayoutExpand  ||  flag == GSAutoLayoutWeakExpand)
	{
	  return flag;
	}
    }

  return GSAutoLayoutAlignCenter;
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  NSArray *tabViewItems = [self tabViewItems];
  int i, count = [tabViewItems count];

  for (i = 0; i < count; i++)
    {
      NSTabViewItem *item = [tabViewItems objectAtIndex: i];
      NSView *subView = [item view];
      GSAutoLayoutAlignment flag;
      flag = [subView autoLayoutDefaultVerticalAlignment];
      
      if (flag == GSAutoLayoutExpand  ||  flag == GSAutoLayoutWeakExpand)
	{
	  return flag;
	}
    }

  return GSAutoLayoutAlignCenter;
}

@end

@implementation NSView (GSAutoLayoutDisplayContainers)

- (void) setDisplayAutoLayoutContainers: (BOOL)flag
{
  NSArray *subviews = [self subviews];
  int i, count = [subviews count];
  
  for (i = 0; i < count; i++)
    {
      NSView *subview = [subviews objectAtIndex: i];
      [subview setDisplayAutoLayoutContainers: flag];
    }
}

@end

@implementation NSWindow (GSAutoLayoutDisplayContainers)

- (void) setDisplayAutoLayoutContainers: (BOOL)flag
{
  [[self contentView] setDisplayAutoLayoutContainers: flag];
}

@end

@implementation NSTabView (GSAutoLayoutDisplayContainers)

- (void) setDisplayAutoLayoutContainers: (BOOL)flag
{
  NSArray *tabViewItems = [self tabViewItems];
  int i, count = [tabViewItems count];

  for (i = 0; i < count; i++)
    {
      NSTabViewItem *item = [tabViewItems objectAtIndex: i];
      [[item view] setDisplayAutoLayoutContainers: flag];
    }
}

@end
