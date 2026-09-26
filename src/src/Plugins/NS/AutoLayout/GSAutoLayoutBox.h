/* -*-objc-*-
   GSAutoLayoutBox.h

   Copyright (C) 2002 Free Software Foundation, Inc.

   Author: Nicola Pero <n.pero@mi.flashnet.it>
   Date: November 2002

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

#ifndef _GNUstep_H_GSAutoLayoutBox
#define _GNUstep_H_GSAutoLayoutBox

#ifndef GNUSTEP
# include <Foundation/Foundation.h>
# include <AppKit/AppKit.h>
# include "GNUstep.h"
#else
# include <AppKit/NSView.h>
#endif

#include "GSAutoLayoutDefaults.h"

/*
 * The API in this file should remain stable even if we completely
 * rewrite the box internals.
 */

/* PUBLIC STABLE API */

typedef enum 
{
  /* A standard box lays out views over an irregular grid.  */
  GSAutoLayoutStandardBox = 0,

  /* A proportional box lays out views over a regular grid.  */
  GSAutoLayoutProportionalBox = 1

} GSAutoLayoutBoxType;

/* This protocol should be implemented by both GSAutoLayoutHBox and GSAutoLayoutVBox.  */
@protocol GSAutoLayoutBox

/* There are two variants of box - the 'standard' one, and the
 * 'proportional' one.  By default, a box is 'standard'.  You can
 * change the type using the following methods.
 */
- (void) setBoxType: (GSAutoLayoutBoxType)type;
- (GSAutoLayoutBoxType) boxType;

/* Add a view at the end of the box.  */
- (void) addView: (NSView *)aView;

/* Remove a view from the box.  */
- (void) removeView: (NSView *)aView;

/* The number of views hold in the box.  */
- (int) numberOfViews;

/* Normally, there is nothing you need to do to have views
 * autosize/autoresize properly inside a box.  Still, in some cases
 * you will need to override the automatic flags with your own.  For
 * those cases, you can use the following methods.  Mostly, I'd expect
 * you will never need those methods, except when you will want to use
 * setXXXAlignment:forView: to replace the default
 * GSAutoLayoutAlignCenter with GSAutoLayoutAlignBottom or
 * GSAutoLayoutAlignTop.
 */

- (void) setMinimumSize: (NSSize)aSize  forView: (NSView *)aView;
- (NSSize) minimumSizeForView: (NSView *)aView;

- (void) setHorizontalAlignment: (GSAutoLayoutAlignment)flag  
			forView: (NSView *)aView;
- (GSAutoLayoutAlignment) horizontalAlignmentForView: (NSView *)aView;

- (void) setVerticalAlignment: (GSAutoLayoutAlignment)flag  
		      forView: (NSView *)aView;
- (GSAutoLayoutAlignment) verticalAlignmentForView: (NSView *)aView;

- (void) setLeftPadding: (float)padding  forView: (NSView *)aView;
- (float) leftPaddingForView: (NSView *)aView;

- (void) setRightPadding: (float)padding  forView: (NSView *)aView;
- (float) rightPaddingForView: (NSView *)aView;

- (void) setBottomPadding: (float)padding  forView: (NSView *)aView;
- (float) bottomPaddingForView: (NSView *)aView;

- (void) setTopPadding: (float)padding  forView: (NSView *)aView;
- (float) topPaddingForView: (NSView *)aView;

/* The 'proportion' of a view is a delicate concept.  For 'standard'
 * boxes, it has not much of a meaning actually :-) it is ignored and
 * you should not set it.  For 'proportional' boxes, it is the number
 * of units that the view takes.  For example, in a proportional box,
 * a view with proportion == 2.0 will take up the double of space than
 * a view with proportion == 1.0 (the default).
 */
- (void) setProportion: (float)proportion  forView: (NSView *)aView;
- (float) proportionForView: (NSView *)aView;

/* Will be needed to support adding/removing views from a box dynamically,
 * such as inside a graphical interface development tool.  */
/* - (void) update; */

- (void) sizeToFitContent;
- (NSSize) minimumSizeForContent;
@end

#endif 
