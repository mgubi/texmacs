/* -*-objc-*-
   NSViewSize.h

   Copyright (C) 2002 Free Software Foundation, Inc.

   Author: Nicola Pero <n.pero@mi.flashnet.it>
   Date: April 2002, November 2002

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

#ifndef _GNUstep_H_NSViewSize
#define _GNUstep_H_NSViewSize

#ifndef GNUSTEP
# include <Foundation/Foundation.h>
# include <AppKit/AppKit.h>
# include "GNUstep.h"
#else
# include <AppKit/NSView.h>
#endif

/* 
 * The gui API does not always support a satisfactory native autosizing
 * mechanism - most classes have some sort of autosizing, more or less
 * satisfactory, but not all.  We really need a good working one - if that
 * doesn't happen, we are in serious trouble.  So by using categories, we
 * implement our own autosizing mechanism on top on the gui API one -
 * mostly wrapping the gui one (which is -sizeToFit), but, by using a
 * wrapper, we know we can modify the autosizing behaviour whenever it is
 * not up to our needs.  This is not always possible ... we do what we can
 * here, and try to work around unsolvable problems here by inserting
 * additional intermediate NSViews in the view tree when we use the
 * objects.
 */

@interface NSView (sizeToContent)

/* 
 * Changes the size of the view so that it exactly fits its content.
 * In the gui API -sizeToFit is similar ... so this method is often a
 * wrapper around -sizeToFit.  The reason we need a wrapper is that in
 * the gui it's not clear if -sizeToFit fits the size to the contents
 * or the contents to the size! ... in many cases its implementation
 * is sort of broken in that respect - we want to make sure we have a
 * reliable portable sizeToFitContent implementation under our control
 * here.
 *
 * The default implementation does nothing - it just assumes that the
 * view has been correctly sized by whoever set it up.  Most
 * subclasses will override that behaviour using the knowledge of the
 * specific data they display.
 */
- (void) sizeToFitContent;

/*
 * This returns the minimum size needed to display the content.  The
 * default implementation is complex, because there is no way of
 * getting the actual minimumSizeForContent for standard controls; the
 * best you can do is use -sizeToFit (or our more consistently correct
 * wrapper -sizeToFitContent).  What we do is, we save the current
 * frame of the view; we call sizeToFitContent; we read the size
 * (which we interpret as the minimumSizeForContent); then we restore
 * the original frame, and return the minimum size we determined.
 *
 * It would all be more logical if the gui had provided -minimumSize
 * and defined -(void) sizeToFit {[self setFrameSize: [self
 * minimumSize]];}.
 *
 * Because of this complex/perverse implementation, the less you call
 * this method, the better. :-)
 */
- (NSSize) minimumSizeForContent;

@end

#endif /* _GNUstep_H_NSViewSize */
