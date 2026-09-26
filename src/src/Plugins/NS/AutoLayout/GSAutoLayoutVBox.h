/* -*-objc-*-
   GSAutoLayoutVBox.h

   Copyright (C) 2002 Free Software Foundation, Inc.

   Author: Nicola Pero <n.pero@mi.flashnet.it>
   Date: March-November 2002

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

/* This file is generate by GSAutoLayoutHBox.h by replacing 'hbox' with 'vbox'.
 * Please be very careful with case (GSVbox is part of the old
 * gnustep-gui box system).  */

#ifndef _GNUstep_H_GSAutoLayoutVBox
#define _GNUstep_H_GSAutoLayoutVBox

#ifndef GNUSTEP
# include <Foundation/Foundation.h>
# include <AppKit/AppKit.h>
# include "GNUstep.h"
#else
# include <AppKit/NSView.h>
#endif

#include "GSAutoLayoutBox.h"

@class NSNotification;
@class NSArray;
@class NSMutableArray;

#include "GSAutoLayoutManager.h"

/* NB: Class hierarchy and ivar layout might change ... but the class should 
 * implement the GSAutoLayoutBox protocol.  All public (and hopefully stable) methods
 * are in that protocol.  */
@interface GSAutoLayoutVBox : NSView <GSAutoLayoutBox>
{
  /* The info on the views.  */
  NSMutableArray *_viewInfo;

  /* YES if there is any view with GSAutoLayoutExpand alignment in the
     horizontal direction, NO otherwise  */
  BOOL _hExpand;

  /* YES if there is any view with GSAutoLayoutWeakExpand alignment in the
     horizontal direction, NO otherwise.  */
  BOOL _hWeakExpand;

  /* Idem in vertical.  */
  BOOL _vExpand;
  BOOL _vWeakExpand;

  /* The GSAutoLayoutManagers.  */
  GSAutoLayoutManager *_hManager;
  GSAutoLayoutManager *_vManager;
  
  /* The id identifying our line with the horizontal
     GSAutoLayoutManager.  */
  id _line;

  /* YES if we display red lines to represent the autolayout (for 
   * debugging/graphical editing purposes); NO if not (the default,
   * we are invisible by default).
   */
  BOOL _displayAutoLayoutContainers;
}

/* Please look in the GSAutoLayoutBox protocol for the actual public methods;
 * the following methods might change without notice.  */

- (void) setAutoLayoutManager: (GSAutoLayoutManager *)aLayoutManager;
- (GSAutoLayoutManager *)autoLayoutManager;

- (void) autoLayoutManagerChangedHLayout: (NSNotification *)notification;
- (void) autoLayoutManagerChangedVLayout: (NSNotification *)notification;

@end

#endif 
