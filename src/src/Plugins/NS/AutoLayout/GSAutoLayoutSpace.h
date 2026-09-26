/* -*-objc-*-
   GSAutoLayoutSpace.h

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

#ifndef _GNUstep_H_GSAutoLayoutSpace
#define _GNUstep_H_GSAutoLayoutSpace

#ifndef GNUSTEP
# include <Foundation/Foundation.h>
# include <AppKit/AppKit.h>
# include "GNUstep.h"
#else
# include <AppKit/NSView.h>
#endif

#include "GSAutoLayoutDefaults.h"

/* A GSAutoLayoutSpace, by default, has zero size.  It has:
 *
 * halign="wexpand"
 * valign="wexpand"
 * hPadding="0"
 * vPadding="0"
 *
 * In practice, if you put a GSAutoLayoutSpace object in any autolayout
 * container such as GSGrid, GSHBox or GSVBox, the object will start
 * with size 0, and expand to get more space when the box is expanded
 * in any direction.
 * You need such an object if you want additional space to be put into
 * paddings or empty space between objects, or for other special
 * alignment needs.
 *
 * Please note that this object will expand in all directions; for
 * example, if you put a <space /> inside a <hbox>, then the <hbox>
 * will expand in the vertical direction as well as the horizontal
 * one.  If you only want to expand in the horizontal direction, use a
 * <hspace /> (ie, a GSAutoLayoutHSpace).
 */
@interface GSAutoLayoutSpace : NSView
@end

#endif 
