/* -*-objc-*-
   GSAutoLayoutSpace.m

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

#include "GSAutoLayoutSpace.h"

@implementation GSAutoLayoutSpace

- (GSAutoLayoutAlignment) autoLayoutDefaultHorizontalAlignment
{
  return GSAutoLayoutWeakExpand;
}

- (GSAutoLayoutAlignment) autoLayoutDefaultVerticalAlignment
{
  return GSAutoLayoutWeakExpand;
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
  [self setFrameSize: NSZeroSize];
}

- (NSSize) minimumSizeForContent
{
  return NSZeroSize;
}

@end
