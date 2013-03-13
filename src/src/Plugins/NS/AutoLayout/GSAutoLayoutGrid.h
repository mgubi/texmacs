/* -*-objc-*-
   GSAutoLayoutGrid.h

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

#ifndef _GNUstep_H_GSAutoLayoutGrid
#define _GNUstep_H_GSAutoLayoutGrid

#ifndef GNUSTEP
# include <Foundation/Foundation.h>
# include <AppKit/AppKit.h>
# include "GNUstep.h"
#else
# include <AppKit/NSView.h>
#endif

@class NSNotification;
@class NSArray;
@class NSMutableArray;

#include "GSAutoLayoutManager.h"
#include "GSAutoLayoutBox.h"

/* This is just a quick hack at a grid.  It doesn't support span.  It
   will be rewritten gradually.
 */

/*
 * This is the grid; it uses two autolayout managers, one for the rows
 * and one for the columns.  In other words, you can imagine a table
 * with rows and columns; every view in the grid is exactly positioned
 * in a cell of the table at a specified row and column.
 *
 * The 'span' is supported, representing views that take up multiple
 * rows or multiple columns.
 *
 * If the autolayout manager used for rows (or columns) is a
 * proportional one, the proportion is supported, representing rows
 * (or columns) that take up proportionally more space.
 *
 * The column autolayout manager decides on the size of each column.
 * A "line" of the column autolayout manager is a row of the grid.
 *
 * The row autolayout manager decides on the size of each row.  A
 * "line" of the row autolayout manager is a column of the grid.
 */

/* NB: Class hierarchy and ivar layout might change ... still
 * experimental.  */
@interface GSAutoLayoutGrid : NSView
{
  /* The info on the views.  The order does not matter; the objects in
   * the array are instances of the private GSAutoLayoutGridViewInfo
   * class.  */
  NSMutableArray *_viewInfo;

  /* (Cache) YES if there is any view with GSAutoLayoutExpand
   * alignment in the horizontal direction, NO otherwise.  */
  BOOL _hExpand;

  /* (Cache) YES if there is any view with GSAutoLayoutWeakExpand
   * alignment in the horizontal direction, NO otherwise.  */
  BOOL _hWeakExpand;

  /* (Cache) Idem in vertical.  */
  BOOL _vExpand;
  BOOL _vWeakExpand;

  /* The GSAutoLayoutManagers.  */
  GSAutoLayoutManager *_columnManager;
  GSAutoLayoutManager *_rowManager;
  
  /* The ids identifying our rows with the _columnManager.  */
  NSMutableArray *_rows;

  /* The ids identifying our columns with the _rowManager.  */
  NSMutableArray *_columns;

  /* YES if we display red lines to represent the autolayout (for 
   * debugging/graphical editing purposes); NO if not (the default,
   * we are invisible by default).
   */
  BOOL _displayAutoLayoutContainers;

  /* YES if we are in the process of performing an -updateLayout.  In
   * that case, we ignore concurrent notifications from the autolayout
   * managers about changes in layout, and will just always reread the
   * entire layout from the autolayout manager at the end.  */
  BOOL _performingLayoutUpdate;
}

/* NB: All the GSAutoLayoutGrid methods do *not* cause any autolayout
 * until you call -updateLayout.  This is for efficiency; you can do a
 * bunch of changes to the grid, and then have it do the autolayout
 * and all the view resizing and repositioning in a single go at the
 * end.  */

/* Add a row at the end of the grid (the grid starts with 0 rows and 0
 * columns; you need at least 1 row and 1 column to display
 * anything).  */
- (void) addRow;

/* Remove a row at the end of the grid.  */
- (void) removeRow;

/* The number of rows hold in the grid.  */
- (unsigned int) numberOfRows;

/* Add a column at the end of the grid (the grid starts with no
 * columns).  */
- (void) addColumn;

/* Remove a column at the end of the grid.  */
- (void) removeColumn;

/* The number of columns hold in the grid.  */
- (unsigned int) numberOfColumns;

/* Add a view to the grid.  The row and column must already exist;
 * they are numbered from 0.  If there is already a view in the
 * row/column (either because it was put there, or because it was put
 * elsewhere but it extends to this row/column position because of its
 * span), an exception is raised.  */
- (void) addView: (NSView *)view
	   inRow: (int)row
	  column: (int)column;

/* Remove a view from the grid.  */
- (void) removeView: (NSView *)view;

/* The number of views hold in a row of the grid.  */
- (int) numberOfViewsInRow: (int)row;

/* The number of views hold in a column of the grid.  */
- (int) numberOfViewsInColumn: (int)column;

/* Normally, there is nothing you need to do to have views
 * autosize/autoresize properly inside a grid.  Still, in some cases
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

/* Note: If the new span causes the view to overflow the grid, or to
 * overlap another view, an exception is raised.
 */
- (void) setRowSpan: (int)span  forView: (NSView *)aView;
- (int) rowSpanForView: (NSView *)aView;

/* Note: If the new span causes the view to overflow the grid, or to
 * overlap another view, an exception is raised.
 */
- (void) setColumnSpan: (int)span  forView: (NSView *)aView;
- (int) columnSpanForView: (NSView *)aView;

/* The 'proportion' of a row or column is only used by 'proportional'
 * grids (either by row or by column), and it is used as a measure of
 * the proportion of the row or column, as compared to a standard row
 * or column of proportion 1.  For example, in a proportional column
 * grid, a column with proportion == 2.0 will take up the double of
 * column space than the same column with proportion == 1.0 (the
 * default).
 */
- (void) setProportion: (float)proportion  forRow: (int)row;
- (float) proportionForRow: (int)row;

- (void) setProportion: (float)proportion  forColumn: (int)column;
- (float) proportionForColumn: (int)column;

/* After making any changes to the grid, you should call this method
 * to have it update its own layout.  */
- (void) updateLayout;

- (void) sizeToFitContent;
- (NSSize) minimumSizeForContent;

/* If column layout is done using a standard or proportional layout
 * manager; this determines if the columns can be of different sizes
 * or if they are always of the same (or proportional) size.  */
- (void) setColumnGridType: (GSAutoLayoutBoxType)type;
- (GSAutoLayoutBoxType) columnGridType;

/* If row layout is done using a standard or proportional layout
 * manager; this determines if the rows can be of different sizes or
 * if they are always of the same (or proportional) size.  */
- (void) setRowGridType: (GSAutoLayoutBoxType)type;
- (GSAutoLayoutBoxType) rowGridType;

/* The following methods are subject to change.  */
- (void) setColumnAutoLayoutManager: (GSAutoLayoutManager *)aLayoutManager;
- (GSAutoLayoutManager *)columnAutoLayoutManager;

- (void) setRowAutoLayoutManager: (GSAutoLayoutManager *)aLayoutManager;
- (GSAutoLayoutManager *)rowAutoLayoutManager;

- (void) rowAutoLayoutManagerChangedLayout: (NSNotification *)notification;
- (void) columnAutoLayoutManagerChangedLayout: (NSNotification *)notification;

@end

#endif 
