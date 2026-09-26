/* -*-objc-*-
   GSAutoLayoutManager.h

   Copyright (C) 2002 - 2008 Free Software Foundation, Inc.

   Author: Nicola Pero <nicola.pero@meta-innovation.com>
   Date: April 2002 - March 2008

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

#ifndef _GNUstep_H_GSAutoLayoutManager
#define _GNUstep_H_GSAutoLayoutManager

#ifndef GNUSTEP
# include <Foundation/Foundation.h>
# include <AppKit/AppKit.h>
# include "GNUstep.h"
#else
# include <Foundation/NSObject.h>
#endif

#include "GSAutoLayoutDefaults.h"

/* This design is experimental and could be changed.  */

/*
 * There are potentially infinite ways in which you may want to
 * arrange your objects in a window. :-)
 *
 * Renaissance/AutoLayout provides a few classes which should allow
 * you to autolayout your objects, with a small effort, in
 * most standard cases.
 *
 * The basic intelligent objects in these classes are the
 * GSAutoLayoutManager objects.
 *
 * There are two main subclasses, GSAutoLayoutStandardManager and
 * GSAutoLayoutProportionalManager.  You are not supposed to create
 * other subclasses, and only rarely to interact with them directly -
 * autolayout managers are mainly used internally by the user-level
 * objects (boxes and grids).
 *
 * A single GSAutoLayoutManager performs the basic autolayout
 * operations on a line.  Basically, it manages a line, and decides
 * how to break the line into segments, or how to line up segments in
 * order to build up the line.  Additionally, the autolayout manager
 * can manage multiple lines at the same time, and break those lines
 * in segments (/build those lines from segments) in such a way that
 * the resulting layout for the different lines are related between
 * them: which means that all the lines must be of the same total
 * size, and that there is a general division of this total size in
 * parts, called line parts (which might have different sizes, eg,
 * line part 0 could be different from line part 1), and each segment
 * on each line takes up exactly a number of line parts (called the
 * 'span' of the segment; eg, the first segment on a line could have
 * span=1 and take line part 0 and the second one could have span=2
 * and take line part 1 and line part 2).  Different subclasses use
 * different criteria to determine the optimal division of the lines
 * in line parts. [To understand the requirement of supporting
 * multiple lines, think of a table.  Each row in the table is a line
 * to be broken in columns - the line parts; and all rows in the table
 * must be the same total size, and must be broken in line parts in a
 * similar way.  The cells are the segments; normally they have span 1
 * and each cell (segment) is inside a single column (line part), but
 * the system also supports a cell (segment) taking up two columns
 * (line parts).  In this framework all rows (lines) would share the
 * same autolayout manager)].
 *
 * The line parts form an invisible grid over which the segment are
 * placed.
 *
 * Finally, when an autolayout manager has made the layout with
 * segments, in each segment it aligns the `segment content' according
 * to the padding and alignment which was specified for that segment.
 *
 * We consider all this the primitive autolayout operation, at least for
 * our boxes and grids.
 *
 * GSAutoLayoutManager is an abstract class; its concrete subclasses
 * provide different strategies of implementing this primitive
 * autolayout operation (eg, GSAutoLayoutStandardManager breaks the
 * line into line parts of unrelated size - while
 * GSAutoLayoutProportionalManager breaks the line into line parts of
 * equal (or proportional) size).
 *
 * To manage lines autolayout, a GSAutoLayoutManager needs some
 * information about what is to be displayed in the lines.  Clients
 * (usually box and grid objects) register themselves with the
 * GSAutoLayoutManager.  A client can register a line, and is given
 * back an id, which uniquely identify that line.  The client can then
 * update the autolayout manager information about that line (and the
 * segments contained in that line) at any time.  Whenever asked, the
 * autolayout manager performs full layout depending on the
 * information it has on the lines and segments.  When the autolayout
 * manager changes the layout of the lines, it posts a
 * GSAutoLayoutManagerChangedLayout notification, which the clients
 * should observe.  Once a client is informed that the layout has
 * changed, the client can request to the autolayout manager
 * information about the new way its line has been broken into
 * segments.
 *
 * This design is extremely general, but it's not extremely efficient.
 * Efficiency is irrelevant, since in normal conditions real window
 * layouts are composed of a few elements (a box normally does not
 * contain more than 10 elements).
 *
 * The GSAutoLayoutManager uses the following information on each
 * segment contained in a line:
 *
 *  - each segment on a line is identified by an integer, starting
 *  from 0 and going up.  The only importance of this integer is to
 *  allow the clients and the autolayout manager to have a way of
 *  identifying segments on a line, and to specify the sequence of
 *  segments on the line - segment 0 is always before segment 1 on the
 *  line etc.  Please note that - because of the span - there is no
 *  relationship between the numbers used in one line and on the other
 *  one (that is, you should not expect segment 4 on one line to be
 *  aligned with segment 4 on another line).  This number is really an
 *  internal identifier used between the client and the autolayout
 *  manager.  There can be no gaps in a line; to create visual gaps,
 *  you need to insert empty views that occupy a segment and display
 *  nothing in it.
 *
 *  - the left and right padding of the segment.  These are used so that
 *  there can be some space around the segment content.
 *
 *  - the minimum size of the segment content.
 *
 *  - the minimum size of the segment - this is computed by summing up
 *  the left padding, the right padding, and the minimum size of the
 *  segment content.  The GSAutoLayoutManager, no matter what
 *  algorithm uses to break the line into segments, should never make
 *  a segment shorter than this size.
 *
 *  - the alignment type for the segment content.  This might either
 *  be expand, weak expand, center, min or max.  If the alignment is
 *  expand, then it means the segment likes to be expanded - that is,
 *  the minimum size is enough to display some information, but making
 *  the view bigger displays more information - so it's good.  Any
 *  other values means that the view already displays all its
 *  information in its minimum size; it's then a matter of aesthetics
 *  and taste to decide what to do if more space is available.  If the
 *  alignment type is 'expand', the autolayout manager should,
 *  whenever possible, try to expand the segment.  The behaviour when
 *  the alignment flag is something else might depend on the specific
 *  autolayout manager.  Generally, the autolayout manager will always
 *  try to expand segments with an alignment of expand, but can't
 *  guarantee that segments with another alignment won't be expanded
 *  too (if they are lined up with segments with an expand flag in
 *  another line, they will be expanded too).  If the segments get
 *  expanded, then the segment content is placed inside the segment
 *  according to the alignment flags: if it is either min, max or
 *  center, then this is how the segment contents are to be aligned
 *  inside the segment (after the padding have been taken into
 *  accounts).  Finally, an alignment of 'weak expand' means that the
 *  segment contents doesn't like being expanded, but if the segment
 *  has to be expanded, then the segment contents should be expanded
 *  too for aesthetical reasons.
 *
 *  - a span (an integer) for the segment.  The default is 1.  This is
 *  is only meaningful when multiple lines are being laid out, in
 *  which case it is the number of line parts (the 'line parts' are an
 *  invisible grid over which the segments are placed; you can think
 *  of a line part as a 'column' when laying out the cells in a table
 *  row) that the segment takes up.  When the span is 1 for all
 *  segments, all segments in a line are numerated sequentially and,
 *  for example, segment 5 in one line is expected to have the same
 *  size as segment 5 in another line in the final layout.  When there
 *  are segments with a span which is not 1, then that's no longer
 *  necessarily true.
 *  
 * It is also possible to set information for specific line parts:
 *
 *  - the minimum size of the line part.  Useful when managing
 *  multiple lines, eg, you can set a minimum size for a column in a
 *  table.
 *
 *  - a flag to mark the column as expand or wexpand.  Useful when
 *  managing multiple lines, eg, you can decide that you want a column
 *  in a table to expand in preference to another one.
 *
 *  - a proportion (a float) for the line part.  The default is 1.  A
 *  GSAutoLayoutProportionalManager interprets it as a scaling of the
 *  number of basic units that the line part takes up.  Eg, a line
 *  part with proportion=2 would automatically have double the size of
 *  one with proportions=1.  If you think of the line parts as the
 *  'columns' in a line, then a proportion of 1 for all of them means
 *  that all columns have exactly the same size; changing the
 *  proportion of a column makes it bigger/smaller compared to the
 *  other ones.  The standard manager ignores this information.
 */

/* This struct is used to store and return layout information for a
 * segment (or line part).  */
typedef struct 
{
  float position;
  float length;
} GSAutoLayoutSegmentLayout;

@class NSMutableSet;

@interface GSAutoLayoutManager : NSObject
{
  /* The GSAutoLayoutManagerLine objects, which store the information
   * on each segment, and the final autolayout information.  */
  NSMutableSet *_lines;

  /* A dictionary that maps a line part index (as a NSNumber) to an
   * GSAutoLayoutManagerLineInformationPart object.  Used to store
   * information on line parts that have special settings.  */
  NSMutableDictionary *_linePartInformation;

  /* The following array is created and populated during an autolayout
   * update/computation.  First, we create the _lineParts array that
   * is an array of GSAutoLayoutManagerLinePart objects, each of which
   * includes all information on that specific line part; then, we
   * compute the minimum layout of the line parts; then, we do the
   * full layout by allocating the excess size to the various line
   * parts.  Finally, we can then use the final linePart autolayout to
   * generate the autolayout information stored in the _lines array.
   */
  NSMutableArray *_lineParts;

  /* The minimum length of the lines.  */
  float _minimumLength;

  /* The current length of the lines.  */
  float _length;

  /* If we need to recompute the minimum layout.  Set to YES when
   * a segment's attribute changes.  */
  BOOL _needsUpdateMinimumLayout;

  /* If we need to recompute the layout.  Set to YES when a line's
   * forced length is changed.  */
  BOOL _needsUpdateLayout;
}

/* There are two types of layout an autolayout manager should be able
 * to perform.  
 *
 * The first is bottom-to-top autolayout, that is, building lines from
 * the composing segments.  In this type of autolayout, the autolayout
 * manager starts by basically setting all segments to their minimum
 * size, and then adjusting this layout (by enlarging segments) to
 * meet the constrains (lines of the same length, and other constrains
 * imposed by the segment flags and segment relationships such as span
 * or proportion).  The resulting layout is the layout of minimum
 * length which still satisfies all constrains (segments are >= their
 * minimum length, all lines are of the same size, etc).  This layout
 * is the starting point of all layout changes - all layout changes
 * are always relative to this minimum layout.  The autolayout manager
 * automatically and always computes and keeps updated this layout
 * every time a layout change occurs, because this ideal minimum
 * layout is needed as a reference to actually build the actual
 * layout.  When you first display objects in a window, everything
 * is/should normally be displayed by default in this layout, unless a
 * frame change is later performed at the user request.  Whenever you
 * change the attributes of some of the segments (its expand flag, or
 * its minimum length, or its span or the proportion of the line
 * part), this minimum autolayout is automatically recomputed as soon
 * as you invoke -updateLayout.
 *
 * The second is top-to-bottom autolayout, that is, breaking lines in
 * segments.  This type of autolayout is needed when the user acts on
 * the window in some way, and this action modifies the layout
 * (typically by enlarging or reducing the window size, or by moving a
 * splitview divider bar, or similar).  In this type of autolayout,
 * the autolayout manager is informed that the length of one (or more)
 * of its lines has been forced to be a certain fixed amount
 * (different than the minimum length).  The autolayout manager starts
 * by considering the forced lengths of the lines, and searching for
 * the minimum forced length; it makes all lines of that length -
 * unless that would be less than the minimum line lenght, in which
 * case the autolayout manager will simply adopt the minimum length
 * layout, refusing to resize the lines it is managing below their
 * minimum length - knowing that this means that part of the lines
 * will be clipped in the window.  The autolayout manager then
 * computes the difference between the actual line length and the
 * minimum line lenght, and decides how to share the difference in
 * length between the different segments.
 *
 * To cause autolayout to be performed/updated, you call
 * -updateLayout.  When this method is called, the layout manager
 * first calls -internalUpdateMinimumLayout to recompute its minimum
 * layout (if any attribute of any segment or line part changed);
 * then, if the minimum layout changed, or some other thing requiring
 * the layout to be updated happened, the layout manager computes the
 * new line length taking into account forced line lengths.  It
 * computes the minimum forced line length of all the lines.  If the
 * forced length is less than the minimum length, the autolayout
 * manager sets the line length to _length, but actually uses the
 * minimum layout as the layout of views - which means some views are
 * likely going out the line! normally that simply results in clipping
 * of them.  It then calls -internalUpdateLayout to update the layout.
 * If -internalUpdateLayout returns YES, it posts an
 * GSAutoLayoutManagerChangedLayoutNotification.
 *
 * So, if the layout changed in any way, a notification is posted; you
 * need to read the new layout from the autolayout manager and apply
 * it.  If the layout did not change, no notification is posted.
 *
 * Whenever you use an autolayout manager, you should keep in mind the
 * (cool) possibility that you may want at some point to share the
 * autolayout manager with another autocontainer on the window; eg, to
 * have two autocontainers in different parts of the window that have
 * the perfect identical autolayout.  Because the autolayout manager
 * might be shared, you should always assume that you can get a
 * notification that the layout changed even if you didn't trigger the
 * layout update yourself.
 *
 * You should call this method after you have sent to the autolayout
 * manager all updated or new information about the layout you have.
 * In a typical session, you first update the layout information by
 * calling many times methods adding/removing/modifying segments/lines
 * and/or modifying line parts and/or forcing lines to be of certain
 * lengths, then finally you perform new layout by calling
 * -updateLayout.
 */
- (void) updateLayout;

/* This is a method that subclasses can use in their implementation of
 * -internalUpdateMinimumLayout.  It removes all objects from the
 * _lineParts array, and then fills it up with the right number of
 * line parts.  It also makes sure that any information set for
 * specific line parts is copied into the _lineParts array, and
 * available as the _info field of any line part.  Finally, it will
 * also iterate over all segments, and for each of them, set the
 * _linePart index.  This method is never called directly, but your
 * subclass almost certainly needs to call it at the beginning of
 * -internalUpdateMinimumLayout.
 */
- (void) internalUpdateLineParts;

/* This is a method that subclasses can use in their implementation of
 * -internalUpdateMinimumLayout.  It computes the minimum layout of
 * all segments (and stores it in _lines) from the minimum layout of
 * all line parts (read from _lineParts).
 */
- (void) internalUpdateSegmentsMinimumLayoutFromLineParts;

/* This is a method that subclasses can use in their implementation of
 * -internalUpdateLayout.  It computes the layout of all segments (and
 * stores it in _lines) from the layout of all line parts (read from
 * _lineParts).  It also computes the layout of all segment contents
 * taking into account the alignment and paddings specified for each
 * segment.
 */
- (void) internalUpdateSegmentsLayoutFromLineParts;

/* Subclasses should override this method to update the minimum
 * layout.  This method is called when the superclass has determined
 * that there is a need to update the minimum layout.  The subclass
 * should recompute the minimum layout from scratch starting from the
 * segments and from the segments (and line parts) info (the forced
 * line lengths should be ignored).  The results should be stored in
 * both the _lineParts and _lines arrays.
 *
 * A recommended implementation can take advantage of some handy
 * methods that this class provide.  It should build up the _lineParts
 * array, and compute the minimum layout there.  To build up the
 * _lineParts, you almost certainly want to use the
 * -internalCreateLinePartsArray provided here.  Once you have the
 * _lineParts array, you should work out your subclass autolayout
 * magic on the line parts and segments to compute the minimum layout
 * for the line parts, which you should store in the _lineParts (there
 * is a _minimumLayout field for each line part).  You should then
 * propagate that layout to the segments, which can be done by just
 * calling the -internalUpdateSegmentsMinimumLayoutFromLineParts which
 * will use this line part minimum layout to compute the segment
 * minimum layout and store it in the _lines array.
 *
 * This method should return YES if there was a change in the minimum
 * layout, and NO if at the end of the recomputation, the minimum
 * layout was found to be the same.  Returning NO in certain cases
 * prevents further useless computations to be done, but it is only
 * for efficiency - it's safe to always return YES.
 */
- (BOOL) internalUpdateMinimumLayout;

/* Subclasses should override this method to update the layout.  This
 * method is called when the superclass has determined that there is a
 * need to update the layout, and after the minimum layout has been
 * updated if there is a need to, and the new _length that the lines
 * must have has been computed.  This method is only called if this
 * _length is bigger than the _minimumLength.  The subclass should
 * decide how to distribute the difference between the _length and the
 * _minimumLength in each line part and segments.
 *
 * The recommended implementation is to work your subclass autolayout
 * magic on the _lineParts, and store the new layout in there.  Then
 * call the handy -internalUpdateSegmentsLayoutFromLineParts to
 * compute the layout of all segments from the layout of the line
 * parts.
 *
 * This method should return YES if there was a change in the layout,
 * and NO if at the end of the recomputation, the layout was found to
 * be the same.  Returning NO prevents the notification for changed
 * layout to be sent to clients, so it's better to return NO if we can
 * determine that no layout change was done.
 */
- (BOOL) internalUpdateLayout;


/* NB: All the GSAutoLayoutManager methods do *not* cause any
 * autolayout until you call -updateLayout.  */

/* Add a new line to the autolayout manager.  The returned id is an
 * identifier for that line used in all subsequent communications with
 * the layout manager.  The line is created with no segments inside.
 */
- (id) addLine;

/* Remove a line from the autolayout manager.  */
- (void) removeLine: (id)line;

/* Force the lenght of a line.  Normally called to inform the autolayout
 * manager of a resizing operated by outside.  Use length < 0 to remove
 * a forcing on a line.  */
- (void) forceLength: (float)length
	      ofLine: (id)line;

/* Insert a new segment in a line.  The segment is inserted at the
 * specified index; all following segments are automatically shifted
 * (the segment numbers of those segments will change too).  */
- (void) insertNewSegmentAtIndex: (int)segment
			  inLine: (id)line;

/* Remove a segment from a line.  All segments following this one will
 * be automatically be shifted (the segment number will change
 * too).  */
- (void) removeSegmentAtIndex: (int)segment
		       inLine: (id)line;

/* Return the number of segments in that line.  */
- (unsigned int) segmentCountInLine: (id)line;

/* Return the total number of line parts.  This method requires the
 * autolayout to have been done and be up-to-date; it doesn't perform
 * any layout itself.  */
- (unsigned int) linePartCount;

/* Return the number of line parts in that line.  Some lines might be
 * truncated (eg, if they are being built).  This is obtained by
 * looping on the segments in the line, and multiplying each of them
 * for its span.  This method requires the autolayout to have been
 * done and be up-to-date; it doesn't perform any layout itself.  */
- (unsigned int) linePartCountInLine: (id)line;

/* Set/read the various autolayout information for segments in a line.  */
- (void) setMinimumLength: (float)min
		alignment: (GSAutoLayoutAlignment)flag
	    bottomPadding: (float)bottomPadding
	       topPadding: (float)topPadding
		     span: (int)span
	 ofSegmentAtIndex: (int)segment
		   inLine: (id)line;

- (float) minimumLengthOfSegmentAtIndex: (int)segment
				 inLine: (id)line;

- (GSAutoLayoutAlignment) alignmentOfSegmentAtIndex: (int)segment
					     inLine: (id)line;

- (int) spanOfSegmentAtIndex: (int)segment
		      inLine: (id)line;

- (float) bottomPaddingOfSegmentAtIndex: (int)segment
				 inLine: (id)line;

- (float) topPaddingOfSegmentAtIndex: (int)segment
			      inLine: (id)line;

/* Set/read the various autolayout information for line parts.  The
 * autolayout manager automatically assumes that each line part has
 * the default values (proportion == 1.0, minimumLength == 0.0,
 * alwaysExpand == NO, neverExpands == NO) unless you explicitly set
 * different values by using the setxxx:xxx:xxx:ofLinePartAtIndex:
 * method.  To revert to the default values, use
 * -removeInformationOnLinePartAtIndex:.  Please note that you have to
 * remove this information explicitly, else it will automatically be
 * kept even if you remove all segments from the autolayout manager.
 * This is a feature - for example, if you remove all views in a
 * column in a table and then add some new views to the column, the
 * information on the column is kept unless you explicitly decide you
 * want to change or reset it.
 *
 * The minimum length is the total line part length, irrespective of
 * paddings/content of the actual segments.  It would make no sense to
 * have separate paddings/content sizes, because a line part has no
 * paddings/content.  Line parts form an invisible grid over which the
 * segments are placed.  In the simplest non-trivial example, a
 * segment could cover 2 line parts - in that case it's still clear
 * what the paddings/content of the segment are, but it's unclear what
 * the paddings/content of the line part would be.
 *
 * The default for 'alwaysExpands' and 'neverExpands' is NO, meaning
 * that the column's expand behaviour will be determined by the views
 * inside it.
 *
 * When 'alwaysExpand' flag is set to YES, the column will always
 * automatically expand when new screen size is available, even if
 * none of the views inside the column are marked as expanding.  The
 * views inside the column still keep their alignment which determines
 * how they react to the column's expansion.
 *
 * When 'neverExpand' flag is set to YES, the column will never expand
 * when new screen size is available, even if some or all of the views
 * inside the column are marked as expanding.
 */
- (void) setMinimumLength: (float)min
	    alwaysExpands: (BOOL)alwaysExpands
	     neverExpands: (BOOL)neverExpands
	       proportion: (float)proportion
	ofLinePartAtIndex: (int)linePart;

- (float) proportionOfLinePartAtIndex: (int)linePart;

- (float) minimumLengthOfLinePartAtIndex: (int)linePart;

- (BOOL) alwaysExpandsOfLinePartAtIndex: (int)linePart;

- (BOOL) neverExpandsOfLinePartAtIndex: (int)linePart;

/* Remove information stored on a line part.  */
- (void) removeInformationOnLinePartAtIndex: (int)linePart;

/* Read the result of autolayout for a line.  The clients should use
 * these methods to get the new layout when they receive the
 * GSAutoLayoutManagerChangedLayoutNotification.  */
- (float) lineLength;

/* This returns the final layout of the segment *contents*.  Raises
 * an exception if you ask for a non-existing segment.  */
- (GSAutoLayoutSegmentLayout) layoutOfSegmentAtIndex: (int)segment
					      inLine: (id)line;

/* This returns the final layout of the line parts.  It is used to
 * draw the dotted lines used when displaying visually how the
 * autolayout has been done, to help debugging autolayout issues.  It
 * could also be useful in other situations - for example if you are
 * using the autolayout manager to draw a real table with table
 * headers this will give you the size of each table header.  You
 * obviously need to perform the autolayout before using this method.
 * Also note that requesting the layout of a non-existing line part
 * will cause an exception; because the number of line parts is
 * computed anew every time during autolayout, please make sure to
 * check [autoLayoutManager linePartCount] before calling this method,
 * or be ready to catch exceptions.  */
- (GSAutoLayoutSegmentLayout) layoutOfLinePartAtIndex: (int)linePart;

/* The minimum length of a line in the minimum autolayout.  Useful for
 * implementing -minimumSizeForContent.  */
- (float) minimumLineLength;
@end

extern NSString *GSAutoLayoutManagerChangedLayoutNotification;

#endif 
