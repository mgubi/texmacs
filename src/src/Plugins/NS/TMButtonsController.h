
/******************************************************************************
* MODULE     : TMButtonsController.h
* DESCRIPTION: Controller for the widget bar
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#import <Cocoa/Cocoa.h>

/*! The rows of icons above the canvas (main, mode, focus and user icons).
 Each row is made from the items of a menu: the buttons between separators
 are grouped in segmented controls, the texts are labels, and the items with
 a view (input fields, pop-up menus, ...) show this view. With the
 preference "icon bars" set to the left or the right (setSide:, setRight:),
 the main and mode rows are columns at that side of the document instead
 (sideBar), as in the Vue interface: their items go from top to bottom,
 their separators are horizontal and their menus open away from the edge of
 the window. */
@interface TMButtonsController : NSObject {
  NSMutableArray *rowArray;     // the view of each row
  NSMutableArray *menuArray;    // the menu of each row (keeps the items)
  NSMutableArray *shownArray;   // whether each row is visible
  NSView *view;
  NSBox *line;           // the hairline below the rows
  NSView *divider;       // between the main/mode and focus/user rows
  BOOL side;             // the main and mode rows are columns at the left
  BOOL right;            // ... or at the right (mirrored: main outside)
  NSView *sidePane;      // the columns (scrolled) and a hairline beside them
  NSScrollView *sideScroll;
  NSView *sideView;      // the columns, in the scroll view
  NSBox *sideLine;
@public
  BOOL upward;           // the menus open above the buttons (the footer)
  id contextTarget;      // not retained: told of a right click (or of a
  SEL contextAction;     // Control or Option click) on a button, with its
                         // menu item and the event
}
- (void) setMenu:(NSMenu *)menu forRow:(unsigned) idx;
- (void) setVisible:(BOOL) flag forRow:(unsigned) idx;
- (void) layout;
- (NSView*) bar;
/*! The main and mode rows as columns at the left (or above the document) */
- (void) setSide:(BOOL) flag;
/*! The columns at the right of the document rather than at its left */
- (void) setRight:(BOOL) flag;
/*! The columns: their view, its width (0 when there is none), and their
 layout once the view has its frame */
- (NSView*) sideBar;
- (CGFloat) sideWidth:(CGFloat) height;
- (void) layoutSide;
- (void) printSide;
/*! A row of buttons made from a menu, for a bar of another view (the
 interactive footer): not placed in the bar of the controller, which keeps
 the menu while the row is in use */
- (NSView*) rowForMenu:(NSMenu*) menu;
@end
