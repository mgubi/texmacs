
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
 a view (input fields, pop-up menus, ...) show this view. */
@interface TMButtonsController : NSObject {
  NSMutableArray *rowArray;     // the view of each row
  NSMutableArray *menuArray;    // the menu of each row (keeps the items)
  NSMutableArray *shownArray;   // whether each row is visible
  NSView *view;
  NSBox *line;           // the hairline below the rows
  NSView *divider;       // between the main/mode and focus/user rows
}
- (void) setMenu:(NSMenu *)menu forRow:(unsigned) idx;
- (void) setVisible:(BOOL) flag forRow:(unsigned) idx;
- (void) layout;
- (NSView*) bar;
@end
