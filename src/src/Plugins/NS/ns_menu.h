/******************************************************************************
* MODULE     : ns_menu.h
* DESCRIPTION: Menus for the NS port
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NS_MENU_H
#define NS_MENU_H

#include "ns_widget.h"
#include "promise.hpp"

#ifdef MAC_COCOA_H

class ns_simple_widget_rep;

/*! A menu item executing a TeXmacs command.
 The item may also be drawn by a simple widget (see tm_button.cpp).
 */
@interface TMMenuItem : NSMenuItem
{
  command_rep *cmd;
  ns_simple_widget_rep* wid;
}
- (void)setCommand:(command_rep *)_c;
- (void)setWidget:(ns_simple_widget_rep *)_w;
- (void)doit;
@end

/*! A menu whose items are computed when it is shown for the first time. */
@interface TMLazyMenu : NSMenu <NSMenuDelegate>
{
  promise_rep<widget> *pm;
  BOOL forced;
}
- (void)setPromise:(promise_rep<widget> *)p;
@end

/*! A menu rendered as a table of icons (tile_menu). */
@interface TMTileView : NSMatrix
{
  int cols;
}
- (id) initWithObjects:(NSArray*)objs cols:(int)_cols;
- (void) click:(TMTileView*)tile;
@end

#endif

/*! A popup menu (contextual menus of the editor).
 NS counterpart of qt_menu_rep.
 */
class ns_menu_rep: public ns_widget_rep {
  NSMenuItem* item;
  coord2 position;  //!< in screen coordinates, as in the Qt interface
public:
  ns_menu_rep (NSMenuItem* _item);
  ~ns_menu_rep ();
  virtual void send (slot s, blackbox val);
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);
  virtual TMMenuItem* as_menuitem ();
};

NSMenu* to_nsmenu (widget w);
NSMenuItem* to_nsmenuitem (widget w);

#endif // defined NS_MENU_H
