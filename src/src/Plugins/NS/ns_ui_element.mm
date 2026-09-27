/******************************************************************************
 * MODULE     : ns_ui_element.mm
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2018  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "mac_cocoa.h"
#include "ns_ui_element.h"
#include "ns_menu.h"
#include "ns_picture.h"
#include "ns_renderer.h"
#include "ns_utilities.h"
#include "ns_simple_widget.h"

#include "analyze.hpp"
#include "converter.hpp"
#include "wencoding.hpp"
#include "gui.hpp"
#include "message.hpp"

NSColor* to_nscolor (color col);

/******************************************************************************
 * Helpers
 ******************************************************************************/


static NSImage*
to_nsimage (url u) {
  NSBitmapImageRep* rep= xpm_image (u);
  if (!rep) return nil;
  NSImage* img= [[[NSImage alloc] initWithSize: [rep size]] autorelease];
  [img addRepresentation: rep];
  return img;
}

static string
text_of (widget w) {
  // The text of a text widget, used for the tooltips of balloons
  if (is_nil (w)) return "";
  ns_widget nsw= concrete (w);
  if (nsw->type != ns_widget_rep::text_widget) return "";
  typedef quartet<string, int, color, bool> T;
  return open_box<T> (((ns_ui_element_rep*) nsw.rep)->operator blackbox ()).x1;
}

/*! A button, a check box or a popup button executing a TeXmacs command.
 The kind of the button tells how the command is called: without arguments,
 with the state of the check box, or with the selected item.
 */
@interface TMCommandButton : NSButton
{
  command_rep *cmd;
  int kind;  // 0: plain, 1: check box, 2: popup
}
- (void)setCommand:(command_rep *)_c kind:(int)_k;
- (void)doit:(id)sender;
@end

@implementation TMCommandButton
- (void)setCommand:(command_rep *)_c kind:(int)_k
{
  if (cmd) { DEC_COUNT_NULL(cmd); } cmd = _c; kind= _k;
  if (cmd) {
    INC_COUNT_NULL(cmd);
    [self setTarget:self];
    [self setAction:@selector(doit:)];
  }
}
- (void)dealloc { [self setCommand:NULL kind:0]; [super dealloc]; }
- (void)doit:(id)sender
{
  (void) sender;
  if (!cmd) return;
  command c (cmd);
  if (kind == 1) c (list_object (object ([self state] == NSControlStateValueOn)));
  else c ();
}
@end

@interface TMCommandPopUp : NSPopUpButton
{
  command_rep *cmd;
}
- (void)setCommand:(command_rep *)_c;
- (void)doit:(id)sender;
@end

@implementation TMCommandPopUp
- (void)setCommand:(command_rep *)_c
{
  if (cmd) { DEC_COUNT_NULL(cmd); } cmd = _c;
  if (cmd) {
    INC_COUNT_NULL(cmd);
    [self setTarget:self];
    [self setAction:@selector(doit:)];
  }
}
- (void)dealloc { [self setCommand:NULL]; [super dealloc]; }
- (void)doit:(id)sender
{
  (void) sender;
  if (!cmd) return;
  command c (cmd);
  c (list_object (object (from_nsstring ([self titleOfSelectedItem]))));
}
@end

static NSView*
stack_of (array<widget> a, bool vertical) {
  NSStackView* sv= [[[NSStackView alloc] init] autorelease];
  [sv setOrientation: vertical? NSUserInterfaceLayoutOrientationVertical
                              : NSUserInterfaceLayoutOrientationHorizontal];
  [sv setAlignment: vertical? NSLayoutAttributeLeading
                            : NSLayoutAttributeCenterY];
  for (int i=0; i<N(a); i++) {
    if (is_nil (a[i])) continue;
    NSView* v= concrete (a[i])->as_nsview ();
    if (v) [sv addArrangedSubview: v];
  }
  return sv;
}

static NSView*
placeholder (string what) {
  // FIXME: widgets which are not implemented yet
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_ui_element: no view for " << what << LF;
  NSTextField* t= [NSTextField labelWithString: to_nsstring ("[" * what * "]")];
  [t setTextColor: [NSColor disabledControlTextColor]];
  return t;
}


/******************************************************************************
 * Refresh widgets (see QTMRefreshWidget and QTMRefreshableWidget)
 ******************************************************************************/

widget make_menu_widget (object wid);
widget as_widget (object obj);
extern bool menu_caching;

/*! The state of a refresh widget: its content is recomputed from a menu
 (refresh_widget) or from a promise (refreshable_widget) when a refresh of
 its kind is requested with SLOT_REFRESH. */
struct ns_refresh_state {
  ns_widget_rep* parent;
  bool     refreshable;
  string   strwid;
  object   prom;
  string   kind;
  object   curobj;
  widget   cur;
  hashmap<object,widget> cache;
  ns_refresh_state (): curobj (false), cache (widget ()) {}
  bool recompute (string what);
};

bool
ns_refresh_state::recompute (string what) {
  if (what != "init" && kind != "any" && kind != what) return false;
  eval ("(lazy-initialize-force)");
  widget previous= cur;
  if (refreshable) {
    object xwid= call (prom);
    if (curobj == xwid) return false;
    if (!is_widget (xwid)) return false;
    curobj= xwid;
    cur= as_widget (xwid);
  }
  else {
    string s= "'(vertical (link " * strwid * "))";
    object xwid= call ("menu-expand", eval (s));
    if (cache->contains (xwid)) {
      if (curobj == xwid) return false;
      curobj= xwid;
      cur= cache [xwid];
    }
    else {
      curobj= xwid;
      cur= make_menu_widget (eval (s));
      if (menu_caching) cache (xwid)= cur;
    }
  }
  if (!is_nil (previous) && previous != cur) parent->remove_child (previous);
  parent->add_child (cur);
  return true;
}

@interface TMRefreshView : NSView
{
  ns_refresh_state* state;
  NSView* content;
}
- (id) initWithState: (ns_refresh_state*) st;
- (void) refresh: (NSNotification*) n;
@end

@implementation TMRefreshView
- (id) initWithState: (ns_refresh_state*) st
{
  self= [super initWithFrame: NSMakeRect (0, 0, 10, 10)];
  if (self) {
    state= st;
    content= nil;
    [self setTranslatesAutoresizingMaskIntoConstraints: NO];
    [[NSNotificationCenter defaultCenter]
      addObserver: self selector: @selector(refresh:)
             name: @"TMRefresh" object: nil];
    [self show: "init"];
  }
  return self;
}
- (void) dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver: self];
  tm_delete (state);
  [super dealloc];
}
- (void) show: (string) kind
{
  if (!state->recompute (kind) && content) return;
  if (content) [content removeFromSuperview];
  content= is_nil (state->cur)? nil: concrete (state->cur)->as_nsview ();
  if (!content) return;
  [content setTranslatesAutoresizingMaskIntoConstraints: NO];
  [self addSubview: content];
  [NSLayoutConstraint activateConstraints: @[
    [content.leadingAnchor constraintEqualToAnchor: self.leadingAnchor],
    [content.trailingAnchor constraintEqualToAnchor: self.trailingAnchor],
    [content.topAnchor constraintEqualToAnchor: self.topAnchor],
    [content.bottomAnchor constraintEqualToAnchor: self.bottomAnchor]]];
}
- (void) refresh: (NSNotification*) n
{
  string kind= from_nsstring ([[n userInfo] objectForKey: @"kind"]);
  NSView* old= content;
  [self show: kind];
  if (content != old) {
    // the window takes the size of its new contents (as in the Qt interface)
    // in the main windows, the tools are laid out again
    NSWindow* win= [self window];
    NSView* root= [win contentView];
    if (root && [[root identifier] isEqualToString: @"TMMainView"])
      [[NSNotificationCenter defaultCenter]
        postNotificationName: @"TMToolsChanged" object: root];
    else if (win && root) {
      NSSize fs= [root fittingSize];
      if (fs.width > 0 && fs.height > 0) [win setContentSize: fs];
    }
  }
}
@end

/******************************************************************************
 * Choice lists (see QTMListView)
 ******************************************************************************/

@interface TMChoiceList : NSObject <NSTableViewDataSource, NSTableViewDelegate>
{
  command_rep* cmd;
  NSArray* items;
  BOOL multiple;
  NSTableView* table;
}
- (id) initWithItems: (NSArray*) its command: (command_rep*) c
            multiple: (BOOL) m table: (NSTableView*) t;
@end

@implementation TMChoiceList
- (id) initWithItems: (NSArray*) its command: (command_rep*) c
            multiple: (BOOL) m table: (NSTableView*) t
{
  self= [super init];
  if (self) {
    items= [its retain]; cmd= c; INC_COUNT_NULL (cmd);
    multiple= m; table= t;
  }
  return self;
}
- (void) dealloc
{
  [items release]; DEC_COUNT_NULL (cmd);
  [super dealloc];
}
- (NSInteger) numberOfRowsInTableView: (NSTableView*) tv
{
  (void) tv; return [items count];
}
- (id) tableView: (NSTableView*) tv objectValueForTableColumn: (NSTableColumn*) col
             row: (NSInteger) row
{
  (void) tv; (void) col; return [items objectAtIndex: row];
}
- (void) tableViewSelectionDidChange: (NSNotification*) n
{
  (void) n;
  if (!cmd) return;
  NSIndexSet* sel= [table selectedRowIndexes];
  object l= null_object ();
  if (multiple) {
    for (NSUInteger i= [sel lastIndex]; i != NSNotFound;
         i= [sel indexLessThanIndex: i])
      l= cons (from_nsstring ([items objectAtIndex: i]), l);
  }
  else if ([sel count] > 0)
    l= object (from_nsstring ([items objectAtIndex: [sel firstIndex]]));
  else l= object ("");
  command c (cmd);
  c (list_object (l));
}
@end

static NSView*
choice_list (command cmd, array<string> vals, array<string> chosen, bool multiple) {
  NSMutableArray* its= [NSMutableArray array];
  for (int i=0; i<N(vals); i++) [its addObject: to_label (vals[i])];
  NSTableView* t= [[[NSTableView alloc] init] autorelease];
  NSTableColumn* col= [[[NSTableColumn alloc] initWithIdentifier: @"c"] autorelease];
  [col setWidth: 200];
  [t addTableColumn: col];
  [t setHeaderView: nil];
  [t setAllowsMultipleSelection: multiple];
  TMChoiceList* ds= [[TMChoiceList alloc] initWithItems: its command: cmd.rep
                                               multiple: multiple table: t];
  // NOTE: the data source lives as long as the table (released with it)
  [t setDataSource: ds];
  [t setDelegate: ds];
  NSMutableIndexSet* sel= [NSMutableIndexSet indexSet];
  for (int i=0; i<N(vals); i++)
    if (contains (vals[i], chosen)) [sel addIndex: i];
  [t reloadData];
  [t selectRowIndexes: sel byExtendingSelection: NO];
  NSScrollView* sv= [[[NSScrollView alloc] init] autorelease];
  [sv setDocumentView: t];
  [sv setHasVerticalScroller: YES];
  [sv setTranslatesAutoresizingMaskIntoConstraints: NO];
  [[sv.heightAnchor constraintGreaterThanOrEqualToConstant: 120] setActive: YES];
  [[sv.widthAnchor constraintGreaterThanOrEqualToConstant: 200] setActive: YES];
  return sv;
}

static double
length_in_points (string l) {
  // Approximate sizes of the resize widgets (FIXME: as qt_decode_length)
  if (ends (l, "px")) return as_double (l (0, N(l) - 2));
  if (ends (l, "em")) return 12.0 * as_double (l (0, N(l) - 2));
  if (ends (l, "ex")) return 6.0 * as_double (l (0, N(l) - 2));
  return 0.0;
}

/******************************************************************************
 * ns_ui_element_rep
 ******************************************************************************/

ns_ui_element_rep::ns_ui_element_rep (types _type, blackbox _load)
  : ns_widget_rep (_type), load (_load) {}

ns_ui_element_rep::~ns_ui_element_rep () {}

blackbox
ns_ui_element_rep::get_payload (ns_widget nsw, types check_type) {
  ASSERT (check_type == none || nsw->type == check_type,
          c_string ("get_payload: widget " * nsw->type_as_string() *
                    " was not of the expected type."));
  switch (nsw->type) {
    case horizontal_menu:   case vertical_menu:    case horizontal_list:
    case vertical_list:     case tile_menu:        case aligned_widget:
    case minibar_menu:      case menu_separator:   case menu_group:
    case pulldown_button:   case pullright_button: case menu_button:
    case text_widget:       case xpm_widget:       case toggle_widget:
    case enum_widget:       case choice_widget:    case filtered_choice_widget:
    case scrollable_widget: case hsplit_widget:    case vsplit_widget:
    case tabs_widget:       case icon_tabs_widget: case resize_widget:
    case refresh_widget:    case refreshable_widget: case balloon_widget:
    case glue_widget:       case tree_view_widget:
      return static_cast<ns_ui_element_rep*> (nsw.rep)->load;
    default:
      return blackbox ();
  }
}

ns_ui_element_rep::operator blackbox () {
  return load;
}

ns_ui_element_rep::operator tree () {
  return tree (TUPLE, "ns_ui_element", type_as_string ());
}

/*! A vertical menu is shown as a native popup menu (see ns_menu_rep). */
widget
ns_ui_element_rep::make_popup_widget () {
  if (type == vertical_menu)
    return tm_new<ns_menu_rep> (as_menuitem ());
  return ns_widget_rep::make_popup_widget ();
}

/******************************************************************************
 * Menu items (menus and toolbars)
 ******************************************************************************/

/*! The keyboard shortcut of a menu item (see conv_sub in the Qt interface):
 M- is command, C- control, A- option and S- shift, an upper case letter
 implies shift; the shortcuts of several keys are shown in the title. */
static NSString*
shortcut_key (string k) {
  static hashmap<string,int> keys (0);
  if (N(keys) == 0) {
    keys ("return")= NSCarriageReturnCharacter; keys ("enter")= NSEnterCharacter;
    keys ("tab")= NSTabCharacter; keys ("space")= ' ';
    keys ("backspace")= NSBackspaceCharacter; keys ("delete")= NSDeleteCharacter;
    keys ("escape")= 0x1b;
    keys ("left")= NSLeftArrowFunctionKey; keys ("right")= NSRightArrowFunctionKey;
    keys ("up")= NSUpArrowFunctionKey; keys ("down")= NSDownArrowFunctionKey;
    keys ("home")= NSHomeFunctionKey; keys ("end")= NSEndFunctionKey;
    keys ("pageup")= NSPageUpFunctionKey; keys ("pagedown")= NSPageDownFunctionKey;
    for (int i=1; i<=12; i++) keys ("F" * as_string (i))= NSF1FunctionKey + i - 1;
  }
  if (keys->contains (k)) {
    unichar c= (unichar) keys[k];
    return [NSString stringWithCharacters: &c length: 1];
  }
  if (N(k) == 1) return to_nsstring (k);
  return nil;
}

static bool
parse_shortcut (string ks, NSString*& key, NSEventModifierFlags& mask) {
  mask= 0;
  while (N(ks) > 2 && ks[1] == '-') {
    if (ks[0] == 'M') mask |= NSEventModifierFlagCommand;
    else if (ks[0] == 'C') mask |= NSEventModifierFlagControl;
    else if (ks[0] == 'A') mask |= NSEventModifierFlagOption;
    else if (ks[0] == 'S') mask |= NSEventModifierFlagShift;
    else return false;
    ks= ks (2, N(ks));
  }
  key= shortcut_key (ks);
  if (!key) return false;
  if (N(ks) == 1 && is_upcase (ks[0])) {
    mask |= NSEventModifierFlagShift;
    key= [key lowercaseString];
  }
  return true;
}

static NSString*
shortcut_text (NSString* key, NSEventModifierFlags mask) {
  NSMutableString* s= [NSMutableString string];
  if (mask & NSEventModifierFlagControl) [s appendString: @"\u2303"];
  if (mask & NSEventModifierFlagOption)  [s appendString: @"\u2325"];
  if (mask & NSEventModifierFlagShift)   [s appendString: @"\u21E7"];
  if (mask & NSEventModifierFlagCommand) [s appendString: @"\u2318"];
  [s appendString: [key uppercaseString]];
  return s;
}

static void
set_shortcut (NSMenuItem* mi, string ks) {
  if (N(ks) == 0) return;
  array<string> strokes= tokenize (ks, " ");
  NSString* key; NSEventModifierFlags mask;
  if (N(strokes) == 1 && parse_shortcut (ks, key, mask)) {
    [mi setKeyEquivalent: key];
    [mi setKeyEquivalentModifierMask: mask];
    return;
  }
  // several keys: in the title
  NSMutableArray* parts= [NSMutableArray array];
  for (int i=0; i<N(strokes); i++) {
    if (!parse_shortcut (strokes[i], key, mask)) return;
    [parts addObject: shortcut_text (key, mask)];
  }
  [mi setTitle: [NSString stringWithFormat: @"%@\t%@", [mi title],
                  [parts componentsJoinedByString: @" "]]];
}

static TMMenuItem*
new_item (NSString* title) {
  return [[[TMMenuItem alloc] initWithTitle: title action: NULL
                              keyEquivalent: @""] autorelease];
}

static TMMenuItem*
submenu_item (array<widget> a) {
  TMMenuItem* mi= new_item (@"Menu");
  NSMenu *menu= [[[NSMenu alloc] init] autorelease];
  [menu setAutoenablesItems: NO];
  for (int i=0; i<N(a); i++) {
    if (is_nil (a[i])) break;
    NSMenuItem* item= concrete (a[i])->as_menuitem ();
    if (item) [menu addItem: item];
  }
  [mi setSubmenu: menu];
  return mi;
}

TMMenuItem*
ns_ui_element_rep::as_menuitem () {
  switch (type) {
    case horizontal_menu: case vertical_menu: case horizontal_list:
    case vertical_list:   case minibar_menu:
      return submenu_item (open_box<array<widget> > (load));

    case tile_menu:
    {
      typedef pair<array<widget>, int> T;
      T x= open_box<T> (load);
      NSMutableArray *tiles= [NSMutableArray arrayWithCapacity: N(x.x1)];
      for (int i=0; i<N(x.x1); i++) {
        if (is_nil (x.x1[i])) break;
        NSMenuItem* item= concrete (x.x1[i])->as_menuitem ();
        if (item) [tiles addObject: item];
      }
      TMTileView* tv= [[[TMTileView alloc] initWithObjects: tiles
                                                      cols: x.x2] autorelease];
      TMMenuItem* mi= new_item (@"Tile");
      [mi setView: tv];
      return mi;
    }

    case menu_separator:
      return (TMMenuItem*) [NSMenuItem separatorItem];

    case menu_group:
    {
      typedef pair<string, int> T;
      T x= open_box<T> (load);
      TMMenuItem* mi= new_item (to_label (x.x1));
      NSMutableParagraphStyle *pstyle=
        [[[NSParagraphStyle defaultParagraphStyle] mutableCopy] autorelease];
      [pstyle setAlignment: NSTextAlignmentCenter];
      NSDictionary* attrs= [NSDictionary dictionaryWithObjectsAndKeys:
                            pstyle, NSParagraphStyleAttributeName, nil];
      [mi setAttributedTitle: [[[NSAttributedString alloc]
                                 initWithString: [mi title]
                                     attributes: attrs] autorelease]];
      [mi setEnabled: NO];
      return mi;
    }

    case pulldown_button: case pullright_button:
    {
      typedef pair<widget, promise<widget> > T;
      T x= open_box<T> (load);
      TMMenuItem* mi= concrete (x.x1)->as_menuitem ();
      if (!mi) mi= new_item (@"");
      TMLazyMenu *lm= [[[TMLazyMenu alloc] init] autorelease];
      [lm setAutoenablesItems: NO];
      [lm setPromise: x.x2.rep];
      [mi setSubmenu: lm];
      return mi;
    }

    case menu_button:
    {
      typedef quintuple<widget, command, string, string, int> T;
      T x= open_box<T> (load);
      bool ok= (x.x5 & WIDGET_STYLE_INERT) == 0;
      TMMenuItem* mi= concrete (x.x1)->as_menuitem ();
      if (!mi) mi= new_item (@"");
      [mi setCommand: x.x2.rep];
      [mi setEnabled: (ok? YES: NO)];
      // the keyboard shortcut is shown, but the keys go to the editor (the
      // lazy menus have no key equivalents, see TMLazyMenu)
      set_shortcut (mi, x.x4);
      // NOTE: as in the Qt interface, the prefixes (v, * and o) and the
      // pressed buttons are shown by a check mark
      bool check= (x.x3 != "") || (x.x5 & WIDGET_STYLE_PRESSED);
      [mi setState: (check? NSControlStateValueOn: NSControlStateValueOff)];
      return mi;
    }

    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x= open_box<T> (load);
      TMMenuItem* mi= concrete (x.x1)->as_menuitem ();
      if (mi) [mi setToolTip: to_label (text_of (x.x2))];
      return mi;
    }

    case text_widget:
    {
      typedef quartet<string, int, color, bool> T;
      T x= open_box<T> (load);
      return new_item (to_label (x.x1));
    }

    case xpm_widget:
    {
      url u= open_box<url> (load);
      NSImage* img= to_nsimage (u);
      TMMenuItem* mi= new_item (@"");
      [mi setRepresentedObject: img];
      [mi setImage: img];
      return mi;
    }

    default:
      // The other widgets (toggles, enums, ...) are shown by their view
      return ns_widget_rep::as_menuitem ();
  }
}

/******************************************************************************
 * Views (dialogs and other windows)
 ******************************************************************************/

NSView*
ns_ui_element_rep::as_nsview () {
  switch (type) {
    case horizontal_menu: case horizontal_list: case minibar_menu:
      return stack_of (open_box<array<widget> > (load), false);

    case vertical_menu: case vertical_list:
      return stack_of (open_box<array<widget> > (load), true);

    case aligned_widget:
    {
      typedef triple<array<widget>, array<widget>, coord4> T;
      T x= open_box<T> (load);
      NSGridView* g= [[[NSGridView alloc] init] autorelease];
      for (int i=0; i < min (N(x.x1), N(x.x2)); i++) {
        NSView* l= is_nil (x.x1[i])? nil: concrete (x.x1[i])->as_nsview ();
        NSView* r= is_nil (x.x2[i])? nil: concrete (x.x2[i])->as_nsview ();
        if (!l) l= [[[NSView alloc] init] autorelease];
        if (!r) r= [[[NSView alloc] init] autorelease];
        [g addRowWithViews: [NSArray arrayWithObjects: l, r, nil]];
      }
      return g;
    }

    case tile_menu:
    {
      typedef pair<array<widget>, int> T;
      T x= open_box<T> (load);
      int cols= max (x.x2, 1);
      NSGridView* g= [[[NSGridView alloc] init] autorelease];
      [g setRowSpacing: 2];
      [g setColumnSpacing: 2];
      NSMutableArray* row= [NSMutableArray array];
      for (int i=0; i<N(x.x1); i++) {
        if (is_nil (x.x1[i])) break;
        NSView* v= concrete (x.x1[i])->as_nsview ();
        [row addObject: v? v: [[[NSView alloc] init] autorelease]];
        if ((int) [row count] == cols) {
          [g addRowWithViews: row];
          row= [NSMutableArray array];
        }
      }
      if ([row count] > 0) {
        while ((int) [row count] < cols)
          [row addObject: [NSGridCell emptyContentView]];
        [g addRowWithViews: row];
      }
      return g;
    }

    case menu_separator:
    {
      NSBox* b= [[[NSBox alloc] init] autorelease];
      [b setBoxType: NSBoxSeparator];
      return b;
    }

    case menu_group:
    {
      typedef pair<string, int> T;
      T x= open_box<T> (load);
      NSTextField* t= [NSTextField labelWithString: to_label (x.x1)];
      [t setTextColor: [NSColor secondaryLabelColor]];
      return t;
    }

    case text_widget:
    {
      typedef quartet<string, int, color, bool> T;
      T x= open_box<T> (load);
      NSTextField* t= [NSTextField labelWithString: to_label (x.x1)];
      if ((x.x2 & WIDGET_STYLE_INERT) != 0)
        [t setTextColor: [NSColor disabledControlTextColor]];
      return t;
    }

    case xpm_widget:
      return [NSImageView imageViewWithImage: to_nsimage (open_box<url> (load))];

    case menu_button:
    {
      typedef quintuple<widget, command, string, string, int> T;
      T x= open_box<T> (load);
      TMCommandButton* b= [[[TMCommandButton alloc] init] autorelease];
      [b setBezelStyle: NSBezelStyleRounded];
      ns_widget w= concrete (x.x1);
      if (w->type == text_widget) {
        typedef quartet<string, int, color, bool> T2;
        [b setTitle: to_label (open_box<T2> (get_payload (w)).x1)];
      }
      else {
        // icons and colored glue (color palettes) are shown as images
        [b setTitle: @""];
        NSView* cv= w->as_nsview ();
        if ([cv isKindOfClass: [NSImageView class]])
          [b setImage: [(NSImageView*) cv image]];
        [b setBezelStyle: NSBezelStyleSmallSquare];
      }
      [b setCommand: x.x2.rep kind: 0];
      [b setEnabled: (x.x5 & WIDGET_STYLE_INERT) == 0];
      return b;
    }

    case toggle_widget:
    {
      typedef triple<command, bool, int> T;
      T x= open_box<T> (load);
      TMCommandButton* b= [[[TMCommandButton alloc] init] autorelease];
      [b setButtonType: NSButtonTypeSwitch];
      [b setTitle: @""];
      [b setState: x.x2? NSControlStateValueOn: NSControlStateValueOff];
      [b setCommand: x.x1.rep kind: 1];
      [b setEnabled: (x.x3 & WIDGET_STYLE_INERT) == 0];
      return b;
    }

    case enum_widget:
    {
      typedef quintuple<command, array<string>, string, int, string> T;
      T x= open_box<T> (load);
      TMCommandPopUp* p= [[[TMCommandPopUp alloc] init] autorelease];
      for (int i=0; i<N(x.x2); i++)
        if (x.x2[i] != "") [p addItemWithTitle: to_label (x.x2[i])];
      [p selectItemWithTitle: to_label (x.x3)];
      [p setCommand: x.x1.rep];
      [p setEnabled: (x.x4 & WIDGET_STYLE_INERT) == 0];
      return p;
    }

    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x= open_box<T> (load);
      NSView* v= concrete (x.x1)->as_nsview ();
      if (v) [v setToolTip: to_label (text_of (x.x2))];
      return v;
    }

    case scrollable_widget:
    {
      typedef pair<widget, int> T;
      T x= open_box<T> (load);
      NSScrollView* sv= [[[NSScrollView alloc] init] autorelease];
      [sv setHasVerticalScroller: YES];
      [sv setDocumentView: concrete (x.x1)->as_nsview ()];
      return sv;
    }

    case resize_widget:
    {
      typedef triple<string, string, string> T1;
      typedef quartet<widget, int, T1, T1> T;
      T x= open_box<T> (load);
      NSView* v= concrete (x.x1)->as_nsview ();
      if (!v) return v;
      // FIXME: only the minimal sizes (in px, em and ex) are applied
      double w= length_in_points (x.x3.x1), h= length_in_points (x.x4.x1);
      if (w > 0 || h > 0) [v setTranslatesAutoresizingMaskIntoConstraints: NO];
      if (w > 0) [[v.widthAnchor constraintGreaterThanOrEqualToConstant: w] setActive: YES];
      if (h > 0) [[v.heightAnchor constraintGreaterThanOrEqualToConstant: h] setActive: YES];
      return v;
    }

    case glue_widget:
    {
      typedef quartet<bool, bool, SI, SI> T;
      T x= open_box<T> (load);
      NSView* v= [[[NSView alloc] init] autorelease];
      [v setTranslatesAutoresizingMaskIntoConstraints: NO];
      if (!x.x1) [[v.widthAnchor constraintEqualToConstant: x.x3] setActive: YES];
      if (!x.x2) [[v.heightAnchor constraintEqualToConstant: x.x4] setActive: YES];
      return v;
    }

    case hsplit_widget: case vsplit_widget:
    {
      typedef pair<widget, widget> T;
      T x= open_box<T> (load);
      NSSplitView* sv= [[[NSSplitView alloc] init] autorelease];
      [sv setVertical: type == hsplit_widget];
      NSView* v1= concrete (x.x1)->as_nsview ();
      NSView* v2= concrete (x.x2)->as_nsview ();
      if (v1) [sv addArrangedSubview: v1];
      if (v2) [sv addArrangedSubview: v2];
      return sv;
    }

    case refresh_widget: case refreshable_widget:
    {
      ns_refresh_state* st= tm_new<ns_refresh_state> ();
      st->parent= this;
      st->refreshable= (type == refreshable_widget);
      if (st->refreshable) {
        typedef pair<object, string> T;
        T x= open_box<T> (load);
        st->prom= x.x1; st->kind= x.x2;
      }
      else {
        typedef pair<string, string> T;
        T x= open_box<T> (load);
        st->strwid= x.x1; st->kind= x.x2;
      }
      return [[[TMRefreshView alloc] initWithState: st] autorelease];
    }

    case tabs_widget: case icon_tabs_widget:
    {
      // FIXME: the icons of icon_tabs_widget
      array<widget> tabs, bodies;
      if (type == tabs_widget) {
        typedef pair<array<widget>, array<widget> > T;
        T x= open_box<T> (load);
        tabs= x.x1; bodies= x.x2;
      }
      else {
        typedef triple<array<url>, array<widget>, array<widget> > T;
        T x= open_box<T> (load);
        tabs= x.x2; bodies= x.x3;
      }
      NSTabView* tv= [[[NSTabView alloc] init] autorelease];
      for (int i=0; i < min (N(tabs), N(bodies)); i++) {
        NSTabViewItem* it= [[[NSTabViewItem alloc] init] autorelease];
        [it setLabel: to_label (text_of (tabs[i]))];
        NSView* body= is_nil (bodies[i])? nil: concrete (bodies[i])->as_nsview ();
        if (body) {
          NSView* holder= [[[NSView alloc] init] autorelease];
          [body setTranslatesAutoresizingMaskIntoConstraints: NO];
          [holder addSubview: body];
          [NSLayoutConstraint activateConstraints: @[
            [body.leadingAnchor constraintEqualToAnchor: holder.leadingAnchor constant: 8],
            [body.trailingAnchor constraintEqualToAnchor: holder.trailingAnchor constant: -8],
            [body.topAnchor constraintEqualToAnchor: holder.topAnchor constant: 8],
            [body.bottomAnchor constraintEqualToAnchor: holder.bottomAnchor constant: -8]]];
          [it setView: holder];
        }
        [tv addTabViewItem: it];
      }
      return tv;
    }

    case choice_widget:
    {
      typedef quintuple<command, array<string>, array<string>, bool, int> T;
      T x= open_box<T> (load);
      return choice_list (x.x1, x.x2, x.x3, x.x4);
    }

    case filtered_choice_widget:
    {
      // FIXME: the filter field
      typedef quartet<command, array<string>, string, string> T;
      T x= open_box<T> (load);
      array<string> chosen;
      chosen << x.x3;
      return choice_list (x.x1, x.x2, chosen, false);
    }

    default:
      // FIXME: tree views
      return placeholder (type_as_string ());
  }
}

/******************************************************************************
 * Glue widgets with a colored background (color menus)
 ******************************************************************************/

NSBitmapImageRep*
ns_glue_widget_rep::render () {
  NSSize s= NSMakeSize (max (w / PIXEL, 1), max (h / PIXEL, 1));
  NSBitmapImageRep *im=
    [[[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                             pixelsWide: s.width
                                             pixelsHigh: s.height
                                          bitsPerSample: 8
                                        samplesPerPixel: 4
                                               hasAlpha: YES
                                               isPlanar: NO
                                         colorSpaceName: NSDeviceRGBColorSpace
                                            bytesPerRow: 0
                                           bitsPerPixel: 0] autorelease];
  NSGraphicsContext* gc=
    [NSGraphicsContext graphicsContextWithBitmapImageRep: im];
  if (gc && is_atomic (col) && col != "") {
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext setCurrentContext: gc];
    [to_nscolor (named_color (col->label)) setFill];
    NSRectFill (NSMakeRect (0, 0, s.width, s.height));
    [NSGraphicsContext restoreGraphicsState];
  }
  // FIXME: patterns (non atomic colors)
  return im;
}

TMMenuItem*
ns_glue_widget_rep::as_menuitem () {
  TMMenuItem* mi= [[[TMMenuItem alloc] initWithTitle: to_nsstring (as_string (col))
                                              action: NULL
                                       keyEquivalent: @""] autorelease];
  NSBitmapImageRep* rep= render ();
  NSImage* img= [[[NSImage alloc] initWithSize: [rep size]] autorelease];
  [img addRepresentation: rep];
  [mi setImage: img];
  [mi setEnabled: NO];
  return mi;
}

NSView*
ns_glue_widget_rep::as_nsview () {
  NSBitmapImageRep* rep= render ();
  NSImage* img= [[[NSImage alloc] initWithSize: [rep size]] autorelease];
  [img addRepresentation: rep];
  return [NSImageView imageViewWithImage: img];
}
