/******************************************************************************
 * MODULE     : ns_ui_element.mm
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2018  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "MacOS/mac_cocoa.h"
#import <objc/runtime.h>
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
  // NOTE: as in the layouts of Qt, the views without a natural size (lists,
  // scroll views, containers) take the extra space
  [sv setDistribution: NSStackViewDistributionFill];
  // NOTE: as in the Qt interface, no spacing (TeXmacs puts glues)
  [sv setSpacing: 0];
  for (int i=0; i<N(a); i++) {
    if (is_nil (a[i])) continue;
    NSView* v= concrete (a[i])->as_nsview ();
    if (!v) continue;
    [sv addArrangedSubview: v];
    // as in the box layouts of Qt, the containers stretch across the list
    // (the controls keep their size)
    // (the controls and the grids keep their size, as in Qt)
    if ([v isKindOfClass: [NSGridView class]]) continue;
    bool stretch= ![v isKindOfClass: [NSControl class]];
    if (!stretch) continue;
    NSLayoutConstraint* c= vertical
      ? [v.widthAnchor constraintEqualToAnchor: sv.widthAnchor]
      : [v.heightAnchor constraintEqualToAnchor: sv.heightAnchor];
    [c setPriority: NSLayoutPriorityDefaultHigh - 10];
    [c setActive: YES];
    // across the list, the containers stretch; along it, only the lists
    // and scroll views take the extra space (the grids keep their rows)
    [v setContentHuggingPriority: NSLayoutPriorityDefaultLow - 1
                  forOrientation: vertical? NSLayoutConstraintOrientationHorizontal
                                          : NSLayoutConstraintOrientationVertical];
    if (![v isKindOfClass: [NSGridView class]])
      [v setContentHuggingPriority: NSLayoutPriorityDefaultLow - 1
                    forOrientation: vertical? NSLayoutConstraintOrientationVertical
                                            : NSLayoutConstraintOrientationHorizontal];
  }
  // the extensible glues along the list share the extra space equally
  NSString* along= vertical? @"V": @"H";
  NSView* first= nil;
  for (NSView* v in [sv arrangedSubviews]) {
    NSString* id= [v identifier];
    if (!id || ![id hasPrefix: @"TMGlue"] ||
        [id rangeOfString: along].location == NSNotFound) continue;
    if (!first) { first= v; continue; }
    NSLayoutConstraint* c= vertical
      ? [v.heightAnchor constraintEqualToAnchor: first.heightAnchor]
      : [v.widthAnchor constraintEqualToAnchor: first.widthAnchor];
    [c setPriority: NSLayoutPriorityDefaultHigh];
    [c setActive: YES];
  }
  return sv;
}

@interface TMFlippedDocView : NSView
@end

@implementation TMFlippedDocView
- (BOOL) isFlipped { return YES; }
@end

/*! When a tab is chosen, the dialog takes the size of the new page (in the
 main windows, the tools are laid out again). */
@interface TMTabHelper : NSObject <NSTabViewDelegate>
@end

@implementation TMTabHelper
- (void) tabView: (NSTabView*) tv didSelectTabViewItem: (NSTabViewItem*) it
{
  (void) it;
  NSWindow* win= [tv window];
  NSView* root= [win contentView];
  if (!win || !root) return;
  if ([[root identifier] isEqualToString: @"TMMainView"]) {
    [[NSNotificationCenter defaultCenter]
      postNotificationName: @"TMToolsChanged" object: root];
    return;
  }
  [root layoutSubtreeIfNeeded];
  NSSize fs= [root fittingSize];
  if (fs.width <= 0 || fs.height <= 0) return;
  // the top left corner stays in place
  NSRect f= [win frame];
  NSRect nf= [win frameRectForContentRect: NSMakeRect (0, 0, fs.width, fs.height)];
  nf.origin.x= f.origin.x;
  nf.origin.y= NSMaxY (f) - nf.size.height;
  [win setFrame: nf display: YES animate: [win isVisible]];
}
@end

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

@interface TMChoiceList : NSObject <NSTableViewDataSource, NSTableViewDelegate,
                                    NSSearchFieldDelegate>
{
  command_rep* cmd;
  NSArray* all;      // all the items
  NSArray* items;    // the items which pass the filter
  BOOL multiple;
  BOOL filtering;
  NSTableView* table;
}
- (id) initWithItems: (NSArray*) its command: (command_rep*) c
            multiple: (BOOL) m table: (NSTableView*) t;
- (void) setFilter: (NSString*) f;
@end

@implementation TMChoiceList
- (id) initWithItems: (NSArray*) its command: (command_rep*) c
            multiple: (BOOL) m table: (NSTableView*) t
{
  self= [super init];
  if (self) {
    all= [its retain]; items= [its retain]; cmd= c; INC_COUNT_NULL (cmd);
    multiple= m; table= t;
  }
  return self;
}
- (void) dealloc
{
  [all release]; [items release]; DEC_COUNT_NULL (cmd);
  [super dealloc];
}
- (void) setFilter: (NSString*) f
{
  // As QTMListView::setFilterRegularExpression (case insensitive)
  NSMutableArray* a= [NSMutableArray array];
  NSRegularExpression* re= nil;
  if ([f length] > 0)
    re= [NSRegularExpression regularExpressionWithPattern: f
          options: NSRegularExpressionCaseInsensitive error: nil];
  for (NSString* it in all) {
    if ([f length] == 0) [a addObject: it];
    else if (re) {
      if ([re firstMatchInString: it options: 0
                           range: NSMakeRange (0, [it length])])
        [a addObject: it];
    }
    else if ([it rangeOfString: f options: NSCaseInsensitiveSearch].location
             != NSNotFound)
      [a addObject: it];
  }
  // the selected values remain selected (without calling the command)
  NSMutableSet* chosen= [NSMutableSet set];
  NSIndexSet* sel= [table selectedRowIndexes];
  for (NSUInteger i= [sel firstIndex]; i != NSNotFound;
       i= [sel indexGreaterThanIndex: i])
    if (i < [items count]) [chosen addObject: [items objectAtIndex: i]];
  [items release];
  items= [a retain];
  filtering= YES;
  [table reloadData];
  NSMutableIndexSet* nsel= [NSMutableIndexSet indexSet];
  for (NSUInteger i=0; i<[items count]; i++)
    if ([chosen containsObject: [items objectAtIndex: i]]) [nsel addIndex: i];
  [table selectRowIndexes: nsel byExtendingSelection: NO];
  filtering= NO;
}
- (void) controlTextDidChange: (NSNotification*) n
{
  [self setFilter: [[n object] stringValue]];
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
  if (!cmd || filtering) return;
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
choice_list (command cmd, array<string> vals, array<string> chosen, bool multiple,
             string filter= "", bool filtered= false) {
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
  if (!filtered) return sv;
  // a filter above the list (see the Qt interface)
  NSSearchField* f= [[[NSSearchField alloc] init] autorelease];
  [f setStringValue: to_label (filter)];
  [f setDelegate: ds];
  [ds setFilter: [f stringValue]];
  NSStackView* st= [NSStackView stackViewWithViews:
                     [NSArray arrayWithObjects: f, sv, nil]];
  [st setOrientation: NSUserInterfaceLayoutOrientationVertical];
  [st setAlignment: NSLayoutAttributeLeading];
  [st setSpacing: 2];
  [[f.widthAnchor constraintEqualToAnchor: sv.widthAnchor] setActive: YES];
  return st;
}

/******************************************************************************
 * Tree views (see QTMTreeView)
 ******************************************************************************/

@interface TMTreeNode : NSObject
{
@public
  tree t;
  NSMutableArray* kids;
}
@end

@implementation TMTreeNode
- (void) dealloc { [kids release]; [super dealloc]; }
- (NSString*) label
{
  if (is_atomic (t)) return to_label (t->label);
  return to_label (as_string (L(t)));
}
- (NSArray*) children
{
  if (!kids) {
    kids= [[NSMutableArray alloc] init];
    if (is_compound (t))
      for (int i=0; i<N(t); i++) {
        TMTreeNode* n= [[[TMTreeNode alloc] init] autorelease];
        n->t= t[i];
        [kids addObject: n];
      }
  }
  return kids;
}
@end

@interface TMTreeList : NSObject <NSOutlineViewDataSource, NSOutlineViewDelegate>
{
@public
  command_rep* cmd;
  TMTreeNode* root;
  NSOutlineView* view;
}
@end

@implementation TMTreeList
- (void) dealloc { DEC_COUNT_NULL (cmd); [root release]; [super dealloc]; }
- (NSInteger) outlineView: (NSOutlineView*) ov numberOfChildrenOfItem: (id) item
{
  (void) ov;
  return [[(item? item: root) children] count];
}
- (id) outlineView: (NSOutlineView*) ov child: (NSInteger) i ofItem: (id) item
{
  (void) ov;
  return [[(item? item: root) children] objectAtIndex: i];
}
- (BOOL) outlineView: (NSOutlineView*) ov isItemExpandable: (id) item
{
  (void) ov;
  return [[item children] count] > 0;
}
- (id) outlineView: (NSOutlineView*) ov objectValueForTableColumn: (NSTableColumn*) c
            byItem: (id) item
{
  (void) ov; (void) c;
  return [item label];
}
- (void) outlineViewSelectionDidChange: (NSNotification*) n
{
  // the command gets the subtree (there are no roles yet) and -1, as in Qt
  (void) n;
  id item= [view itemAtRow: [view selectedRow]];
  if (!cmd || !item) return;
  command c (cmd);
  c (list_object (object (((TMTreeNode*) item)->t), object (-1)));
}
@end

static NSView*
tree_view (command cmd, tree data) {
  NSOutlineView* ov= [[[NSOutlineView alloc] init] autorelease];
  NSTableColumn* col= [[[NSTableColumn alloc] initWithIdentifier: @"t"] autorelease];
  [col setWidth: 250];
  [ov addTableColumn: col];
  [ov setOutlineTableColumn: col];
  [ov setHeaderView: nil];
  TMTreeList* ds= [[TMTreeList alloc] init];
  ds->cmd= cmd.rep; INC_COUNT_NULL (ds->cmd);
  ds->root= [[TMTreeNode alloc] init];
  ds->root->t= data;
  ds->view= ov;
  // NOTE: the data source lives as long as the view
  objc_setAssociatedObject (ov, "TMTreeList", ds, OBJC_ASSOCIATION_RETAIN);
  [ds release];
  [ov setDataSource: ds];
  [ov setDelegate: ds];
  [ov reloadData];
  NSScrollView* sv= [[[NSScrollView alloc] init] autorelease];
  [sv setDocumentView: ov];
  [sv setHasVerticalScroller: YES];
  [sv setTranslatesAutoresizingMaskIntoConstraints: NO];
  [[sv.heightAnchor constraintGreaterThanOrEqualToConstant: 150] setActive: YES];
  [[sv.widthAnchor constraintGreaterThanOrEqualToConstant: 250] setActive: YES];
  return sv;
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
  // NOTE: native key equivalents, as usual on macOS (the Qt interface puts
  // them in the title with the keyboard layouts other than the US one)
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
  // in the title, as in the Qt interface
  [mi setTitle: [NSString stringWithFormat: @"%@ \u250A %@", [mi title],
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
      // As in the Qt interface: the left column is aligned to the right,
      // the right one to the left, with spacings of 6 points plus the
      // separations of TeXmacs
      NSGridView* g= [[[NSGridView alloc] init] autorelease];
      for (int i=0; i < min (N(x.x1), N(x.x2)); i++) {
        NSView* l= is_nil (x.x1[i])? nil: concrete (x.x1[i])->as_nsview ();
        NSView* r= is_nil (x.x2[i])? nil: concrete (x.x2[i])->as_nsview ();
        if (!l) l= [NSGridCell emptyContentView];
        if (!r) r= [NSGridCell emptyContentView];
        [g addRowWithViews: [NSArray arrayWithObjects: l, r, nil]];
      }
      [g setColumnSpacing: 6 + x.x3.x1 / PIXEL];
      [g setRowSpacing: 6 + x.x3.x2 / PIXEL];
      [g setRowAlignment: NSGridRowAlignmentNone];
      [g setYPlacement: NSGridCellPlacementCenter];
      if ([g numberOfColumns] >= 2) {
        [[g columnAtIndex: 0] setXPlacement: NSGridCellPlacementTrailing];
        [[g columnAtIndex: 1] setXPlacement: NSGridCellPlacementLeading];
      }
      // NOTE: the grid keeps its natural height in a container which takes
      // the extra space (NSGridView would give it to its first row)
      NSView* box= [[[NSView alloc] init] autorelease];
      [box setTranslatesAutoresizingMaskIntoConstraints: NO];
      [g setTranslatesAutoresizingMaskIntoConstraints: NO];
      [box addSubview: g];
      NSLayoutConstraint* hh= [g.heightAnchor constraintEqualToConstant: 0];
      [hh setPriority: NSLayoutPriorityDefaultLow - 20];
      [NSLayoutConstraint activateConstraints: @[
        [g.leadingAnchor constraintEqualToAnchor: box.leadingAnchor],
        [g.trailingAnchor constraintEqualToAnchor: box.trailingAnchor],
        [g.topAnchor constraintEqualToAnchor: box.topAnchor],
        [g.bottomAnchor constraintLessThanOrEqualToAnchor: box.bottomAnchor], hh]];
      return box;
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
        // flat, with a border under the mouse (as the tool buttons of Qt)
        [b setBezelStyle: NSBezelStyleAccessoryBarAction];
        [b setShowsBorderOnlyWhileMouseInside: YES];
        [b setImagePosition: NSImageOnly];
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
      if (x.x4 & WIDGET_STYLE_MINI) {
        [p setControlSize: NSControlSizeSmall];
        [p setFont: [NSFont systemFontOfSize:
                      [NSFont systemFontSizeForControlSize: NSControlSizeSmall]]];
      }
      // the width given by TeXmacs (see QTMComboBox::addItemsAndResize)
      [p sizeToFit];
      if (x.x5 != "") {
        NSSize sz= ns_decode_length (x.x5, "", [p fittingSize]);
        [p setTranslatesAutoresizingMaskIntoConstraints: NO];
        NSLayoutConstraint* c= [p.widthAnchor constraintEqualToConstant:
                                  max (sz.width, [p fittingSize].width)];
        [c setPriority: NSLayoutPriorityDefaultHigh - 5];
        [c setActive: YES];
      }
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
      // NOTE: the lists are already scrollable; the other contents follow
      // the width of the scroll view (as QScrollArea::setWidgetResizable)
      NSView* v= concrete (x.x1)->as_nsview ();
      NSView* inner= v;
      while ([inner isKindOfClass: [NSStackView class]] &&
             [[(NSStackView*) inner arrangedSubviews] count] == 1)
        inner= [[(NSStackView*) inner arrangedSubviews] firstObject];
      if (!v || [inner isKindOfClass: [NSScrollView class]]) return v;
      NSScrollView* sv= [[[NSScrollView alloc] init] autorelease];
      [sv setHasVerticalScroller: YES];
      [sv setDrawsBackground: NO];
      NSView* doc= [[[TMFlippedDocView alloc] init] autorelease];
      [doc setTranslatesAutoresizingMaskIntoConstraints: NO];
      [v setTranslatesAutoresizingMaskIntoConstraints: NO];
      [doc addSubview: v];
      [sv setDocumentView: doc];
      NSClipView* clip= [sv contentView];
      [NSLayoutConstraint activateConstraints: @[
        [v.leadingAnchor constraintEqualToAnchor: doc.leadingAnchor],
        [v.trailingAnchor constraintEqualToAnchor: doc.trailingAnchor],
        [v.topAnchor constraintEqualToAnchor: doc.topAnchor],
        [v.bottomAnchor constraintEqualToAnchor: doc.bottomAnchor],
        [doc.leadingAnchor constraintEqualToAnchor: clip.leadingAnchor],
        [doc.trailingAnchor constraintEqualToAnchor: clip.trailingAnchor],
        [doc.topAnchor constraintEqualToAnchor: clip.topAnchor]]];
      // the contents fill at least the height of the scroll view
      NSLayoutConstraint* fill= [doc.heightAnchor constraintEqualToAnchor: clip.heightAnchor];
      [fill setPriority: NSLayoutPriorityDefaultLow];
      [fill setActive: YES];
      [[doc.heightAnchor constraintGreaterThanOrEqualToAnchor: clip.heightAnchor] setActive: YES];
      return sv;
    }

    case resize_widget:
    {
      // As in the Qt interface: the minimal, default and maximal sizes
      typedef triple<string, string, string> T1;
      typedef quartet<widget, int, T1, T1> T;
      T x= open_box<T> (load);
      NSView* v= concrete (x.x1)->as_nsview ();
      if (!v) return v;
      NSSize ref= [v fittingSize];
      if (ref.width < 1) ref.width= 100;
      if (ref.height < 1) ref.height= 22;
      NSSize mins= ns_decode_length (x.x3.x1, x.x4.x1, ref);
      NSSize defs= ns_decode_length (x.x3.x2, x.x4.x2, ref);
      NSSize maxs= ns_decode_length (x.x3.x3, x.x4.x3, ref);
      [v setTranslatesAutoresizingMaskIntoConstraints: NO];
      NSMutableArray* cs= [NSMutableArray array];
      if (NSEqualSizes (mins, defs) && NSEqualSizes (defs, maxs)) {
        [cs addObject: [v.widthAnchor constraintEqualToConstant: defs.width]];
        [cs addObject: [v.heightAnchor constraintEqualToConstant: defs.height]];
      }
      else {
        [cs addObject: [v.widthAnchor constraintGreaterThanOrEqualToConstant: mins.width]];
        [cs addObject: [v.heightAnchor constraintGreaterThanOrEqualToConstant: mins.height]];
        [cs addObject: [v.widthAnchor constraintLessThanOrEqualToConstant: max (maxs.width, mins.width)]];
        [cs addObject: [v.heightAnchor constraintLessThanOrEqualToConstant: max (maxs.height, mins.height)]];
        NSLayoutConstraint* dw= [v.widthAnchor constraintEqualToConstant: defs.width];
        NSLayoutConstraint* dh= [v.heightAnchor constraintEqualToConstant: defs.height];
        [dw setPriority: NSLayoutPriorityDefaultLow];
        [dh setPriority: NSLayoutPriorityDefaultLow];
        [cs addObject: dw];
        [cs addObject: dh];
      }
      [NSLayoutConstraint activateConstraints: cs];
      return v;
    }

    case glue_widget:
    {
      typedef quartet<bool, bool, SI, SI> T;
      T x= open_box<T> (load);
      // NOTE: an extensible glue takes the extra space (as the spacers of
      // Qt), shared with the other glues of its list (see stack_of)
      NSView* v= [[[NSView alloc] init] autorelease];
      [v setTranslatesAutoresizingMaskIntoConstraints: NO];
      if (!x.x1) [[v.widthAnchor constraintEqualToConstant: x.x3] setActive: YES];
      else {
        [[v.widthAnchor constraintGreaterThanOrEqualToConstant: x.x3] setActive: YES];
        [v setContentHuggingPriority: 1
                      forOrientation: NSLayoutConstraintOrientationHorizontal];
      }
      if (!x.x2) [[v.heightAnchor constraintEqualToConstant: x.x4] setActive: YES];
      else {
        [[v.heightAnchor constraintGreaterThanOrEqualToConstant: x.x4] setActive: YES];
        [v setContentHuggingPriority: 1
                      forOrientation: NSLayoutConstraintOrientationVertical];
      }
      [v setIdentifier: [NSString stringWithFormat: @"TMGlue%s%s",
                          x.x1? "H": "", x.x2? "V": ""]];
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
      array<widget> tabs, bodies;
      array<url> icons;
      if (type == tabs_widget) {
        typedef pair<array<widget>, array<widget> > T;
        T x= open_box<T> (load);
        tabs= x.x1; bodies= x.x2;
      }
      else {
        typedef triple<array<url>, array<widget>, array<widget> > T;
        T x= open_box<T> (load);
        icons= x.x1; tabs= x.x2; bodies= x.x3;
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
      // As in the Qt interface, the dialog takes the size of the page shown
      // (see TMTabHelper)
      {
        TMTabHelper* th= [[[TMTabHelper alloc] init] autorelease];
        objc_setAssociatedObject (tv, "TMTabHelper", th, OBJC_ASSOCIATION_RETAIN);
        [tv setDelegate: th];
      }
      if (type == tabs_widget) return tv;
      // the icon tabs: a segmented control with the icons above the tabs
      [tv setTabViewType: NSNoTabsBezelBorder];
      NSSegmentedControl* sc= [[[NSSegmentedControl alloc] init] autorelease];
      NSInteger n= [tv numberOfTabViewItems];
      [sc setSegmentCount: n];
      for (NSInteger i=0; i<n; i++) {
        [sc setLabel: [[tv tabViewItemAtIndex: i] label] forSegment: i];
        if (i < N(icons)) [sc setImage: to_nsimage (icons[i]) forSegment: i];
        [sc setImageScaling: NSImageScaleProportionallyDown forSegment: i];
      }
      [sc setSelectedSegment: 0];
      [sc setTarget: tv];
      [sc setAction: @selector(takeSelectedTabViewItemFromSender:)];
      NSStackView* st= [NSStackView stackViewWithViews:
                         [NSArray arrayWithObjects: sc, tv, nil]];
      [st setOrientation: NSUserInterfaceLayoutOrientationVertical];
      [st setAlignment: NSLayoutAttributeCenterX];
      [[tv.widthAnchor constraintEqualToAnchor: st.widthAnchor] setActive: YES];
      return st;
    }

    case choice_widget:
    {
      typedef quintuple<command, array<string>, array<string>, bool, int> T;
      T x= open_box<T> (load);
      return choice_list (x.x1, x.x2, x.x3, x.x4);
    }

    case filtered_choice_widget:
    {
      typedef quartet<command, array<string>, string, string> T;
      T x= open_box<T> (load);
      array<string> chosen;
      chosen << x.x3;
      return choice_list (x.x1, x.x2, chosen, false, x.x4, true);
    }

    case tree_view_widget:
    {
      typedef triple<command, tree, tree> T;
      T x= open_box<T> (load);
      return tree_view (x.x1, x.x2);
    }

    default:
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
  else if (gc && !is_atomic (col)) {
    // a pattern, drawn by the renderer (see qt_glue_widget_rep::render)
    ns_renderer_rep ren ((int) s.width, (int) s.height);
    ren.begin (gc);
    ren.set_shrinking_factor (1);
    rectangle r= rectangle (0, 0, (SI) s.width, (SI) s.height);
    ren.set_origin (0, 0);
    ren.encode (r->x1, r->y1);
    ren.encode (r->x2, r->y2);
    ren.set_clipping (r->x1, r->y2, r->x2, r->y1);
    ren.set_shrinking_factor (std_shrinkf);
    ren.set_background (col);
    ren.clear_pattern (5*r->x1, 5*r->y2, 5*r->x2, 5*r->y1);
    ren.end ();
  }
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
