/******************************************************************************
* MODULE     : ns_dialogues.mm
* DESCRIPTION: Dialogs (file chooser, questions, inputs) for the NS port
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_cocoa.h"
#import <PDFKit/PDFKit.h>
#import <objc/runtime.h>
#include "ns_other_widgets.h"
#include "ns_utilities.h"
#include "ns_simple_widget.h"
#include "ns_gui.h"

#include "analyze.hpp"
#include "converter.hpp"
#include "wencoding.hpp"
#include "gui.hpp"
#include "dictionary.hpp"
#include "message.hpp"
#include "scheme.hpp"
#include "url.hpp"
#ifdef USE_GS
#include "Ghostscript/gs_utilities.hpp"
#endif
#include "file.hpp"
#include "MacOS/mac_images.h"
#include "editor.hpp"
#include "convert.hpp"


/******************************************************************************
* ns_chooser_widget_rep
******************************************************************************/

ns_chooser_widget_rep::ns_chooser_widget_rep (command _cmd, string _type,
                                              string _prompt):
  ns_widget_rep (file_chooser), cmd (_cmd), type (_type), prompt (_prompt),
  position (coord2 (0, 0)), size (coord2 (100, 100)), file ("")
{
  // the filter and the default suffix of the type (as qt_chooser_widget_rep):
  // without them, a name saved without a suffix was given none by the panel,
  // which asked about replacing the file of that name rather than the one
  // saved (#400)
  if (!set_type (_type)) set_type ("generic");
}

/*! The filter of the file type (see qt_chooser_widget_rep::set_type). */
bool
ns_chooser_widget_rep::set_type (const string& _type) {
  filter_name= "";
  suffixes= array<string> ();
  if (_type == "directory" || _type == "generic") {
    type= _type;
    return true;
  }
  if (format_exists (_type)) {
    filter_name= translate (as_string (call ("format-get-name", _type))
                            * " file");
    array<object> a= as_array_object (call ("format-get-suffixes*", _type));
    for (int i=1; i<N(a); i++) suffixes << as_string (a[i]);
  }
  else if (_type == "image") {
    filter_name= translate ("Image file");
    suffixes << string ("png") << string ("jpg") << string ("jpeg")
             << string ("bmp") << string ("gif") << string ("pdf")
             << string ("eps") << string ("ps") << string ("svg")
             << string ("tif") << string ("tiff");
  }
  else return false;
  type= _type;
  return true;
}

void
ns_chooser_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_VISIBILITY:
      check_type<bool> (val, s);
      break;
    case SLOT_SIZE:
      check_type<coord2> (val, s);
      size= open_box<coord2> (val);
      break;
    case SLOT_POSITION:
      check_type<coord2> (val, s);
      position= open_box<coord2> (val);
      break;
    case SLOT_KEYBOARD_FOCUS:
      check_type<bool> (val, s);
      perform_dialog ();
      break;
    case SLOT_STRING_INPUT:
      check_type<string> (val, s);
      break;
    case SLOT_INPUT_TYPE:
      check_type<string> (val, s);
      set_type (open_box<string> (val));
      break;
    case SLOT_FILE:
      check_type<string> (val, s);
      file= open_box<string> (val);
      break;
    case SLOT_DIRECTORY:
      check_type<string> (val, s);
      directory= open_box<string> (val);
      directory= as_string (url_pwd () * url_system (directory));
      break;
    default:
      ns_widget_rep::send (s, val);
  }
}

blackbox
ns_chooser_widget_rep::query (slot s, int type_id) {
  switch (s) {
    case SLOT_POSITION:
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (position);
    case SLOT_SIZE:
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (size);
    case SLOT_STRING_INPUT:
      check_type_id<string> (type_id, s);
      return close_box<string> (file);
    default:
      return ns_widget_rep::query (s, type_id);
  }
}

widget
ns_chooser_widget_rep::read (slot s, blackbox index) {
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (index, s);
      return this;
    case SLOT_FORM_FIELD:
      check_type<int> (index, s);
      return this;
    case SLOT_FILE: case SLOT_DIRECTORY:
      check_type_void (index, s);
      return this;
    default:
      return ns_widget_rep::read (s, index);
  }
}

widget
ns_chooser_widget_rep::plain_window_widget (string s, command q) {
  win_title= s;
  quit= q;
  return this;
}

/*! The pop-up menu of the filters of a file panel, and its delegate. */
@interface TMChooserFilter : NSObject <NSOpenSavePanelDelegate>
{
@public
  NSSavePanel* panel;
  NSArray* types;
  NSString* suffix; // the default suffix of a name saved without one
}
- (void) choose: (NSPopUpButton*) sender;
@end

@implementation TMChooserFilter
- (void) choose: (NSPopUpButton*) sender
{
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  [panel setAllowedFileTypes: [sender indexOfSelectedItem] == 0? types: nil];
#pragma clang diagnostic pop
}
/*! A name typed without a suffix gets the default one before the panel
    asks whether to replace a file, also with the filter of all files: the
    suffix added after the panel closed (below) made its question about
    another file than the one saved (#400). */
- (NSString*) panel: (id) sender userEnteredFilename: (NSString*) name
          confirmed: (BOOL) ok
{
  (void) sender; (void) ok;
  if (suffix && [[name pathExtension] length] == 0)
    return [name stringByAppendingPathExtension: suffix];
  return name;
}
- (void) dealloc
{
  [types release];
  [suffix release];
  [super dealloc];
}
@end

void
ns_chooser_widget_rep::perform_dialog () {
  // The result is queried by TeXmacs with SLOT_STRING_INPUT
  bool save= (prompt != "");
  NSSavePanel* panel;
  if (save) panel= [NSSavePanel savePanel];
  else {
    NSOpenPanel* op= [NSOpenPanel openPanel];
    [op setCanChooseDirectories: type == "directory"];
    [op setCanChooseFiles: type != "directory"];
    [op setAllowsMultipleSelection: NO];
    panel= op;
  }
  if (win_title != "") [panel setMessage: to_label (win_title)];
  if (save) {
    string text= prompt;
    if (ends (text, ":")) text= text (0, N(text) - 1);
    if (ends (text, " as")) text= text (0, N(text) - 3);
    [panel setPrompt: to_label (translate (text))];
  }
  if (directory != "")
    [panel setDirectoryURL: [NSURL fileURLWithPath: to_nsstring (directory)]];
  if (file != "" && save)
    [panel setNameFieldStringValue: to_nsstring (as_string (tail (url_system (file))))];

  // the filters: the file type or all files (as in the Qt interface)
  TMChooserFilter* filter= nil;
  if (N(suffixes) > 0 && type != "directory") {
    NSMutableArray* types= [NSMutableArray array];
    string desc;
    for (int i=0; i<N(suffixes); i++) {
      [types addObject: to_nsstring (suffixes[i])];
      desc << (i == 0? "": " ") << "*." << suffixes[i];
    }
    filter= [[[TMChooserFilter alloc] init] autorelease];
    filter->panel= panel;
    filter->types= [types retain];
    if (save) {
      filter->suffix= [to_nsstring (suffixes[0]) retain];
      [panel setDelegate: filter];
    }
    NSPopUpButton* pop= [[[NSPopUpButton alloc] init] autorelease];
    [pop addItemWithTitle: to_label (filter_name * " (" * desc * ")")];
    [pop addItemWithTitle: to_label (translate ("All files (*)"))];
    [pop setTarget: filter];
    [pop setAction: @selector(choose:)];
    [pop sizeToFit];
    NSStackView* acc= [NSStackView stackViewWithViews:
                        [NSArray arrayWithObjects:
                          [NSTextField labelWithString:
                            to_label (translate ("Format") * ":")], pop, nil]];
    [acc setEdgeInsets: NSEdgeInsetsMake (8, 8, 8, 8)];
    [acc setFrameSize: [acc fittingSize]];
    [panel setAccessoryView: acc];
    if ([panel isKindOfClass: [NSOpenPanel class]])
      [(NSOpenPanel*) panel setAccessoryViewDisclosed: YES];
    [filter choose: pop];
    [panel setAllowsOtherFileTypes: YES];
  }

  file= "#f";
  NSInteger answer= [panel runModal];
  [panel setDelegate: nil];
  if (answer == NSModalResponseOK) {
    string name= from_nsstring_utf8 ([[panel URL] path]); // not cork
    // the default suffix, when there is none (see qt_chooser_widget_rep)
    if (save && N(suffixes) > 0 && suffix (url_system (name)) == "")
      name= name * "." * suffixes[0];
    file= "(system->url " * scm_quote (name) * ")";
    if (type == "image") {
      // the size of the image (see qt_pretty_image_size)
      string w= "", h= "";
      int ww, hh;
      string ext= locase_all (suffix (url_system (name)));
      if (ext != "pdf" && ext != "ps" && ext != "eps" &&
          mac_image_size (url_system (name), ww, hh)) {
        SI pt = get_current_editor()->as_length ("1pt");
        SI par= get_current_editor()->as_length ("1par");
        if (ww <= 0 || hh <= 0 || ww * pt > par) w= "1par";
        else { w= as_string (ww) * "pt"; h= as_string (hh) * "pt"; }
      }
      file= "(list " * file * " \"" * w * "\" \"" * h * "\" \"\" \"\")";
    }
  }
  cmd ();
  if (!is_nil (quit)) quit ();
}

/******************************************************************************
* ns_field_widget_rep and ns_inputs_list_widget_rep
******************************************************************************/

ns_field_widget_rep::ns_field_widget_rep (ns_inputs_list_widget_rep* _parent,
                                          string _prompt):
  ns_widget_rep (field_widget), prompt (_prompt), input (""), parent (_parent) {}

void
ns_field_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_STRING_INPUT:
      check_type<string> (val, s);
      input= scm_quote (open_box<string> (val));
      break;
    case SLOT_INPUT_TYPE:
      check_type<string> (val, s);
      type= open_box<string> (val);
      break;
    case SLOT_INPUT_PROPOSAL:
      check_type<string> (val, s);
      proposals << open_box<string> (val);
      break;
    case SLOT_KEYBOARD_FOCUS:
      parent->send (s, val);
      break;
    default:
      ns_widget_rep::send (s, val);
  }
}

blackbox
ns_field_widget_rep::query (slot s, int type_id) {
  switch (s) {
    case SLOT_STRING_INPUT:
      check_type_id<string> (type_id, s);
      return close_box<string> (input);
    default:
      return ns_widget_rep::query (s, type_id);
  }
}

ns_inputs_list_widget_rep::ns_inputs_list_widget_rep (command _cmd,
                                                      array<string> _prompts):
  ns_widget_rep (input_widget), cmd (_cmd), size (coord2 (100, 100)),
  position (coord2 (0, 0)), win_title (""), style (0)
{
  for (int i = 0; i < N(_prompts); i++)
    add_child (tm_new<ns_field_widget_rep> (this, _prompts[i]));
}

widget
ns_inputs_list_widget_rep::plain_window_widget (string s, command q) {
  (void) q; // The widget already has a command (dialogue_command)
  win_title= s;
  return this;
}

void
ns_inputs_list_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_VISIBILITY:
      check_type<bool> (val, s);
      break;
    case SLOT_SIZE:
      check_type<coord2> (val, s);
      size= open_box<coord2> (val);
      break;
    case SLOT_POSITION:
      check_type<coord2> (val, s);
      position= open_box<coord2> (val);
      break;
    case SLOT_KEYBOARD_FOCUS:
      check_type<bool> (val, s);
      perform_dialog ();
      break;
    default:
      ns_widget_rep::send (s, val);
  }
}

blackbox
ns_inputs_list_widget_rep::query (slot s, int type_id) {
  switch (s) {
    case SLOT_POSITION:
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (position);
    case SLOT_SIZE:
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (size);
    case SLOT_STRING_INPUT:
      if (N(children) > 0) return field(0)->query (s, type_id);
      return ns_widget_rep::query (s, type_id);
    default:
      return ns_widget_rep::query (s, type_id);
  }
}

widget
ns_inputs_list_widget_rep::read (slot s, blackbox val) {
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (val, s);
      return this;
    case SLOT_FORM_FIELD:
    {
      check_type<int> (val, s);
      int index= open_box<int> (val);
      if (N(children) > index)
        return static_cast<widget_rep*> (children[index].rep);
      return widget ();
    }
    default:
      return ns_widget_rep::read (s, val);
  }
}

ns_field_widget_rep*
ns_inputs_list_widget_rep::field (int i) {
  return static_cast<ns_field_widget_rep*> (children[i].rep);
}

void
ns_inputs_list_widget_rep::perform_dialog () {
  NSAlert* alert= [[[NSAlert alloc] init] autorelease];
  if ((N(children) == 1) && (field(0)->type == "question")) {
    // A question: one button per proposal, the first one being the default
    ns_field_widget_rep* f= field(0);
    [alert setMessageText: to_label (f->prompt)];
    [alert setAlertStyle: NSAlertStyleInformational];
    for (int i=0; i<N(f->proposals); i++)
      [alert addButtonWithTitle: to_label (upcase_first (f->proposals[i]))];
    [alert addButtonWithTitle: to_label (translate ("Cancel"))];
    NSModalResponse r= [alert runModal];
    int i= (int) (r - NSAlertFirstButtonReturn);
    if (i >= 0 && i < N(f->proposals)) f->input= scm_quote (f->proposals[i]);
    else f->input= "#f";
  }
  else {
    // A list of fields, each with a prompt and a combo box
    [alert setMessageText: to_label (win_title)];
    NSGridView* grid= [[[NSGridView alloc] init] autorelease];
    NSMutableArray* boxes= [NSMutableArray array];
    for (int i=0; i<N(children); i++) {
      ns_field_widget_rep* f= field(i);
      NSTextField* label= [NSTextField labelWithString: to_label (f->prompt)];
      NSTextField* box;
      if (f->type == "password")
        // hidden, and without the proposals (as in Qt)
        box= [[[NSSecureTextField alloc]
                initWithFrame: NSMakeRect (0, 0, 300, 24)] autorelease];
      else {
        NSComboBox* cb= [[[NSComboBox alloc]
                           initWithFrame: NSMakeRect (0, 0, 300, 24)] autorelease];
        for (int j=0; j<N(f->proposals); j++)
          [cb addItemWithObjectValue: to_label (f->proposals[j])];
        if (N(f->proposals) > 0) [cb setStringValue: to_label (f->proposals[0])];
        box= cb;
      }
      [grid addRowWithViews: [NSArray arrayWithObjects: label, box, nil]];
      [boxes addObject: box];
    }
    [grid setFrameSize: [grid fittingSize]];
    [alert setAccessoryView: grid];
    [alert addButtonWithTitle: to_label (translate ("Ok"))];
    [alert addButtonWithTitle: to_label (translate ("Cancel"))];
    if (N(children) > 0)
      [[alert window] setInitialFirstResponder: [boxes objectAtIndex: 0]];
    bool ok= [alert runModal] == NSAlertFirstButtonReturn;
    for (int i=0; i<N(children); i++)
      field(i)->input= ok? scm_quote (from_label ([[boxes objectAtIndex: i]
                                                    stringValue]))
                         : string ("#f");
  }
  if (!is_nil (cmd)) cmd ();
}

/******************************************************************************
* ns_input_text_widget_rep
******************************************************************************/

/*! The behavior of the input fields of the Qt interface (QTMLineEdit):
 the input is committed with return, and when the field loses the focus if
 the preference gui:line-input:autocommit is on; escape cancels; tab and the
 arrows complete with the proposals; the continuous fields (searching,
 replacing, spelling, forms) send each change with the last key. */
@interface TMInputTextHelper : NSObject <NSTextFieldDelegate>
{
@public
  ns_input_text_widget_rep* wid;
  NSArray* proposals;
  NSInteger current;
  BOOL handled;
}
- (id) initWithWidget: (ns_input_text_widget_rep*) w
            proposals: (NSArray*) p;
@end

@implementation TMInputTextHelper
- (id) initWithWidget: (ns_input_text_widget_rep*) w
            proposals: (NSArray*) p
{
  // NOTE: the helper keeps the widget alive, as QTMInputTextWidgetHelper
  self= [super init];
  if (self) {
    wid= w; INC_COUNT (wid);
    proposals= [p retain]; current= -1; handled= NO;
  }
  return self;
}

- (void) dealloc
{
  [proposals release];
  if (wid) { if (wid->view) wid->view= nil; DEC_COUNT (wid); }
  [super dealloc];
}

- (void) complete: (NSTextView*) tv forward: (BOOL) fw
{
  // The next proposal which extends the text before the cursor
  NSString* text= [tv string];
  NSRange sel= [tv selectedRange];
  NSString* prefix= [text substringToIndex: MIN (sel.location,
                                                 [text length])];
  NSMutableArray* ok= [NSMutableArray array];
  for (NSString* p in proposals)
    if ([p hasPrefix: prefix]) [ok addObject: p];
  if ([ok count] == 0) return;
  NSUInteger k= [ok indexOfObject: text];
  if (k == NSNotFound) k= fw? 0: [ok count] - 1;
  else k= fw? (k + 1) % [ok count]: (k + [ok count] - 1) % [ok count];
  NSString* c= [ok objectAtIndex: k];
  [tv setString: c];
  [tv setSelectedRange: NSMakeRange ([prefix length],
                                     [c length] - [prefix length])];
}

- (BOOL) control: (NSControl*) control textView: (NSTextView*) tv
    doCommandBySelector: (SEL) sel
{
  (void) control;
  if (!wid) return NO;
  if (wid->continuous ()) {
    string key= "";
    if (sel == @selector(insertNewline:)) key= "return";
    else if (sel == @selector(cancelOperation:)) key= "escape";
    else if (sel == @selector(moveUp:)) key= "up";
    else if (sel == @selector(moveDown:)) key= "down";
    else if (sel == @selector(insertTab:)) key= "tab";
    else if (sel == @selector(insertBacktab:)) key= "S-tab";
    else if (sel == @selector(scrollPageUp:)) key= "pageup";
    else if (sel == @selector(scrollPageDown:)) key= "pagedown";
    if (key == "") return NO;
    wid->send_key (from_nsstring ([tv string]), key);
    return YES;
  }
  if (sel == @selector(cancelOperation:)) {
    handled= YES;
    wid->commit (false);
    return YES;
  }
  if (sel == @selector(insertNewline:)) {
    handled= YES;
    wid->commit (true);
    return YES;
  }
  if ([proposals count] > 0 &&
      (sel == @selector(insertTab:) || sel == @selector(moveDown:))) {
    [self complete: tv forward: YES];
    return YES;
  }
  if ([proposals count] > 0 &&
      (sel == @selector(insertBacktab:) || sel == @selector(moveUp:))) {
    [self complete: tv forward: NO];
    return YES;
  }
  return NO;
}

- (void) controlTextDidChange: (NSNotification*) n
{
  (void) n;
  handled= NO;
  if (wid && wid->continuous ()) {
    NSTextField* f= [n object];
    wid->send_key (from_nsstring ([f stringValue]), "none");
  }
}

- (void) controlTextDidEndEditing: (NSNotification*) n
{
  // The field loses the focus
  (void) n;
  if (!wid || handled || wid->continuous ()) return;
  wid->commit (wid->can_autocommit () &&
               get_preference ("gui:line-input:autocommit") == "on");
}
@end

ns_input_text_widget_rep::ns_input_text_widget_rep (command _cmd, string _type,
                                                    array<string> _proposals,
                                                    int _style, string _width):
  ns_widget_rep (input_widget), cmd (_cmd), type (_type),
  proposals (_proposals), input (""), style (_style), width (_width),
  ok (false), done (false), view (nil)
{
  if (type == "password") proposals= array<string> (0);
  if (N(proposals) > 0) input= proposals[0];
}

string
ns_input_text_widget_rep::field_type () {
  // "name#serial:type" (see QTMLineEdit::set_type)
  int i= search_forwards (":", 0, type);
  return i >= 0? type (i+1, N(type)): type;
}

bool
ns_input_text_widget_rep::continuous () {
  string t= field_type ();
  string name= type;
  int i= search_forwards (":", 0, type);
  string serial= "";
  if (i >= 0) {
    name= type (0, i);
    int j= search_forwards ("#", 0, name);
    if (j >= 0) serial= name (j+1, N(name));
  }
  return starts (t, "search") || starts (t, "replace-") ||
         starts (t, "spell") || starts (serial, "form-");
}

bool
ns_input_text_widget_rep::can_autocommit () {
  return !(ends (type, "search") || ends (type, "replace") ||
           starts (type, "interactive"));
}

void
ns_input_text_widget_rep::send_key (string s, string key) {
  // As QTMLineEdit::keyPressEvent for the continuous fields
  input= s;
  the_gui->process_command (cmd, list_object (list_object (object (s),
                                                           object (key))));
}

NSView*
ns_input_text_widget_rep::as_nsview () {
  NSTextField* f= (type == "password")
    ? [[[NSSecureTextField alloc] init] autorelease]
    : [[[NSTextField alloc] init] autorelease];
  [f setStringValue: to_label (input)];
  [f setIdentifier: to_nsstring (type)];
  if (style & WIDGET_STYLE_MINI) [f setControlSize: NSControlSizeSmall];
  [f setFont: [NSFont systemFontOfSize:
                 [NSFont systemFontSizeForControlSize: [f controlSize]]]];
  NSMutableArray* props= [NSMutableArray array];
  if (N(proposals) > 1 || (N(proposals) == 1 && N(proposals[0]) > 0))
    for (int i=0; i<N(proposals); i++)
      [props addObject: to_label (proposals[i])];
  // NOTE: the text field keeps its delegate (the delegate is a weak reference)
  TMInputTextHelper* h= [[[TMInputTextHelper alloc] initWithWidget: this
                                                          proposals: props]
                          autorelease];
  objc_setAssociatedObject (f, "TMInputTextHelper", h,
                            OBJC_ASSOCIATION_RETAIN);
  [f setDelegate: h];
  // The width (see QTMLineEdit::sizeHint)
  NSSize sz= ns_decode_length (width, "", NSMakeSize (150, 22));
  // NOTE: as in the Qt interface, the fields shrink when there is no room
  [f setTranslatesAutoresizingMaskIntoConstraints: NO];
  NSLayoutConstraint* c= [f.widthAnchor constraintEqualToConstant: sz.width];
  // NOTE: above the stretching of the lists and glues (see stack_of)
  [c setPriority: NSLayoutPriorityDefaultHigh - 5];
  [c setActive: YES];
  [[f.widthAnchor constraintGreaterThanOrEqualToConstant: min (sz.width, 30.0)]
    setActive: YES];
  [f setContentCompressionResistancePriority: NSLayoutPriorityDefaultLow - 1
                              forOrientation: NSLayoutConstraintOrientationHorizontal];
  view= f;
  return f;
}

void
ns_input_text_widget_rep::commit (bool flag) {
  NSTextField* f= (NSTextField*) view;
  if (flag) {
    done = false;
    ok   = true;
    if (f) input= from_label ([f stringValue]);
  }
  else if (f) [f setStringValue: to_label (input)];
  if (done) return;
  done= true;
  the_gui->process_command (cmd, ok? list_object (object (input))
                                   : list_object (object (false)));
}

/******************************************************************************
* ns_tm_embedded_widget_rep
******************************************************************************/

ns_tm_embedded_widget_rep::ns_tm_embedded_widget_rep (command _quit):
  ns_widget_rep (embedded_tm_widget), container (nil), quit (_quit) {}

// (defined in ns_tm_widget.mm: the document is centered when it is smaller)
@interface TMClipView : NSClipView
@end

void
ns_tm_embedded_widget_rep::show_canvas () {
  // As in the Qt interface, where the canvas of an editor is a scroll view
  // (QTMScrollView): the canvas is the document view of a scroll view which
  // fills the container. The size of the editor is that of the clip view,
  // not that of the document, which is scrolled (the wheel, the scroll bar,
  // and the cursor kept visible, see SLOT_SCROLL_POSITION). A canvas pinned
  // to the container whose frame also followed the extents of the document
  // made the editor be typeset again at each update (#620).
  if (!container || is_nil (main_widget)) return;
  for (NSView* old in [[[container subviews] copy] autorelease])
    [old removeFromSuperview];
  NSView* v= concrete (main_widget)->as_nsview ();
  if (!v) return;
  NSView* w= v;
  if (concrete (main_widget)->type == simple_widget) {
    NSScrollView* sv= [[[NSScrollView alloc] init] autorelease];
    // NOTE: no horizontal scroll bar (the page follows the width), and a
    // vertical one which is not hidden: one shown or hidden with the
    // contents would change the width, hence the contents (legacy scroll
    // bars; the overlay ones of the trackpads take no room)
    [sv setHasVerticalScroller: YES];
    [sv setHasHorizontalScroller: NO];
    [sv setAutohidesScrollers: NO];
    [sv setBorderType: NSNoBorder];
    [sv setContentView: [[[TMClipView alloc] init] autorelease]];
    [sv setDrawsBackground: YES];
    [sv setBackgroundColor: [NSColor whiteColor]];
    [v setTranslatesAutoresizingMaskIntoConstraints: YES];
    [sv setDocumentView: v];
    w= sv;
  }
  [w setTranslatesAutoresizingMaskIntoConstraints: NO];
  [container addSubview: w];
  [NSLayoutConstraint activateConstraints: @[
    [w.leadingAnchor constraintEqualToAnchor: container.leadingAnchor],
    [w.trailingAnchor constraintEqualToAnchor: container.trailingAnchor],
    [w.topAnchor constraintEqualToAnchor: container.topAnchor],
    [w.bottomAnchor constraintEqualToAnchor: container.bottomAnchor]]];
}

void
ns_tm_embedded_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_DESTROY:
      if (!is_nil (quit)) quit ();
      quit= command ();
      break;
    default:
      if (!is_nil (main_widget)) main_widget->send (s, val);
  }
}

blackbox
ns_tm_embedded_widget_rep::query (slot s, int type_id) {
  if (!is_nil (main_widget)) return main_widget->query (s, type_id);
  return ns_widget_rep::query (s, type_id);
}

widget
ns_tm_embedded_widget_rep::read (slot s, blackbox index) {
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (index, s);
      return this;
    case SLOT_SCROLLABLE:
    case SLOT_CANVAS:
      check_type_void (index, s);
      return main_widget;
    default:
      return ns_widget_rep::read (s, index);
  }
}

void
ns_tm_embedded_widget_rep::write (slot s, blackbox index, widget w) {
  switch (s) {
    case SLOT_SCROLLABLE:
      check_type_void (index, s);
      main_widget= w;
      show_canvas ();
      break;
    default:
      ns_widget_rep::write (s, index, w);
  }
}

NSView*
ns_tm_embedded_widget_rep::as_nsview () {
  // NOTE: as in the Qt interface, a container, since the canvas is usually
  // given after the view was requested
  if (!container) {
    container= [[NSView alloc] initWithFrame: NSMakeRect (0, 0, 100, 30)];
    [container setTranslatesAutoresizingMaskIntoConstraints: NO];
    // The default size: as Qt, which gives a window at most two thirds of
    // the screen, the size hint of the editor (the screen) limited to two
    // thirds of the screen; below the default size of a resize widget
    // around it (the fields of the search tool, ...), and resizable
    NSSize scr= [[NSScreen mainScreen] visibleFrame].size;
    if (scr.width < 1 || scr.height < 1) scr= NSMakeSize (1200, 800);
    NSLayoutConstraint* dw=
      [container.widthAnchor constraintEqualToConstant: floor (scr.width * 2 / 3)];
    NSLayoutConstraint* dh=
      [container.heightAnchor constraintEqualToConstant: floor (scr.height * 2 / 3)];
    [dw setPriority: NSLayoutPriorityDefaultLow - 1];
    [dh setPriority: NSLayoutPriorityDefaultLow - 1];
    NSLayoutConstraint* mw= [container.widthAnchor constraintGreaterThanOrEqualToConstant: 40];
    NSLayoutConstraint* mh= [container.heightAnchor constraintGreaterThanOrEqualToConstant: 20];
    [mw setPriority: NSLayoutPriorityDefaultHigh];
    [mh setPriority: NSLayoutPriorityDefaultHigh];
    [NSLayoutConstraint activateConstraints: @[dw, dh, mw, mh]];
    show_canvas ();
  }
  return container;
}

/******************************************************************************
* Color picker (see qt_color_picker_widget_rep)
******************************************************************************/

ns_color_picker_widget_rep::ns_color_picker_widget_rep (command cmd, bool bg,
                                                        array<tree> proposals):
  ns_widget_rep (none), _commandAfterExecution (cmd), _pickPattern (false)
{ (void) bg; (void) proposals; }

ns_color_picker_widget_rep::~ns_color_picker_widget_rep () {}

widget
ns_color_picker_widget_rep::plain_window_widget (string s, command q) {
  (void) q;
  _windowTitle= s;
  return this;
}

void
ns_color_picker_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_VISIBILITY:
      check_type<bool> (val, s);
      if (open_box<bool> (val)) showDialog ();
      break;
    default:
      ns_widget_rep::send (s, val);
  }
}

void
ns_color_picker_widget_rep::showDialog () {
  // A modal dialog with a color well, as the color dialog of Qt
  // NOTE: as in the Qt interface, the proposals and the patterns are ignored
  NSAlert* alert= [[[NSAlert alloc] init] autorelease];
  [alert setMessageText: to_label (_windowTitle != ""? _windowTitle:
                                   string ("Choose a color"))];
  NSColorWell* well= [[[NSColorWell alloc]
                        initWithFrame: NSMakeRect (0, 0, 120, 40)] autorelease];
  [well setColor: [NSColor whiteColor]];
  [alert setAccessoryView: well];
  [alert addButtonWithTitle: to_label (translate ("Ok"))];
  [alert addButtonWithTitle: to_label (translate ("Cancel"))];
  bool ok= [alert runModal] == NSAlertFirstButtonReturn;
  [[NSColorPanel sharedColorPanel] orderOut: nil];
  if (!ok || is_nil (_commandAfterExecution)) return;
  NSColor* c= [[well color] colorUsingColorSpace: [NSColorSpace sRGBColorSpace]];
  if (!c) return;
  char buf[16];
  snprintf (buf, 16, "#%02x%02x%02x",
            (int) round (255 * [c redComponent]),
            (int) round (255 * [c greenComponent]),
            (int) round (255 * [c blueComponent]));
  _commandAfterExecution (list_object (object (tree (string (buf)))));
}

/******************************************************************************
* Printing (see qt_printer_widget_rep)
******************************************************************************/

ns_printer_widget_rep::ns_printer_widget_rep (command cmd, url ps_pdf_file):
  ns_widget_rep (none), commandAfterExecution (cmd), file (ps_pdf_file) {}

widget
ns_printer_widget_rep::plain_window_widget (string s, command q) {
  (void) s;
  commandAfterExecution= q;
  return this;
}

void
ns_printer_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_VISIBILITY:
      check_type<bool> (val, s);
      if (open_box<bool> (val)) showDialog ();
      break;
    case SLOT_REFRESH:
      break;
    default:
      ns_widget_rep::send (s, val);
  }
}

void
ns_printer_widget_rep::showDialog () {
  // The PDF file is printed with the print panel of the system; the
  // PostScript files are converted with Ghostscript first
  url pdf= file;
  if (suffix (file) != "pdf") {
#ifdef USE_GS
    if (has_gs ()) {
      // the pages keep their size (ps2pdf)
      pdf= url_temp (".pdf");
      string cmd= gs_prefix ();
      cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite ";
      cmd << "-sOutputFile=" << sys_concretize (pdf) << " ";
      cmd << sys_concretize (file);
      system (cmd);
    }
#endif
    // NOTE: without Ghostscript, pdf is still the PostScript file
    if (pdf == file || !exists (pdf)) {
      call ("set-message", object ("Could not convert the file for printing"),
            object ("Print"));
      return;
    }
  }
  NSURL* u= [NSURL fileURLWithPath: to_nsstring_utf8 (concretize (pdf))];
  PDFDocument* d= [[[PDFDocument alloc] initWithURL: u] autorelease];
  bool done= false;
  if (d) {
    NSPrintOperation* op=
      [d printOperationForPrintInfo: [NSPrintInfo sharedPrintInfo]
                        scalingMode: kPDFPrintPageScaleToFit autoRotate: YES];
    [op setShowsPrintPanel: YES];
    [op setShowsProgressPanel: YES];
    done= [op runOperation];
  }
  else call ("set-message", object ("Could not read the file for printing"),
             object ("Print"));
  if (pdf != file) remove (pdf);
  if (done && !is_nil (commandAfterExecution)) commandAfterExecution ();
}
