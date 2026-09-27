/******************************************************************************
* MODULE     : ns_dialogues.mm
* DESCRIPTION: Dialogs (file chooser, questions, inputs) for the NS port
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
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

static NSString*
to_label (string s) {
  if (looks_utf8 (s) && !(looks_ascii (s) || looks_universal (s)))
    return to_nsstring (s);
  return to_nsstring_utf8 (s);
}

static string
from_label (NSString* s) {
  // Inputs are returned in the cork encoding, like in the Qt interface
  return utf8_to_cork (from_nsstring (s));
}

/******************************************************************************
* ns_chooser_widget_rep
******************************************************************************/

ns_chooser_widget_rep::ns_chooser_widget_rep (command _cmd, string _type,
                                              string _prompt):
  ns_widget_rep (file_chooser), cmd (_cmd), type (_type), prompt (_prompt),
  position (coord2 (0, 0)), size (coord2 (100, 100)), file ("") {}

bool
ns_chooser_widget_rep::set_type (const string& _type) {
  // FIXME: filters for the file types, as in qt_chooser_widget_rep
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

  file= "#f";
  if ([panel runModal] == NSModalResponseOK) {
    string name= from_nsstring ([[panel URL] path]);
    file= "(system->url " * scm_quote (name) * ")";
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
      NSComboBox* box= [[[NSComboBox alloc]
                          initWithFrame: NSMakeRect (0, 0, 300, 24)] autorelease];
      for (int j=0; j<N(f->proposals); j++)
        [box addItemWithObjectValue: to_label (f->proposals[j])];
      if (N(f->proposals) > 0) [box setStringValue: to_label (f->proposals[0])];
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

@interface TMInputTextHelper : NSObject <NSTextFieldDelegate>
{
  ns_input_text_widget_rep* wid;
}
- (id) initWithWidget: (ns_input_text_widget_rep*) w;
- (void) commit: (id) sender;
@end

@implementation TMInputTextHelper
- (id) initWithWidget: (ns_input_text_widget_rep*) w
{
  self= [super init];
  if (self) wid= w;
  return self;
}
- (void) commit: (id) sender
{
  (void) sender;
  if (wid) wid->commit (true);
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

NSView*
ns_input_text_widget_rep::as_nsview () {
  // FIXME: completion with the proposals and file names
  NSTextField* f= (type == "password")
    ? [[[NSSecureTextField alloc] init] autorelease]
    : [[[NSTextField alloc] init] autorelease];
  [f setStringValue: to_label (input)];
  // NOTE: the helper lives as long as the text field
  TMInputTextHelper* h= [[TMInputTextHelper alloc] initWithWidget: this];
  [f setTarget: h];
  [f setAction: @selector(commit:)];
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
  ns_widget_rep (embedded_tm_widget), quit (_quit) {}

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
      break;
    default:
      ns_widget_rep::write (s, index, w);
  }
}

NSView*
ns_tm_embedded_widget_rep::as_nsview () {
  if (is_nil (main_widget)) return nil;
  return concrete (main_widget)->as_nsview ();
}

/******************************************************************************
* Color picker and printer (not implemented yet)
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
  // FIXME: use NSColorPanel
  NOT_IMPLEMENTED ("ns_color_picker_widget_rep::showDialog");
}

ns_printer_widget_rep::ns_printer_widget_rep (command cmd, url ps_pdf_file):
  ns_widget_rep (none), commandAfterExecution (cmd)
{ (void) ps_pdf_file; }

widget
ns_printer_widget_rep::plain_window_widget (string s, command q) {
  (void) s; (void) q;
  return this;
}

void
ns_printer_widget_rep::send (slot s, blackbox val) {
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
ns_printer_widget_rep::showDialog () {
  // FIXME: use NSPrintOperation on the PDF file
  NOT_IMPLEMENTED ("ns_printer_widget_rep::showDialog");
}
