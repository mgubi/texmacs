
/******************************************************************************
* MODULE     : ns_other_widgets.h
* DESCRIPTION: some aqua widgets class declarations
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NS_OTHER_WIDGETS_H
#define NS_OTHER_WIDGETS_H


#include "ns_widget.h"
#include "scheme.hpp"


@class TMWidgetHelper;
@class TMButtonsController;
@class TMWindowController;


class ns_window_widget_rep: public ns_widget_rep {
protected:
  int win_id;        //!< Unique integer identifier, returned by SLOT_IDENTIFIER.
  string orig_name;  //!< Unique name assigned to the window.
  command quit;      //!< Command to be executed when the window is closed.
  bool fake;         //!< Whether this truly is a window (or a docked widget).
  

public:
  TMWindowController *wc;
  
public:
  ns_window_widget_rep (ns_widget wid, string _name, command _quit,
                        bool _fake= false);
  ~ns_window_widget_rep ();
  
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
  //  virtual void connect (slot s, widget w2, slot s2);
  //  virtual void deconnect (slot s, widget w2, slot s2);
  
  TMWindowController *get_windowcontroller() { return wc; };
  virtual inline string get_nickname () { return orig_name; }
  command get_quit () { return quit; }
};

class ns_popup_widget_rep: public ns_widget_rep {
public:
  command quit;
  void* panel;  //!< the window (TMPopupPanel)
  bool tooltip; //!< a tooltip, which does not disappear with the mouse moves
  
  ns_popup_widget_rep (widget wid, command q);
  ~ns_popup_widget_rep ();
  
  virtual widget popup_window_widget (string s);
  
  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
};


class ns_tm_widget_rep: public ns_view_widget_rep {
public:	
  NSScrollView *sv;
	NSTextField *leftField, *rightField;
	TMButtonsController *bc;
	TMWidgetHelper *wh;
  NSArray *menu_items;  //!< the menus of the menu bar for this window
  bool full_screen;     //!< the full screen mode of TeXmacs (SLOT_FULL_SCREEN)
  
  ns_widget int_prompt;
  ns_widget int_input;
  NSView* prompt_view;  //!< the interactive prompt, in the footer

  // the interactive footer (the preference "interactive footer"): the
  // properties of the text at the cursor and the tags around it as menus,
  // see (texmacs menus footer-menu), in place of the texts of the footer
  // while the editor shows those (footer_menus) and not a message; the
  // rows are made again only when the expansions of the menus change, as
  // in the Vue interface
  bool footer_menus;
  object footer_env_menu, footer_path_menu; //!< their last expansions
  TMButtonsController *fc;   //!< makes the rows of buttons
  NSView *footer_env, *footer_path; //!< the rows (or nil)
  NSView *footer_clip;       //!< clips the tags on their left
  void update_footer_menus ();

  bool visibility[10];  //!< the bars and tools, as in qt_tm_widget_rep

public:
  command quit;
  widget main_widget;  //!< the canvas (the editor)
  widget tool_widgets[4]; //!< right, left, bottom and extra tools
  NSView* tool_views[4];  //!< their containers
  NSView* tool_handles[2];  //!< to resize the right and left tools
  double tool_widths[2];    //!< their widths chosen by the user (or 0)

  ns_tm_widget_rep (int mask, command _quit);
  ~ns_tm_widget_rep ();
	
	virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
	//  virtual void notify (slot s, blackbox new_val);
	
	//  virtual void connect (slot s, widget w2, slot s2);
	//  virtual void deconnect (slot s, widget w2, slot s2);
  virtual widget plain_window_widget (string s, command q);
	
	void layout();
  void updateVisibility();
  void do_interactive_prompt();
  void end_interactive_prompt();
  void install_main_menu ();
  void window_became_main ();
  void window_full_screen (bool flag);
};



/*! A simple texmacs input widget.
 
 This is a stripped down version of ns_tm_widget_rep, whose underlying widget
 isn't a QTMWindow anymore, but a regular QTMWidget because it is intended to be
 embedded somewhere else.
 
 */
class ns_tm_embedded_widget_rep: public ns_widget_rep {
  widget main_widget;
  NSView* container;  //!< holds the canvas, which may come later
  void show_canvas ();
  
public:
  command quit;
  
  ns_tm_embedded_widget_rep (command _quit);
  
  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  virtual void     write (slot s, blackbox index, widget w);
  
  virtual NSView* as_nsview ();
};



/*! A text input field with autocompletion.
 */
class ns_input_text_widget_rep: public ns_widget_rep {
protected:
  command             cmd;
  string             type;
  array<string> proposals;
  string            input;
  int               style;
  string            width;
  bool                 ok;
  
  bool         done;  //!< Has the command been executed after a modification?
  
public:
  ns_input_text_widget_rep (command _cmd, string _type, array<string> _proposals,
                            int _style, string _width);
  
  NSView* view;

  virtual NSView* as_nsview ();
  void commit (bool ok);
  void send_key (string s, string key);
  string field_type ();
  bool continuous ();
  bool can_autocommit ();
  string get_input () { return input; }
  bool is_ok () { return ok; }
  };

class ns_field_widget_rep;



/*! A dialog with a list of inputs and ok and cancel buttons.
 
 In the general case each input is a ns_field_widget_rep which we lay out in a
 vertical table. However, for simple yes/no/cancel questions we try to use a
 system default dialog
 
 TODO?
 We try to use OS dialogs whenever possible, but this still needs improvement.
 We should also use a custom Qt widget and then bundle it in a modal window if
 required, so as to eventually be able to return something embeddable in
 as_qwidget(), in case we want to reuse this.
 */
class ns_inputs_list_widget_rep: public ns_widget_rep {
protected:
  command cmd;
  coord2 size, position;
  string win_title;
  int style;
  
public:
  ns_inputs_list_widget_rep (command, array<string>);
  
  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
  virtual widget plain_window_widget (string s, command q);
  
protected:
  void perform_dialog();
  ns_field_widget_rep* field (int i);
};

/*! Each of the fields in a ns_inputs_list_widget_rep.
 
 Each field is composed of a prompt (a label) and an input (a QTMComboBox).
 */
class ns_field_widget_rep: public ns_widget_rep {
  string           prompt;
  string            input;
  string             type;
  array<string> proposals;
  ns_inputs_list_widget_rep* parent;
  
public:
  ns_field_widget_rep (ns_inputs_list_widget_rep* _parent, string _prompt);
  
  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  
  //virtual QWidget* as_qwidget ();
  
  friend class ns_inputs_list_widget_rep;
  friend class QTMFieldWidgetHelper;
};



/*!
 A file/directory chooser dialog, using native dialogs where available.
 See @link widget.cpp @endlink for an explanation of send(), query(),
 read(), etc.
 */
class ns_chooser_widget_rep: public ns_widget_rep {
protected:
  command cmd;           //!< Scheme closure to execute when the file is chosen
  command quit;          //!< Execute when the dialog closes.
  string type;           //!< File types to filter in the dialog
  string prompt;         //!< Is this a "Save" dialog?
  string win_title;      //!< Set by plain_window_widget()
  
  string directory; //!< Set this property sending SLOT_DIRECTORY to this widget
  coord2 position;  //!< Set this property sending SLOT_POSITION to this widget
  coord2 size;      //!< Set this property sending SLOT_SIZE to this widget
  string file;      //!< Set this property sending SLOT_FILE to this widget
  
  string filter_name;       //!< The name of the file type (for the filter)
  array<string> suffixes;   //!< Its suffixes (the first one is the default)
  
public:
  ns_chooser_widget_rep (command, string, string);
  
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual widget plain_window_widget (string s, command q);
  
  bool set_type (const string& _type);
  void perform_dialog();
};



/**
 * This implements a color picker widget, using the native dialogs where
 * available.
 *
 * The "factory" function for this widget is called color_picker_widget(),
 * in ns_dialogues.cpp
 *
 * Please @see ns_widget_rep for some important info.
 */
class ns_color_picker_widget_rep: public ns_widget_rep {
public:
  ns_color_picker_widget_rep (command, bool, array<tree>);
  ~ns_color_picker_widget_rep ();
  
  virtual void            send (slot s, blackbox val);
  widget   plain_window_widget (string s, command q);
  
  void showDialog();
  
protected:
  string            _windowTitle;
  command _commandAfterExecution;
  bool              _pickPattern;
};


/*!
 * This implements a printer widget, using QTMPrinterDialog.
 *
 * The "factory" function for this widget is called printer_widget(),
 * in ns_widget.cpp
 *
 * All printing options set by the user at this stage are applied as a
 * postprocessing of an already typeset postscript document.
 * Either we instruct the printing system to print specific pages, etc., or
 * we take the Postscript file generated by TeXmacs and create a new temporary
 * one applying the options set by the user in the print dialog and then send
 * this new file to the printer.
 *
 * @see ns_printer_widget_rep::showDialog()
 */
class ns_printer_widget_rep: public ns_widget_rep {
public:
  ns_printer_widget_rep (command, url);
  ~ns_printer_widget_rep () { };
  
  virtual void send (slot s, blackbox val);
  virtual widget plain_window_widget (string s, command q);
  
  void showDialog ();
  
private:
  command commandAfterExecution;    //! scheme closure to execute after printing
  url file;                         //! the PDF file to be printed
};


#endif // defined NS_OTHER_WIDGETS_H
