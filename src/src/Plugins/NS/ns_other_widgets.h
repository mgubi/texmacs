
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


@class TMWidgetHelper;
@class TMButtonsController;

class ns_tm_widget_rep: public ns_view_widget_rep {
public:	
  NSScrollView *sv;
	NSTextField *leftField, *rightField;
	TMButtonsController *bc;
	TMWidgetHelper *wh;
	NSToolbar *toolbar;
	
  
  ns_widget int_prompt;
  ns_widget int_input;

  bool visibility[5]; 

public:
  ns_tm_widget_rep (int mask = 0);
  ~ns_tm_widget_rep ();
	
	virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
	//  virtual void notify (slot s, blackbox new_val);
	
	//  virtual void connect (slot s, widget w2, slot s2);
	//  virtual void deconnect (slot s, widget w2, slot s2);
	virtual widget plain_window_widget (string s);
	
	void layout();
  void updateVisibility();
  void do_interactive_prompt();
};


@class TMWindowController;

class ns_window_widget_rep: public widget_rep {
public:	
	TMWindowController *wc;
	
public:
  ns_window_widget_rep (NSWindow *win);
  ~ns_window_widget_rep ();
	
	virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
	//  virtual void connect (slot s, widget w2, slot s2);
	//  virtual void deconnect (slot s, widget w2, slot s2);
	
	TMWindowController *get_windowcontroller();
};


#endif // defined NS_OTHER_WIDGETS_H
