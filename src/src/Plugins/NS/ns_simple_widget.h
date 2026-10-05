
/******************************************************************************
* MODULE     : ns_simple_widget.h
* DESCRIPTION: Aqua simple widget class
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NS_SIMPLE_WIDGET_H
#define NS_SIMPLE_WIDGET_H

#include "hashset.hpp"
#include "message.hpp"
#include "basic_renderer.hpp"

//#include "widget.hpp"
#include "ns_widget.h"

/*! A widget containing a TeXmacs canvas.
 
 This canvas can be used both for input or output of typesetted documents.
 Editors (editor_rep), output-only widgets (box_widget_rep) and
 other classes are extensions to a "simple_widget", quite a misnomer...
 
 */

class ns_simple_widget_rep: public ns_widget_rep {
  
  typedef struct t_slot_entry {
    int seq;
    slot_id id;
    blackbox val;
    t_slot_entry() : seq(-1), id (slot_id__LAST), val (blackbox()) { }
    t_slot_entry(const t_slot_entry& other)
    : seq (other.seq), id (other.id), val (other.val) { };
    bool operator< (const t_slot_entry& b) const { return this->seq < b.seq; }
    bool operator<= (const t_slot_entry& b) const { return this->seq <= b.seq; }
  } t_slot_entry;
  
  t_slot_entry sent_slots[slot_id__LAST];
  
  int sequencer;

  NSView *view;   //!< the canvas, which covers the visible part of doc
  NSView *doc;    //!< a view of the size of the document (for scrolling)
  
public:
  ns_simple_widget_rep ();
  ~ns_simple_widget_rep ();
	
  virtual bool is_editor_widget ();
  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                             array<double> data= array<double> ());
  virtual void handle_set_zoom_factor (double zoom);
  virtual void handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2);
  
  ////////////////////// Handling of TeXmacs' messages
  
  void save_send_slot (slot s, blackbox val);
  void reapply_sent_slots();
  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
  ////////////////////// NS semantics of abstract texmacs widgets

  virtual TMMenuItem *as_menuitem();
  virtual NSView* as_nsview ();

  NSBitmapImageRep *impress ();
  
  ////////////////////// backing store management
  
  static void repaint_all (); // called by ns_gui_rep::update()

public:
  
  // this needs to be accesible from TMView
  NSBitmapImageRep*       backingPixmap;
  int                     ring;  //!< the first row of the view in it
  void draw_backing_store (NSRect rect); //!< TMView drawRect
  void unroll_backing_store ();          //!< ring 0 (to save it, say)
  NSPoint                 cursor_pos;  //!< for the input methods
  void follow_visible_part ();  //!< the canvas follows the scrolling
  NSRect viewport ();           //!< the visible part, as in Qt
  coord4 extents;               //!< the extents of the document
  void invalidate_all ();       //!< also for the benchmark (ns_gui.mm)
  bool is_invalid ();

protected:
  
  static hashset<pointer> all_widgets;
  rectangles              invalid_regions;
  NSPoint                 backing_pos;
  NSSize                  last_viewport; // as notified to TeXmacs (resizes)
  
  void invalidate_rect (int x1, int y1, int x2, int y2);
  void repaint_invalid_regions ();
  void repaint_invalid_regions_bis ();
  void shift_backing_store (int dx, int dy);
  basic_renderer get_renderer ();


};

// Export for TeXmacs' use
typedef ns_simple_widget_rep simple_widget_rep;


#endif // defined NS_SIMPLE_WIDGET_H
