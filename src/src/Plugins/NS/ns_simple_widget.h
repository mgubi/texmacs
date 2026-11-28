
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
  int                     cring; //!< its first column
  // drawn by the GPU (gpu_layers_enabled, vue_gpu.hpp): the backing store
  // is a texture (gpu_store) instead of backingPixmap, shown in a
  // CAMetalLayer of the view, and scrolled by copies (ring and cring 0)
  picture                 gpu_store;
  renderer                gpu_ren;     //!< its renderer
  void*                   gpu_layer;   //!< the CAMetalLayer of the view
  void*                   gpu_presenter; //!< shows it once a refresh
  bool                    gpu_dirty;   //!< the layer shows an older store
  bool                    gpu_sync;    //!< a scroll: the next frame is shown
                                       //!< with its transaction
  double                  gpu_sync_last; //!< when it was last (ns_bench_now)
  // the pixels of the store per point: those of the screen of the window
  // of the canvas (1 or 2), each canvas its own (#989); the global
  // retina_factor is set to it while the canvas is painted (the renderers
  // and the pictures read it), and is otherwise the default (default_factor:
  // canvases in no window, the images of the widgets, Scheme)
  int                     factor;      //!< 0 until first painted
  int                     dbg_size[4]; //!< (TEXMACS_NS_DEBUG_DRAW)
  static int              default_factor;
  static bool             factor_fixed; //!< by the user (retina-factor...)
  int canvas_factor ();                //!< what it should be now
  void set_factor (int f);             //!< a new store, all repainted
  static bool gpu_mode ();
  bool has_store ();
  int store_width ();                  //!< in pixels
  int store_height ();
  void show_gpu_store ();              //!< at the next refresh
  bool present_gpu_store (bool in_transaction= false); //!< now (false:
                                       //!< to be done again)
  NSView* canvas_view () { return view; }
  bool backing_piece (int i, double a, double b, bool vertical,
                      double& pa, double& pb, double& off);
  void draw_backing_store (NSRect rect); //!< TMView drawRect
  void unroll_backing_store ();          //!< ring 0 (to save it, say)
  // the smooth zoom, as in the Vue interface: the picture of the canvas
  // when the zoom changes, carried to the new size and faded out over the
  // canvas repainted at the new zoom (see start_zoom_transition)
  double                  zoom_now;    //!< the zoom of the editor (or 0)
  bool                    zoom_pending; //!< until the repaint at the new zoom
  void*                   zoom_image;  //!< the picture of the canvas (CGImageRef)
  NSRect                  zoom_rect;   //!< where it was (in the scroll view)
  NSPoint                 zoom_origin; //!< the document then (same)
  double                  zoom_ratio;  //!< new zoom / old zoom
  void start_zoom_transition (double new_zoom);
  void run_zoom_transition ();
  NSPoint                 cursor_pos;  //!< for the input methods
  void follow_visible_part ();  //!< the canvas follows the scrolling
  NSRect viewport ();           //!< the visible part, as in Qt
  coord4 extents;               //!< the extents of the document
  bool too_large_said;          //!< its frame was cut at NS_HUGE_FRAME
  void invalidate_all ();       //!< also for the benchmark (ns_gui.mm)
  static void invalidate_every (); //!< all the widgets (change of theme)
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

#ifdef __OBJC__
#include "wheel_steps.hpp"

// The wheel scrolls the scroll view sv as a QAbstractScrollArea of the Qt
// port (TMView.mm, issue #971), by single steps of step points (0: those of
// an editor, a sixteenth of the view plus a point); x and y carry the
// fractions of a step. NO when the view cannot scroll that way (at its end):
// the event then goes to the enclosing views, as Qt ignores it
BOOL ns_wheel_like_qt (NSScrollView* sv, NSEvent* event, int step,
                       qt_wheel_axis& x, qt_wheel_axis& y);
bool ns_wheel_native ();  // TEXMACS_NS_WHEEL=native: as AppKit scrolls
bool ns_wheel_dropped (NSEvent* event); // as -[QNSView scrollWheel:]
int ns_wheel_mods (NSEvent* event);     // QT_WHEEL_..., latched (Qt)

/*! The scroll view of an editor: the wheel over its scroll bars scrolls
 them as QScrollBar, over its margins see ns_wheel_margins_scroll (TMView.mm) */
@interface TMEditorScrollView : NSScrollView
@end

/*! A scroll view of widgets, which the wheel scrolls as a QScrollArea of
 the Qt port: single steps of 20 points */
@interface TMScrollAreaView : NSScrollView
{
@public
  qt_wheel_axis wheelX, wheelY;
}
@end
#endif // defined __OBJC__


// the pixels of a canvas per point on a screen of this backingScaleFactor
int ns_canvas_factor_for (double backing_scale);

#endif // defined NS_SIMPLE_WIDGET_H
