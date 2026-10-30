
/******************************************************************************
 * MODULE     : ns_simple_widget.mm
 * DESCRIPTION: NextStep simple widget class
 * COPYRIGHT  : (C) 2018  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "merge_sort.hpp"
#include "basic.hpp"
#include "hashset.hpp"
#include "iterator.hpp"

#include "MacOS/mac_cocoa.h"
#include "ns_simple_widget.h"
#include "ns_utilities.h"
#include "ns_renderer.h"
#include "ns_gui.h"
#include "../Vue/vue_gpu.hpp"
#include "boot.hpp"        // get_preference
#include "scheme.hpp"      // call (the zoom of the editor)
#import <QuartzCore/QuartzCore.h> // the smooth zoom
#include <time.h>

// The benchmark (TEXMACS_NS_BENCH, ns_gui.mm): the time spent drawing into
// the backing stores (TeXmacs and the renderer), and showing them (the
// views drawing the backing stores into the window), in seconds, and the
// pixels drawn into the backing stores
double ns_bench_paint= 0.0, ns_bench_display= 0.0, ns_bench_pixels= 0.0;
double ns_bench_move= 0.0, ns_bench_repaint= 0.0;

double
ns_bench_now () {
  struct timespec ts;
  clock_gettime (CLOCK_MONOTONIC, &ts);
  return ts.tv_sec + 1e-9 * ts.tv_nsec;
}
#import "TMView.h"


/*! A view of the size of the document, in which the canvas (TMView) covers
 the visible part only, as the canvas of the Qt interface: its backing store
 has the size of the visible part, which scales to long documents. */
@interface TMDocView : NSView
{
@public
  ns_simple_widget_rep* wid;
}
- (void) scrolled: (NSNotification*) n;
@end

@implementation TMDocView
- (BOOL) isFlipped { return YES; }
- (BOOL) isOpaque { return NO; }
// NOTE: TeXmacs draws synchronously, like the Qt interface
+ (BOOL) isCompatibleWithResponsiveScrolling { return NO; }

// NOTE: in a list of widgets (Auto Layout), the size of the widget (the
// virtual keyboard, a texmacs-output, for instance), as the size hint of the
// QWidget in Qt; without it, the glues around it took all the width. The
// document view of the scroll view of an editor is sized by its frame.
- (NSSize) intrinsicContentSize
{
  if (!wid || [[self superview] isKindOfClass: [NSClipView class]])
    return NSMakeSize (NSViewNoIntrinsicMetric, NSViewNoIntrinsicMetric);
  SI w= 0, h= 0;
  wid->handle_get_size_hint (w, h);
  return to_nssize (w, h);
}

- (void) viewDidMoveToSuperview
{
  // Follow the scrolling and the resizing of the clip view (see
  // QTMWidget::scrollContentsBy)
  [[NSNotificationCenter defaultCenter] removeObserver: self];
  NSView* clip= [self superview];
  if ([clip isKindOfClass: [NSClipView class]]) {
    [clip setPostsBoundsChangedNotifications: YES];
    [clip setPostsFrameChangedNotifications: YES];
    [[NSNotificationCenter defaultCenter]
      addObserver: self selector: @selector(scrolled:)
             name: NSViewBoundsDidChangeNotification object: clip];
    [[NSNotificationCenter defaultCenter]
      addObserver: self selector: @selector(scrolled:)
             name: NSViewFrameDidChangeNotification object: clip];
  }
}

- (void) dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver: self];
  [super dealloc];
}

- (void) scrolled: (NSNotification*) n
{
  // The canvas moves to the visible part, and TeXmacs updates at once so
  // that the canvas is repainted before it is displayed
  // NOTE: not while the window is being built (there may be no editor yet)
  (void) n;
  if (!wid) return;
  wid->follow_visible_part ();
  if (wid->has_store () && [NSApp isRunning] && [self window])
    the_gui->force_update ();
}
@end

/*! What shows the backing store of a canvas drawn by the GPU, once a
 refresh of its screen: a repaint only marks the canvas (show_gpu_store),
 and the presenter, woken up, copies the store into the layer of the view
 and presents it at the next refresh, then sleeps until the next mark. To
 present at each repaint made the repaints wait for the screen: a layer has
 three drawables, which the compositor gives back when it has shown them,
 so that more repaints than refreshes blocked in nextDrawable; AppKit
 likewise draws the views marked as needing display once a refresh. Driven
 by a CADisplayLink of the view (macOS 14), which follows the refreshes of
 its screen, or else by a timer at 120 Hz. */
@interface TMGpuPresenter : NSObject
{
@public
  ns_simple_widget_rep* wid;
  id link;              // a CADisplayLink (macOS 14), or nil
  NSTimer* timer;       // else
}
- (void) wake;
- (void) stop;
- (void) tick: (id) sender;
@end

@implementation TMGpuPresenter
- (void) wake
{
  if (!wid || !wid->canvas_view ()) return;
  if (@available (macOS 14.0, *)) {
    if (link == nil) {
      // (made through AppKit, so that the class itself is not linked to)
      link= [[(id) wid->canvas_view () displayLinkWithTarget: self
                                          selector: @selector(tick:)] retain];
      [link addToRunLoop: [NSRunLoop mainRunLoop] forMode: NSRunLoopCommonModes];
    }
    [link setPaused: NO];
    return;
  }
  if (timer == nil) {
    timer= [[NSTimer timerWithTimeInterval: 1.0 / 120 target: self
                                  selector: @selector(tick:)
                                  userInfo: nil repeats: YES] retain];
    [[NSRunLoop mainRunLoop] addTimer: timer forMode: NSRunLoopCommonModes];
  }
}

- (void) sleep
{
  if (link != nil) [link setPaused: YES];
  if (timer != nil) { [timer invalidate]; [timer release]; timer= nil; }
}

- (void) stop
{
  wid= NULL;
  [self sleep];
  if (link != nil) { [link invalidate]; [link release]; link= nil; }
}

- (void) tick: (id) sender
{
  (void) sender;
  static int dbg= -1;
  if (dbg < 0) dbg= (getenv ("TEXMACS_NS_DEBUG_DRAW") != NULL);
  if (!wid || !wid->gpu_dirty) { [self sleep]; return; }
  double t= ns_bench_now ();
  bool done= false;
  NS_ENTRY_BEGIN
    if (wid->present_gpu_store ()) { wid->gpu_dirty= false; [self sleep]; }
    done= true;
  NS_ENTRY_END ("the presentation of a canvas drawn by the GPU")
  if (!done) { wid->gpu_dirty= false; [self sleep]; }
  ns_bench_display += ns_bench_now () - t;
  if (dbg) fprintf (stderr, "PRESENT %.3f ms at %.3f\n", 1000 * (ns_bench_now () - t), t);
}
@end

ns_simple_widget_rep::ns_simple_widget_rep ()
: ns_widget_rep (simple_widget),  sequencer (0), view (nil), doc (nil),
  backingPixmap (nil), ring (0), cring (0), gpu_ren (NULL), gpu_layer (NULL),
  gpu_presenter (NULL), gpu_dirty (false),
  zoom_now (0), zoom_pending (false), zoom_image (NULL),
  zoom_rect (NSZeroRect), zoom_origin (NSZeroPoint), zoom_ratio (1),
  extents (coord4 (0, 0, 0, 0)),
  last_viewport (NSZeroSize) { }

ns_simple_widget_rep::~ns_simple_widget_rep () {
  all_widgets->remove ((pointer) this);
  if (view) {
    [(TMView*) view setWidget: NULL];
    [view release];
  }
  if (doc) {
    ((TMDocView*) doc)->wid= NULL;
    [doc release];
  }
  [backingPixmap release];
  if (zoom_image) CGImageRelease ((CGImageRef) zoom_image);
  if (gpu_ren != NULL) tm_delete (gpu_ren);
  if (gpu_presenter != NULL) {
    [(TMGpuPresenter*) gpu_presenter stop];
    [(TMGpuPresenter*) gpu_presenter release];
  }
  if (gpu_layer != NULL) [(CALayer*) gpu_layer release];
}

/******************************************************************************
* The backing store on the GPU (Metal: gpu_layers_enabled)
******************************************************************************/

bool
ns_simple_widget_rep::gpu_mode () {
  return gpu_layers_enabled ();
}

bool
ns_simple_widget_rep::has_store () {
  return gpu_mode () ? !is_nil (gpu_store) : backingPixmap != nil;
}

int
ns_simple_widget_rep::store_width () {
  if (gpu_mode ()) return is_nil (gpu_store) ? 0 : gpu_store->get_width ();
  return backingPixmap ? (int) [backingPixmap pixelsWide] : 0;
}

int
ns_simple_widget_rep::store_height () {
  if (gpu_mode ()) return is_nil (gpu_store) ? 0 : gpu_store->get_height ();
  return backingPixmap ? (int) [backingPixmap pixelsHigh] : 0;
}

// the snapshots (TEXMACS_NS_SNAPSHOT) draw the views, which do not show
// the CAMetalLayer of a canvas: its store is read back and drawn over the
// image of the root view, where the canvas is
static void
draw_gpu_canvases (NSView* root, NSView* v, NSBitmapImageRep* rep) {
  if ([NSStringFromClass ([v class]) isEqualToString: @"TMView"]) {
    ns_simple_widget_rep* wid= (ns_simple_widget_rep*) [(TMView*) v widget];
    NSRect vis= [v visibleRect];
    if (wid == NULL || !wid->has_store () || NSIsEmptyRect (vis)) return;
    NSRect r= [v convertRect: [v bounds] toView: root];
    NSGraphicsContext* gc= [NSGraphicsContext graphicsContextWithBitmapImageRep: rep];
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext setCurrentContext: gc];
    CGContextRef cgc= [gc CGContext];
    // the coordinates of the canvas (flipped), within the root view
    if ([root isFlipped]) CGContextTranslateCTM (cgc, r.origin.x, r.origin.y);
    else {
      CGContextTranslateCTM (cgc, r.origin.x, r.origin.y + r.size.height);
      CGContextScaleCTM (cgc, 1, -1);
    }
    wid->draw_backing_store (vis);
    [NSGraphicsContext restoreGraphicsState];
    return;
  }
  for (NSView* w in [v subviews]) draw_gpu_canvases (root, w, rep);
}

void
ns_draw_gpu_canvases (NSView* root, NSBitmapImageRep* rep) {
  if (!ns_simple_widget_rep::gpu_mode () || root == nil || rep == nil) return;
  draw_gpu_canvases (root, root, rep);
}

void
ns_simple_widget_rep::show_gpu_store () {
  // at the next refresh of the screen (TMGpuPresenter)
  if (!view || is_nil (gpu_store)) return;
  gpu_dirty= true;
  if (gpu_presenter == NULL) {
    TMGpuPresenter* p= [[TMGpuPresenter alloc] init];
    p->wid= this;
    gpu_presenter= (void*) p;
  }
  [(TMGpuPresenter*) gpu_presenter wake];
}

bool
ns_simple_widget_rep::present_gpu_store () {
  // the view hosts a CAMetalLayer (made when first shown: by name, so that
  // a build without Metal needs no QuartzCore), into which the store is
  // copied, scaled when the pixels of the screen are not those of the store
  // (a retina_factor of 1 on a Retina screen); nothing to do while the view
  // is not in a window (it is shown when it is put back in one)
  if (!view || is_nil (gpu_store) || ![view window]) return true;
  if (gpu_layer == NULL) {
    Class mlc= NSClassFromString (@"CAMetalLayer");
    if (mlc == nil) return;
    CALayer* l= [[mlc alloc] init];
    [l setOpaque: NO];
    [view setLayer: l];
    [view setWantsLayer: YES];
    gpu_layer= (void*) l;
  }
  CALayer* l= (CALayer*) gpu_layer;
  double sc= [[view window] backingScaleFactor];
  if ([l contentsScale] != sc) [l setContentsScale: sc];
  NSSize sz= [view bounds].size;
  int dw= (int) ceil (sz.width * sc), dh= (int) ceil (sz.height * sc);
  // (a failure is not tried again before the next repaint)
  gpu_show_picture (gpu_layer, gpu_store, dw, dh);
  return true;
}

NSRect
ns_simple_widget_rep::viewport () {
  // The part of the document seen in the scroll view, with its full size
  // (the document is centered when it is smaller, see TMClipView), or the
  // whole document for the canvases without a scroll view
  if (!doc) return NSZeroRect;
  NSView* clip= [doc superview];
  if ([clip isKindOfClass: [NSClipView class]]) {
    NSRect r= [clip bounds];
    r.origin.x= max (r.origin.x, (CGFloat) 0);
    r.origin.y= max (r.origin.y, (CGFloat) 0);
    return r;
  }
  return [doc bounds];
}

void
ns_simple_widget_rep::follow_visible_part () {
  // The canvas covers the visible part of the document view
  if (!view || !doc) return;
  // NOTE: nothing is visible before the document view is shown (its bounds
  // can be huge, for the embedded widgets with infinite extents)
  if (![doc window]) return;
  NSRect r= NSIntersectionRect ([doc visibleRect], [doc bounds]);
  if (NSIsEmptyRect (r)) return;
  if (!NSEqualRects (r, [view frame])) [view setFrame: r];
  // NOTE: as QTMWidget::resizeEventBis, TeXmacs is told when the viewport
  // changes its size (the documents whose size follows the one of the
  // window, as the papyrus mode, are laid out again)
  NSSize vs= viewport ().size;
  if (!NSEqualSizes (vs, last_viewport)) {
    last_viewport= vs;
    coord2 p= from_nssize (vs);
    the_gui->process_resize (this, p.x1, p.x2);
  }
}

/*! The view of the canvas, created when it is needed for the first time
 (see qt_simple_widget_rep::as_qwidget).
 */
NSView*
ns_simple_widget_rep::as_nsview () {
  if (doc) return doc;
  SI width, height;
  handle_get_size_hint (width, height);
  NSSize sz = to_nssize (coord2 (width, height));
  doc= [[TMDocView alloc] initWithFrame: NSMakeRect (0, 0, sz.width, sz.height)];
  ((TMDocView*) doc)->wid= this;
  TMView* v= [[TMView alloc] initWithFrame: NSMakeRect (0, 0, sz.width, sz.height)];
  [v setWidget: this];
  [doc addSubview: v];
  view= v;
  reapply_sent_slots ();
  all_widgets->insert ((pointer) this);
  backing_pos= [view frame].origin;
  return doc;
}

#if 0
QWidget*
ns_simple_widget_rep::as_qwidget () {
  qwid = new QTMWidget (0, this);
  reapply_sent_slots();
  SI width, height;
  handle_get_size_hint (width, height);
  QSize sz = to_qsize (width, height);
  scrollarea()->editor_flag= is_editor_widget ();
  scrollarea()->setExtents (QRect (QPoint(0,0), sz));
  canvas()->resize (sz);
  
  
  all_widgets->insert((pointer) this);
  backing_pos = canvas()->origin ();
  
  return qwid;
}
#endif

/******************************************************************************
 * Empty handlers for redefinition by our subclasses editor_rep,
 * box_widget_rep...
 ******************************************************************************/

bool
ns_simple_widget_rep::is_editor_widget () {
  return false;
}

void
ns_simple_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  gui_root_extents (w, h);
}

void
ns_simple_widget_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
}

void
ns_simple_widget_rep::handle_keypress (string key, time_t t) {
  (void) key; (void) t;
}

void
ns_simple_widget_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  (void) has_focus; (void) t;
}

void
ns_simple_widget_rep::handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                                    array<double> data) {
  (void) kind; (void) x; (void) y; (void) mods; (void) t; (void) data;
}

void
ns_simple_widget_rep::handle_set_zoom_factor (double zoom) {
  (void) zoom;
}

void
ns_simple_widget_rep::handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2) {
  (void) win; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
ns_simple_widget_rep::handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2) {
  (void) win; (void) x1; (void) y1; (void) x2; (void) y2;
}


/******************************************************************************
 * Handling of TeXmacs messages
 ******************************************************************************/

/*! Stores messages (SLOTS) sent to this widget for later replay.
 
 This is useful for recompilation of the QWidget inside as_qwidget() in some
 cases, where state information of the parsed widget (i.e. the qt_widget) is
 stored by us directly in the QWidget, and thus is lost if we delete it.
 
 Each SLOT is stored only once, repeated occurrences of the same one overwriting
 previous ones. Sequence information is also stored, allowing for correct replay.
 */
void
ns_simple_widget_rep::save_send_slot (slot s, blackbox val) {
  sent_slots[s].seq = sequencer;
  sent_slots[s].val = val;
  sent_slots[s].id  = s.sid;
  sequencer = (sequencer + 1) % slot_id__LAST;
}

void
ns_simple_widget_rep::reapply_sent_slots () {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << ">>>>>>>> reapply_sent_slots() for widget: " << type_as_string() << LF;
  
//  t_slot_entry sorted_slots[slot_id__LAST];
  array<t_slot_entry> sorted_slots (sent_slots, slot_id__LAST);
  merge_sort (sorted_slots);
 // for (int i = 0; i < slot_id__LAST; ++i)
 //   sorted_slots[i] = sent_slots[i];
//  qSort (&sorted_slots[0], &sorted_slots[slot_id__LAST]);
  
  for (int i = 0; i < slot_id__LAST; ++i)
    if (sorted_slots[i].seq >= 0)
      this->send (sorted_slots[i].id, sorted_slots[i].val);
  
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "<<<<<<<< reapply_sent_slots() for widget: " << type_as_string() << LF;
}

void
ns_simple_widget_rep::send (slot s, blackbox val) {
  save_send_slot (s, val);
  
  switch (s) {
    case SLOT_INVALIDATE:
    {
      check_type<coord4> (val, s);
      coord4 p= open_box<coord4> (val);
      
      {
        ns_renderer_rep* ren = the_ns_renderer ();
        {
          coord2 pt_or = from_nspoint (backing_pos);
          SI ox = -pt_or.x1;
          SI oy = -pt_or.x2;
          ren->set_origin (ox,oy);
        }
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        invalidate_rect (x1, y2, x2, y1);
      }
    }
      break;
      
    case SLOT_INVALIDATE_ALL:
    {
      check_type_void (val, s);
      invalidate_all ();
    }
      break;
      
    case SLOT_EXTENTS:
    {
      check_type<coord4>(val, s);
      coord4 p = open_box<coord4> (val);
      extents= p;
      NSRect rect = to_nsrect (p);
      // NOTE: the scroll view centers the document when it is smaller; the
      // sizes remain within the limits of AppKit (some embedded widgets have
      // infinite extents)
      rect.size.width = min (rect.size.width , 5000000.0);
      rect.size.height= min (rect.size.height, 5000000.0);
      {
        // NOTE: a page which follows the size of the clip view (a page
        // "automatic") is up to a point larger, from the roundings between
        // points and TeXmacs units: it is the size of the clip view (else
        // it scrolls by a point, and has a scroll bar)
        NSView* clip= [doc superview];
        if ([clip isKindOfClass: [NSClipView class]]) {
          NSSize cs= [clip bounds].size;
          if (rect.size.width > cs.width && rect.size.width <= cs.width + 1)
            rect.size.width= cs.width;
          if (rect.size.height > cs.height && rect.size.height <= cs.height + 1)
            rect.size.height= cs.height;
        }
      }
      [doc setFrameSize: rect.size];
      {
        // NOTE: the clip view centers the document when it scrolls; a
        // document which became smaller (a zoom out) is centered at once
        NSScrollView* sv= [doc enclosingScrollView];
        NSClipView* cv= [sv contentView];
        if (cv) {
          NSPoint o= [cv constrainBoundsRect: [cv bounds]].origin;
          if (!NSEqualPoints (o, [cv bounds].origin)) {
            [cv scrollToPoint: o];
            [sv reflectScrolledClipView: cv];
          }
        }
      }
      follow_visible_part ();
    }
      break;
      
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
      coord2 sz = open_box<coord2> (val);
      [doc setFrameSize: to_nssize (sz)]; // FIXME?
      follow_visible_part ();
    }
      break;
      
    case SLOT_SCROLL_POSITION:
    {
      check_type<coord2>(val, s);
      coord2  p = open_box<coord2> (val);
      // NOTE: p is the center of the visible part (see qt_simple_widget_rep)
      NSPoint pt = to_nspoint(p);
      NSSize sz = [doc visibleRect].size;
      pt.y -= sz.height/2;
      pt.x -= sz.width/2;
      [doc scrollPoint: pt];
      follow_visible_part ();
    }
      break;
      
    case SLOT_ZOOM_FACTOR:
    {
      check_type<double> (val, s);
      double new_zoom = open_box<double> (val);
      start_zoom_transition (new_zoom);
      handle_set_zoom_factor (new_zoom);
    }
      break;
      
    case SLOT_MOUSE_GRAB:
    {
      check_type<bool> (val, s);
      bool grab = open_box<bool>(val);
      // as in the Qt interface, the canvas gets the focus
      if (grab && view && [view window] && [[view window] firstResponder] != view)
        [[view window] makeFirstResponder: view];
    }
      break;
      
    case SLOT_MOUSE_POINTER:
    {
      typedef pair<string, string> T;
      check_type<T> (val, s);
      T contents = open_box<T> (val); // x1 = name, x2 = mask.
      /*
       if (contents.x2 == "")   // mask == ""
       ;                      // set default pointer.
       else                     // set new pointer
       ;
       */
      NOT_IMPLEMENTED ("ns_simple_widget::SLOT_MOUSE_POINTER");
    }
      break;
      
    case SLOT_CURSOR:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
      cursor_pos= to_nspoint (p);
    }
      break;
      
    default:
      ns_widget_rep::send(s, val);
      return;
  }
  
  if (DEBUG_QT_WIDGETS && s != SLOT_INVALIDATE)
    debug_widgets << "ns_simple_widget_rep: sent " << slot_name (s)
    << "\t\tto widget\t" << type_as_string() << LF;
}

blackbox
ns_simple_widget_rep::query (slot s, int type_id) {
  // Some slots are too noisy
  if (DEBUG_QT_WIDGETS && (s != SLOT_IDENTIFIER))
    debug_widgets << "ns_simple_widget_rep: queried " << slot_name(s)
    << "\t\tto widget\t" << type_as_string() << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      // as in Qt: the identifier of the window which shows the canvas (the
      // editor is attached to it; without it, the editor did not follow the
      // zoom, nor the size of the window)
      if (view && [view window]) {
        widget w= ns_window_widget_of ([view window]);
        if (!is_nil (w)) return w->query (s, type_id);
      }
      if (parent)
        return parent->query (s, type_id);
      else
        return close_box<int>(0);
    }
    case SLOT_INVALID:
    {
      return close_box<bool> (is_invalid ());
    }
      
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      // The position of the canvas in its window, from the top left corner
      // of the window (see qt_simple_widget_rep)
      if (!doc || ![doc window]) return close_box<coord2> (coord2 (0, 0));
      NSRect r= [doc convertRect: [doc visibleRect] toView: nil];
      NSRect f= [[doc window] frame];
      NSPoint pt= NSMakePoint (r.origin.x, f.size.height - NSMaxY (r));
      return close_box<coord2> (from_nspoint (pt));
    }
      
    // NOTE: as in the Qt interface, the size and the visible part are those
    // of the viewport (the window on the document), which does not depend
    // on the extents of the document; otherwise the documents whose width
    // follows the one of the window are typeset again and again
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_nssize (viewport ().size));
    }
      
    case SLOT_SCROLL_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_nspoint (viewport ().origin));
    }
      
    case SLOT_EXTENTS:
    {
      check_type_id<coord4> (type_id, s);
      return close_box<coord4> (extents);
    }
      
    case SLOT_VISIBLE_PART:
    {
      check_type_id<coord4> (type_id, s);
      if (!doc) return close_box<coord4> (coord4 (0, 0, 0, 0));
      return close_box<coord4> (from_nsrect (viewport ()));
    }
      
    default:
      return ns_widget_rep::query(s, type_id);
  }
}

widget
ns_simple_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_simple_widget_rep::read " << slot_name(s)
    << "\tWidget id: " << id << LF;
  
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (index, s);
      if (view && [view window]) return ns_window_widget_of ([view window]);
      if (parent) return parent->read(s, index);
      return widget ();
    default:
      return ns_widget_rep::read (s, index);
  }
}


/******************************************************************************
 * Translation into QAction for insertion in menus (i.e. for buttons)
 ******************************************************************************/

// Prints the current contents of the canvas onto a bitmap (autoreleased),
// with retina_factor pixels per point, and whose size is in points
NSBitmapImageRep*
ns_simple_widget_rep::impress () {
  SI width, height;
  handle_get_size_hint (width, height);
  NSSize s = to_nssize (coord2 (width, height));
  NSInteger pw= max ((NSInteger) ceil (s.width * retina_factor), (NSInteger) 1);
  NSInteger ph= max ((NSInteger) ceil (s.height * retina_factor), (NSInteger) 1);
  NSBitmapImageRep* im =
  [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                          pixelsWide: pw
                                          pixelsHigh: ph
                                       bitsPerSample: 8
                                     samplesPerPixel: 4
                                            hasAlpha: YES
                                            isPlanar: NO
                                      colorSpaceName: NSDeviceRGBColorSpace
                                         bytesPerRow: 4 * pw
                                        bitsPerPixel: 32];
  [im autorelease];
  // transparent fill
  memset ([im bitmapData], 0, [im bytesPerRow] * ph);
  if (DEBUG_QT)
    debug_qt << "impress (" << s.width << "," << s.height << ")\n";
  NSGraphicsContext* cg = [NSGraphicsContext graphicsContextWithBitmapImageRep: im];
  {
    ns_renderer_rep *ren = the_ns_renderer();
    ren->begin (cg);
    rectangle r = rectangle (0, 0, pw, ph);
    ren->set_origin (0, 0);
    ren->encode (r->x1, r->y1);
    ren->encode (r->x2, r->y2);
    ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
    {
      // we do not want to be interrupted here...
      the_gui->set_check_events (false);
      handle_repaint (ren, r->x1, r->y2, r->x2, r->y1);
      the_gui->set_check_events (true);
    }
    ren->end();
  }
  // The renderer draws with y going down in a context where it goes up, so
  // that the rows are reversed (see ns_image_renderer_rep)
  {
    NSInteger bpr= [im bytesPerRow];
    unsigned char* data= [im bitmapData];
    STACK_NEW_ARRAY (row, unsigned char, bpr);
    for (NSInteger i=0; i < ph/2; i++) {
      memcpy (row, data + i*bpr, bpr);
      memcpy (data + i*bpr, data + (ph-1-i)*bpr, bpr);
      memcpy (data + (ph-1-i)*bpr, row, bpr);
    }
    STACK_DELETE_ARRAY (row);
  }
  [im setSize: NSMakeSize (pw / (double) retina_factor,
                           ph / (double) retina_factor)];
  return im;
}

/******************************************************************************
 * Backing store management
 ******************************************************************************/


void
ns_simple_widget_rep::invalidate_rect (int x1, int y1, int x2, int y2) {
  if (x1 >= x2 || y1 >= y2) return;
  rectangle r = rectangle (x1, y1, x2, y2);
  // cout << "invalidating " << r << LF;
  invalid_regions = invalid_regions | rectangles (r);
}

void
ns_simple_widget_rep::invalidate_all () {
  // NOTE: the backing store has the size of the visible part
  NSSize sz= view? [view frame].size: NSZeroSize;
  invalid_regions = rectangles();
  invalidate_rect (0, 0, (int) ceil (sz.width * retina_factor),
                   (int) ceil (sz.height * retina_factor));
  //QSize sz = canvas()->surface()->size();
  //cout << "invalidate all " << LF;
  //invalidate_rect (0, 0, retina_factor * sz.width(),
  //                 retina_factor * sz.height());
}

bool
ns_simple_widget_rep::is_invalid () {
  return !is_nil (invalid_regions);
}




basic_renderer
ns_simple_widget_rep::get_renderer() {
  if (gpu_mode ()) {
    if (gpu_ren == NULL)
      gpu_ren= gpu_picture_renderer (gpu_store, std_shrinkf * retina_factor);
    return (basic_renderer) gpu_ren;
  }
  ns_renderer_rep * ren = the_ns_renderer();
  ren->begin ([NSGraphicsContext graphicsContextWithBitmapImageRep: backingPixmap]);
  return ren;
}

/*
 This function is called by the ns_gui::update method (via repaint_all) to keep
 the backing store in sync and propagate the changes to the surface on screen.
 First we check that the backing store geometry is right and then we
 request to the texmacs canvas widget to repaint the regions which were
 marked invalid. Subsequently, for each succesfully repainted region, we
 propagate its contents from the backing store to the onscreen surface.
 If repaint has been interrupted we do not propagate the changes and proceed
 to mark the region invalid again.
 */

inline float mmin (float a, float b) { return (a>b? b: a); }
inline float mmax (float a, float b) { return (a>b? a: b); }

// NOTE: the backing store wraps around in both directions: the pixel
// (x, y) of the view (in the pixels of the backing store, from the top
// left, as the invalid rectangles and the device coordinates of the
// renderer) is the pixel ((x + cring) mod W, (y + ring) mod H) of the
// backing store (W x H its size, in the same coordinates; in its memory,
// the row is H-1 - that, since the backing store is drawn upside down). A
// scroll only changes cring and ring: moving all the pixels (a new backing
// store and a copy, or a memmove: about 1 ms for 2000x1300 pixels) took
// most of a step of a scroll. The views draw the backing store, and the
// canvas paints into it, in up to four pieces where it wraps.

static unsigned int
uncovered_pixel () {
  // what is uncovered: transparent until it is repainted (red with
  // TEXMACS_NS_DEBUG_RED; NSBitmapFormatAlphaFirst, the bytes A R G B)
  static int red= -1;
  if (red < 0) red= (getenv ("TEXMACS_NS_DEBUG_RED") != NULL);
  unsigned int fill= 0;
  if (red) {
    unsigned char px[4]= { 255, 255, 0, 0 };
    memcpy (&fill, px, 4);
  }
  return fill;
}

void
ns_simple_widget_rep::shift_backing_store (int dx, int dy) {
  // the pixel (x, y) of the view gets the pixel (x + dx, y + dy)
  unsigned char* data= [backingPixmap bitmapData];
  int W= (int) [backingPixmap pixelsWide], H= (int) [backingPixmap pixelsHigh];
  int bpr= (int) [backingPixmap bytesPerRow];
  if (!data || W < 1 || H < 1) return;
  unsigned int fill= uncovered_pixel ();
  if (dx != 0) {
    cring= (((cring + dx) % W) + W) % W;
    int x0= dx > 0? max (W - dx, 0): 0, x1= dx > 0? W: min (-dx, W);
    for (int r= 0; r < H; r++) {
      unsigned int* row= (unsigned int*) (data + r*bpr);
      for (int x= x0; x < x1; x++) row[(x + cring) % W]= fill;
    }
  }
  if (dy != 0) {
    ring= (((ring + dy) % H) + H) % H;
    int y0= dy > 0? max (H - dy, 0): 0, y1= dy > 0? H: min (-dy, H);
    for (int y= y0; y < y1; y++) {
      unsigned int* row= (unsigned int*) (data + (H-1 - (y + ring) % H)*bpr);
      for (int c= 0; c < W; c++) row[c]= fill;
    }
  }
}

void
ns_simple_widget_rep::unroll_backing_store () {
  // the pixels in the order of the view (cring and ring 0)
  if ((ring == 0 && cring == 0) || !backingPixmap) return;
  unsigned char* data= [backingPixmap bitmapData];
  int W= (int) [backingPixmap pixelsWide], H= (int) [backingPixmap pixelsHigh];
  int bpr= (int) [backingPixmap bytesPerRow];
  if (!data || W < 1 || H < 1) { ring= cring= 0; return; }
  unsigned char* old= (unsigned char*) malloc (bpr * H);
  memcpy (old, data, bpr * H);
  for (int y= 0; y < H; y++) {
    unsigned char* to= data + (H-1 - y)*bpr;
    unsigned char* from= old + (H-1 - (y + ring) % H)*bpr;
    // the columns [0, W - cring) of the view, then [W - cring, W)
    memcpy (to, from + 4*cring, 4*(W - cring));
    memcpy (to + 4*(W - cring), from, 4*cring);
  }
  free (old);
  ring= cring= 0;
}

bool
ns_simple_widget_rep::backing_piece (int i, double a, double b, bool vertical,
                                     double& pa, double& pb, double& off) {
  // the piece i (0 or 1) of [a, b) (in the pixels of the view), before and
  // after where the backing store wraps, and how far it is moved in it
  double N= vertical? store_height (): store_width ();
  double o= vertical? ring: cring;
  double cut= N - o;
  pa= i == 0? a: max (a, cut);
  pb= i == 0? min (b, cut): b;
  off= i == 0? o: o - N;
  return pb > pa;
}

void
ns_simple_widget_rep::draw_backing_store (NSRect rect) {
  if (gpu_mode ()) {
    // (shown in the layer: drawn here for the snapshots, which draw the
    // views) the store read back, its rows from the top as in the view
    int W= store_width (), H= store_height ();
    if (W < 1 || H < 1) return;
    NSMutableData* buf= [NSMutableData dataWithLength: (NSUInteger) W * H * 4];
    if (!gpu_read_picture (gpu_store, (unsigned char*) [buf mutableBytes])) return;
    CGDataProviderRef prov= CGDataProviderCreateWithCFData ((CFDataRef) buf);
    CGColorSpaceRef cs= CGColorSpaceCreateDeviceRGB ();
    CGImageRef img= CGImageCreate (W, H, 8, 32, W * 4, cs,
                                   (uint32_t) kCGImageAlphaPremultipliedLast |
                                   (uint32_t) kCGBitmapByteOrder32Big,
                                   prov, NULL, false, kCGRenderingIntentDefault);
    CGColorSpaceRelease (cs);
    CGDataProviderRelease (prov);
    if (!img) return;
    CGContextRef cgc= [[NSGraphicsContext currentContext] CGContext];
    double k= retina_factor;
    CGContextSaveGState (cgc);
    CGContextClipToRect (cgc, NSRectToCGRect (rect));
    // the view is flipped: the image upside down
    CGContextTranslateCTM (cgc, 0, H / k);
    CGContextScaleCTM (cgc, 1, -1);
    CGContextDrawImage (cgc, CGRectMake (0, 0, W / k, H / k), img);
    CGContextRestoreGState (cgc);
    CGImageRelease (img);
    return;
  }
  // the part rect of the view (in points), in at most four pieces
  // NOTE: through an image which reads the pixels of the backing store
  // where they are: drawing the NSBitmapImageRep made its memory copy on
  // write, so that the first change of each of its pages after it was
  // shown copied the page (0.7 ms for the columns uncovered by a step of
  // a horizontal scroll, which touch all the pages, 2000x1100 pixels)
  unsigned char* data= [backingPixmap bitmapData];
  int W= (int) [backingPixmap pixelsWide], H= (int) [backingPixmap pixelsHigh];
  int bpr= (int) [backingPixmap bytesPerRow];
  if (!data || W < 1 || H < 1) return;
  CGContextRef cgc= [[NSGraphicsContext currentContext] CGContext];
  CGDataProviderRef prov= CGDataProviderCreateWithData (NULL, data, bpr * H, NULL);
  CGColorSpaceRef cs= CGColorSpaceCreateDeviceRGB ();
  // NSBitmapFormatAlphaFirst, premultiplied: the bytes A R G B
  CGImageRef img= CGImageCreate (W, H, 8, 32, bpr, cs,
                                 kCGImageAlphaPremultipliedFirst |
                                 kCGBitmapByteOrder32Big,
                                 prov, NULL, false, kCGRenderingIntentDefault);
  CGColorSpaceRelease (cs);
  CGDataProviderRelease (prov);
  if (!img) return;
  double k= retina_factor;
  // only what the backing store holds: the rectangle asked for may go
  // beyond the view (the snapshots ask for the whole document view when it
  // is centered in a wider clip view, from x < 0), and the part outside the
  // store, cropped from the image, was drawn shifted and wrapped around
  double x0= max (rect.origin.x * k, 0.0);
  double x1= min ((rect.origin.x + rect.size.width) * k, (double) W);
  double y0= max (rect.origin.y * k, 0.0);
  double y1= min ((rect.origin.y + rect.size.height) * k, (double) H);
  for (int i= 0; i < 2; i++) {
    double xa, xb, xo;
    if (!backing_piece (i, x0, x1, false, xa, xb, xo)) continue;
    for (int j= 0; j < 2; j++) {
      double ya, yb, yo;
      if (!backing_piece (j, y0, y1, true, ya, yb, yo)) continue;
      // the rows [ya+yo, yb+yo) of the backing store are those from H-(yb+yo)
      // in its memory (the top of the image); the image is drawn upside
      // down in the flipped view, as the NSBitmapImageRep with
      // respectFlipped: NO
      CGImageRef sub= CGImageCreateWithImageInRect (img,
        CGRectMake (xa + xo, H - (yb + yo), xb - xa, yb - ya));
      if (!sub) continue;
      CGContextDrawImage (cgc, CGRectMake (xa / k, ya / k, (xb - xa) / k,
                                           (yb - ya) / k), sub);
      CGImageRelease (sub);
    }
  }
  CGImageRelease (img);
}

void
ns_simple_widget_rep::repaint_invalid_regions () {
  double tr0= ns_bench_now ();
  // the zoom the editor starts with (the first one is not sent to the
  // widget): the smooth zoom needs the old zoom to scale from
  if (zoom_now == 0 && is_editor_widget ())
    zoom_now= as_double (call ("get-window-zoom-factor"));
  bool repainted= !is_nil (invalid_regions);
  repaint_invalid_regions_bis ();
  if (zoom_pending && repainted) run_zoom_transition ();
  ns_bench_repaint += ns_bench_now () - tr0;
}

void
ns_simple_widget_rep::repaint_invalid_regions_bis () {
  
  follow_visible_part ();
  NSPoint origin = [view frame].origin;
  bool gpu= gpu_mode ();
  NSSize sz = NSMakeSize (store_width (), store_height ());
  
  // update backing store origin wrt. TeXmacs document
  // NOTE: there is nothing to move before the backing store has a size
  if (!has_store () || store_width () < 1 || store_height () < 1)
    backing_pos= origin;
  // NOTE: when the backing store moves or changes its size, all the view
  // is shown again (the view keeps its old image on screen otherwise)
  // NOTE: the backing store moves by whole pixels (retina_factor per point),
  // and backing_pos by what was moved: the scroll positions are whole pixels
  // of the screen, which may be half pixels of the backing store (with a
  // retina_factor of 1 on a Retina screen); the rest is moved later
  bool moved= false;
  int dx = (int) round (retina_factor * (origin.x - backing_pos.x));
  int dy = (int) round (retina_factor * (origin.y - backing_pos.y));
  if (dx != 0 || dy != 0) {
    double tm0= ns_bench_now ();
    moved= true;
    if (getenv ("TEXMACS_NS_DEBUG_DRAW"))
      fprintf (stderr, "SHIFT %g -> %g dy %d size %d\n", backing_pos.y, origin.y, dy, store_height ());
    backing_pos.x += dx / (double) retina_factor;
    backing_pos.y += dy / (double) retina_factor;
    // NOTE: nothing is moved: where the view starts in the backing store
    // (on the GPU: a copy, shifted)
    if (gpu) gpu_translate_picture (gpu_store, -dx, -dy);
    else shift_backing_store (dx, dy);
    //cout << "SCROLL CONTENTS BY " << dx << " " << dy << LF;
    
    rectangles invalid;
    while (!is_nil (invalid_regions)) {
      rectangle r = invalid_regions->item ;
      //      rectangle q = rectangle (r->x1+dx,r->y1-dy,r->x2+dx,r->y2-dy);
      rectangle q = rectangle (r->x1-dx,r->y1-dy,r->x2-dx,r->y2-dy);
      invalid = rectangles (q, invalid);
      //cout << r << " ---> " << q << LF;
      invalid_regions = invalid_regions->next;
    }
    
    sz = NSMakeSize (store_width (), store_height ()); // new size
    ns_bench_move += ns_bench_now () - tm0;
    
    invalid_regions = invalid & rectangles (rectangle (0,0,
                                                      sz.width,sz.height));
    
    if (dy<0)
      invalidate_rect (0,0,sz.width,mmin (sz.height,-dy));
    else if (dy>0)
      invalidate_rect (0,mmax (0,sz.height-dy),sz.width,sz.height);
    
    if (dx<0)
      invalidate_rect (0,0,mmin (-dx,sz.width),sz.height);
    else if (dx>0)
      invalidate_rect (mmax (0,sz.width-dx),0,sz.width,sz.height);
    
    // we call update now to allow repainting of invalid regions
    // this cannot be done directly since interpose_handler needs
    // to be run at least once in some situations
    // (for example when scrolling is initiated by TeXmacs itself)
    //the_gui->update();
    //  QAbstractScrollArea::viewport()->scroll (-dx,-dy);
    // QAbstractScrollArea::viewport()->update();
    //qrgn += QRect (QPoint (0,0),sz);
  }
  
  //cout << "   repaint QPixmap of size " << backingPixmap.width() << " x "
  // << backingPixmap.height() << LF;
  // update backing store size
  {
    // NOTE: sizes in pixels, rounded as the pixels of the backing store
    NSSize _oldSize = NSMakeSize (store_width (), store_height ());
    NSSize _new_logical_Size = [view frame].size;
    NSSize _newSize = NSMakeSize (ceil (_new_logical_Size.width * retina_factor),
                                  ceil (_new_logical_Size.height * retina_factor));

    //cout << "      surface size of " << _newSize.width() << " x "
    // << _newSize.height() << LF;
    
    // NOTE: nothing to draw in an empty canvas (not laid out yet)
    if (_newSize.width < 1 || _newSize.height < 1) return;
    if (gpu && ((_newSize.width != _oldSize.width) ||
                (_newSize.height != _oldSize.height))) {
      // a new store, transparent until painted, as the bitmap (the
      // background of the window is shown where TeXmacs does not paint)
      moved= true;
      if (gpu_ren != NULL) { tm_delete (gpu_ren); gpu_ren= NULL; }
      gpu_store= gpu_backing_picture ((int) _newSize.width, (int) _newSize.height, true);
      invalidate_rect (0, 0, _newSize.width, _newSize.height);
    }
    else if ((_newSize.width != _oldSize.width)||(_newSize.height != _oldSize.height)) {
      moved= true;
      unroll_backing_store ();
      // cout << "RESIZING BITMAP"<< LF;
      NSBitmapImageRep *newBackingPixmap = [[NSBitmapImageRep alloc]
                                         initWithBitmapDataPlanes:NULL
                                         pixelsWide:_newSize.width
                                         pixelsHigh:_newSize.height
                                         bitsPerSample:8
                                         samplesPerPixel:4
                                         hasAlpha:YES
                                         isPlanar:NO
                                         colorSpaceName:NSDeviceRGBColorSpace
                                         bitmapFormat:NSBitmapFormatAlphaFirst
                                         bytesPerRow:0
                                         bitsPerPixel:0];
      // NOTE: the new parts are transparent, so that the background of the
      // window is shown where TeXmacs does not paint (as for the tooltips
      // of the Qt interface)
      memset ([newBackingPixmap bitmapData], 0,
              [newBackingPixmap bytesPerRow] * [newBackingPixmap pixelsHigh]);
      NSGraphicsContext* gc = [NSGraphicsContext graphicsContextWithBitmapImageRep: newBackingPixmap];
      [NSGraphicsContext saveGraphicsState];
      [NSGraphicsContext setCurrentContext: gc];
      [backingPixmap drawAtPoint: NSMakePoint (0, 0)];
      if (_newSize.width > _oldSize.width)
        invalidate_rect (_oldSize.width, 0, _newSize.width, _newSize.height);
      if (_newSize.height > _oldSize.height)
        invalidate_rect (0,_oldSize.height, _newSize.width, _newSize.height);
      [NSGraphicsContext restoreGraphicsState];
      [backingPixmap release];
      backingPixmap = newBackingPixmap;
    }
  }
  
  // repaint invalid rectangles
  {
    rectangles new_regions;
    if (!is_nil (invalid_regions)) {
      rectangle lub= least_upper_bound (invalid_regions);
      if (area (lub) < 1.2 * area (invalid_regions))
        invalid_regions= rectangles (lub);
      
      basic_renderer_rep* ren = get_renderer ();
      
      coord2 pt_or = from_nspoint (backing_pos);
      SI ox = -pt_or.x1;
      SI oy = -pt_or.x2;
      
      rectangles rects = invalid_regions;
      invalid_regions = rectangles();
      
      NSSize bs= NSMakeSize (store_width (), store_height ());
      double t0= ns_bench_now ();
      while (!is_nil (rects)) {
        // NOTE: with a margin of one pixel, since the conversion to the
        // coordinates of TeXmacs loses the first row (seams while scrolling)
        rectangle r0 = rects->item;
        rectangle rr = rectangle (max (r0->x1 - 1, (SI) 0), max (r0->y1 - 1, (SI) 0),
                                  min (r0->x2 + 1, (SI) bs.width),
                                  min (r0->y2 + 1, (SI) bs.height));
        //cout << "repainting " << r0 << "\n";
        ns_bench_pixels += (double) (rr->x2 - rr->x1) * (rr->y2 - rr->y1);
        // in pieces, where the backing store wraps
        for (int i= 0; i < 4; i++) {
          double xa, xb, xo, ya, yb, yo;
          if (!backing_piece (i & 1, rr->x1, rr->x2, false, xa, xb, xo) ||
              !backing_piece (i >> 1, rr->y1, rr->y2, true, ya, yb, yo))
            continue;
          // the pixels [xa, xb) x [ya, yb) of the view are moved by (xo, yo)
          // in the backing store: so are the device coordinates of the renderer
          SI ix= (SI) xo, iy= (SI) yo;
          rectangle r= rectangle ((SI) xa + ix, (SI) ya + iy,
                                  (SI) xb + ix, (SI) yb + iy);
          ren->set_origin (ox + ix * ren->pixel, oy - iy * ren->pixel);
          ren->encode (r->x1, r->y1);
          ren->encode (r->x2, r->y2);
          ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
          handle_repaint (ren, r->x1, r->y2, r->x2, r->y1);
        }
        if (gui_interrupted ()) {
          //cout << "interrupted repainting of  " << r0 << "\n";
          //ren->set_pencil (green);
          //ren->line (r->x1, r->y1, r->x2, r->y2);
          //ren->line (r->x1, r->y2, r->x2, r->y1);
          invalidate_rect (r0->x1, r0->y1, r0->x2, r0->y2);
        }
        //qrgn += qr;
        rects = rects->next;
      }
      ren->end();
      double t1= ns_bench_now ();
      ns_bench_paint += t1 - t0;
      
      // propagate immediately the changes to the screen
      if (gpu) {
        if (!moved) show_gpu_store (); // (timed when presented)
      }
      else if (!moved) {
        double k= retina_factor;
        [view displayRect: NSMakeRect (lub->x1 / k, lub->y1 / k,
                                       (lub->x2 - lub->x1) / k,
                                       (lub->y2 - lub->y1) / k)];
        ns_bench_display += ns_bench_now () - t1;
      }
    } // !is_nil (invalid_regions)
  }
  if (moved && gpu) show_gpu_store ();
  else if (moved) {
    double t2= ns_bench_now ();
    [view display];
    ns_bench_display += ns_bench_now () - t2;
  }
}

hashset<pointer> ns_simple_widget_rep::all_widgets;

void
ns_simple_widget_rep::invalidate_every () {
  iterator<pointer> i = iterate(ns_simple_widget_rep::all_widgets);
  while (i->busy())
    static_cast<ns_simple_widget_rep*>(i->next())->invalidate_all ();
}

void
ns_simple_widget_rep::repaint_all () {
  iterator<pointer> i = iterate(ns_simple_widget_rep::all_widgets);
  while (i->busy()) {
    ns_simple_widget_rep *w = static_cast<ns_simple_widget_rep*>(i->next());
    // NOTE: as with isVisible in Qt, not the views outside a visible window
    // (the canvas of a buffer which left its window, for instance)
    if (w->view && [[w->view window] isVisible] &&
        ![w->view isHiddenOrHasHiddenAncestor])
      w->repaint_invalid_regions();
  }
}

/******************************************************************************
* The smooth zoom (as vue_simple_widget_rep::start_zoom_transition)
*
* When the zoom of an editor changes, the page does not jump from the old
* size to the new: for a moment the picture of the canvas at the old zoom
* and that at the new zoom move together from the place and size of the
* first to those of the second, as in the Vue interface; then the canvas
* itself is shown again. Both are opaque, the new one above (for a zoom
* in it is smaller than the view at first, and the old one, grown, shows
* around it; for a zoom out it covers the view). A point
* p of the document (in points, from its top left) is at A1 + p in the
* scroll view before the zoom, and at A2 + r p after it (A: the place of
* the document, r the ratio of the zooms): a point x of the old picture
* goes to r x + T, T = A2 - r A1. The pictures are layers above the
* canvas, on the background of the scroll view, animated by Core
* Animation. The preference "smooth zoom" (default on) turns it off; zooms
* by less than 3% (a pinch, a step of the wheel) are immediate.
******************************************************************************/

@interface TMZoomOverlay : NSView
@end

@implementation TMZoomOverlay
- (BOOL) isFlipped { return YES; }
- (NSView*) hitTest: (NSPoint) p { (void) p; return nil; }
@end

static const double zoom_duration= 0.15; // seconds

// the picture of the canvas, as the snapshots draw it (retained, or NULL)
static CGImageRef
canvas_image (ns_simple_widget_rep* w, NSView* view) {
  NSRect vis= [view bounds];
  if (NSIsEmptyRect (vis)) return NULL;
  double k= retina_factor;
  int W= (int) ceil (vis.size.width * k), H= (int) ceil (vis.size.height * k);
  NSBitmapImageRep* rep= [[NSBitmapImageRep alloc]
    initWithBitmapDataPlanes: NULL pixelsWide: W pixelsHigh: H
    bitsPerSample: 8 samplesPerPixel: 4 hasAlpha: YES isPlanar: NO
    colorSpaceName: NSDeviceRGBColorSpace bytesPerRow: 0 bitsPerPixel: 0];
  if (!rep) return NULL;
  NSGraphicsContext* gc= [NSGraphicsContext graphicsContextWithBitmapImageRep: rep];
  [NSGraphicsContext saveGraphicsState];
  [NSGraphicsContext setCurrentContext: gc];
  CGContextRef cgc= [gc CGContext];
  CGContextTranslateCTM (cgc, 0, H);
  CGContextScaleCTM (cgc, k, -k);   // the canvas is flipped
  w->draw_backing_store (vis);
  [NSGraphicsContext restoreGraphicsState];
  CGImageRef img= CGImageRetain ([rep CGImage]);
  [rep release];
  return img;
}

void
ns_simple_widget_rep::start_zoom_transition (double new_zoom) {
  double old_zoom= zoom_now;
  zoom_now= new_zoom;
  if (zoom_image) { CGImageRelease ((CGImageRef) zoom_image); zoom_image= NULL; }
  zoom_pending= false;
  if (!is_editor_widget () || old_zoom <= 0 || new_zoom <= 0) return;
  double r= new_zoom / old_zoom;
  if (fabs (log (r)) < 0.03 || !has_store () || !view || !doc) return;
  if (get_preference ("smooth zoom", "on") == "off") return;
  NSScrollView* sv= [doc enclosingScrollView];
  if (!sv || ![view window]) return;
  zoom_image= (void*) canvas_image (this, view);
  if (!zoom_image) return;
  zoom_rect= [sv convertRect: [view bounds] fromView: view];
  zoom_origin= [sv convertPoint: NSZeroPoint fromView: doc];
  zoom_ratio= r;
  zoom_pending= true;
}

// a layer with a picture, its top left corner at the anchor
static CALayer*
picture_layer (CGImageRef img, NSRect r) {
  CALayer* l= [CALayer layer];
  [l setContents: (NSObject*) img];
  [l setContentsGravity: kCAGravityResize];
  [l setAnchorPoint: CGPointMake (0, 0)];
  [l setBounds: CGRectMake (0, 0, r.size.width, r.size.height)];
  [l setPosition: CGPointMake (r.origin.x, r.origin.y)];
  return l;
}

// the layer goes from (p0, scale s0) to (p1, scale s1), and its opacity
// from o0 to o1 (in the transaction of the caller)
static void
animate_layer (CALayer* l, CGPoint p0, double s0, CGPoint p1, double s1,
               double o0, double o1) {
  CABasicAnimation* pos= [CABasicAnimation animationWithKeyPath: @"position"];
  pos.fromValue= [NSValue valueWithPoint: NSPointFromCGPoint (p0)];
  pos.toValue= [NSValue valueWithPoint: NSPointFromCGPoint (p1)];
  CABasicAnimation* sc= [CABasicAnimation animationWithKeyPath: @"transform.scale"];
  sc.fromValue= @(s0);
  sc.toValue= @(s1);
  [l setPosition: p1];
  [l setTransform: CATransform3DMakeScale (s1, s1, 1)];
  [l addAnimation: pos forKey: @"position"];
  [l addAnimation: sc forKey: @"scale"];
  if (o0 != o1) {
    CABasicAnimation* op= [CABasicAnimation animationWithKeyPath: @"opacity"];
    op.fromValue= @(o0);
    op.toValue= @(o1);
    [l setOpacity: o1];
    [l addAnimation: op forKey: @"opacity"];
  }
}

void
ns_simple_widget_rep::run_zoom_transition () {
  zoom_pending= false;
  CGImageRef old_img= (CGImageRef) zoom_image;
  zoom_image= NULL;
  NSScrollView* sv= [doc enclosingScrollView];
  CGImageRef new_img= sv? canvas_image (this, view): NULL;
  if (!old_img || !new_img || !sv) {
    if (old_img) CGImageRelease (old_img);
    if (new_img) CGImageRelease (new_img);
    return;
  }
  // the overlay covers the visible part of the scroll view (flipped), with
  // its background: the canvas does not show while the pictures move
  NSRect cf= [[sv contentView] frame];
  TMZoomOverlay* ov= [[TMZoomOverlay alloc] initWithFrame: cf];
  [ov setWantsLayer: YES];
  [[ov layer] setBackgroundColor: [[sv backgroundColor] CGColor]];
  [sv addSubview: ov positioned: NSWindowAbove relativeTo: nil];
  NSPoint a1= [ov convertPoint: zoom_origin fromView: sv];
  NSPoint a2= [ov convertPoint: [sv convertPoint: NSZeroPoint fromView: doc]
                      fromView: sv];
  NSRect r1= [ov convertRect: zoom_rect fromView: sv];
  NSRect r2= [ov convertRect: [view bounds] fromView: view];
  double r= zoom_ratio;
  double tx= a2.x - r * a1.x, ty= a2.y - r * a1.y;
  // the old picture, from its place to r x + T; the new one, from the
  // place where it would be at the old zoom, (x - T) / r, to its own
  CGPoint old0= CGPointMake (r1.origin.x, r1.origin.y);
  CGPoint old1= CGPointMake (r * r1.origin.x + tx, r * r1.origin.y + ty);
  CGPoint new0= CGPointMake ((r2.origin.x - tx) / r, (r2.origin.y - ty) / r);
  CGPoint new1= CGPointMake (r2.origin.x, r2.origin.y);
  // both opaque, the new one above: for a zoom in it starts smaller than
  // the view, and the old one, grown, shows around it (the background
  // showed when the old one faded out); for a zoom out it covers the view
  CALayer* nl= picture_layer (new_img, r2);
  CALayer* ol= picture_layer (old_img, r1);
  [[ov layer] addSublayer: ol];
  [[ov layer] addSublayer: nl];
  CGImageRelease (old_img);
  CGImageRelease (new_img);
  if (getenv ("TEXMACS_NS_DEBUG_DRAW"))
    fprintf (stderr, "ZOOM r=%g a1=(%g,%g) a2=(%g,%g) old (%g,%g)->(%g,%g) "
             "new (%g,%g)->(%g,%g)\n", r, a1.x, a1.y, a2.x, a2.y,
             old0.x, old0.y, old1.x, old1.y, new0.x, new0.y, new1.x, new1.y);
  [CATransaction begin];
  [CATransaction setAnimationDuration: zoom_duration];
  [CATransaction setAnimationTimingFunction:
    [CAMediaTimingFunction functionWithName: kCAMediaTimingFunctionEaseOut]];
  [CATransaction setCompletionBlock: ^{
    [ov removeFromSuperview];
    [ov release];
  }];
  animate_layer (nl, new0, 1.0 / r, new1, 1.0, 1.0, 1.0);
  animate_layer (ol, old0, 1.0, old1, r, 1.0, 1.0);
  [CATransaction commit];
}
