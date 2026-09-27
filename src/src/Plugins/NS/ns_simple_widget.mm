
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
  if (wid->backingPixmap && [NSApp isRunning] && [self window])
    the_gui->force_update ();
}
@end

ns_simple_widget_rep::ns_simple_widget_rep ()
: ns_widget_rep (simple_widget),  sequencer (0), view (nil), doc (nil),
  backingPixmap (nil), extents (coord4 (0, 0, 0, 0)) { }

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
      [doc setFrameSize: rect.size];
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

// Prints the current contents of the canvas onto a QPixmap
NSBitmapImageRep*
ns_simple_widget_rep::impress () {
  SI width, height;
  handle_get_size_hint (width, height);
  NSSize s = to_nssize (width, height);
  NSSize phys_s = s;
  phys_s.width *= retina_factor;
  phys_s.height *= retina_factor;
  NSBitmapImageRep* im =
  [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                          pixelsWide: phys_s.width
                                          pixelsHigh: phys_s.height
                                       bitsPerSample: 8
                                     samplesPerPixel: 4
                                            hasAlpha: YES
                                            isPlanar: NO
                                      colorSpaceName: NSDeviceRGBColorSpace
                                         bytesPerRow: 4 * phys_s.width
                                        bitsPerPixel: 32];
  if (DEBUG_QT)
    debug_qt << "impress (" << s.width << "," << s.height << ")\n";
  NSGraphicsContext* cg = [NSGraphicsContext graphicsContextWithBitmapImageRep: im];
  {
    ns_renderer_rep *ren = the_ns_renderer();
    ren->begin (cg);
    // transparent fill
    [[NSColor colorWithDeviceWhite:1.0 alpha:0.0] drawSwatchInRect: NSMakeRect(0, 0, phys_s.width, phys_s.height)];
    
    rectangle r = rectangle (0, 0,  phys_s.width, phys_s.height);
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

void
ns_simple_widget_rep::repaint_invalid_regions () {
  
  follow_visible_part ();
  NSPoint origin = [view frame].origin;
  NSSize sz = [backingPixmap size];
  
  // update backing store origin wrt. TeXmacs document
  // NOTE: there is nothing to move before the backing store has a size
  if (!backingPixmap || [backingPixmap pixelsWide] < 1 ||
      [backingPixmap pixelsHigh] < 1)
    backing_pos= origin;
  // NOTE: when the backing store moves or changes its size, all the view
  // is shown again (the view keeps its old image on screen otherwise)
  bool moved= false;
  if ((backing_pos.x != origin.x)||(backing_pos.y != origin.y)) {
    moved= true;
    int dx =  retina_factor * (origin.x - backing_pos.x);
    int dy =  retina_factor * (origin.y - backing_pos.y);
    if (getenv ("TEXMACS_NS_DEBUG_DRAW"))
      fprintf (stderr, "SHIFT %g -> %g dy %d size %g\n", backing_pos.y, origin.y, dy, [backingPixmap size].height);
    backing_pos.x = origin.x;
    backing_pos.y = origin.y;
    NSBitmapImageRep *newBackingPixmap = [[NSBitmapImageRep alloc]
                                          initWithBitmapDataPlanes:NULL
                                          pixelsWide:sz.width
                                          pixelsHigh:sz.height
                                          bitsPerSample:8
                                          samplesPerPixel:4
                                          hasAlpha:YES
                                          isPlanar:NO
                                          colorSpaceName:NSDeviceRGBColorSpace
                                          bitmapFormat:NSBitmapFormatAlphaFirst
                                          bytesPerRow:0
                                          bitsPerPixel:0];
    NSGraphicsContext* gc = [NSGraphicsContext graphicsContextWithBitmapImageRep: newBackingPixmap];
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext setCurrentContext: gc];
    if (getenv ("TEXMACS_NS_DEBUG_RED")) {
      [[NSColor redColor] setFill];
      NSRectFill (NSMakeRect (0, 0, sz.width, sz.height));
    }
    [backingPixmap drawAtPoint: NSMakePoint (-dx, -dy)];
    [NSGraphicsContext restoreGraphicsState];
    [backingPixmap release];
    backingPixmap = newBackingPixmap;
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
    
    sz = [backingPixmap size]; // new size
    
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
    NSSize _oldSize = NSMakeSize ([backingPixmap pixelsWide],
                                  [backingPixmap pixelsHigh]);
    NSSize _new_logical_Size = [view frame].size;
    NSSize _newSize = NSMakeSize (ceil (_new_logical_Size.width * retina_factor),
                                  ceil (_new_logical_Size.height * retina_factor));

    //cout << "      surface size of " << _newSize.width() << " x "
    // << _newSize.height() << LF;
    
    // NOTE: nothing to draw in an empty canvas (not laid out yet)
    if (_newSize.width < 1 || _newSize.height < 1) return;
    if ((_newSize.width != _oldSize.width)||(_newSize.height != _oldSize.height)) {
      moved= true;
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
      
      NSSize bs= NSMakeSize ([backingPixmap pixelsWide], [backingPixmap pixelsHigh]);
      while (!is_nil (rects)) {
        // NOTE: with a margin of one pixel, since the conversion to the
        // coordinates of TeXmacs loses the first row (seams while scrolling)
        rectangle r0 = rects->item;
        rectangle r = rectangle (max (r0->x1 - 1, (SI) 0), max (r0->y1 - 1, (SI) 0),
                                 min (r0->x2 + 1, (SI) bs.width),
                                 min (r0->y2 + 1, (SI) bs.height));
        NSRect qr = NSMakeRect (r0->x1 / retina_factor, r0->y1 / retina_factor,
                          (r0->x2 - r0->x1) / retina_factor,
                          (r0->y2 - r0->y1) / retina_factor);
        //cout << "repainting " << r0 << "\n";
        ren->set_origin (ox, oy);
        ren->encode (r->x1, r->y1);
        ren->encode (r->x2, r->y2);
        ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
        handle_repaint (ren, r->x1, r->y2, r->x2, r->y1);
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
      
      // propagate immediately the changes to the screen
      if (!moved)
        [view displayRect: NSMakeRect (lub->x1 / retina_factor,
                                       lub->y1 / retina_factor,
                                       (lub->x2 - lub->x1) / retina_factor,
                                       (lub->y2 - lub->y1) / retina_factor)];
    } // !is_nil (invalid_regions)
  }
  if (moved) [view display];
}

hashset<pointer> ns_simple_widget_rep::all_widgets;

void
ns_simple_widget_rep::repaint_all () {
  iterator<pointer> i = iterate(ns_simple_widget_rep::all_widgets);
  while (i->busy()) {
    ns_simple_widget_rep *w = static_cast<ns_simple_widget_rep*>(i->next());
    if (w->view && ![w->view isHiddenOrHasHiddenAncestor]) w->repaint_invalid_regions();
  }
}
