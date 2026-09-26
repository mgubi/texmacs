/******************************************************************************
 * MODULE     : ns_ui_element.mm
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2018  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/





/******************************************************************************
 * glue widget
 ******************************************************************************/

{
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


NSBitmapImageRep*
ns_glue_widget_rep::render () {
  NSSize s = to_nssize (w, h);
  NSBitmapImageRep *im = [[NSBitmapImageRep alloc]
                          initWithBitmapDataPlanes: NULL
                          pixelsWide: s.width
                          pixelsHigh: s.height
                          bitsPerSample: 8
                          samplesPerPixel: 4
                          hasAlpha: YES
                          isPlanar: NO
                          colorSpaceName: NSDeviceRGBColorSpace
                          bitmapFormat: NSBitmapFormatAlphaFirst
                          bytesPerRow: 0
                          bitsPerPixel: 0];
  NSGraphicsContext* gc = [NSGraphicsContext graphicsContextWithBitmapImageRep: im];
  if (gc) {
    ns_renderer_rep* ren = the_ns_renderer();
    ren->begin (gc);
    rectangle r = rectangle (0, 0, s.width(), s.height());
    ren->set_origin (0,0);
    ren->encode (r->x1, r->y1);
    ren->encode (r->x2, r->y2);
    ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
    
    if (col == "") {
      // do nothing
    } else {
      if (is_atomic (col)) {
        color c = named_color (col->label);
        ren->set_background (c);
        ren->set_pencil (c);
        ren->fill (r->x1, r->y2, r->x2, r->y1);
      } else {
        ren->set_shrinking_factor (std_shrinkf);
        brush old_b = ren->get_background ();
        ren->set_background (col);
        ren->clear_pattern (5*r->x1, 5*r->y2, 5*r->x2, 5*r->y1);
        ren->set_background (old_b);
        ren->set_shrinking_factor (1);
      }
    }
    ren->end();
  }
  return im;
}

QAction *
ns_glue_widget_rep::as_qaction() {
  QAction* a = new QTMAction();
  a->setText (to_qstring (as_string (col)));
  QIcon icon;
#if 0
  tree old_col = col;
  icon.addPixmap (render(), QIcon::Active, QIcon::On);
  col = "";
  icon.addPixmap (render(), QIcon::Normal, QIcon::On);
  col = old_col;
#else
  icon.addPixmap (render ());
#endif
  a->setIcon (icon);
  a->setEnabled (false);
  return a;
}

NSView*
ns_glue_widget_rep::as_nsview () {
  QLabel* qw = new QLabel();
  qw->setText (to_qstring (as_string (col)));
  qw->setPixmap (render ());
  qw->setMinimumSize (to_qsize (w, h));
  //  w->setEnabled(false);
  qwid = qw;
  return qwid;
}

