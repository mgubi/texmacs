<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Renderers at work>

  <section|How boxes use the renderer>

  The box side is documented in <hlink|the boxes document|boxes.en.tm>; the
  interaction with the renderer is concentrated in a few methods of
  <cpp|box_rep> (<verbatim|Typeset/boxes.hpp>,
  <verbatim|Typeset/Boxes/Basic/boxes.cpp>):

  <\description-paragraphs>
    <item*|<cpp|virtual void display (renderer ren) = 0>>Paint the box
    itself, not its children, in its local coordinates.

    <item*|<cpp|virtual void pre_display (renderer& ren)>>

    <item*|<cpp|virtual void post_display (renderer& ren)>>Change the
    renderer state before the box and its children are painted, and restore
    it afterwards. Examples: clipping (<cpp|clip_box_rep>), transformations
    (<cpp|transformed_box_rep>), page numbers (<cpp|page_box_rep>),
    background ornaments (<cpp|art_box_rep>, <cpp|highlight_box_rep>).

    <item*|<cpp|virtual void redraw (renderer ren, path p, rectangles&
    l)>>The driver of the traversal.

    <item*|<cpp|virtual void display_background (renderer ren)>>

    <item*|<cpp|virtual void redraw_background (renderer ren)>>

    <item*|<cpp|void clear (renderer ren, SI x1, SI y1, SI x2, SI
    y2)>>Repaint only the backgrounds of a region, used by the editor to
    clear parts of the page with the correct background.
  </description-paragraphs>

  The default <cpp|box_rep::redraw> reads, in abridged form:

  <\cpp-code>
    void

    box_rep::redraw (renderer ren, path p, rectangles& l) {

    \ \ if ((nr_painted&15) == 15 && ren-\<gtr\>is_screen &&
    gui_interrupted (true)) return;

    \ \ ren-\<gtr\>move_origin (x0, y0);

    \ \ SI delta= ren-\<gtr\>retina_pixel;

    \ \ if (ren-\<gtr\>is_visible (x3- delta, y3- delta, x4+ delta, y4+
    delta)) {

    \ \ \ \ ...

    \ \ \ \ pre_display (ren);

    \ \ \ \ <em|redraw the children, starting near the path p>

    \ \ \ \ if (<em|interrupted>) clear_incomplete (...);

    \ \ \ \ else {

    \ \ \ \ \ \ l= rectangle (x3+ ren-\<gtr\>ox, y3+ ren-\<gtr\>oy, x4+
    ren-\<gtr\>ox, y4+ ren-\<gtr\>oy);

    \ \ \ \ \ \ display (ren);

    \ \ \ \ \ \ if (!ren-\<gtr\>is_screen) display_links (ren);

    \ \ \ \ \ \ if (nr_painted \<less\> 15) ren-\<gtr\>apply_shadow (x1, y1,
    x2, y2);

    \ \ \ \ \ \ nr_painted++;

    \ \ \ \ }

    \ \ \ \ post_display (ren);

    \ \ }

    \ \ ren-\<gtr\>move_origin (-x0, -y0);

    }
  </cpp-code>

  Notice the following points:

  <\itemize>
    <item>The visibility test uses the ink extents <cpp|(x3, y3, x4, y4)>,
    enlarged by one <cpp|retina_pixel> to compensate for truncation.

    <item>Children are drawn <em|before> the parent's own <cpp|display>, so
    that <cpp|display> of a composite box draws on top of its children;
    backgrounds are painted in <cpp|pre_display> or through
    <cpp|redraw_background>.

    <item>The rectangles returned in <cpp|l> are in absolute coordinates
    (the origin is added); they describe what has actually been repainted
    and are used by the editor to copy only the completed parts of the
    shadow to the screen when drawing was interrupted.

    <item>On screen, every sixteenth box checks for pending user events with
    <cpp|gui_interrupted>; if drawing is interrupted, the remaining
    rectangles are invalidated again and drawn later.
  </itemize>

  Graphics boxes (<verbatim|Typeset/Boxes/Graphics/>) and decorations use
  <cpp|line>, <cpp|lines>, <cpp|arc>, <cpp|polygon> and the fill routines;
  <cpp|effect_box_rep> uses <cpp|shadow (picture&, ...)> and
  <cpp|draw_picture> (see <hlink|the renderer API|renderer-api.en.tm>); image boxes use <cpp|draw_scalable>.

  <section|The repaint pipeline of the editor>

  <subsection|Invalidation>

  When the document changes, the editor calls
  <cpp|edit_interface_rep::invalidate (SI x1, SI y1, SI x2, SI y2)> with a
  rectangle in document coordinates. It multiplies the rectangle by
  <cpp|magf> and sends it to the widget with <cpp|send_invalidate>. The
  <name|Qt> widget <cpp|qt_simple_widget_rep> receives it as
  <cpp|SLOT_INVALIDATE>, converts it to device pixels with the main
  renderer (<cpp|set_origin>, <cpp|outer_round>, <cpp|decode>) and records
  it with <cpp|invalidate_rect>.

  <subsection|Repainting>

  Periodically, the <abbr|GUI> calls
  <cpp|qt_simple_widget_rep::repaint_invalid_regions>. This routine updates
  the backing pixmap (scrolling and resizing it, and invalidating the
  uncovered parts), obtains the renderer with <cpp|get_renderer ()> (the
  shared <cpp|the_qt_renderer (device_pixel_ratio ())> on which
  <cpp|begin> has been called with the backing pixmap), and then, for each
  invalid rectangle:

  <\cpp-code>
    ren-\<gtr\>set_origin (ox, oy);

    ren-\<gtr\>encode (r-\<gtr\>x1, r-\<gtr\>y1);

    ren-\<gtr\>encode (r-\<gtr\>x2, r-\<gtr\>y2);

    ren-\<gtr\>set_clipping (r-\<gtr\>x1, r-\<gtr\>y2, r-\<gtr\>x2,
    r-\<gtr\>y1);

    handle_repaint (ren, r-\<gtr\>x1, r-\<gtr\>y2, r-\<gtr\>x2,
    r-\<gtr\>y1);

    if (gui_interrupted ()) invalidate_rect (r0-\<gtr\>x1, r0-\<gtr\>y1,
    r0-\<gtr\>x2, r0-\<gtr\>y2);
  </cpp-code>

  Finally it calls <cpp|ren-\<gtr\>end ()> and schedules the repainted
  region for display on the screen. At this level the renderer is at its
  reset zoom, so that the <cpp|SI> coordinates are those of the window,
  <abbr|i.e.> document coordinates multiplied by <cpp|magf>.

  <subsection|Repainting a document>

  <cpp|edit_interface_rep::handle_repaint> (in
  <verbatim|Edit/Interface/edit_repaint.cpp>) divides the rectangle by
  <cpp|magf> and calls <cpp|draw_with_stored>. The main steps are:

  <\description>
    <item*|<cpp|draw_with_stored>>If the requested region is entirely
    contained in <cpp|stored_rects>, the region is restored from the
    <cpp|stored> shadow instead of being retypeset and redrawn. This backing
    store is only filled while editing graphics
    (<cpp|inside_active_graphics ()>), so that the graphical cursor and the
    object being edited can be redrawn quickly over an unchanged document.
    Otherwise it calls <cpp|draw_with_shadow>; unless drawing was
    interrupted, it then calls <cpp|draw_post> on the shadow and copies the
    result to the window with <cpp|win-\<gtr\>put_shadow>.

    <item*|<cpp|draw_with_shadow>>Prepare the shadow with
    <cpp|win-\<gtr\>new_shadow (shadow)> and
    <cpp|win-\<gtr\>get_shadow (shadow, ...)>, set the zoom factor of both
    renderers to the editor's <cpp|zoomf>, call <cpp|draw_pre> and
    <cpp|draw_text>, and reset the zoom factors. If drawing was interrupted,
    only the rectangles which were completely drawn are copied to the
    window.

    <item*|<cpp|draw_pre>>Paint the background (<cpp|draw_background>,
    which uses <cpp|clear_device>, <cpp|box_rep::clear> or
    <cpp|clear_pattern>) and the area around the page
    (<cpp|draw_surround>).

    <item*|<cpp|draw_text>>Set the background and call
    <cpp|eb-\<gtr\>redraw (ren, ...)> on the root box <cpp|eb>, starting
    from the path of the cursor so that the neighbourhood of the cursor is
    drawn first.

    <item*|<cpp|draw_post>>Draw the overlays which are not part of the box
    tree: context and focus rectangles (<cpp|draw_env>), the selections
    (<cpp|draw_selection>), active graphics (<cpp|draw_graphics>), the
    cursor (<cpp|draw_cursor>) and the keyboard help
    (<cpp|draw_keys>).
  </description>

  Other widgets follow the same protocol at a smaller scale: for instance
  <cpp|box_widget_rep::handle_repaint> in
  <verbatim|Texmacs/Window/tm_button.cpp> redraws a single box, and
  <cpp|QTMImpressIconEngine> repaints a widget into an icon using a
  temporary <cpp|qt_renderer_rep>.

  <section|Printing and export>

  Printing uses the same box traversal with a printer renderer, created by
  the factory function declared in <verbatim|Graphics/Renderer/printer.hpp>:

  <\explain>
    <cpp|renderer printer (url ps_file_name, int dpi, int nr_pages= 1,
    string page_type= "a4", bool landscape= false, double paper_w= 21.0,
    double paper_h= 29.7)><explain-synopsis|create a printer renderer>
  <|explain>
    Return a <abbr|PDF> renderer (<cpp|pdf_hummus_renderer>) if
    <cpp|PDF_RENDERER> is defined, <cpp|use_pdf ()> holds, and either the
    file has the suffix <verbatim|pdf> or <cpp|use_ps ()> does not hold.
    Otherwise return a PostScript <cpp|printer_rep>. The paper size is in
    centimeters; <cpp|page_type> is normalized through the <scheme> routine
    <scm|standard-paper-size>. The file is written when the renderer is
    deleted.
  </explain>

  <cpp|edit_main_rep::print_doc> (in <verbatim|Edit/Editor/edit_main.cpp>)
  typesets the document for paper at the printing resolution, creates the
  printer, sets the metadata, and then for each page:

  <\cpp-code>
    tree bg= env-\<gtr\>read (BG_COLOR);

    ren-\<gtr\>set_background (bg);

    if (bg != "white" && bg != "#ffffff")

    \ \ ren-\<gtr\>clear_pattern (0, (SI) -h, (SI) w, 0);

    rectangles rs;

    the_box[0]-\<gtr\>sx(i)= 0;

    the_box[0]-\<gtr\>sy(i)= 0;

    the_box[0][i]-\<gtr\>redraw (ren, path (0), rs);

    if (i\<less\>end-1) ren-\<gtr\>next_page ();
  </cpp-code>

  and finally <cpp|tm_delete (ren)>. When <name|Ghostscript> support is
  compiled in (<cpp|USE_GS>), <cpp|print_doc> may print to a temporary file
  in the other format and convert it with <cpp|gs_to_pdf> or
  <cpp|gs_to_ps>. <cpp|print_to_file>, <cpp|print_buffer> and
  <cpp|export_ps> are thin wrappers around <cpp|print_doc>.

  Snippets (<abbr|e.g.> for exporting a selection as an image) go through
  <cpp|edit_main_rep::print_snippet>, which calls either
  <cpp|make_eps (url name, box b, int dpi)> (a one page printer renderer of
  page type <verbatim|user> fitted to the ink extents of the box) or, for
  bitmap formats with <name|Qt>, <cpp|make_raster_image (url name, box b,
  double zoomf)>, which creates a <cpp|native_picture>, draws on it with a
  <cpp|picture_renderer> and saves it with <cpp|save_picture>. Both are
  defined in <verbatim|Typeset/Boxes/Basic/boxes.cpp>.

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
