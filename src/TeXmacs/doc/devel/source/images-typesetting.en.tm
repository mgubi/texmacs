<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Images in documents>

  <section|The <markup|image> primitive>

  An image is included with

  <\verbatim-code>
    \<less\>image\|<em|file>\|<em|width>\|<em|height>\|<em|x-offset>\|<em|y-offset>\<gtr\>
  </verbatim-code>

  (see <hlink|graphics primitives|../format/regular/prim-graphics.en.tm>
  for the user level description). It is typeset by
  <cpp|concater_rep::typeset_image> in
  <source-link|Typeset/Concat/concat_active.cpp|src/Typeset/Concat/concat_active.cpp>, which requires exactly five
  arguments and proceeds as follows.

  <paragraph|The file.>If the first argument evaluates to a string, it is
  the name of the file, in Cork encoding, relative to the document
  (<cpp|env-\<gtr\>base_file_name>). It is converted to <name|UTF-8> and
  resolved; a name without suffix is also tried with the suffixes
  <verbatim|.eps> and <verbatim|.pdf>, and a file which cannot be found is
  replaced by <verbatim|$TEXMACS_PATH/misc/pixmaps/unknown.ps>. An empty
  name gives the error \Pno image\Q.

  If the first argument is a tuple
  <verbatim|(tuple (raw-data <em|bytes>) <em|name>)>, the image is
  <em|embedded> in the document. It is then designated by a <em|ramdisc>
  <abbr|URL> (<cpp|url_ramdisc>, <source-link|System/Classes/url.cpp|src/System/Classes/url.cpp>) whose
  root holds the bytes themselves, followed by the file name
  <verbatim|image.<em|name>>, so that the suffix of the original file
  determines the format. The file functions (<cpp|load_string>,
  <cpp|exists>, ...) recognize such <abbr|URL>s, and the converters which
  need a real file materialize them into temporary files.

  <paragraph|The size.>The original size is obtained with
  <cpp|image_size> (see <hlink|image files|images-files.en.tm>), converted
  from points to logical units at the resolution <cpp|env-\<gtr\>dpi>.
  While the width and height arguments are evaluated, the environment
  variables <verbatim|w-length> and <verbatim|h-length> are set to the
  original width and height, so that the length units <verbatim|w> and
  <verbatim|h>, and percentages, refer to the size of the image: a width
  of <verbatim|50%> is half the original width. An empty width or height is
  computed from the other dimension so as to keep the aspect ratio; if both
  are empty, the original size is used. A non positive result is replaced
  by a quarter of the original size.

  <paragraph|The offset.>The offsets are evaluated in the same way, with
  <verbatim|w-length> and <verbatim|h-length> now set to the final size, and
  the image box is shifted accordingly with <cpp|move_box>.

  <paragraph|The box.><cpp|image_box (ip, u, w, h, alpha, pixel)>
  (<source-link|Typeset/Boxes/Basic/basic_boxes.cpp|src/Typeset/Boxes/Basic/basic_boxes.cpp>) creates an
  <cpp|image_box_rep>, which holds a <cpp|scalable> obtained from
  <cpp|load_scalable_image (u, w, h, "", pixel)> and the current opacity.
  Its extents are those of the scalable image, and its <cpp|display>
  method calls <cpp|ren-\<gtr\>draw_scalable>. Nothing is loaded or
  rasterized at typesetting time: only the size of the file is needed. The
  animation boxes (<source-link|Typeset/Boxes/Animate/animate_boxes.cpp|src/Typeset/Boxes/Animate/animate_boxes.cpp>) use
  <cpp|image_box> too.

  Other ways of including images in a document, which are not covered
  here, are the images inside pictures of the graphics editor (see
  <hlink|the graphics editor|graphics-editor.en.tm>), and images used as
  backgrounds (next section).

  <section|Background patterns>

  The background of a box, a page or a table cell may be a <em|pattern>, a
  tree <verbatim|(pattern <em|url> <em|width> <em|height>
  [<em|effect>])> stored in a pattern brush. Patterns are drawn by
  <cpp|renderer_rep::clear_pattern> (<source-link|Graphics/Renderer/renderer.cpp|src/Graphics/Renderer/renderer.cpp>):

  <\enumerate>
    <item>the original size of the image is computed with
    <cpp|image_size>, at 600 dots per inch;

    <item>the size of one tile is the original size when the width or
    height is empty, an integer number of logical units, a percentage of
    the area to be filled, or, with the <verbatim|@> suffix, a percentage of
    the other dimension multiplied or divided by the aspect ratio of the
    image; it is rounded up to whole pixels;

    <item>a scalable image of the tile size, with the effect of the
    pattern, is created, and every tile which meets both the area and the
    clipping rectangle is drawn with <cpp|draw_scalable>.
  </enumerate>

  The <scheme> side of patterns (the pattern and gradient selectors of the
  format menus) is in <source-link|generic/pattern-selector.scm|TeXmacs/progs/generic/pattern-selector.scm> and
  <source-link|generic/pattern-tools.scm|TeXmacs/progs/generic/pattern-tools.scm>. The <abbr|PDF> renderer does not use
  <cpp|clear_pattern>: it turns patterns into <abbr|PDF> tiling patterns
  (see <hlink|images in <abbr|PDF> output|images-pdf.en.tm>).

  <section|Inserting images>

  The <menu|Insert|Image> menu (<source-link|generic/insert-menu.scm|TeXmacs/progs/generic/insert-menu.scm>) offers
  <menu|Link image> and <menu|Insert image>, which call
  <scm|make-link-image> and <scm|make-inline-image>
  (<source-link|generic/generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>) through the file chooser. Both call
  the editor routine <cpp|edit_text_rep::make_image (file, link, w, h, x,
  y)> (<source-link|Edit/Modify/edit_text.cpp|src/Edit/Modify/edit_text.cpp>), with the name made relative to
  the current buffer:

  <\description>
    <item*|Linked images>The <markup|image> tag contains the file name. Local
    absolute names are rerooted to <verbatim|file:>, and the name is
    converted to Cork encoding.

    <item*|Embedded images>The file is read into memory and the tag contains
    <verbatim|(tuple (raw-data <em|bytes>) <em|name>)>, where <em|name> is
    the last component of the file name. A missing file or a file without
    suffix is refused with a message in the footer.
  </description>

  The width and height proposed by the file chooser come from
  <cpp|qt_pretty_image_size>. When files are dropped on a document, the
  image names of the dropped content are made relative to the document if
  possible (<cpp|relativize> in <source-link|Edit/Interface/edit_mouse.cpp|src/Edit/Interface/edit_mouse.cpp>).

  <section|Exporting selections as images>

  The editor can typeset a fragment and save it as an image:
  <cpp|edit_main_rep::print_snippet> produces <abbr|EPS> or <abbr|PDF>
  through a printer renderer, or a bitmap through a picture renderer (see
  <hlink|renderers at work|renderer-pipeline.en.tm>, section \Pprinting and
  export\Q). Its <scheme> wrapper is <scm|print-snippet>, which returns the
  extents and the baseline of the typeset fragment.

  <verbatim|$TEXMACS_PATH/progs/convert/images/tmimage.scm> uses it for
  two commands:

  <\description>
    <item*|<scm|export-selection-as-graphics>>Export the selection to a file
    whose format is given by its suffix, or by the preference
    <verbatim|texmacs-\<gtr\>image:format> when the name has none
    (<abbr|SVG> if a <abbr|PDF> to <abbr|SVG> converter exists,
    <abbr|PDF> otherwise). The selection is wrapped in a minimal table or a
    <markup|document-at> so that the image has the width of the material
    and, for single lines, a known baseline; mathematics keeps its inline or
    display style. For <abbr|SVG>, the <TeXmacs> source of the selection is
    embedded in the image so that it can be edited again from
    <name|Inkscape> (<scm|refactor-svg>); for <abbr|PDF>, it is attached as
    a <TeXmacs> document (<scm|embbed-tm-selection-in-pdf>, see <hlink|PDF
    attachments|images-pdf.en.tm>).

    <item*|<scm|clipboard-copy-image>>Export the selection to a temporary
    file in the preferred format and put it on the clipboard
    (<name|Qt> only).
  </description>

  <section|Pitfalls>

  <\itemize>
    <item>In <cpp|renderer_rep::clear_pattern>
    (<source-link|Graphics/Renderer/renderer.cpp:408|src/Graphics/Renderer/renderer.cpp:408>), the height of a tile is
    set to the image height when <verbatim|pattern[1]> (the <em|width>) is
    empty, instead of testing <verbatim|pattern[2]>. A pattern with an empty
    height and an explicit width gets the height of the whole area, and a
    pattern with an empty width and an explicit height ignores that height.

    <item>When the format requested in <scm|export-selection-as-graphics>
    has no converter, the code falls back on <abbr|PDF> by rebuilding the
    file name from <verbatim|(substring surl (- sl sufl) sl)>, which is the
    <em|suffix> of the name rather than the part before it
    (<source-link|tmimage.scm:259|TeXmacs/progs/convert/images/tmimage.scm:259>). Exporting <verbatim|a/b.png> without a
    <abbr|PDF> to <name|PNG> converter therefore writes a file called
    <verbatim|pngpdf> in the current directory.

    <item><cpp|typeset_gr_effect> and <cpp|typeset_gr_transform>
    (<source-link|Typeset/Concat/concat_graphics.cpp:199|src/Typeset/Concat/concat_graphics.cpp:199>, <verbatim|211>)
    call <cpp|typeset_error> on a wrong number of arguments but do not
    return, and then read the missing arguments.

    <item>An embedded image is stored with its <em|whole> file name, and the
    ramdisc <abbr|URL> is <verbatim|image.<em|name>>, for instance
    <verbatim|image.photo.png>. Only the last suffix matters, so this works,
    but code which inspects these names must not assume that the part after
    <verbatim|image.> is a suffix.

    <item>The image size is taken from the size cache. If an image file is
    replaced by a file with another aspect ratio, the image is drawn with the
    old proportions until the document is typeset again.
  </itemize>

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
