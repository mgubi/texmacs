<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Image files: sizes and conversions>

  <source-link|System/Files/image_files.cpp|src/System/Files/image_files.cpp> is the single entry point for
  everything that concerns image <em|files>: their original size and their
  conversion into the three formats which the rest of <TeXmacs> needs
  (<name|PNG> for the screen, <abbr|PDF> and <abbr|EPS> for printing). Each
  function tries the available tools in a fixed order, depending on the
  build options <cpp|QTTEXMACS>, <cpp|USE_RESVG>, <cpp|USE_GS>,
  <cpp|PDF_RENDERER>, <cpp|USE_IMLIB2> and <cpp|MACOSX_EXTENSIONS>, and
  on the external programs found at run time.

  <section|Image sizes>

  <\explain>
    <cpp|void image_size (url image, int& w, int& h)><explain-synopsis|original
    size in points>
  <|explain>
    Return the size of an image in points (1/72 inch), from the size cache
    <cpp|img_box> if possible. Otherwise call <cpp|image_size_sub> and cache
    the result. If no size can be determined, report an error and use
    <math|35\<times\>35> points.
  </explain>

  <cpp|image_size_sub> dispatches on the suffix and on the available tools,
  in this order:

  <\enumerate>
    <item><abbr|PDF>: <cpp|pdf_image_size>, which uses the crop box of the
    first page as read by <name|PDFHummus> (<cpp|hummus_pdf_image_size>),
    or, without the <abbr|PDF> renderer, <name|Ghostscript>
    (<cpp|gs_PDFimage_size>), or else <name|ImageMagick>. Rotated pages
    have their width and height exchanged.

    <item><abbr|SVG>: <cpp|svg_image_size>, which tries <name|resvg>, then
    <name|Qt>, and finally reads the <verbatim|width> and
    <verbatim|height> attributes of the <verbatim|svg> element. Both
    libraries assume 96 pixels per inch.

    <item>PostScript and <abbr|EPS>: the <verbatim|%%BoundingBox> comment
    (<cpp|ps_bounding_box>, <cpp|ps_read_bbox>), which also stores the
    origin of the box in the size cache.

    <item>On <name|macOS> builds without <name|Qt> 6:
    <cpp|mac_image_size>.

    <item>Formats readable by <name|Qt> (<cpp|qt_supports>, which excludes
    <abbr|PDF> and PostScript on purpose, because <name|Qt> renders them
    blurred): <cpp|qt_image_size>, which converts pixels to points with the
    resolution stored in the file (<cpp|QImage::dotsPerMeterX>).

    <item><name|Imlib2> (<name|X11> builds only).

    <item><name|Ghostscript>, for PostScript files without a bounding box:
    <cpp|gs_image_size> runs the <verbatim|bbox> device.

    <item><name|ImageMagick>: <cpp|imagemagick_image_size> runs
    <verbatim|identify> and converts with the density recorded in the
    file; an undefined density is taken as 90 pixels per inch.
  </enumerate>

  Two other functions give sizes in <em|pixels>. <cpp|native_image_size>
  returns the natural size of a raster or <abbr|SVG> image (with
  <name|resvg> or <name|Qt>), and otherwise the size in points scaled to 300
  pixels per inch. <cpp|qt_pretty_image_size> (in
  <source-link|Plugins/Qt/qt_utilities.cpp|src/Plugins/Qt/qt_utilities.cpp>) proposes a width and height for a
  newly inserted image: its size in points, or <verbatim|1par> if it is
  wider than the paragraph.

  <section|Conversions>

  <\explain>
    <cpp|void image_to_png (url image, url png, int w= 0, int h=
    0)><explain-synopsis|convert to a bitmap>
  <|explain>
    Convert an image to <name|PNG> at <math|w\<times\>h> <em|pixels>. The
    tools are tried in this order: the <name|macOS> frameworks (builds
    without <name|Qt> 6), <name|Qt> (<cpp|qt_convert_image>, which renders
    <abbr|SVG> files with <name|QtSvg>), <name|Ghostscript>
    (<cpp|gs_to_png>, for PostScript and <abbr|PDF>), a <scheme> converter
    (<cpp|call_scm_converter>), and finally <name|ImageMagick>. If no
    output file was produced, <verbatim|unknown.png> is copied instead.
  </explain>

  <\explain>
    <cpp|void image_to_eps (url image, url eps, int w_pt= 0, int h_pt= 0,
    int dpi= 0)><explain-synopsis|convert to <abbr|EPS>>

    <cpp|void image_to_pdf (url image, url pdf, int w_pt= 0, int h_pt= 0,
    int dpi= 0)><explain-synopsis|convert to <abbr|PDF>>
  <|explain>
    Convert an image to a vector format, sized <math|w_pt\<times\>h_pt>
    points. Both functions first try to keep vector images vectorial:
    an <abbr|SVG> file which <name|Qt> cannot read goes through a <scheme>
    converter (in practice <name|Inkscape>), and PostScript or <abbr|PDF>
    files go through <name|Ghostscript> (<cpp|gs_to_eps>,
    <cpp|gs_to_pdf>). Raster images are then wrapped with <name|Qt>
    (<cpp|qt_image_to_pdf>, which prints the image with a
    <cpp|QPrinter>, downsampling it to <cpp|dpi> if it is larger),
    and as a last resort converted with <name|ImageMagick>.
    <cpp|image_to_pdf> also tries a <scheme> converter before
    <name|ImageMagick>. On failure, <verbatim|unknown.eps> or
    <verbatim|unknown.pdf> is copied to the destination.
  </explain>

  <cpp|image_to_psdoc (image)> converts an image to a temporary <abbr|EPS>
  file and returns its contents as a string; <cpp|ps_load (image)> returns
  the contents of PostScript files directly and calls
  <cpp|image_to_psdoc> for other formats. <cpp|image_to_psdoc> is also
  exported to <scheme> as <scm|image-\<gtr\>psdoc>, and serves as the
  converter from bitmap formats to <verbatim|postscript-document> in
  <source-link|init-images.scm|TeXmacs/progs/convert/images/init-images.scm>.

  <section|<scheme> converters>

  <cpp|call_scm_converter (image, dest)> asks the <scheme> converter graph
  whether a converter exists between the two suffixes
  (<scm|file-converter-exists?> on dummy names <verbatim|x.<em|suf>>) and,
  if so, runs it with <scm|file-convert>. It returns whether the
  destination exists afterwards. The converter graph itself is described in
  <hlink|converters to other data formats|conversions.en.tm>.

  The image formats and their converters are declared in
  <verbatim|$TEXMACS_PATH/progs/convert/images/init-images.scm> with
  <scm|define-format> and <scm|converter>. Each converter has a
  <scm|:require> clause which tests for the external program, so the graph
  adapts to the installation. The main ones are:

  <\description>
    <item*|<abbr|PDF>>to PostScript with <name|Ghostscript>
    (<scm|gs-convert>, device <verbatim|eps2write>); to <name|PNG>,
    <name|JPEG> and <name|TIFF> with <name|pdftocairo>, <name|Ghostscript>
    or <name|ImageMagick>, at the resolution given by the preference
    <verbatim|texmacs-\<gtr\>image:raster-resolution> (300 by default); to
    <abbr|SVG> with <name|pdftocairo> or <name|pdf2svg>.

    <item*|PostScript>to <abbr|PDF> with <verbatim|ps2pdf>.

    <item*|<abbr|SVG>>to PostScript, <abbr|PDF> and <name|PNG> with
    <name|Inkscape>; to <name|PNG> with <name|rsvg-convert> when
    <name|Inkscape> is absent; to a PostScript document with
    <scm|image-\<gtr\>psdoc> when <name|Qt> 5 or later is available.

    <item*|Other vector formats><name|Xfig> (<verbatim|fig2ps>),
    <name|Xmgrace> and <name|Geogebra>, to PostScript (and <abbr|SVG> or
    <name|PNG> for <name|Geogebra>).

    <item*|Bitmaps><name|JPEG>, <name|TIFF>, <name|GIF>, <name|PNG> and
    <name|PNM> to a PostScript document with <scm|image-\<gtr\>psdoc>, and
    between each other with <name|ImageMagick>'s <verbatim|convert>.
  </description>

  The same file defines <scm|gs-binary>, <scm|has-gs?>,
  <scm|has-convert?> and <scm|has-pdftocairo?>. On <name|Windows>,
  <verbatim|convert> is never used, because a system command of that name
  exists.

  <section|<name|Ghostscript>>

  <source-link|Plugins/Ghostscript/gs_utilities.cpp|src/Plugins/Ghostscript/gs_utilities.cpp> (compiled with
  <cpp|USE_GS>, which the <name|CMake> build always sets) runs the
  <name|Ghostscript> executable; nothing is linked. The executable is
  <verbatim|gs> (on <name|Windows>, the first <verbatim|gswin*c.exe>
  found under <verbatim|C:\\Program Files*\\gs>), or the copy shipped with
  <TeXmacs> when <cpp|GS_EXE> is defined. The functions are:

  <\description-paragraphs>
    <item*|<cpp|has_gs>, <cpp|gs_prefix>, <cpp|eps_device>>Availability, the
    quoted command, and the <abbr|EPS> device, <verbatim|eps2write> for
    version 9.14 and later and <verbatim|epswrite> before.

    <item*|<cpp|gs_supports>>True for <verbatim|ps>, <verbatim|eps> and
    <verbatim|pdf>.

    <item*|<cpp|gs_image_size>, <cpp|gs_PDFimage_size>>Sizes: the bounding
    box comment or the <verbatim|bbox> device for PostScript; the crop box
    (or media box) and rotation reported by <verbatim|pdf_info.ps> for
    <abbr|PDF>.

    <item*|<cpp|gs_to_png>, <cpp|gs_to_eps>, <cpp|gs_to_pdf>>Conversions of
    images. PostScript inputs are translated by the origin of their bounding
    box with inline PostScript code, rather than with
    <verbatim|-dEPSCrop>, which mishandles boxes not starting at the
    origin. <cpp|gs_to_eps> restores the original bounding box afterwards
    (<cpp|gs_fix_bbox>), because the <abbr|EPS> devices compute their own.

    <item*|<cpp|gs_to_pdf (doc, pdf, landscape, h, w)>,
    <cpp|gs_to_ps>>Conversions of whole printed documents, used by
    <cpp|edit_main_rep::print_doc> when printing to the format which the
    native printer does not produce.

    <item*|<cpp|gs_PDF_EmbedAllFonts>>Re-distill a <abbr|PDF> file with all
    fonts embedded (see <hlink|images in <abbr|PDF>
    output|images-pdf.en.tm>).

    <item*|<cpp|gs_check>>Run a document through the <verbatim|nullpage>
    device and report errors.

    <item*|<cpp|pdf_version>>The <abbr|PDF> version of a file, or the
    version selected in the preference <verbatim|texmacs-\<gtr\>pdf:version>
    (1.4 by default).
  </description-paragraphs>

  <source-link|Plugins/Ghostscript/ghostscript.cpp|src/Plugins/Ghostscript/ghostscript.cpp> is only used by the
  <name|X11> port: <cpp|ghostscript_run> renders a PostScript image into an
  <name|X11> pixmap through the <verbatim|GHOSTVIEW> protocol.

  <section|<name|ImageMagick>>

  <name|ImageMagick> is the last resort of all conversions.
  <cpp|has_image_magick> tests for <verbatim|convert> in the path (always
  false on <name|Windows>), and <cpp|call_imagemagick_convert> runs
  <verbatim|convert>, downsampling raster images which are larger than
  needed for the requested size and resolution. If neither
  <name|Ghostscript> nor <name|ImageMagick> is installed, the first failed
  conversion prints a recommendation to install them
  (<cpp|inform_about_dependencies>).

  <section|Pitfalls>

  <\itemize>
    <item>The <name|Inkscape> converters of <source-link|init-images.scm|TeXmacs/progs/convert/images/init-images.scm>
    (lines 152-160) use the command line options <verbatim|-z>,
    <verbatim|-f>, <verbatim|-P>, <verbatim|-A> and
    <verbatim|--export-png>, which were removed in <name|Inkscape> 1.0.
    With a current <name|Inkscape> these conversions fail. Since
    <cpp|image_to_eps> and <cpp|image_to_pdf> try them first for <abbr|SVG>
    files that <name|Qt> cannot read, such images then fall back on
    rasterization or on the placeholder image.

    <item><scm|pdf-file-\<gtr\>gs-raster> (<verbatim|init-images.scm:95-104>)
    always uses the <verbatim|pngalpha> device, although it is registered as
    the converter from <abbr|PDF> to <name|JPEG> and to <name|TIFF> as well
    (lines 311-319): these conversions write <name|PNG> data into files
    with the wrong suffix.

    <item><cpp|gs_to_eps> (<verbatim|Plugins/Ghostscript/gs_utilities.cpp:402-406>)
    passes the input file directly after a <verbatim|-c> PostScript
    fragment, without <verbatim|-f>. <name|Ghostscript> then interprets the
    file name as PostScript code, so the conversion of PostScript and
    <abbr|EPS> inputs fails. <cpp|gs_to_png> and <cpp|gs_to_pdf> use
    <verbatim|-f> correctly.

    <item><cpp|qt_convert_image> (<source-link|Plugins/Qt/qt_utilities.cpp:556|src/Plugins/Qt/qt_utilities.cpp:556>)
    saves <cpp|im.scaled (w, h)> even when <cpp|w> or <cpp|h> is zero, the
    default of <cpp|image_to_png>; <cpp|QImage::scaled> then returns a
    null image and nothing is written. All current callers pass a positive
    size.

    <item>The sizes of <abbr|SVG> files depend on the library which reads
    them (<name|resvg> and <name|QtSvg> assume 96 pixels per inch,
    <name|ImageMagick> 90), and the first successful answer is cached. The
    same file may therefore get different sizes on different installations.

    <item>The size cache is keyed by <abbr|URL> only. It is invalidated
    when the picture cache notices that a file has changed, but the
    typesetter's image boxes keep the old size until the document is
    retypeset.
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
