<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Handwriting recognition (experimental)>

  <section|Status>

  <TeXmacs> contains the beginning of a recognizer for handwritten
  characters: a widget in which strokes are drawn with the mouse or a pen, a
  dialog which learns characters from examples, and a matcher which compares
  a drawing with the learned examples. It is not connected to the editor:
  the result of a recognition is only printed on the console, and the
  dialog is opened by calling <scm|(learn-glyphs)> by hand. The drawing
  widget only exists in the X11 (<name|Widkit>) port; the <name|Qt> ports
  return an empty widget (<cpp|ink_widget> in <source-link|qt_widget.cpp:643|src/Plugins/Qt/qt_widget.cpp:643>).

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Handwriting/poly_line.hpp|src/Graphics/Handwriting/poly_line.hpp>,
    <source-link|poly_line.cpp|src/Graphics/Handwriting/poly_line.cpp>>Points,
    polylines and contours (lists of strokes), their geometry,
    normalization, vertex detection and invariants.

    <item*|<source-link|learn_handwriting.cpp|src/Graphics/Handwriting/learn_handwriting.cpp>,
    <source-link|recognize_handwriting.cpp|src/Graphics/Handwriting/recognize_handwriting.cpp>>The
    table of learned glyphs and the matcher.

    <item*|<source-link|smoothen.cpp|src/Graphics/Handwriting/smoothen.cpp>><cpp|simplify>,
    which removes superfluous points of a stroke.

    <item*|<source-link|Plugins/Widkit/Misc/ink_widget.cpp|src/Plugins/Widkit/Misc/ink_widget.cpp>>The
    drawing widget of the X11 port.

    <item*|<source-link|utils/handwriting/handwriting.scm|TeXmacs/progs/utils/handwriting/handwriting.scm>>The
    <scheme> side: storage of the examples and the learning dialog.
  </description-paragraphs>

  <section|Data and algorithm>

  A <em|point> is an <cpp|array\<less\>double\<gtr\>>, a stroke
  (<cpp|poly_line>) an array of points and a glyph (<cpp|contours>) an
  array of strokes. The <markup|ink> widget of <scm|tm-widget> records one
  stroke per press and drag of the mouse, and, when the pointer leaves the
  widget or a stroke ends, passes the strokes to its <scheme> callback as a
  list of lists of coordinate pairs (<cpp|ink_widget_rep::commit>).

  To compare glyphs, <cpp|invariants> (<source-link|poly_line.cpp|src/Graphics/Handwriting/poly_line.cpp>)
  first normalizes a glyph into the unit square and then describes it by:

  <\itemize>
    <item>a <em|discrete> part: the number of strokes and, at level 1, the
    number of vertices (sharp turns) of each stroke, found by
    <cpp|vertices>;

    <item>a <em|continuous> part: 21 points sampled at equal distances
    along each stroke and, at level 1, the positions of the vertices along
    the stroke.
  </itemize>

  <cpp|register_glyph> stores both levels of invariants of an example with
  its name. <cpp|recognize_glyph> splits a drawing into characters, taking
  each stroke together with the following ones until a stroke starts to the
  right of the previous one without lying above it (<cpp|attached>), and for each character looks for the
  learned examples with the same number of strokes and the same discrete
  invariants at level 1. Among these, the example with the smallest
  Euclidean distance between the continuous invariants wins. If there is
  none at level 1, level 2 (without vertices) is tried. The names of the
  winners are concatenated.

  The matcher can be tried without the widget, from <scheme>, with
  <scm|glyph-register> and <scm|glyph-recognize>, which take glyphs as lists
  of strokes of <verbatim|(x y)> pairs. On 2026-10-06, after learning a
  vertical line as <verbatim|l> and a closed loop as <verbatim|o>, slightly
  different drawings were recognized correctly, and two vertical strokes
  side by side gave <verbatim|ll>.

  <section|The learning dialog>

  <scm|learn-glyphs> (<source-link|handwriting.scm|TeXmacs/progs/utils/handwriting/handwriting.scm>)
  shows a widget with the drawing area, buttons to learn the drawing as a
  Latin letter, a digit or a Greek letter, and a button to recognize it.
  The examples are kept in a hash table from names to lists of glyphs and
  saved with <scm|save-object> in <verbatim|~/.TeXmacs/system/glyphs.scm>;
  they are loaded and registered with <scm|glyph-register> before the first
  learning or recognition. The path is written literally, so it ignores
  <verbatim|$TEXMACS_HOME_PATH>: a private home set for tests, or the
  home directory of <TeXmacs> on <name|Windows>, which is not
  <verbatim|~/.TeXmacs>, are not used (issue #305 of
  <verbatim|mgubi/texmacs>).

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
