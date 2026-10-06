<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Tables and dynamic markup>

  <section|Tables>

  Tables are edited mostly by <c++> routines of the editor
  (<scm|table-insert-row>, <scm|table-insert-column>, <scm|cell-set-format>,
  <scm|table-set-format>, ...), which the <scheme> code in
  <verbatim|progs/table/> combines into commands. The mode predicate
  <scm|in-table?> holds when the cursor is inside a <markup|table> tag; the
  hooks use the finer predicate <scm|table-markup-context?>
  (<source-link|generic/generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>), which recognizes the table
  environments (<markup|tabular>, <markup|block>, ...) around a
  <markup|tformat> or <markup|table>.

  <\description>
    <item*|<source-link|table/table-edit.scm|TeXmacs/progs/table/table-edit.scm>>The groups
    <verbatim|table-tag> and <verbatim|wide-table-tag> (which also make
    tabulars and blocks variants of each other); <key|return> inserts a new
    row below the current one (or a paragraph break inside a multi-paragraph
    cell); the structured insertion and removal hooks insert and remove
    columns (horizontal) and rows (vertical); the geometry and swipe hooks
    change the horizontal and vertical alignment of the current cells
    (and <scm|geometry-default> removes their formatting); and the commands behind the table and cell menus
    (<scm|table-set-halign>, <scm|cell-set-span>, <scm|cell-set-border>,
    <scm|table-insert-blank-row>, ...). The setters
    <scm|cell-set-format*> and <scm|table-set-format*> keep the
    <verbatim|*-hmode> and <verbatim|*-width> (and the vertical
    counterparts) consistent.

    <item*|<source-link|table/table-kbd.scm|TeXmacs/progs/table/table-kbd.scm>>The <key|table ...> prefix (in
    the mode <scm|in-table?>): <key|table H>, <key|table V>, <key|table B>
    and <key|table P> for table alignment, borders and padding, the lower
    case letters for cells, and <key|table m> followed by
    <key|c>, <key|h>, <key|v> or <key|t> for the <em|cell mode>
    (<scm|set-cell-mode>), which decides whether cell commands act on the
    current cell, row, column or the whole table.

    <item*|<source-link|table/table-menu.scm|TeXmacs/progs/table/table-menu.scm>>The table and cell menus, and a
    redefinition of <scm|standard-focus-menu> for tables, which shows the
    table and cell menus directly in the focus menu.

    <item*|<source-link|table/table-widgets.scm|TeXmacs/progs/table/table-widgets.scm>,
    <source-link|table/table-tools.scm|TeXmacs/progs/table/table-tools.scm>>The table and cell property dialogs
    and side tools.
  </description>

  <section|Dynamic markup>

  The directory <verbatim|progs/dynamic/> groups the markup whose content
  changes during editing or presentation. Its modules are loaded lazily
  through their menus and keyboards (<source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>); the
  folding, script and spreadsheet keyboards are registered for the mode
  <scm|always?>, so their conditions are tested by the keymaps themselves.

  <subsection|Folding, switches, overlays and slides>

  <source-link|dynamic/fold-edit.scm|TeXmacs/progs/dynamic/fold-edit.scm> handles three families of tags, recognized
  by <scm|dynamic-context?>:

  <\description>
    <item*|Toggles>Tags with a folded and an unfolded form
    (the groups <verbatim|folded-tag> and <verbatim|unfolded-tag>,
    declared with <scm|define-fold> in <source-link|dynamic/dynamic-drd.scm|TeXmacs/progs/dynamic/dynamic-drd.scm>,
    and summarized/detailed tags; <scm|toggle-context?>). <scm|alternate-toggle> switches between the
    two forms.

    <item*|Switches>Tags with several branches of which one is shown
    (the groups <verbatim|alternative-tag>, <verbatim|unroll-tag>
    and <verbatim|expanded-tag>; <scm|switch-context?>). The routines
    <scm|switch-index>, <scm|switch-to>, <scm|switch-insert-at> and
    <scm|switch-remove-at> manipulate the branches; the structured insertion
    hooks insert and remove branches.

    <item*|Overlays>The <markup|overlays> tag of presentations, with the
    current overlay number (<scm|overlays-current>,
    <scm|overlays-switch-to>).
  </description>

  On top of these, <scm|dynamic-extremal> and <scm|dynamic-incremental>
  (bound through <scm|dynamic-first>, <scm|dynamic-last>,
  <scm|dynamic-previous> and <scm|dynamic-next>) move to the first, last,
  previous or next state of the innermost dynamic tag;
  <scm|dynamic-operate-on-buffer> and <scm|dynamic-traverse-buffer>
  apply such an operation recursively to the whole document (or the current
  slide), which is how the presentation keys of <source-link|fold-kbd.scm|TeXmacs/progs/dynamic/fold-kbd.scm> step
  through a talk. <scm|dynamic-make-slides> converts a presentation into a
  sequence of slides, and the <markup|screens> and <markup|slideshow>
  routines (<scm|screens-switch-to>, <scm|screens-show-all>, ...) implement
  the beamer style. At the end of the file, three of these entry points
  are redefined to call <scm|former> and then move the cursor into the
  graphics of a graphical slide (<markup|gr-screen>), so that slides made
  of pictures can be edited with the graphics editor.

  <source-link|dynamic/fold-markup.scm|TeXmacs/progs/dynamic/fold-markup.scm> contains the <scheme> functions called
  from the rendering of <markup|screens> (the slide index and the navigation
  links); <source-link|dynamic/fold-menu.scm|TeXmacs/progs/dynamic/fold-menu.scm> the insert menus and the
  presentation toolbars.

  <subsection|Sessions and programs>

  Sessions (<markup|session> with <markup|input>, <markup|output>,
  <markup|errput>, <markup|textput> fields) and programs (<markup|program>, a
  variant of sessions with the same kinds of fields) are edited by
  <source-link|dynamic/session-edit.scm|TeXmacs/progs/dynamic/session-edit.scm> and the nearly identical
  <source-link|dynamic/program-edit.scm|TeXmacs/progs/dynamic/program-edit.scm>. The editing side is:

  <\itemize>
    <item>context predicates for fields (<scm|field-context?>,
    <scm|field-input-context?>, <scm|field-folded-context?>, ...);

    <item><key|return> in an input field evaluates it, or inserts a line
    break for multi-line input (<scm|session-multiline-input?>, or
    <key|S-return>); if the plug-in supports it, the plug-in is first asked
    whether the input is complete (<verbatim|input-done?>);

    <item>the arrow keys, <key|home>/<key|end> and <key|pageup>/<key|pagedown>
    move between fields (redefinitions of <scm|kbd-horizontal>, ... for
    <scm|field-context?>); <key|tab> asks the plug-in for completions;

    <item>structured insertion and removal of fields, folding of
    input/output pairs (<scm|alternate-toggle>), splitting of sessions,
    and special cut and paste of whole fields.
  </itemize>

  The evaluation itself (<scm|session-evaluate> and <scm|session-feed>, the
  request queue and the callbacks which fill in the output) is described in
  <hlink|the life of a session|plugins-sessions.en.tm>. <scheme> sessions
  are evaluated directly by <scm|scheme-eval>.

  <subsection|Scripts, plots and converters>

  <source-link|dynamic/scripts-edit.scm|TeXmacs/progs/dynamic/scripts-edit.scm> evaluates expressions <em|in place>
  with the plug-in given by the <verbatim|prog-scripts> environment
  variable: <scm|script-eval> and <scm|script-approx> replace a selection
  or formula by its value, <scm|script-apply> applies a function, and the
  tags <markup|script-input>/<markup|script-output> keep the input
  together with the result (toggled with <scm|alternate-toggle>).
  <scm|script-feed> is the in-place analogue of <scm|session-feed>. The
  same file implements plots (<markup|plot-curve>, <markup|plot-surface>,
  ..., turned into <name|Gnuplot> commands by <scm|script-plot-command>
  and evaluated through the plug-in) and converters (<key|return> in a
  <markup|converter-eval> tag replaces it by its content converted from
  another format, such as <LaTeX>, with the snippet converters); <source-link|dynamic/scripts-plot.scm|TeXmacs/progs/dynamic/scripts-plot.scm>
  the interactive plot editor.

  <subsection|Spreadsheets>

  Spreadsheets are tables whose cells may contain formulas
  (<source-link|dynamic/calc-table.scm|TeXmacs/progs/dynamic/calc-table.scm>, <source-link|dynamic/calc-edit.scm|TeXmacs/progs/dynamic/calc-edit.scm>).
  Cells are named as in common spreadsheets (<scm|cell-name>,
  <scm|cell-ref-encode>); references to other cells and ranges in a formula
  are rewritten into <markup|calc-ref> tags (<scm|cell-input-expand>),
  and <scm|calc-now> walks the document, collects the inputs which have
  to be recomputed, and sends them one by one to the scripting plug-in
  (<scm|calc-feed>, which uses <scm|silent-feed*>); each result is stored
  in the output field and in the table <scm|calc-output>, and the next
  pending input is evaluated. <scm|calc> schedules this after
  250<nbsp>ms of idle time. The keyboard of <source-link|calc-kbd.scm|TeXmacs/progs/dynamic/calc-kbd.scm> is
  only active when a scripting plug-in is available (<scm|calc-ready?>).
  The same machinery drives the generated exercises of the
  <verbatim|icourse> style (<markup|calc-generate>, <markup|calc-answer>,
  <markup|calc-check>).

  <subsection|Animations>

  <source-link|dynamic/animate-edit.scm|TeXmacs/progs/dynamic/animate-edit.scm> inserts the animation tags
  (<scm|make-anim-constant>, <scm|make-anim-translate-right>, ...), gives
  their parameters to the focus menu (<scm|customizable-parameters>,
  <scm|parameter-choice-list>), lets the geometry keys change their
  duration and speed (<scm|geometry-speed>, <scm|geometry-horizontal>),
  and controls time bending (<scm|accelerate-set-type>, ...) and playback
  (<scm|anim-play>, <scm|reset-players>). The rendering of the animations
  is done by the typesetter.

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
