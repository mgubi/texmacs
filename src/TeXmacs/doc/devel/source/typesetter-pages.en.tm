<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Page breaking and the construction of pages>

  <section|The pager>

  At the end of each typesetting pass, the typesetter owns the array
  <cpp|l> of page items of the whole document (see <hlink|incremental
  typesetting|typesetter-bridges.en.tm>). It hands this array to a
  <cpp|pager_rep> (<verbatim|Typeset/Page/pager.hpp>), whose job is to cut
  it into pages and to produce the final document box:

  <\cpp-code>
    pager ppp= tm_new\<less\>pager_rep\<gtr\> (br-\<gtr\>ip, env, l);

    box rb= ppp-\<gtr\>make_pages ();
  </cpp-code>

  The constructor of <cpp|pager_rep> (<verbatim|Typeset/Page/pager.cpp>)
  reads the page geometry with <cpp|env-\<gtr\>get_page_pars>
  (<cpp|text_width>, <cpp|text_height>, <cpp|width>, <cpp|height>, the
  margins <cpp|odd>, <cpp|even>, <cpp|top>, <cpp|bot>) and the page
  parameters: <verbatim|page-extend> and <verbatim|page-shrink>
  (<cpp|may_extend>, <cpp|may_shrink>), the header and footer separations,
  the separations around footnotes and floats (<cpp|fn_sep>,
  <cpp|fnote_sep>, <cpp|float_sep>), the footnote bar length, and the
  quality of page breaking <verbatim|page-breaking> (<cpp|quality> is 0
  for <verbatim|sloppy>, 1 for <verbatim|medium> and 2 otherwise). The
  flag <cpp|paper> is true when <verbatim|page-medium> is
  <verbatim|paper>.

  <cpp|pager_rep::make_pages> calls either <cpp|pages_make> (paper) or
  <cpp|papyrus_make> (screen modes), which fill the array <cpp|pages> with
  one box per page. It then arranges the pages on the screen: when
  <verbatim|page-packet> is larger than one, several pages are put side by
  side; on paper, each page may be framed with a <cpp|page_border_box>
  according to <verbatim|page-border>. The result is a <cpp|scatter_box>
  of the pages, wrapped in a <cpp|move_box>.

  <\description>
    <item*|Paper>The page height is the space <cpp|space (text_height-
    may_shrink, text_height, text_height+ may_extend)>. The page breaker
    returns one <cpp|pagelet> per page, and <cpp|pages_make_page> turns
    each of them into a page box.

    <item*|Papyrus>The page height is essentially infinite
    (<cpp|MAX_SI \<gtr\>\<gtr\> 1>). The page breaker is still used, since
    it is responsible for placing floats and footnotes, but it must return
    a single pagelet. The page is as tall as its contents (for the
    <verbatim|beamer> medium at least as tall as the user page height).
  </description>

  <section|Skeletons, pagelets and insertions>

  The result of page breaking is a <cpp|skeleton>, <abbr|i.e.> an
  <cpp|array\<less\>pagelet\<gtr\>> (<verbatim|Typeset/Page/skeleton.hpp>).
  A <cpp|pagelet> describes the contents of one page (or one column) as a
  list of <cpp|insertion>s, together with its total height <cpp|ht> (a
  <cpp|space>), its penalty <cpp|pen> and the stretch factor chosen for it.
  An insertion is a vertical piece of material:

  <\cpp-code>
    struct insertion_rep: concrete_struct {

    \ \ tree\ \ \ \ \ \ type;\ \ \ \ \ // type of insertion

    \ \ path\ \ \ \ \ \ begin;\ \ \ \ // begin location in array of page_items

    \ \ path\ \ \ \ \ \ end;\ \ \ \ \ \ // end location in array of page_items

    \ \ skeleton\ \ sk;\ \ \ \ \ \ \ // or possible subpagelets (used for multicolumns)

    \ \ space\ \ \ \ \ ht;\ \ \ \ \ \ \ // height of pagelet

    \ \ space\ \ \ \ \ xh;\ \ \ \ \ \ \ // extra stretchable height (used for certain floats)

    \ \ vpenalty\ \ pen;\ \ \ \ \ \ // penalty associated to pagelet

    \ \ double\ \ \ \ stretch;\ \ // between -1 and 1 for determining final height

    \ \ SI\ \ \ \ \ \ \ \ top_cor;\ \ // top correction

    \ \ SI\ \ \ \ \ \ \ \ bot_cor;\ \ // bottom correction

    \ \ int\ \ \ \ \ \ \ nr_cols;\ \ // number of columns

    \ \ ...

    };
  </cpp-code>

  The <cpp|type> is the empty string for a portion of the main text, and a
  tuple starting with <verbatim|"float">, <verbatim|"footnote">,
  <verbatim|"if-page-break"> or <verbatim|"multi-column"> otherwise. The
  paths <cpp|begin> and <cpp|end> delimit a range of page items. For the
  main text they are paths <cpp|(i)> into the page items of the document.
  Floats and footnotes are addressed by longer paths
  <cpp|(i, j, k)>: item <cpp|k> of the <cpp|j>-th float attached to page
  item <cpp|i> (see the functions <cpp|access> and <cpp|sub> in
  <verbatim|Typeset/Page/page_breaker.cpp>). Multi-column insertions carry
  one sub-pagelet per column in <cpp|sk>.

  Costs are measured by a <cpp|vpenalty> (<verbatim|Typeset/Page/vpenalty.hpp>),
  a pair of integers compared lexicographically: the main penalty
  <cpp|pen> and the <em|excentricity> <cpp|exc>, a squared deviation from
  the ideal page height computed by <cpp|as_vpenalty>. The main penalty is
  the sum of the penalties of the page items at which the document is
  broken (0 between paragraphs, 1 inside paragraphs, 10 or 100 near the
  borders of a paragraph, see the stacker in <hlink|paragraph
  formatting|typesetter-lines.en.tm>) plus the following constants when a
  page does not fit exactly:

  <\description>
    <item*|<cpp|EXTEND_PAGE_PENALTY>, <cpp|REDUCE_PAGE_PENALTY>>The
    contents only fit when the page is extended or shrunk within the
    allowed tolerance.

    <item*|<cpp|TOO_SHORT_PENALTY>, <cpp|TOO_LONG_PENALTY>>The page is
    underfull or overfull even with the tolerances; these are scaled by the
    ratio between the actual and the desired height.

    <item*|<cpp|UNBALANCED_COLUMNS>, <cpp|LONGER_LATTER_COLUMN>>The
    columns of a multi-column portion cannot be balanced.

    <item*|<cpp|BAD_FLOATS_PENALTY>>The floats cannot be placed
    consistently with their placement specifications.
  </description>

  The helper <cpp|stretch_space (spc, stretch)> maps a stretch factor in
  <math|<around|[|-1,1|]>> to a length between <cpp|spc-\<gtr\>min> and
  <cpp|spc-\<gtr\>max>.

  <section|Floats and footnotes>

  Floating material travels from the concater to the page breaker as
  follows.

  <\enumerate>
    <item>For a <markup|float> tag (which is also used by the style files
    to implement footnotes, with type <verbatim|footnote>),
    <cpp|concater_rep::typeset_float> evaluates the type and the placement
    string (for instance <verbatim|"tbh">; forced to <verbatim|"h"> when
    <cpp|env-\<gtr\>page_floats> is false), and calls
    <cpp|make_lazy_vstream>. This typesets the body at the width of the
    page into a <cpp|lazy_vstream>, a vertical stream of page items whose
    <cpp|channel> is the tuple (type, placement). The stream is wrapped in
    a <cpp|control_box> and printed as a <cpp|FLOAT_ITEM>. The
    <markup|if-page-break> tag, which inserts material only if a page
    break occurs at a given position, works in the same way with the
    channel <verbatim|("if-page-break" pos sep)>.

    <item>During paragraph formatting, <cpp|lazy_paragraph_rep::line_print>
    retrieves the lazy stream (<cpp|get_leaf_lazy>) and collects it in the
    array <cpp|fl> of the current line. <cpp|line_end> attaches this array
    to the page item of the line: floats are therefore anchored to the line
    on which they occur.

    <item>The page breaker turns every attached stream into an insertion,
    and decides on which page, and where on it, the insertion is placed.
  </enumerate>

  Marginal notes (<markup|line-note>, <markup|page-note>) are not floats:
  they are placed next to their line by <cpp|line_end>.

  <section|The page breaking algorithm>

  The page breaker is called through

  <\explain>
    <cpp|skeleton break_pages (array\<less\>page_item\<gtr\> l, space ph, int
    qual, space fn_sep, space fnote_sep, space float_sep, font fn, int
    first_page)><explain-synopsis|break a document into pages>
  <|explain>
    Defined in <verbatim|Typeset/Page/page_breaker.cpp>. If the user
    preference <verbatim|"new style page breaking"> is not
    <verbatim|"off"> (it is <verbatim|"on"> by default, see
    <verbatim|texmacs/texmacs/tm-server.scm>), the call is delegated to
    <cpp|new_break_pages> in <verbatim|Typeset/Page/new_breaker.cpp>.
    Otherwise the older <cpp|page_breaker_rep> is used.
  </explain>

  <subsection|Preprocessing>

  The constructor of <cpp|new_breaker_rep> computes, for each page item
  <cpp|i>:

  <\itemize>
    <item>its height including the following glue, <cpp|body_ht[i]>, and
    the cumulated heights <cpp|body_tot[i]>, so that the natural height of
    any range of lines is a difference of two entries;

    <item>top and bottom corrections <cpp|body_cor[i]>, which account for
    lines whose ink extends above or below the normal font height;

    <item>the insertions attached to the item (<cpp|ins_list[i]>, built
    with <cpp|make_insertion (lazy_vstream, path)>), and the cumulated
    heights of footnotes (<cpp|foot_tot>), floats (<cpp|float_tot>), and of
    single-column floats and footnotes inside multi-column text
    (<cpp|wide_tot>), as well as the extra height of
    <markup|if-page-break> material (<cpp|break_ht>);

    <item>the number of columns (<cpp|col_number>, <cpp|col_same>) and
    whether a page break or a new page is compulsory after the item
    (<cpp|must_break>, <cpp|must_new>, from <cpp|PAGE_CONTROL_ITEM>s
    <markup|page-break>, <markup|new-page> and <markup|new-dpage>).
  </itemize>

  With these tables, <cpp|compute_space (b1, b2)> returns the total height
  (as a <cpp|space>) of the page which starts at break <cpp|b1> and ends at
  break <cpp|b2>, including the footnotes and floats anchored in between,
  without looking at the individual items.

  <subsection|Break points and floats>

  A page break point is a <cpp|path>. The simplest form <cpp|(i)> means a
  break before page item <cpp|i>. A break may also carry a list of
  postponed floats:
  <math|<around*|(|i,i<rsub|1>,j<rsub|1>,i<rsub|2>,j<rsub|2>,\<ldots\>|)>>
  means a break before item <cpp|i>, where the floats with indices
  <math|<around*|(|i<rsub|k>,j<rsub|k>|)>> in <cpp|ins_list>, although
  anchored before the break, are deferred to the following page. When enumerating candidates,
  the breaker considers, for each line, all ways of postponing a suffix of
  the pending floats. A float whose placement contains <verbatim|f> forces
  the pending floats to be resolved; if their placement letters are
  incompatible (the local variable <cpp|float_status> reaches 3), the break
  receives <cpp|BAD_FLOATS_PENALTY> (or
  the search from this start stops if an acceptable break has already been
  found).

  <subsection|Dynamic programming>

  The search is a shortest-path computation over break points, similar to
  line breaking. For each reached break <cpp|b>, the hashmaps
  <cpp|best_prev> and <cpp|best_pens> store the best previous break and
  the accumulated <cpp|vpenalty>. <cpp|find_page_breaks ()> maintains a
  worklist <cpp|todo_list> of breaks from which pages still have to be
  started, beginning with <cpp|path (0)>, and calls <cpp|find_page_breaks
  (b1)> for them.

  <cpp|find_page_breaks (b1)> enumerates the candidate ends <cpp|b2> of a
  page starting at <cpp|b1>, in increasing order. For each candidate whose
  break penalty is below <cpp|HYPH_INVALID>, it computes the height
  <cpp|spc= compute_space (b1, b2)> of the page and the cost

  <\equation*>
    pen<around*|(|b<rsub|2>|)>=pen<around*|(|b<rsub|1>|)>+penalty<around*|(|b<rsub|2>|)>+<text|<cpp|as_vpenalty>><around*|(|spc<rsub|def>-height<rsub|def>|)>+<text|fit
    penalties>,
  </equation*>

  where the fit penalties are <cpp|EXTEND_PAGE_PENALTY>,
  <cpp|REDUCE_PAGE_PENALTY>, <cpp|TOO_SHORT_PENALTY> or
  <cpp|TOO_LONG_PENALTY> as described above. The excentricity and the fit
  The excentricity is not charged for the last page
  (<cpp|last_break>), and an underfull page is not penalized when it is the
  last one or ends at a forced page break. If the cost is better than the one stored for
  <cpp|b2>, <cpp|b2> is updated and queued. The enumeration stops as soon
  as the minimal height of the page exceeds the maximal page height, or at
  a forced page break. On papyrus, the only candidate end is the end of the
  document.

  The parameter <cpp|quality> controls the search. With quality 2
  (<verbatim|professional>, the default), all queued breaks are expanded,
  which yields a globally optimal solution. With lower qualities, only the
  most promising queued break is expanded in each round, which is faster
  but may give worse pages.

  <subsection|Assembling pages>

  Once the search is finished, <cpp|assemble_skeleton> follows the
  <cpp|best_prev> links back from <cpp|path (N(l))> and creates one
  pagelet per page with <cpp|assemble (start, end)> (or
  <cpp|assemble_multi_columns> for pages with several columns):

  <\enumerate>
    <item>The floats anchored on the page (minus those postponed by
    <cpp|end>), together with the floats postponed by <cpp|start>, are
    distributed according to their placement letters. Roughly, floats allowing
    <verbatim|t> go to the top, floats allowing <verbatim|b> to the
    bottom, and the remaining ones stay <em|here>, <abbr|i.e.> in the text
    flow right after the line to which they are anchored.

    <item>The pagelet is built from the top floats, pending
    <markup|if-page-break> material, portions of the main text
    (<cpp|make_insertion (i1, i2)>) interleaved with the here floats, the
    bottom floats, and finally the footnotes, with the separations
    <cpp|float_sep>, <cpp|fnote_sep> and <cpp|fn_sep> in between.

    <item><cpp|format_pagelet (pg, height, last_page)> determines the
    stretch factor of the page, between <math|-1> (maximally shrunk) and
    <math|1> (maximally stretched), and propagates it to the insertions.
    The last page is not stretched when it is not full.
  </enumerate>

  Multi-column material (page items with <cpp|nr_cols \<gtr\> 1>, produced
  by paragraphs with <verbatim|par-columns> larger than one) is handled in
  <verbatim|Typeset/Page/columns_breaker.cpp>. <cpp|break_uniform> splits
  a page into portions with a uniform number of columns, and
  <cpp|break_columns> finds column breaks which balance the columns of
  each portion (searching around the fractions <math|k/n> of the total
  height with <cpp|break_columns_at>). The resulting portions become
  insertions of type <verbatim|("multi-column" n)> with one sub-pagelet
  per column.

  <subsection|The older page breaker>

  The class <cpp|page_breaker_rep> in
  <verbatim|Typeset/Page/page_breaker.cpp> implements the previous
  algorithm, still available by switching off the preference. It organizes
  the page items into three <em|flows> (<cpp|MAIN_FLOW>,
  <cpp|FNOTE_FLOW>, <cpp|FLOAT_FLOW>), represents break points as vectors
  of positions in each flow (<cpp|vbreak>), sorts them by estimated height
  and explores candidate page ends around the ideal position using
  <em|ladders> of incomparable breaks (<cpp|find_next_breaks>,
  <cpp|propose_break>). Its output is a skeleton of the same form.

  <section|Building the page boxes>

  The skeleton is turned into boxes by the methods in
  <verbatim|Typeset/Page/make_pages.cpp>:

  <\description-paragraphs>
    <item*|<cpp|pages_format (array\<less\>page_item\<gtr\> l, SI ht, SI
    tcor, SI bcor)>>Stacks a range of page items into a box of the given
    height with <cpp|format_stack>, which stretches or shrinks the vertical
    glue between the items. On the way, page control items of the form
    <verbatim|env_page> update the local page style (headers, footers,
    page number, background, margins for this page).

    <item*|<cpp|pages_format (insertion ins)>>Extracts the items of an
    insertion with <cpp|sub> and formats them at the height
    <cpp|stretch_space (ins-\<gtr\>ht, ins-\<gtr\>stretch)>; multi-column
    insertions are formatted column by column and placed side by side.

    <item*|<cpp|pages_format (pagelet pg)>>Places the insertions of a page
    below each other with the appropriate separations, and draws the
    footnote rule (a <cpp|line_box> of length <verbatim|page-fnote-barlen>)
    above the first footnote.

    <item*|<cpp|pages_make_page (pagelet pg)>>Sets <verbatim|page-nr> and
    <verbatim|page-the-page> in the environment, typesets the header and
    footer (<cpp|make_header>, <cpp|make_footer>, which evaluate the
    macros <verbatim|page-odd-header>, <verbatim|page-even-footer>
    <abbr|etc.>, or the one-shot <verbatim|page-this-header>), determines
    the background and margin adjustments, and builds a <cpp|page_box>.
    Crop marks are added with <cpp|crop_marks_box> if requested.
  </description-paragraphs>

  The page boxes are the children of the document box returned by
  <cpp|make_pages>. Since headers and footers are regenerated at each pass,
  page numbers in headers are always up to date. Page numbers of labels
  are collected afterwards, on paper and during complete passes only, by
  <cpp|typesetter_rep::determine_page_references>, which asks the page
  boxes for them with <cpp|collect_page_numbers>.

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
