<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Concatenation, line breaking and paragraph formatting>

  <section|The formatter data structures>

  The intermediate results of the typesetter are described by a few small
  classes in <verbatim|Typeset/Format/> and <verbatim|Kernel/Types/>.

  <subsection|Spaces>

  A <cpp|space> (<verbatim|Kernel/Types/space.hpp>) is a triple of lengths
  <cpp|min>, <cpp|def> and <cpp|max>: the minimal, default and maximal
  extent of a stretchable space, in the internal unit <cpp|SI>. Spaces can
  be added, subtracted, multiplied by scalars and compared with <cpp|max>.
  They are used both for horizontal glue between the items on a line and
  for vertical glue between lines and paragraphs.

  <subsection|Line items>

  The concater produces arrays of <cpp|line_item>s
  (<verbatim|Typeset/Format/line_item.hpp>):

  <\cpp-code>
    class line_item_rep: public concrete_struct {

    public:

    \ \ int\ \ \ \ \ \ \ \ type;\ \ \ \ \ \ // the type of the line item

    \ \ int\ \ \ \ \ \ \ \ op_type;\ \ \ // operator type for mathematical symbols

    \ \ box\ \ \ \ \ \ \ \ b;\ \ \ \ \ \ \ \ \ // the box

    \ \ space\ \ \ \ \ \ spc;\ \ \ \ \ \ \ // separation space

    \ \ int\ \ \ \ \ \ \ \ penalty;\ \ \ // penalty for a linebreak after this line_item

    \ \ bool\ \ \ \ \ \ \ limits;\ \ \ \ // line items has limits

    \ \ language\ \ \ lan;\ \ \ \ \ \ \ // language for hyphenating strings

    \ \ tree\ \ \ \ \ \ \ t;\ \ \ \ \ \ \ \ \ // for control items

    \ \ ...

    };
  </cpp-code>

  Each item holds a box together with the space which follows it and the
  penalty for breaking the line after it. Penalties are integers; the
  constants <cpp|HYPH_STD> (<math|10<rsup|4>>), <cpp|HYPH_PANIC>
  (<math|10<rsup|6>>) and <cpp|HYPH_INVALID> (<math|10<rsup|8>>, meaning
  <em|forbidden>) are defined in <verbatim|System/Language/language.hpp>.
  The main item types are:

  <\description-paragraphs>
    <item*|<cpp|STRING_ITEM>>A piece of text in a single font (a
    <cpp|text_box>), which may be hyphenated using the language <cpp|lan>.

    <item*|<cpp|STD_ITEM>>Any other box (an inline formula, an image, a
    fraction, a table, ...).

    <item*|<cpp|MARKER_ITEM>>An invisible zero-width box which marks a
    source position, for instance the border of an inline tag; it gives the
    cursor a place to stand.

    <item*|<cpp|CONTROL_ITEM>>A zero-width item carrying a command in its
    field <cpp|t>: forced line breaks (<markup|next-line>,
    <markup|line-break>, <markup|new-line>), tabulations (<markup|htab>),
    vertical spaces, page-break directives, local changes of paragraph or
    page parameters (tuples <verbatim|env_par> and <verbatim|env_page>), and
    decorations (<markup|datoms>).

    <item*|<cpp|FLOAT_ITEM>>A float, footnote or <markup|if-page-break>
    content; its box is a <cpp|control_box> wrapping a <cpp|lazy> vertical
    stream which will be handed to the page breaker.

    <item*|<cpp|NOTE_LINE_ITEM>, <cpp|NOTE_PAGE_ITEM>>Marginal notes.

    <item*|<cpp|LEFT_BRACKET_ITEM>, <cpp|MIDDLE_BRACKET_ITEM>,
    <cpp|RIGHT_BRACKET_ITEM>>Large delimiters whose size is determined
    after the whole line has been concatenated.

    <item*|<cpp|LSUB_ITEM>, <cpp|LSUP_ITEM>, <cpp|RSUB_ITEM>,
    <cpp|RSUP_ITEM>>Scripts, which are glued to their neighbours during
    post-processing, producing <cpp|GLUE_LSUBS_ITEM>,
    <cpp|GLUE_RSUBS_ITEM>, <cpp|GLUE_LEFT_ITEM>, <cpp|GLUE_RIGHT_ITEM> or
    <cpp|GLUE_BOTH_ITEM>.

    <item*|<cpp|OBSOLETE_ITEM>>An item which has been absorbed by another
    one and will be removed.
  </description-paragraphs>

  <subsection|Page items and stack borders>

  After line breaking, each line becomes a <cpp|page_item>
  (<verbatim|Typeset/Format/page_item.hpp>):

  <\cpp-code>
    class page_item_rep: public concrete_struct {

    public:

    \ \ int\ \ \ \ \ \ \ \ \ \ type;\ \ \ \ // the type of the page item

    \ \ box\ \ \ \ \ \ \ \ \ \ b;\ \ \ \ \ \ \ // the box

    \ \ space\ \ \ \ \ \ \ \ spc;\ \ \ \ \ // separation space

    \ \ int\ \ \ \ \ \ \ \ \ \ penalty; // penalty for a linebreak after this page_item

    \ \ array\<less\>lazy\<gtr\>\ \ fl;\ \ \ \ \ \ // floating objects attached to this item

    \ \ int\ \ \ \ \ \ \ \ \ \ nr_cols; // number of columns

    \ \ tree\ \ \ \ \ \ \ \ \ t;\ \ \ \ \ \ \ // for page control items

    \ \ ...

    };
  </cpp-code>

  The type is <cpp|PAGE_LINE_ITEM> for ordinary lines,
  <cpp|PAGE_HIDDEN_ITEM> for invisible material of height zero,
  <cpp|PAGE_CONTROL_ITEM> for page commands (<markup|page-break>,
  <markup|new-page>, <markup|new-dpage> and <verbatim|env_page> tuples,
  stored in <cpp|t>) and <cpp|PAGE_NOTE_ITEM>. The space <cpp|spc> is the
  vertical glue between the bottom of this line and the top of the next
  one; the <cpp|penalty> is the cost of a page break after the line. Floats
  and footnotes whose anchor lies on the line are attached in <cpp|fl>.

  A <cpp|stack_border> (<verbatim|Typeset/Format/stack_border.hpp>)
  describes how a block of page items interacts with the blocks above and
  below it: the default baseline distance <cpp|height>, the separation
  parameters <cpp|sep>, <cpp|hor_sep>, <cpp|ver_sep>, the corresponding
  values for the first line (<cpp|height_before> <abbr|etc.>), the pending
  vertical spaces <cpp|vspc_before> and <cpp|vspc_after>, and the flags
  <cpp|nobr_before> and <cpp|nobr_after> forbidding page breaks at the
  borders. It allows the typesetter to combine the page items of
  consecutive paragraphs without re-typesetting them
  (<cpp|merge_stack>).

  <subsection|Formats and lazy structures>

  The file <verbatim|Typeset/formatter.hpp> defines a small protocol for
  material which cannot be typeset before its width is known, such as the
  contents of floats, of multi-paragraph table cells and of GUI containers.
  A <cpp|lazy> is a partially typeset object of a certain
  <cpp|lazy_type> (<cpp|LAZY_PARAGRAPH>, <cpp|LAZY_DOCUMENT>,
  <cpp|LAZY_TABLE>, <cpp|LAZY_VSTREAM>, <cpp|LAZY_BOX>, ...). It
  supports two operations:

  <\explain>
    <cpp|lazy lazy_rep::produce (lazy_type request, format
    fm)><explain-synopsis|continue formatting>
  <|explain>
    Formats the structure further according to <cpp|fm> and returns a lazy
    structure of type <cpp|request>. For instance, a paragraph asked for a
    <cpp|LAZY_VSTREAM> with a <cpp|format_vstream> of a given width breaks
    its lines and returns a <cpp|lazy_vstream> of page items; a vertical
    stream asked for a <cpp|LAZY_BOX> stacks its lines into a box.
  </explain>

  <\explain>
    <cpp|format lazy_rep::query (lazy_type request, format
    fm)><explain-synopsis|ask for formatting information>
  <|explain>
    Returns information needed before production. The only query currently
    in use is <cpp|QUERY_VSTREAM_WIDTH>, answered by a <cpp|format_width>
    holding the natural width of a paragraph or table; it is used to size
    table columns.
  </explain>

  The format classes (<verbatim|Typeset/Format/format.hpp>) are
  <cpp|format_none>, <cpp|format_width>, <cpp|format_cell> (width, vertical
  alignment, depth and height of a cell), <cpp|format_vstream> (width plus
  line items to put before and after) and <cpp|query_vstream_width>. The
  function <cpp|make_lazy> in <verbatim|Typeset/Line/lazy_typeset.cpp>
  plays for lazy structures the role that <cpp|make_bridge> plays for
  bridges: it dispatches on the tag and builds a <cpp|lazy_document>,
  <cpp|lazy_surround>, <cpp|lazy_table>, a lazy paragraph, <abbr|etc.>
  Lazy structures are built from scratch each time: they are not
  incremental.

  <section|The concater>

  <subsection|Entry points>

  The concater (<verbatim|Typeset/Concat/concater.hpp>) turns a tree into
  an array of line items. A <cpp|concater_rep> holds the environment, the
  array <cpp|a> of items being produced and a flag <cpp|rigid> which is set
  when the result will surely not be broken into lines. It is used through
  the following functions:

  <\explain>
    <cpp|array\<less\>line_item\<gtr\> typeset_concat (edit_env env, tree
    t, path ip)><explain-synopsis|line items of a paragraph>
  <|explain>
    Typesets <cpp|t> and post-processes the result (<cpp|finish>). This is
    the input of the paragraph formatter.
  </explain>

  <\explain>
    <cpp|box typeset_as_concat (edit_env env, tree t, path
    ip)><explain-synopsis|typeset on a single line>
  <|explain>
    Uses a rigid concater and joins the items into a <cpp|concat_box>,
    using the default width of the separating spaces. This is used for all
    inline material which is not broken into lines: arguments of
    fractions, scripts, table cells in text mode, headers and footers,
    <abbr|etc.> The variant <cpp|typeset_as_box> wraps the result in a
    <cpp|composite_box>; <cpp|typeset_as_atomic> also handles
    <markup|with> and <markup|locus> at the top level.
  </explain>

  <\explain>
    <cpp|array\<less\>line_item\<gtr\> typeset_marker (edit_env env, path
    ip)><explain-synopsis|a marker item>
  <|explain>
    Returns a single <cpp|MARKER_ITEM> for the source position <cpp|ip>.
    Used by the bridges to add cursor positions around structural tags.
  </explain>

  <subsection|Traversal>

  <cpp|concater_rep::typeset (tree t, path ip)> in
  <verbatim|Typeset/Concat/concater.cpp> is a large switch on the label of
  <cpp|t>. Strings are handled according to the current mode
  (<cpp|env-\<gtr\>mode>): text, mathematics or program code. Compound
  trees are dispatched to specialized methods, spread over several files:

  <\description-paragraphs>
    <item*|<verbatim|concat_text.cpp>>Strings, <markup|concat>,
    <markup|document> and <markup|para> nested inside a line (typeset as a
    stacked box with <cpp|typeset_as_stack> <abbr|resp.>
    <cpp|typeset_as_paragraph>), spaces, moves and resizes, floats, notes,
    <markup|datoms>/<markup|dlines>/<markup|dpages>.

    <item*|<verbatim|concat_math.cpp>>Brackets, big operators, scripts,
    fractions, roots, wide accents, trees and inline tables.

    <item*|<verbatim|concat_macro.cpp>>The macro primitives:
    <markup|assign>, <markup|with>, <markup|compound> (macro application),
    <markup|arg>, <markup|value>, <markup|mark>, <markup|eval>, ...

    <item*|<verbatim|concat_active.cpp>, <verbatim|concat_inactive.cpp>>Other
    active markup (conditionals, loci, references, <markup|specific>, flags)
    and source-mode rendering.

    <item*|<verbatim|concat_graphics.cpp>, <verbatim|concat_animate.cpp>,
    <verbatim|concat_gui.cpp>>Graphics, animations, and GUI containers.
  </description-paragraphs>

  Inline macro applications are expanded by the concater itself:
  <cpp|typeset_compound> looks up the macro, pushes the argument bindings
  onto <cpp|env-\<gtr\>macro_arg> and <cpp|env-\<gtr\>macro_src>, and
  typesets the body <cpp|attach_right (f[n], ip)>, surrounded by markers
  for <cpp|descend (ip, 0)> and <cpp|descend (ip, 1)>. When an <markup|arg>
  is met in the body, <cpp|typeset_argument> typesets the argument value
  with its original inverse path, so that it remains editable. Similarly,
  <cpp|typeset_with> changes the environment with <cpp|write_update>,
  typesets the body between two markers and restores the old values. The
  evaluation machinery itself is the subject of <hlink|macro
  expansion|macro-expansion.en.tm>.

  <subsection|Strings, spacing and penalties>

  Strings are cut into words and spaces by the language of the environment.
  <cpp|language_rep::advance (tree t, int& pos)> advances <cpp|pos> over
  the next token and returns a <cpp|text_property> which describes it: the
  kind of space to insert before and after (<cpp|spc_before>,
  <cpp|spc_after>, as indices <cpp|SPC_*> into a table of spaces obtained
  from the font), the penalties for breaking before and after
  (<cpp|pen_before>, <cpp|pen_after>), and, in mathematics, the operator
  type <cpp|op_type>, the <cpp|limits> behaviour and an optional
  <cpp|macro> for rendering.

  In text mode, <cpp|typeset_text_string> emits a <cpp|STRING_ITEM> per
  word (<cpp|typeset_substring>). Spaces do not become items: they are
  added to the <cpp|spc> field of the previous item (<cpp|print (space)>
  takes the maximum of the old and new space), and the penalty of that item
  is lowered to the one of the space with <cpp|penalty_min>, which makes the
  space a legal breakpoint. The table of spaces is
  <cpp|env-\<gtr\>fn-\<gtr\>get_normal_spacing (env-\<gtr\>spacing_policy)>.

  In math mode, <cpp|typeset_math_string> in addition consults the table
  <cpp|succession_status> to suppress spaces between operators that should
  not be separated (for instance a prefix minus after an infix operator),
  forbids breaks after infix operators when their space is removed, and
  uses narrow or wide spacing in condensed and display style. Program mode
  (<cpp|typeset_prog_string>) colors tokens according to the syntax
  highlighting of the language.

  Other commands influence penalties directly: <markup|line-break> sets the
  penalty of the previous item to 0, <markup|no-break> sets it to
  <cpp|HYPH_INVALID>, and <cpp|typeset_hgroup> temporarily replaces the
  language by <cpp|hyphenless_language (env-\<gtr\>lan)> and forbids breaks
  inside the group.

  <subsection|Post-processing>

  When the traversal is over, <cpp|concater_rep::finish> in
  <verbatim|Typeset/Concat/concat_post.cpp> performs four passes over the
  item array:

  <\enumerate>
    <item><cpp|kill_spaces> removes the spaces adjacent to control items at
    the borders, around forced line breaks and around <verbatim|env_par>
    and <verbatim|env_page> controls.

    <item><cpp|pre_glue> merges a subscript immediately followed by a
    superscript (or conversely) into a single script item.

    <item><cpp|handle_brackets> matches left and right bracket items. For
    each matching pair, <cpp|handle_scripts> glues scripts to their base
    (the item before or after them) and <cpp|handle_matching> computes the
    vertical extent of the enclosed material, resizes the brackets
    accordingly, and increments the penalties of all enclosed items. As a
    consequence, line breaks are more expensive inside nested brackets.

    <item><cpp|clean_and_correct> removes obsolete items and inserts
    italic corrections between adjacent boxes.
  </enumerate>

  <section|Paragraph formatting>

  <subsection|From line items to page items>

  A paragraph of the document is formatted by
  <cpp|typeset_stack> in <verbatim|Typeset/Line/lazy_paragraph.cpp>:

  <\cpp-code>
    array\<less\>page_item\<gtr\>

    typeset_stack (edit_env env, tree t, path ip,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ array\<less\>line_item\<gtr\> a, array\<less\>line_item\<gtr\> b,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ stack_border& sb)

    {

    \ \ lazy_paragraph par (env, ip);

    \ \ par-\<gtr\>a= a;

    \ \ par-\<gtr\>a \<less\>\<less\> typeset_concat_or_table (env, t, ip);

    \ \ par-\<gtr\>a \<less\>\<less\> b;

    \ \ par-\<gtr\>format_paragraph ();

    \ \ sb= par-\<gtr\>sss-\<gtr\>sb;

    \ \ return par-\<gtr\>sss-\<gtr\>l;

    }
  </cpp-code>

  The arrays <cpp|a> and <cpp|b> are the surroundings accumulated by the
  bridges (markers, parts of <markup|surround>). The function
  <cpp|typeset_concat_or_table> calls the concater, except for a top-level
  <markup|table>, which is typeset row by row with
  <cpp|typeset_as_var_table> so that it can be broken across pages (each
  row becomes an item followed by a <markup|next-line> control).

  The constructor of <cpp|lazy_paragraph_rep> reads all paragraph
  parameters from the environment: <verbatim|par-mode> (<cpp|mode>:
  justify, left, center, right), <verbatim|par-flexibility>,
  <verbatim|par-hyphen>, the width and margins (<cpp|width>, <cpp|left>,
  <cpp|right>, first indentation), the line and paragraph separations
  (<cpp|line_sep>, <cpp|par_sep>), the shoving parameters (<cpp|sep>,
  <cpp|hor_sep>, <cpp|ver_sep>), the number of columns, and the
  micro-typographic parameters <cpp|kreduce>, <cpp|kstretch>,
  <cpp|contraction>, <cpp|expansion> and <cpp|protrusion>. The lines
  produced are sent to a <cpp|stacker_rep>, stored in the field
  <cpp|sss>.

  <subsection|Units and lines>

  <cpp|lazy_paragraph_rep::format_paragraph> splits the item array into
  <em|paragraph units> at every <markup|new-line> control. For each unit it
  determines the style parameters in force (local changes of
  <verbatim|par-first> or <verbatim|par-no-first> are passed as
  <verbatim|env_par> control items), formats the unit with
  <cpp|format_paragraph_unit>, ends the last line with penalty 0 and calls
  <cpp|sss-\<gtr\>new_paragraph (par_sep)>.

  <cpp|format_paragraph_unit> further splits the unit at <markup|next-line>
  controls, which force a line break without starting a new paragraph, and
  calls <cpp|line_units> on each part. The latter computes the optimal
  break points with <cpp|line_breaks> (see below), and then produces the
  lines one by one:

  <\itemize>
    <item><cpp|line_start> clears the per-line arrays <cpp|items>,
    <cpp|items_sp>, <cpp|spcs>, <cpp|fl> and <cpp|notes>.

    <item><cpp|line_unit> prints the items between two break points with
    <cpp|line_print> and adjusts the line with <cpp|make_unit>.
    <cpp|line_print> splits hyphenated items at the break points, collects
    floats (<cpp|FLOAT_ITEM>, via <cpp|get_leaf_lazy>) and marginal notes,
    registers tabulations, and interprets control items: vertical spaces,
    page-break directives and <verbatim|env_page> tuples are passed on to
    the stacker.

    <item><cpp|make_unit> computes the final inter-item spaces
    <cpp|items_sp>. If the line contains tabs, the remaining space is
    distributed over them according to their weights. In justified mode,
    the natural width <cpp|cur_w-\<gtr\>def> is stretched towards
    <cpp|cur_w-\<gtr\>max> or shrunk towards <cpp|cur_w-\<gtr\>min>; if the
    stretch factor exceeds <cpp|flexibility>, the line is left ragged.
    Before giving up, the routine tries glyph expansion and increased
    kerning (<cpp|expand_glyphs>, <cpp|increase_kerning>) when stretching,
    or glyph contraction and reduced kerning (<cpp|contract_glyphs>,
    <cpp|decrease_kerning>) when shrinking. Margin kerning
    (<cpp|protrude>) is applied if enabled. For centered and right-aligned
    lines the first space is increased accordingly.

    <item><cpp|line_end> applies line decorations (<markup|datoms>), places
    the marginal notes, builds a <cpp|phrase_box> of the items and prints it
    on the stacker together with its floats, the vertical space
    <cpp|line_sep> and the page-break penalty (1 inside a paragraph, 0 at
    its end, and at least <verbatim|par-min-penalty>).
  </itemize>

  The phrase boxes produced here are the ones used to detect changed
  regions of the screen; see <hlink|incremental
  typesetting|typesetter-bridges.en.tm>.

  <section|The line breaking algorithm>

  <subsection|Break points>

  The line breaker lives in <verbatim|Typeset/Line/line_breaker.cpp>. Its
  interface is:

  <\explain>
    <cpp|array\<less\>path\<gtr\> line_breaks (array\<less\>line_item\<gtr\>
    a, int start, int end, SI line_width, SI large_width, SI first_spc, SI
    last_spc, bool ragged)><explain-synopsis|compute line breaks>
  <|explain>
    Returns the sequence of break points for the items <cpp|a[start]>,
    ..., <cpp|a[end-1]>, starting with <cpp|path (start)> and ending
    with <cpp|path (end)>. The parameter <cpp|first_spc> is the indentation
    of the first line and <cpp|last_spc> a space to be reserved at the end
    of the last line. <cpp|large_width> is a width beyond which no line may
    extend even with maximal shrinking of glyphs and kerning.
  </explain>

  A break point is represented by a <cpp|path>. The path <cpp|(i)> means a
  break before item <cpp|i>. A longer path <cpp|(i, j)> means a break inside
  the string item <cpp|i>, after hyphenation position <cpp|j>; further
  elements denote successive hyphenations of the remainder of the same item
  (a very long word may be broken more than once). The helper
  <cpp|hyphenate (item, pos, item1, item2)> splits a string item at a
  hyphenation position into the part before the break (including the
  hyphen, with the hyphenation penalty) and the part after it.

  <subsection|The optimal algorithm>

  By default (<verbatim|par-hyphen> is <verbatim|professional>), the
  breaks are computed by <cpp|line_breaker_rep::compute_breaks>, a dynamic
  programming algorithm in the spirit of the one of <TeX>. For each
  reachable break point <cpp|p>, the hashmap <cpp|best> stores an
  <cpp|lb_info>: the best previous break <cpp|prev>, the accumulated
  penalty <cpp|pen> and the accumulated spacing cost <cpp|pen_spc>.
  Solutions are compared lexicographically (<cpp|test_better>): a smaller
  total penalty always wins, and the spacing cost only decides between
  solutions with equal penalties.

  <cpp|process (pos)> considers the lines that start at <cpp|pos>. It walks
  over the subsequent items, accumulating the width of the line as a
  <cpp|space> (sum of the box widths and the stretchable separating
  spaces). For each item with a penalty below <cpp|HYPH_INVALID>, and for
  forced <markup|line-break>s, it calls <cpp|propose_break>. When the line
  becomes too long and the current item is a string of more than four
  characters, <cpp|break_string> additionally proposes breaks at its
  hyphenation points. The walk stops at the first proposed break for which
  the minimal width of the line exceeds <cpp|large_width>. Finally, <cpp|process> recurses into the break points
  inside the first item which have been reached.

  <cpp|propose_break (new_pos, old_pos, pen, spc)> proposes a line from
  <cpp|old_pos> to <cpp|new_pos> of width <cpp|spc>. During the first pass,
  a line is acceptable if it can be shrunk or stretched to the line width
  (<cpp|spc-\<gtr\>min \<less\>= line_width \<less\>=
  spc-\<gtr\>max>); the last line only needs to be short enough. Its cost
  is the square of the difference between its natural width and the line
  width, in pixels (zero for the last line), and its penalty is the one of
  the break. The total penalty is capped at <cpp|HYPH_INVALID>.

  <cpp|compute_breaks> runs the first pass over all items. If no acceptable
  solution reaches the end (<cpp|best [path (end)]-\<gtr\>pen ==
  HYPH_INVALID>), a second pass accepts underfull and overfull lines with
  penalty <cpp|HYPH_INVALID> and a cost growing quadratically with the
  amount by which they are too short or too long; lines exceeding
  <cpp|large_width> receive a huge extra cost. The solution is then read
  back from <cpp|path (end)> through the <cpp|prev> links
  (<cpp|get_breaks>). A last fix avoids a final line consisting only of
  empty boxes.

  Note that <cpp|line_breaks> adds a tolerance of 5 units to the line width
  to absorb rounding errors when box widths add up to exactly one
  <verbatim|par>.

  <subsection|The greedy algorithm>

  If <verbatim|par-hyphen> is <verbatim|normal>, <cpp|ragged> is true and
  <cpp|compute_ragged_breaks> is used instead. Starting from the beginning
  of the paragraph, <cpp|next_ragged_break> fills the line with as many
  items as fit, then backtracks to the last position where a break is
  allowed, trying hyphenation of the overflowing word first.
  <cpp|empty_line_fix> avoids lines containing only zero-width items.

  <subsection|Hyphenation hooks>

  The line breaker never interprets words itself. It relies on two virtual
  methods of <cpp|language_rep> (<verbatim|System/Language/language.hpp>):

  <\itemize>
    <item><cpp|array\<less\>int\<gtr\> get_hyphens (string s)> returns, for
    each position in <cpp|s>, the penalty for a hyphenation after it
    (<cpp|HYPH_STD> where allowed, <cpp|HYPH_INVALID> elsewhere).

    <item><cpp|void hyphenate (string s, int after, string& l, string& r)>
    splits <cpp|s> at the given position, adding a hyphen to <cpp|l> if
    appropriate.
  </itemize>

  Natural languages implement them with hyphenation patterns
  (<verbatim|System/Language/hyphenate.cpp>); programming languages allow
  breaks only at certain characters. A new language, or a new hyphenation
  strategy, therefore only has to implement these methods (together with
  <cpp|advance>, which determines word boundaries, spaces and break
  penalties between words). The wrapper <cpp|hyphenless_language> disables
  hyphenation.

  <section|Vertical stacking>

  The <cpp|stacker_rep> (<verbatim|Typeset/Stack/stacker.hpp>) collects the
  lines of a paragraph into an array <cpp|l> of page items and maintains
  the border properties <cpp|sb>. Its methods are called by the paragraph
  formatter:

  <\description-paragraphs>
    <item*|<cpp|print (box b, array\<less\>lazy\<gtr\> fl, int
    nr_cols)>>Appends a line. The vertical space between the previous
    line and the new one is computed by the static function <cpp|shove>
    (the flag <cpp|unit_flag>, which could suppress this, is never set in
    the current code).

    <item*|<cpp|print (tree t, int nr_cols, bool before)>>Appends a page
    control item (after the current line unless <cpp|before> is true).

    <item*|<cpp|print (space spc)>, <cpp|penalty (int pen)>>Add vertical
    space or set the penalty after the last real line.

    <item*|<cpp|vspace_before>, <cpp|vspace_after>,
    <cpp|no_page_break_before>, <cpp|no_page_break_after>,
    <cpp|no_break_start>, <cpp|no_break_end>>Record the effect of
    <markup|vspace*>, <markup|vspace>, <markup|no-page-break*>,
    <markup|no-page-break> and of no-break regions.

    <item*|<cpp|new_paragraph (space par_sep)>>Closes a paragraph unit: adds
    the paragraph separation and pending vertical spaces, and multiplies
    the penalties of the first and last lines of the unit by 100 and of the
    second and next-to-last lines by 10. This implements widow and orphan
    control: breaking a page one or two lines away from a paragraph border
    is expensive.
  </description-paragraphs>

  <cpp|shove> determines the distance between two successive lines. If the
  lines are far enough apart for the default baseline distance
  <cpp|height>, it simply uses it. Otherwise, it tries to <em|shove> the
  lines into each other: <cpp|shove_in> computes, column by column, the
  vertical distance needed to keep the ink of the two lines at least
  <cpp|ver_sep> apart, taking into account a horizontal separation
  <cpp|hor_sep>. Thus a line with a tall formula only pushes the next line
  down if the formula actually collides with something below it. The
  optional <cpp|swell> parameters increase the separation for lines with
  large mathematical content.

  <cpp|merge_stack (l, sb, l2, sb2)> appends the page items <cpp|l2> of a
  paragraph to those of the preceding material <cpp|l>, applying
  <cpp|shove> between the last line of <cpp|l> and the first line of
  <cpp|l2>, adding the larger of the pending vertical spaces
  <cpp|sb-\<gtr\>vspc_after> and <cpp|sb2-\<gtr\>vspc_before>, and
  forbidding a page break if one of the two sides requires it. This is how
  <cpp|typesetter_rep::insert_stack> assembles the cached paragraphs into
  the page items of the whole document.

  Two simpler functions are used for vertical material inside lines:
  <cpp|typeset_as_stack> (in <verbatim|Typeset/Stack/stacker.cpp>) stacks
  the children of a <markup|document> each typeset on a single line, and
  <cpp|typeset_as_paragraph> (in <verbatim|Typeset/Line/lazy_vstream.cpp>)
  formats a paragraph with a <cpp|lazy_paragraph> and returns the lines as
  a single box with <cpp|format_vstream_as_box>.

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
