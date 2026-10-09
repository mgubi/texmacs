<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Incremental typesetting: the typesetter and its bridges>

  <section|The typesetter object>

  A typesetter is an instance of <cpp|typesetter_rep>, declared in
  <source-link|Typeset/Bridge/impl_typesetter.hpp|src/Typeset/Bridge/impl_typesetter.hpp>; the type <cpp|typesetter>
  is a plain pointer to it. Its main fields are:

  <\cpp-code>
    class typesetter_rep {

    public:

    \ \ edit_env&\ \ \ \ env;\ \ \ \ \ \ \ \ \ // the typesetting environment

    \ \ bridge\ \ \ \ \ \ \ br;\ \ \ \ \ \ \ \ \ \ // root bridge (for the document body)

    \ \ rectangles\ \ \ change_log;\ \ // regions touched since last typesetting

    \ \ array\<less\>brush\<gtr\> old_bgs;\ \ \ \ \ // page backgrounds of previous pass

    \ \ array\<less\>page_item\<gtr\> l;\ \ \ \ \ // current lines

    \ \ stack_border\ \ \ \ \ sb;\ \ \ \ // border properties

    \ \ array\<less\>line_item\<gtr\> a;\ \ \ \ \ // left surroundings

    \ \ array\<less\>line_item\<gtr\> b;\ \ \ \ \ // right surroundings

    \ \ hashmap\<less\>string,tree\<gtr\> old_patch;

    \ \ bool paper;

    \ \ ...

    };
  </cpp-code>

  The typesetter does not own the environment: it shares the
  <cpp|edit_env> of the editor. During a typesetting pass, the fields
  <cpp|l> and <cpp|sb> accumulate the page items of the document (see
  <hlink|paragraph formatting|typesetter-lines.en.tm>), while <cpp|a> and
  <cpp|b> hold line items which have to be prepended <abbr|resp.> appended
  to the next paragraph (for instance the markers of an enclosing
  <markup|with>, or the left and right parts of a <markup|surround>). The
  hashmap <cpp|old_patch> is used to detect that the environment seen by a
  paragraph differs from the one it saw during the previous pass; see
  below.

  The method <cpp|typesetter_rep::typeset ()> (in
  <source-link|Typeset/Bridge/typesetter.cpp|src/Typeset/Bridge/typesetter.cpp>) performs one complete pass:

  <\enumerate>
    <item>It resets <cpp|l>, <cpp|sb>, <cpp|a>, <cpp|b> and
    <cpp|old_patch>.

    <item>It determines whether the pass will be <em|complete>, <abbr|i.e.>
    whether every bridge will be re-typeset
    (<cpp|br-\<gtr\>my_typeset_will_be_complete ()>, and no
    <markup|show-preamble> or <markup|hide-part> is present). In that case,
    <cpp|env-\<gtr\>complete> is set and the tables used for collecting
    references (<cpp|local_aux>, <cpp|missing>, <cpp|redefined>,
    <cpp|touched>) are cleared.

    <item>It calls <cpp|br-\<gtr\>typeset (PROCESSED+WANTED_PARAGRAPH)> on
    the root bridge, which walks the bridge tree, re-typesets the invalid
    parts and appends all page items to <cpp|l>.

    <item>It creates a <cpp|pager_rep> on <cpp|l> and calls
    <cpp|make_pages>, which breaks the items into pages and returns the
    box for the whole document.

    <item>On paper, during a complete pass, it collects the page numbers of
    all labels (<cpp|determine_page_references>) so that page references
    can be resolved.
  </enumerate>

  <section|Inverse paths>

  Every box and every line item remembers where in the source tree it comes
  from, through an <em|inverse path> <cpp|ip>: the path from the root of the
  edit tree to the subtree, stored in reverse order so that common prefixes
  are shared. The details are explained in <hlink|the boxes|boxes.en.tm>;
  here we only recall the helpers from <source-link|Typeset/boxes.hpp|src/Typeset/boxes.hpp> which
  are used throughout the typesetter:

  <\description-paragraphs>
    <item*|<cpp|descend (ip, i)>>The inverse path of the <cpp|i>-th child.
    If <cpp|ip> is a decoration, <cpp|ip> is returned unchanged: the
    children of an inaccessible subtree are inaccessible too.

    <item*|<cpp|decorate (ip)>, <cpp|decorate_left>,
    <cpp|decorate_middle>, <cpp|decorate_right>>Prepend the negative
    markers <cpp|DECORATION>, <cpp|DECORATION_LEFT>,
    <cpp|DECORATION_MIDDLE> or <cpp|DECORATION_RIGHT>. Boxes with such an
    inverse path are not editable (<cpp|is_decoration>); clicking on them
    places the cursor before, inside or after the subtree at <cpp|ip>.

    <item*|<cpp|attach_right (t, ip)>, <cpp|attach_here>,
    <cpp|attach_middle>, <cpp|attach_deco>>Macros expanding to two
    arguments <cpp|t, ip'>. They attach an inverse path to a tree which is
    not part of the edit tree (typically a macro body or the result of an
    evaluation) through <cpp|attach_dip>. Later, <cpp|obtain_ip (t)>
    recovers the inverse path of such a tree, or of any subtree of the edit
    tree, from the <cpp|ip_observer> attached to it.
  </description-paragraphs>

  Since subtrees of the edit tree carry their inverse path in their
  observers, the concater and the bridges start by checking
  <cpp|is_accessible (ip)>; if the proposed inverse path is a decoration,
  they try <cpp|obtain_ip (t)> and use the real source location when the
  tree comes from the document (for instance a macro argument). This is how
  the text of a macro argument remains editable, although it is typeset
  while traversing the macro body.

  <section|Bridges>

  <subsection|The bridge classes>

  A bridge (<source-link|Typeset/Bridge/bridge.hpp|src/Typeset/Bridge/bridge.hpp>) connects a subtree of the
  document to its typeset representation:

  <\cpp-code>
    class bridge_rep: public abstract_struct {

    public:

    \ \ typesetter\ \ \ \ \ \ \ \ \ \ \ ttt;\ \ \ \ \ \ // the underlying typesetter

    \ \ edit_env&\ \ \ \ \ \ \ \ \ \ \ \ env;\ \ \ \ \ \ // the environment

    \ \ tree\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ st;\ \ \ \ \ \ \ // the present subtree

    \ \ path\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ip;\ \ \ \ \ \ \ // source location of the paragraph

    \ \ int\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ status;\ \ \ // status among above values

    \ \ hashmap\<less\>string,tree\<gtr\> changes;\ \ // changes in the environment

    \ \ array\<less\>page_item\<gtr\>\ \ \ \ \ l;\ \ \ \ \ \ \ \ // the typesetted lines of st

    \ \ stack_border\ \ \ \ \ \ \ \ \ sb;\ \ \ \ \ \ \ // border properties of l

    \ \ link_repository\ \ \ \ \ \ link_env; // loci and links declared inside bridge

    \ \ ...

    };
  </cpp-code>

  The field <cpp|status> combines two bits. The bit <cpp|VALID_MASK> is
  either <cpp|CORRUPTED> (the cached lines are out of date) or
  <cpp|PROCESSED>. The bit <cpp|WANTED_MASK> records whether the bridge was
  typeset as a paragraph (<cpp|WANTED_PARAGRAPH>) or as a paragraph unit
  (<cpp|WANTED_PARUNIT>). The cached lines are reused only if the status
  equals the desired status.

  Bridges are created by <cpp|make_bridge> in
  <source-link|Typeset/Bridge/bridge.cpp|src/Typeset/Bridge/bridge.cpp>, which dispatches on the label of
  the subtree:

  <\description-paragraphs>
    <item*|<cpp|bridge_document>>For <markup|document>: one sub-bridge per
    paragraph. This is the bridge that makes typesetting incremental at the
    paragraph level.

    <item*|<cpp|bridge_with>>For <markup|with>: evaluates the variable
    assignments, then typesets the body with its own bridge. Similar
    bridges exist for <markup|surround> (<cpp|bridge_surround>),
    <markup|locus> (<cpp|bridge_locus>), <markup|mark>
    (<cpp|bridge_mark>), <markup|expand-as> (<cpp|bridge_expand_as>), and
    the decoration and formatting tags <markup|datoms>, <markup|dlines>,
    <markup|dpages> and <markup|tformat> (<cpp|bridge_formatting>).

    <item*|<cpp|bridge_compound>>For macro applications (<markup|compound>
    and all tags <cpp|L(st) \<gtr\>= START_EXTENSIONS>), as well as
    <markup|include>, <markup|hlink>, <markup|action> and a few others. It
    looks up the macro, binds the arguments in <cpp|env-\<gtr\>macro_arg>
    and <cpp|env-\<gtr\>macro_src>, and typesets the macro body through a
    sub-bridge.

    <item*|<cpp|bridge_argument>>For <markup|arg> inside a macro body: it
    typesets the value of the argument through a sub-bridge whose inverse
    path is the source location of the argument.

    <item*|<cpp|bridge_eval>, <cpp|bridge_rewrite>, <cpp|bridge_auto>>For
    constructs whose body is computed (<markup|eval>, <markup|quasi>,
    <markup|map-args>, <markup|extern>, animations; inactive markup and
    error markers go through <cpp|bridge_auto>).

    <item*|<cpp|bridge_hidden>>For <markup|hidden>: typesets the content
    (so that side effects such as labels take place) but flags the
    resulting items as <cpp|PAGE_HIDDEN_ITEM> of zero height.

    <item*|<cpp|bridge_ornament>, <cpp|bridge_art_box>,
    <cpp|bridge_canvas>>For the GUI-like containers in
    <source-link|bridge_gui.cpp|src/Typeset/Bridge/bridge_gui.cpp>.

    <item*|<cpp|bridge_default>>For every other primitive: the subtree is an
    ordinary paragraph, and it is re-typeset as a whole when anything inside
    it changes.
  </description-paragraphs>

  In the preamble mode (<cpp|env-\<gtr\>preamble>), all constructs are
  shown in source form through <cpp|make_inactive_bridge>.

  The granularity of incremental typesetting follows from this list:
  bridges are only created down to the level of paragraphs (and of the
  bodies of the structural constructs which may contain paragraphs). Inside
  a paragraph, there are no bridges any more; a modification anywhere in
  the paragraph invalidates the whole paragraph, which is then re-typeset
  from scratch by the concater and the line breaker.

  <subsection|Notification of changes>

  The editor informs the typesetter about each elementary modification of
  the edit tree. The functions in <source-link|Edit/Modify/edit_modify.cpp|src/Edit/Modify/edit_modify.cpp>
  (<cpp|edit_modify_rep::notify_assign> <abbr|etc.>) convert the path to a
  path relative to the root of the document (<cpp|p / rp>) and call the
  global <cpp|notify_assign>, <cpp|notify_insert>, <cpp|notify_remove>,
  <cpp|notify_split>, <cpp|notify_join>, <cpp|notify_assign_node>,
  <cpp|notify_insert_node> and <cpp|notify_remove_node> from
  <source-link|Typeset/Bridge/typesetter.cpp|src/Typeset/Bridge/typesetter.cpp>. These in turn call the virtual
  methods of the root bridge. An assignment at the empty path replaces the
  root bridge altogether; the three <cpp|*_node> variants are rewritten
  into assignments of the parent.

  The abstract class only requires <cpp|notify_assign>, <cpp|notify_macro>
  and <cpp|notify_change>. Its default implementations of
  <cpp|notify_insert>, <cpp|notify_remove>, <cpp|notify_split> and
  <cpp|notify_join> compute the new value of the parent subtree and call
  <cpp|notify_assign> on it. Bridges that care about finer
  notifications override them:

  <\itemize>
    <item><cpp|bridge_document_rep> keeps an array <cpp|brs> of
    sub-bridges. An assignment or modification below paragraph <cpp|i> is
    forwarded to <cpp|brs[i]> only; an insertion or removal of paragraphs
    creates or deletes sub-bridges and shifts the last item of the inverse
    paths of the following bridges (<cpp|brs2[i+nr]-\<gtr\>ip-\<gtr\>item
    += nr>), without touching their cached lines. The neighbours of the
    modified range are marked with <cpp|notify_change>, because they may be
    affected by surroundings; when the removed paragraphs changed the
    environment (non-empty <cpp|changes>), all following paragraphs are
    marked as well.

    <item><cpp|bridge_with_rep> and similar forward modifications of the
    body to the body bridge, and mark the body as changed
    (<cpp|body-\<gtr\>notify_change ()>) when one of the environment
    assignments is modified. When the body switches between single- and
    multi-paragraph (<cpp|is_multi_paragraph>), the sub-bridge is rebuilt.

    <item><cpp|bridge_compound_rep> does not re-expand the macro when an
    argument changes. Instead, it translates the modification of argument
    <cpp|k> into a call <cpp|notify_macro (MACRO_ASSIGN, var, -1, p, u)>
    (or <cpp|MACRO_INSERT>, <cpp|MACRO_REMOVE>), where <cpp|var> is the
    name of the corresponding macro variable, and sends it to the body
    bridge. Each nested macro application increases the <em|level>. A
    <cpp|bridge_argument_rep> whose name matches <cpp|var> at level zero
    forwards the modification to the sub-bridge for the argument value, so
    that only the affected part of the macro expansion is invalidated.
    Bridges for which no finer information is available fall back on
    <cpp|env-\<gtr\>depends (st, var, level)>, which tests whether the
    subtree depends on the variable.

    <item><cpp|bridge_eval_rep> re-evaluates its body at each typesetting
    and uses the four-argument <cpp|replace_bridge (br, p, oldt, newt, ip)>,
    which compares the old and new expansions and only notifies the
    subtrees that actually differ.
  </itemize>

  Every <cpp|notify_*> method sets <cpp|status= CORRUPTED> on all bridges
  along the path from the root to the modification. Notification is cheap:
  it only updates <cpp|st> and the status flags.

  <subsection|Typesetting a bridge>

  The non-virtual method <cpp|bridge_rep::typeset (int desired_status)> is
  the heart of the incremental algorithm. Abridged, it reads:

  <\cpp-code>
    void

    bridge_rep::typeset (int desired_status) {

    \ \ if (is_accessible (ip)) st= subtree (the_et, reverse (ip));

    \ \ ...

    \ \ if ((status==desired_status) && (N(ttt-\<gtr\>old_patch)==0))

    \ \ \ \ env-\<gtr\>monitored_patch_env (changes);\ \ \ // reuse cached lines

    \ \ else {

    \ \ \ \ hashmap\<less\>string,tree\<gtr\> prev_back (UNINIT);

    \ \ \ \ my_clean_links ();

    \ \ \ \ ...

    \ \ \ \ ttt-\<gtr\>local_start (l, sb);

    \ \ \ \ env-\<gtr\>local_start (prev_back);

    \ \ \ \ my_typeset (desired_status);\ \ \ \ \ \ \ \ \ \ \ // really typeset

    \ \ \ \ env-\<gtr\>local_update (ttt-\<gtr\>old_patch, changes);

    \ \ \ \ env-\<gtr\>local_end (prev_back);

    \ \ \ \ ttt-\<gtr\>local_end (l, sb);

    \ \ \ \ ...

    \ \ \ \ status= desired_status;

    \ \ }

    \ \ ... ttt-\<gtr\>insert_stack (l, sb);

    }
  </cpp-code>

  Two conditions have to hold for the cached result to be reused: the
  bridge must not be corrupted, and the environment in which it is typeset
  must be the same as during the previous pass. The first condition is
  maintained by the notifications. The second is tracked through
  environment patches:

  <\itemize>
    <item>While a bridge is typeset, <cpp|env-\<gtr\>local_start> starts
    recording the old values of all variables written in the environment
    (the hashmap <cpp|back>). Afterwards, <cpp|env-\<gtr\>local_update>
    computes the net change made by the bridge (for instance by an
    <markup|assign> or a counter increment) and stores it in
    <cpp|changes>.

    <item>When a cached bridge is skipped, its effect on the environment is
    replayed by <cpp|monitored_patch_env (changes)>, so that the following
    bridges see the right environment without re-executing anything.

    <item>The hashmap <cpp|ttt-\<gtr\>old_patch> records, for each variable,
    the difference between the value it has now and the value it would
    have had in the previous pass. It becomes non-empty as soon as a
    re-typeset bridge produces changes that differ from the cached ones
    (say, a section was inserted, so that all following section numbers
    change), and empties again when the environments agree. As long as it
    is non-empty, the cache test fails, and the subsequent bridges are
    re-typeset.
  </itemize>

  The virtual method <cpp|my_typeset> does the real work. For
  <cpp|bridge_rep> (and hence <cpp|bridge_default>) it calls
  <cpp|ttt-\<gtr\>insert_paragraph (st, ip)>, which typesets the paragraph
  with <cpp|typeset_stack> (see <hlink|paragraph
  formatting|typesetter-lines.en.tm>) and merges the resulting page items
  into <cpp|ttt-\<gtr\>l> with <cpp|merge_stack>. Structural bridges
  instead modify the environment, possibly insert markers, and recurse:

  <\cpp-code>
    void

    bridge_with_rep::my_typeset (int desired_status) {

    \ \ ... evaluate the variables and new values ...

    \ \ for (i=0; i\<less\>k; i++) env-\<gtr\>write_update (vars[i], newv[i]);

    \ \ ttt-\<gtr\>insert_marker (st, ip);

    \ \ body-\<gtr\>typeset (desired_status);

    \ \ for (i=k-1; i\<gtr\>=0; i--) env-\<gtr\>write_update (vars[i], oldv[i]);

    }
  </cpp-code>

  <cpp|insert_marker> adds zero-width marker line items (produced by
  <cpp|typeset_marker>) for the positions <cpp|descend (ip, 0)> and
  <cpp|descend (ip, 1)> to the surroundings <cpp|ttt-\<gtr\>a> and
  <cpp|ttt-\<gtr\>b>. They will be glued to the first <abbr|resp.> last
  paragraph of the body, so that the cursor can be placed just before or
  after the <markup|with>. Similarly, <cpp|bridge_surround_rep> typesets
  the left and right parts of a <markup|surround> with
  <cpp|typeset_concat> and pushes them with <cpp|insert_surround>.
  <cpp|bridge_document_rep::my_typeset> hands the pending left
  surroundings only to its first paragraph and the right surroundings only
  to its last one.

  When the pass is not on paper (<cpp|ttt-\<gtr\>paper> is false), a bridge
  producing several lines without floats or multiple columns packs them
  into a single page item containing a <cpp|stack_box>. This reduces the
  number of page items the page breaker has to deal with in the (single
  page) <verbatim|papyrus> mode. Control items for
  <cpp|PAGE_THIS_TOP>, <cpp|PAGE_THIS_BOT> and <cpp|PAGE_THIS_BG_COLOR> are
  kept separately.

  The accelerator <cpp|bridge_docrange> in
  <source-link|Typeset/Bridge/bridge_docrange.cpp|src/Typeset/Bridge/bridge_docrange.cpp>, which would organize long
  documents in a binary tree of ranges, is currently disabled
  (<cpp|bridge_document_rep::initialize_acc> always sets <cpp|acc> to the
  nil bridge).

  <subsection|What is re-typeset after an edit>

  Consider typing a character inside the fifth paragraph of a document
  body, which is itself inside a <markup|with>. The editor calls
  <cpp|notify_insert> with the path of the string. The root
  <cpp|bridge_document> forwards it to <cpp|brs[k]> (the <markup|with>),
  which forwards it to its body bridge, a <cpp|bridge_document>, which
  forwards it to the <cpp|bridge_default> of the fifth paragraph. The
  latter only updates its <cpp|st>. All bridges along this path are now
  <cpp|CORRUPTED>.

  At the next <cpp|typeset>, the root bridge and the <markup|with> bridge
  are re-executed (their <cpp|my_typeset> is cheap: they mostly recurse),
  all sibling paragraphs are reused from their caches, and only the fifth
  paragraph goes through concatenation, line breaking and stacking. If the
  modified paragraph changes the environment differently than before
  (typically: a new numbered environment), <cpp|old_patch> becomes non-empty
  and the following paragraphs are re-typeset until the environments agree
  again. Finally, page breaking is done again for the whole list of page
  items.

  The cost of these stages can be measured: with the environment variable
  <verbatim|TEXMACS_EDIT_PROFILE> set, every typesetting which follows an
  edit prints the time spent in the bridges (with the number of bridges
  re-executed and reused), in the pager and in the change log, with the
  part of the view which the editor invalidates, and every repaint prints
  its area and its time (<cpp|edit_profile> in
  <source-link|typesetter.cpp|src/Typeset/Bridge/typesetter.cpp>). In a
  document of 140 pages (1680 paragraphs, 6400 lines, measured in October
  2026 on the <name|Vue> port with the software renderer), a character
  typed in a paragraph costs 2<nbsp>ms in the bridges and 1<nbsp>ms in
  the change log, but 50<nbsp>ms in the pager on paper; on papyrus, with
  no pages to break, the whole typesetting takes 11<nbsp>ms, of which
  8<nbsp>ms go to the visit of the bridges which are reused. A new
  section, which renumbers what follows, re-executes 2600 bridges in
  120<nbsp>ms.

  Most of the time of the pager is the search of the page breaks, which
  tries every line as the start of a page (40<nbsp>ms of the 50). Its
  result, the skeleton, is a function of the heights, spaces, penalties,
  columns and floating objects of the page items, not of their boxes, and
  most edits do not change these numbers: the skeletons of the last few
  such signatures are kept and returned without a search
  (<cpp|new_break_pages> in
  <source-link|new_breaker.cpp|src/Typeset/Page/new_breaker.cpp>; the
  environment variable <verbatim|TEXMACS_PAGE_BREAK_CACHE> may be
  <verbatim|off>, or <verbatim|check> to search anyway and report a
  difference). A character typed in a line then costs a quarter of the
  time in the pager, which still formats every page with its header and
  footer. An edit which changes the number of lines, or the height of
  one, changes the signature and is searched in full.

  That search finds the same breaks as before, in the same order and with
  the same arithmetic, but faster for the positions with no pending float
  (<cpp|find_page_breaks_plain>): their best previous breaks and penalties
  are in arrays instead of tables indexed by paths, no path is made for
  each candidate page, and the height and the penalty of a candidate are
  computed with integers. <verbatim|TEXMACS_PAGE_BREAK_FAST> may be
  <verbatim|0> for the search as it was, <verbatim|1> to <verbatim|3> for
  the first changes only, or <verbatim|check> to run both searches and
  report a difference. The tables which hold the starts to try are kept
  as they were: the starts are tried in the order of their iteration, on
  which the result may depend.

  <paragraph|Measurements>

  The times below were measured in October 2026 on the <name|Vue> port
  with the software renderer, in a view of 800 by 446 points, on two
  documents made for the purpose: 140 pages of plain paragraphs, sections
  and numbered equations (1680 paragraphs, 6400 lines; the search has
  6200 starts and 345000 candidate pages), and 70 pages with footnotes,
  floats and forced page breaks. They are in milliseconds, and vary by a
  factor of up to two from one run to the next on the same machine: the
  figures compared with each other come from the same runs.

  The search of the page breaks, with the changes up to each one (the
  skeletons not kept, so that every edit searches):

  <\big-table|<block|<tformat|<table|<row|<cell|>|<cell|as it
  was>|<cell|arrays>|<cell|and no paths>|<cell|and integers>|<cell|and
  candidates kept>>|<row|<cell|lines of code>|<cell|>|<cell|70>|<cell|85>|<cell|45>|<cell|105>>|<row|<cell|140
  pages>|<cell|43 to 44>|<cell|30 to 37>|<cell|17 to 25>|<cell|7>|<cell|4
  to 5>>|<row|<cell|70 pages with floats>|<cell|21.5>|<cell|19>|<cell|14.5>|<cell|3>|<cell|3.5>>>>>>
    The search of the page breaks, in milliseconds.
  </big-table>

  The last column is a fourth change: the candidates of a start depend on
  the items from the start to the candidate only, and those of a previous
  search are used again for the starts whose items did not change. It
  gains little on these two documents, for the most code and four
  megabytes of candidates kept for the first one, and was taken out for a
  while. It is what counts in a book with an index, see below.

  The whole of an edit in the first document, on paper, before and after
  these changes and the others of October 2026 (the invalid regions cut
  to the view in the <name|Vue> widget, the skeletons kept, the faster
  search):

  <\big-table|<block|<tformat|<table|<row|<cell|>|<cell|bridges>|<cell|pager
  before>|<cell|pager after>|<cell|repaint before>|<cell|repaint
  after>>|<row|<cell|a character in a line>|<cell|2>|<cell|49>|<cell|9 to
  16>|<cell|0.4>|<cell|0.4>>|<row|<cell|a new line>|<cell|3>|<cell|51>|<cell|18
  to 22>|<cell|67>|<cell|0.7>>|<row|<cell|a new section>|<cell|120>|<cell|52>|<cell|20>|<cell|61>|<cell|0.8>>>>>>
    One edit in a document of 140 pages, in milliseconds.
  </big-table>

  Of the 18 to 22<nbsp>ms of the pager for a new line, the search is 6;
  the rest is the setup of the breaker (3 to 5), the signature (2) and
  the pages with their headers and footers (7 to 9), which are made again
  for every page at every pass. On papyrus there is no page to break: the
  whole typesetting of a character takes 11<nbsp>ms, of which 8 are the
  visit of the bridges which are reused.

  A new section, or a new numbered equation, changes counters for the
  rest of the document, and <cpp|old_patch> stays non-empty down to its
  end: every bridge after the edit was typeset again, 2600 of them in
  120<nbsp>ms, whether it used these counters or not. A bridge now
  records the variables which its typesetting reads or writes
  (<cpp|env_table> in <source-link|env.hpp|src/Typeset/env.hpp>, and
  <cpp|bridge_rep::typeset>), and one whose subtree did not change is
  used again when none of the variables of <cpp|old_patch> is among them.
  The conditions are in the comment before <cpp|bridge_rep::typeset>: the
  bridge must have got no line items from the bridges around it (the
  number of an equation ends up in the lines of its body); its record
  must be complete (the reads of the bridges below it are added to it,
  up to a limit; a <scheme> routine makes it unknown); the references and
  attachments which it looked up, recorded with the values found, must
  still have these values; and the variables which changed must be plain
  ones, from which the environment derives no state when they are
  written. The
  variables which a bridge writes count as read: its
  <cpp|changes> only hold those whose value it changed. In the same way a
  paragraph which is removed no longer has every bridge after it typeset
  again: the bridge which follows keeps the changes of the removed one,
  which the next pass compares with the environment.
  <verbatim|TEXMACS_TYPESET_READS> may be <verbatim|off>, or
  <verbatim|check> to typeset again all the same and report a bridge
  whose lines, contents or changes differ.

  <\big-table|<block|<tformat|<table|<row|<cell|>|<cell|bridges typeset
  again>|<cell|before>|<cell|after>>|<row|<cell|a new section, 140
  pages>|<cell|2625, then 676>|<cell|121>|<cell|25>>|<row|<cell|a new
  equation, 140 pages>|<cell|2642, then 1890>|<cell|128>|<cell|39>>|<row|<cell|a
  new section, 70 pages with floats>|<cell|1566, then
  327>|<cell|88>|<cell|50>>|<row|<cell|a new equation, 70 pages with
  floats>|<cell|1583, then 1067>|<cell|82>|<cell|46>>>>>>
    The bridges after an edit which renumbers, in milliseconds.
  </big-table>

  What is still typeset again are the titles and the equations which
  follow, with the bridges inside them (they read or write the counters
  and the current label), and in the second document the paragraphs with
  footnotes. The first typesetting of a document, where every read is
  recorded, takes the same time within the precision of the measure (143
  and 149<nbsp>ms).

  <paragraph|The user manual>

  The same edits were made in the middle of the user manual, opened as
  one book (<menu|Help|Manual|User manual>): 3000 paragraphs and
  structures at its top level, 18000 bridges, 23000 starts for the pages.
  The whole typesetting of an edit, in milliseconds, with the three
  mechanisms off (<verbatim|TEXMACS_TYPESET_READS=off>,
  <verbatim|TEXMACS_PAGE_BREAK_FAST=0>,
  <verbatim|TEXMACS_PAGE_BREAK_CACHE=off>) and on:

  <\big-table|<block|<tformat|<table|<row|<cell|>|<cell|before>|<cell|after>>|<row|<cell|a
  character in a line>|<cell|416>|<cell|72>>|<row|<cell|a new
  line>|<cell|419>|<cell|93 to 117>>|<row|<cell|two paragraphs
  joined>|<cell|1134>|<cell|78>>|<row|<cell|a new
  section>|<cell|1811>|<cell|155>>|<row|<cell|a new numbered
  equation>|<cell|1142>|<cell|127>>>>>>
    One edit in the middle of the user manual, in milliseconds.
  </big-table>

  A real book gains less than the documents made for the measures. The
  manual has an index in two columns, and the height of a candidate page
  with several columns means balancing them: 137000 such candidates took
  210 of the 250<nbsp>ms of the search, at every change of the number of
  lines anywhere in the book. These candidates are those of the previous
  search as long as the index does not change, which is why the
  candidates of each start are kept: the items of the new search are
  matched with those of a previous one (lines added, removed or changed
  in several places, since a pass after another one also changes the page
  numbers of the table of contents and of the index; three searches are
  kept, the documents which are open being searched in turn), and a start
  whose items follow each other as before takes its candidates from it.
  The search then takes 10 to 16<nbsp>ms, and a new line in the manual
  93 to 117<nbsp>ms instead of 359; the first change of the number of
  lines after the manual is opened still takes 370, the index itself
  having changed with its page numbers. The pages are made again at every
  pass, 50<nbsp>ms for 260 of them.

  <paragraph|The automatic labels>

  A new section in the manual still had 10600 bridges typeset again, in
  700<nbsp>ms, for a reason which is not in the typesetter. The entries
  of the table of contents, of the index, of the glossary and of the
  lists of figures and tables each put a label where they stand, which
  gives their page (<markup|auto-label> in
  <verbatim|std-automatic.ts>). Its name was made of a counter: the label
  after <math|n> others was <verbatim|auto-><math|n>, so that a new
  section renamed the labels of all the index entries after it, of which
  the manual has several in most of its paragraphs on the macros. These
  bridges did make something else, the same boxes with other names.

  The names are now made of numbers which stay: <markup|auto-id> with the
  argument <verbatim|new> takes a number for a new label, and with any
  other one gives the last one taken (<cpp|edit_env_rep::exec_auto_id>).
  The style file also defines a macro of that name, which a version of
  <TeXmacs> without the primitive calls instead, and which numbers the
  labels with the counter as before: the same package works in both.
  The numbers are kept by the bridge which is being typeset: typeset
  again, it gives the same numbers to its labels, in their order, and a
  label which it did not have gets a number which no label has. In a
  complete pass, where every bridge is typeset in the order of the
  document, the labels are numbered from 1 again, as the counter did: a
  document which is opened or updated has the names it was saved with,
  and the saved document is the same, byte for byte, as with the counter.
  Between two updates the names differ, for the better: after a new
  section, a line of the table of contents which was not made again
  still refers to the label of its own section, where with the counter
  it referred to the label of whatever had taken its number. The new
  section in the manual takes 155<nbsp>ms instead of 839, with 570
  bridges typeset again. <verbatim|TEXMACS_STABLE_LABELS=off> uses the
  counter as before.

  The numbers are those of a bridge, that is of one typesetter, while the
  labels of a document go to one table: two windows on the same document
  would give different names to the labels made since the last complete
  pass of each, and a name would stand for two places. While a document
  is shown by several views its labels are therefore numbered by the
  counter, which gives the same names in all of them
  (<cpp|edit_typeset_rep::typeset_sub> sets this, and has the document
  typeset as a whole when it changes).

  The entries of the index itself, 1350 of them with their page numbers,
  and the paragraphs with references are used again since the references
  are recorded. The check mode of the recorded reads found one more
  thing a bridge may depend on, in a paragraph of the manual which shows
  the current time: the date and the animations make a record unknown.

  <subsection|Complete typesetting and references>

  References, tables of contents and page numbers need several passes. The
  loop in <cpp|edit_typeset_rep::typeset> (in
  <source-link|Edit/Editor/edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp>) calls <cpp|typeset_sub>, which
  calls the typesetter, as long as the pass was complete and there remain
  undefined (<cpp|env-\<gtr\>missing>) or redefined
  (<cpp|env-\<gtr\>redefined>) labels whose number decreases. Each new pass
  is forced by <cpp|::notify_assign (ttt, path(), ttt-\<gtr\>br-\<gtr\>st)>,
  which invalidates the whole document. After the loop, unused references
  are removed with <cpp|clean_unused>, and remaining problems are reported
  as warnings.

  <subsection|The environment at a given position>

  The editor frequently needs the environment at the cursor (to know the
  current font, mode, language, <abbr|etc.>). This is done by
  <cpp|edit_typeset_rep::typeset_exec_until (path p)>, which caches the
  result per path in the hashmap <cpp|cur>. Unless the experimental option
  <cpp|enable_fastenv> is set, it calls the global <cpp|exec_until (ttt, p /
  rp)>, and hence <cpp|bridge_rep::exec_until>. A bridge which is up to
  date does not re-execute its subtree: if <cpp|p> is at its right border,
  it just applies its cached <cpp|changes>; otherwise <cpp|my_exec_until>
  descends (for documents, the preceding paragraphs are replayed through
  their cached changes). Corrupted bridges fall back on
  <cpp|env-\<gtr\>exec_until (st, p)>, which evaluates the tree (see
  <hlink|macro expansion|macro-expansion.en.tm>).

  <section|From the typeset box to the screen>

  <subsection|Tracking the modified regions>

  After a pass, only the parts of the screen which actually changed need to
  be repainted. This is achieved without comparing box trees, using the
  following trick. Every line of text in a paragraph is a
  <cpp|phrase_box> (<source-link|Typeset/Boxes/Composite/concat_boxes.cpp|src/Typeset/Boxes/Composite/concat_boxes.cpp>),
  a concatenation box with two extra fields: a pointer <cpp|logs_ptr> to a
  change log and the absolute position <cpp|ox>, <cpp|oy> at which it was
  last placed.

  <\itemize>
    <item>At the end of each pass, <cpp|typesetter_rep::typeset (SI& x1,
    SI& y1, SI& x2, SI& y2)> calls <cpp|b-\<gtr\>position_at (0, 0,
    change_log)> on the new document box. The default implementation in
    <source-link|Typeset/Boxes/Basic/boxes.cpp|src/Typeset/Boxes/Basic/boxes.cpp> just recurses into the
    subboxes with updated offsets. When a <cpp|phrase_box> is reached, it
    prepends a pair to the log: its old rectangle (or the empty rectangle
    if the box is new) and its new rectangle.

    <item>When a line disappears (because its paragraph was re-typeset and
    the old boxes are no longer referenced), the destructor
    <cpp|phrase_box_rep::~phrase_box_rep> prepends the pair (empty
    rectangle, old rectangle) to the same log.

    <item>The static function <cpp|requires_update> in
    <source-link|typesetter.cpp|src/Typeset/Bridge/typesetter.cpp> scans these pairs: it keeps the new rectangle
    of new lines, the old rectangle of deleted lines, and both rectangles
    for lines that moved. Lines that were cached and did not move produce
    two equal rectangles and are ignored.
  </itemize>

  The least upper bound of the remaining rectangles is returned in the
  four coordinates, enlarged by the rectangles of pages whose background
  color changed (<cpp|collect_page_colors> compared with
  <cpp|old_bgs>). Since cached paragraphs keep their boxes, typing in a
  paragraph usually only invalidates that paragraph, plus the following
  ones if its height changed.

  <subsection|The editor side>

  The editor class <cpp|edit_typeset_rep>
  (<source-link|Edit/Editor/edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp>) owns the typesetter <cpp|ttt>,
  created in its constructor on the subtree <cpp|subtree (et, rp)> of the
  edit tree. The relevant methods are:

  <\explain>
    <cpp|void edit_typeset_rep::typeset_preamble ()><explain-synopsis|compute
    the initial environment>
  <|explain>
    Resets the environment to the default values, applies the style files
    (<cpp|typeset_style_use_cache>) and the document's initial environment
    <cpp|init>, and stores the result in <cpp|pre>. It is called when the
    style or the initial environment changes.
  </explain>

  <\explain>
    <cpp|void edit_typeset_rep::typeset_prepare ()><explain-synopsis|start
    a pass>
  <|explain>
    Restores the environment <cpp|pre> before a typesetting pass or a call
    to <cpp|exec_until>.
  </explain>

  <\explain>
    <cpp|void edit_typeset_rep::typeset (SI& x1, SI& y1, SI& x2, SI&
    y2)><explain-synopsis|typeset and report the changed region>
  <|explain>
    Runs <cpp|typeset_sub> (which calls <cpp|::typeset (ttt, ...)> and
    stores the box in <cpp|eb>) as many times as needed for the references
    to stabilize, and returns the union of the changed regions. If
    typesetting throws an exception, <cpp|typeset_sub> replaces the
    document by an empty one.
  </explain>

  <\explain>
    <cpp|void edit_typeset_rep::typeset_invalidate (path
    p)><explain-synopsis|force re-typesetting of a subtree>
  <|explain>
    Notifies an assignment of the subtree at <cpp|p> to itself, so that its
    bridges become corrupted. <cpp|typeset_invalidate_all> does the same for
    the whole document after recomputing the preamble; it is triggered by
    changes of the environment (<cpp|THE_ENVIRONMENT>).
    <cpp|typeset_invalidate_players> invalidates animations.
  </explain>

  The actual update of the screen happens in
  <cpp|edit_interface_rep::apply_changes>
  (<source-link|Edit/Interface/edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>). When the change flags
  contain <cpp|THE_TREE> or <cpp|THE_ENVIRONMENT>, it clears the cache of
  cursor environments (<cpp|typeset_invalidate_env>), calls
  <cpp|typeset (x1, y1, x2, y2)>, and invalidates the returned rectangle
  (enlarged by two pixels) with <cpp|invalidate>. The widget then repaints
  that region of <cpp|eb> at the next redraw.

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
