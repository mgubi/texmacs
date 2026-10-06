<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|General architecture of <TeXmacs>>

  <section|Introduction>

  The core of <TeXmacs> is written in <c++>; most of the user interface,
  the menus, the keyboard bindings, the converters and many editing routines
  are written in the extension language <scheme> (currently <name|Guile>).
  <TeXmacs> can be built with the traditional <verbatim|configure> and
  <verbatim|make> utilities, or with <name|CMake>. At the top level of the
  source distribution, one finds:

  <\itemize>
    <item>The <c++> sources in the directory <source-link|src/src|src>.

    <item>The <TeXmacs> runtime data (<scheme> programs, style files, fonts,
    documentation, icons, <abbr|etc.>) in the directory
    <source-link|src/TeXmacs|TeXmacs>. After installation, this directory becomes
    <verbatim|$TEXMACS_PATH>.

    <item>The plug-ins for external systems in <source-link|src/plugins|plugins>. They
    are copied into <verbatim|$TEXMACS_PATH/plugins> when <TeXmacs> is
    built.

    <item>Build support (<source-link|configure.in|configure.in>, <source-link|CMakeLists.txt|src/CMakeLists.txt>,
    <source-link|misc/m4|misc/m4>, <verbatim|cmake>, <abbr|etc.>) and packaging data in
    <source-link|src/misc|misc> and <source-link|src/packages|packages>.
  </itemize>

  This chapter gives a bird's eye view on how these parts fit together.
  Several aspects are described in more detail in other chapters: see the
  documentation about the <hlink|basic data types|types.en.tm>, the
  <hlink|typesetter|typesetter.en.tm>, <hlink|macro expansion|macro-expansion.en.tm>,
  the <hlink|typeset boxes|boxes.en.tm>, the <hlink|server, buffers, views
  and windows|server.en.tm>, the <hlink|renderers|renderer.en.tm> and the
  <hlink|abstract widget system|widgets.en.tm>.

  <section|The <c++> source tree>

  The <c++> sources in <source-link|src/src|src> are organized in the following
  directories:

  <\description>
    <item*|<verbatim|Kernel>>Basic and generic data structures, which are
    used everywhere else. The subdirectory <source-link|Kernel/Abstractions|src/Kernel/Abstractions>
    contains the reference counting machinery (<source-link|basic.hpp|src/Kernel/Abstractions/basic.hpp>),
    commands, observers and black boxes; <verbatim|Kernel/Containers>
    contains arrays, lists, hash tables, hash sets and iterators;
    <verbatim|Kernel/Types> contains strings, trees, tree labels, paths,
    modifications, rectangles and stretchable spaces.

    <item*|<verbatim|Data>>Everything which is related to <TeXmacs>
    documents as data: the global edit tree (<source-link|Data/Document|src/Data/Document>), the
    data relation descriptors (<source-link|Data/Drd|src/Data/Drd>), the observers which are
    attached to trees (<source-link|Data/Observers|src/Data/Observers>), the undo/redo history and
    patches (<verbatim|Data/History>), routines for analyzing and correcting
    trees (<verbatim|Data/Tree>), string utilities and encodings
    (<verbatim|Data/String>), small parsers for syntax highlighting
    (<verbatim|Data/Parser>) and the <c++> part of the
    <hlink|converters|conversions.en.tm> (<verbatim|Data/Convert>).

    <item*|<verbatim|System>>The interaction with the operating system:
    booting and preferences (<source-link|System/Boot|src/System/Boot>), <abbr|URL>s and timers
    (<verbatim|System/Classes>), files (<verbatim|System/Files>), natural and
    programming languages, hyphenation and dictionaries
    (<source-link|System/Language|src/System/Language>), the connections with plug-ins through
    pipes, sockets and dynamic libraries (<source-link|System/Link|src/System/Link>) and
    miscellaneous routines, such as the fast memory allocator
    (<source-link|System/Misc|src/System/Misc>).

    <item*|<verbatim|Graphics>>Graphical data structures and the abstract
    interfaces to the graphical output and the user interface: fonts
    (<verbatim|Graphics/Fonts>) and bitmap glyphs
    (<source-link|Graphics/Bitmap_fonts|src/Graphics/Bitmap_fonts>), colors, pictures, points, curves and
    frames (<verbatim|Graphics/Types>), renderers
    (<source-link|Graphics/Renderer|src/Graphics/Renderer>), the abstract widget and window interface
    (<source-link|Graphics/Gui|src/Graphics/Gui>), handwriting recognition and some generic
    mathematical templates (<source-link|Graphics/Mathematics|src/Graphics/Mathematics>).

    <item*|<verbatim|Typeset>>The <hlink|typesetter|typesetter.en.tm>. It
    contains the typesetting environment and the evaluation of macros
    (<verbatim|Typeset/Env>), the boxes (<source-link|Typeset/Boxes|src/Typeset/Boxes>), the
    incremental bridges between trees and boxes (<source-link|Typeset/Bridge|src/Typeset/Bridge>),
    the concatenation of lines (<source-link|Typeset/Concat|src/Typeset/Concat>), line breaking
    (<source-link|Typeset/Line|src/Typeset/Line>), page breaking (<source-link|Typeset/Page|src/Typeset/Page>),
    stacks and tables.

    <item*|<verbatim|Style>>An alternative, experimental implementation of
    the evaluator for style files and macros, which is independent from the
    typesetter.

    <item*|<verbatim|Edit>>The editor proper. The abstract class
    <cpp|editor_rep> is declared in <source-link|Edit/editor.hpp|src/Edit/editor.hpp>; its
    implementation <cpp|edit_main_rep> (<source-link|Edit/Editor|src/Edit/Editor>) inherits from
    a series of classes which take care of the interface with the user
    (<source-link|Edit/Interface|src/Edit/Interface>), the modification of the document
    (<source-link|Edit/Modify|src/Edit/Modify>), selections, searching and replacing
    (<source-link|Edit/Replace|src/Edit/Replace>) and the processing of the document
    (<source-link|Edit/Process|src/Edit/Process>).

    <item*|<verbatim|Texmacs>>The <TeXmacs> <hlink|server|server.en.tm>,
    which manages buffers, views, windows and projects
    (<source-link|Texmacs/Data|src/Texmacs/Data>, <source-link|Texmacs/Server|src/Texmacs/Server>,
    <source-link|Texmacs/Window|src/Texmacs/Window>), and the main program
    <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>.

    <item*|<verbatim|Scheme>>The interface with the <scheme> interpreter. The
    generic <cpp|object> type and the calling conventions are defined in
    <source-link|Scheme/scheme.hpp|src/Scheme/scheme.hpp> and <source-link|Scheme/Scheme|src/Scheme/Scheme>; the binding
    with <name|Guile> lives in <source-link|Scheme/Guile|src/Scheme/Guile>. The \Pglue\Q which
    exports <c++> routines to <scheme> is generated from the specifications
    <verbatim|Scheme/Glue/build-glue-*.scm> into the files
    <verbatim|Scheme/Glue/glue_*.cpp>.

    <item*|<verbatim|Plugins>>Implementations of the abstract interfaces for
    specific libraries or platforms. For instance, <verbatim|Plugins/Qt>
    contains the default graphical user interface based on <name|Qt>,
    <source-link|Plugins/Freetype|src/Plugins/Freetype> the support for <name|TrueType> and
    <name|OpenType> fonts, <source-link|Plugins/Metafont|src/Plugins/Metafont> the support for
    <TeX> fonts, <source-link|Plugins/Pdf|src/Plugins/Pdf> the <name|PDF> renderer,
    <source-link|Plugins/Unix|src/Plugins/Unix>, <verbatim|Plugins/MacOS> and
    <source-link|Plugins/Windows|src/Plugins/Windows> system specific code,
    <source-link|Plugins/Database|src/Plugins/Database> the database engine, and
    <source-link|Plugins/Widkit|src/Plugins/Widkit> with <source-link|Plugins/X11|src/Plugins/X11> the historical
    <hlink|<name|X11> interface|gui.en.tm>.
  </description>

  Roughly speaking, <verbatim|Kernel> does not depend on anything else;
  <verbatim|Data> and <verbatim|System> only depend on <verbatim|Kernel>
  (and on each other); <verbatim|Graphics> builds on top of these;
  <verbatim|Typeset> uses all previous parts; <verbatim|Edit> relies on the
  typesetter and <verbatim|Texmacs> uses everything. The directories in
  <verbatim|Plugins> provide concrete implementations of abstract classes
  declared elsewhere (such as <cpp|renderer_rep>, <cpp|widget_rep>,
  <cpp|window_rep> or <cpp|font_rep>); which of them are compiled depends
  on the configuration.

  <section|The <TeXmacs> runtime data>

  The directory <source-link|src/TeXmacs|TeXmacs> contains the part of <TeXmacs> which
  is not compiled. It is located at run time through the environment
  variable <verbatim|TEXMACS_PATH>. The most important subdirectories are:

  <\description>
    <item*|<verbatim|progs>>The <scheme> programs. The boot sequence starts
    with <source-link|progs/init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>; the kernel of the <scheme> part
    (<scm|tm-define>, menus, keyboard definitions, modes, plug-in
    declarations, <abbr|etc.>) is in <source-link|progs/kernel|TeXmacs/progs/kernel>. The other
    subdirectories correspond to the various editing modes
    (<verbatim|text>, <verbatim|math>, <verbatim|prog>,
    <verbatim|graphics>, <verbatim|table>, <abbr|etc.>), to converters
    (<verbatim|convert>), fonts (<verbatim|fonts>) and so on.

    <item*|<verbatim|styles> and <verbatim|packages>>The style files
    (<verbatim|.ts>) and style packages.

    <item*|<verbatim|fonts>>Font data: <TeX> font metrics and <name|Type
    1> fonts, <name|TrueType> and <name|OpenType> fonts shipped with
    <TeXmacs>, encodings and virtual fonts, as well as the font database
    (<source-link|font-database.scm|TeXmacs/fonts/font-database.scm> and companions).

    <item*|<verbatim|langs>>Language data: hyphenation patterns and
    dictionaries (<source-link|langs/natural|TeXmacs/langs/natural>) and character encodings
    (<source-link|langs/encoding|TeXmacs/langs/encoding>).

    <item*|<verbatim|doc>>The documentation, including the present
    document.

    <item*|<verbatim|misc>>Miscellaneous data, such as the icons in
    <source-link|misc/pixmaps|TeXmacs/misc/pixmaps>, images, patterns, sounds, themes and helper
    scripts.
  </description>

  User specific data (preferences, personal styles, packages, plug-ins,
  font databases, <abbr|etc.>) are stored in the directory
  <verbatim|$TEXMACS_HOME_PATH>, which defaults to
  <verbatim|~/.TeXmacs> on <name|Unix> systems.

  <section|Internal representation of texts>

  <TeXmacs> represents all texts by trees. All open documents are subtrees of
  a single global tree <cpp|the_et> (the <em|edit tree>, declared in
  <source-link|Data/Document/new_document.hpp|src/Data/Document/new_document.hpp>); each buffer corresponds to a
  child of <cpp|the_et> and each editor knows the path <cpp|rp> to the root
  of its document. The inner nodes of a tree are labeled by <em|tree
  labels>: the built-in labels of the <TeXmacs> format are enumerated in
  <source-link|Kernel/Types/tree_label.hpp|src/Kernel/Types/tree_label.hpp>, and new labels are created on the
  fly for user defined macros (<cpp|make_tree_label>). The leaves of the tree
  are strings, which are either invisible (such as lengths or the names of
  environment variables) or visible (the real text). Properties of the tags,
  such as their arity, the accessibility of their children and the types of
  their arguments, are maintained by the <em|data relation descriptor> or
  DRD (see <source-link|Data/Drd|src/Data/Drd> and the chapter on
  <hlink|macro expansion|macro-expansion.en.tm>).

  The meaning of the text and the way it is typeset essentially depend on the
  current <em|environment>. The environment (see the class
  <cpp|edit_env_rep> in <source-link|Typeset/env.hpp|src/Typeset/env.hpp>) mainly consists of a
  hash table of type <cpp|hashmap\<less\>string,tree\<gtr\>>, which maps
  environment variables to their tree values. The current language and the
  current font are examples of system environment variables; new variables
  can be defined by the user, and macros are nothing but environment
  variables whose values are <markup|macro> trees. When the typesetter enters
  a <markup|with> tag or a macro body, the modified variables are saved in a
  second hash table, so that they can be restored afterwards.

  <subsection|Text>

  All text strings in <TeXmacs> consist of sequences of either specific or
  universal symbols. A specific symbol is a character, different from
  <verbatim|'\\0'>, <verbatim|'\<less\>'> and <verbatim|'\<gtr\>'>. A
  universal symbol is a string starting with <verbatim|'\<less\>'>, followed
  by an arbitrary sequence of characters different from <verbatim|'\\0'>,
  <verbatim|'\<less\>'> and <verbatim|'\<gtr\>'>, and ending with
  <verbatim|'\<gtr\>'>. For instance, <verbatim|\<less\>alpha\<gtr\>> stands
  for the Greek letter alpha and <verbatim|\<less\>#2212\<gtr\>> for the
  <name|Unicode> character with code point <verbatim|U+2212>. Specific
  characters are interpreted in the <em|Cork> encoding (the <TeX> T1
  encoding); the conversions between this internal encoding and <name|UTF-8>
  are done by routines such as <cpp|utf8_to_cork> and <cpp|cork_to_utf8> in
  <source-link|Data/String/converter.cpp|src/Data/String/converter.cpp>. The meaning of symbols does not
  depend on the font which is used, but different fonts may render them in
  a different way (see the chapter on <hlink|fonts|fonts.en.tm>).

  <subsection|The language>

  The language of the text (see the abstract class <cpp|language_rep> in
  <source-link|System/Language/language.hpp|src/System/Language/language.hpp>) is capable of performing a further
  semantic analysis of a text phrase. At least, it is capable of splitting a
  phrase into <em|words> (which are smaller phrases) and to inform the
  typesetter about the desired spaces between words and hyphenation
  information (the methods <cpp|advance>, <cpp|get_hyphens> and
  <cpp|hyphenate>). There are three main kinds of languages:

  <\itemize>
    <item>Natural languages (<cpp|text_language>), which use the hyphenation
    patterns from <verbatim|$TEXMACS_PATH/langs/natural/hyphen>.

    <item>The mathematical language (<cpp|math_language>), which classifies
    mathematical symbols into groups (operators, relations, brackets,
    <abbr|etc.>) and determines the spacing between them. Semantic editing
    of formulas relies in addition on the packrat grammars implemented in
    <verbatim|System/Language/packrat_*.cpp>.

    <item>Programming languages (<cpp|prog_language> and variants), which
    are mainly used for syntax highlighting of computer programs.
  </itemize>

  Spell checking is not done by the languages themselves, but by external
  tools (see <source-link|Plugins/Ispell|src/Plugins/Ispell>).

  <section|Typesetting texts>

  Roughly speaking, the typesetter of <TeXmacs> takes a tree on input and
  produces a box, while accessing and modifying the typesetting environment.
  The typesetting is incremental: the <hlink|typesetter|typesetter.en.tm>
  maintains a tree of <em|bridges> which mirrors the document tree and
  remembers the boxes which were produced during the previous run, so that
  only the modified parts of the document need to be typeset again.

  The <cpp|box> class is multifunctional. Its principal method is used for
  displaying the box on a <hlink|renderer|renderer.en.tm> (the screen, a
  printer, a <name|PDF> file or an image). But it also contains a lot of
  typesetting information, such as logical and ink bounding boxes, the
  positions of scripts, italic corrections, <abbr|etc.>

  Another functionality of <hlink|boxes|boxes.en.tm> is to convert between
  physical cursors (positions on the screen) and logical cursors (paths in
  the edit tree). Actually, boxes are also organized into a tree, which
  often simplifies the conversion. However, because of macro expansions and
  line and page breaking, the conversion routines may become quite intricate.
  Notice also that, besides a horizontal and vertical position, the physical
  cursor also contains an infinitesimal horizontal position. Roughly
  speaking, this infinitesimal coordinate is used to give certain boxes (such
  as color changes) an extra infinitesimal width.

  <section|Making modifications in texts>

  <subsection|Elementary modifications>

  All modifications of documents eventually break down into nine types of
  <em|elementary modifications>, which are declared in
  <source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>:

  <\description>
    <item*|<cpp|MOD_ASSIGN>>Replace a subtree by another tree.

    <item*|<cpp|MOD_INSERT>>Insert a string into a string leaf, or insert a
    sequence of children into a compound tree.

    <item*|<cpp|MOD_REMOVE>>Remove a range of characters from a string leaf,
    or a range of children from a compound tree.

    <item*|<cpp|MOD_SPLIT>>Split a leaf or a node into two consecutive ones
    (for instance, split a paragraph into two paragraphs).

    <item*|<cpp|MOD_JOIN>>The inverse operation: join two consecutive
    leaves or nodes.

    <item*|<cpp|MOD_ASSIGN_NODE>>Change the label of a node.

    <item*|<cpp|MOD_INSERT_NODE>>Insert a new node above a given subtree,
    which becomes one of its children.

    <item*|<cpp|MOD_REMOVE_NODE>>The inverse operation: replace a node by one
    of its children.

    <item*|<cpp|MOD_SET_CURSOR>>A pseudo-modification which does not change
    the tree, but which records a cursor position (useful for undoing and for
    collaborative editing).
  </description>

  A <cpp|modification> consists of its type, a path and possibly a tree; it
  is constructed with the functions <cpp|mod_assign>, <cpp|mod_insert>,
  <abbr|etc.> The corresponding functions <cpp|assign>, <cpp|insert>,
  <cpp|remove>, <cpp|split>, <cpp|join>, <cpp|assign_node>,
  <cpp|insert_node>, <cpp|remove_node> and <cpp|set_cursor> (defined in
  <source-link|Kernel/Abstractions/observer.cpp|src/Kernel/Abstractions/observer.cpp>) take either a reference to a
  tree or a path in <cpp|the_et>, and call <cpp|apply>. From <scheme>, the
  same operations are available as <scm|tree-assign>, <scm|tree-insert>,
  <scm|tree-remove>, <scm|tree-split>, <scm|tree-join>,
  <scm|tree-assign-node>, <scm|tree-insert-node> and
  <scm|tree-remove-node> (and their variants with an exclamation mark).

  <subsection|Observers>

  Every tree may carry an <em|observer> (the field <cpp|obs> of
  <cpp|tree_rep>; see <source-link|Kernel/Abstractions/observer.hpp|src/Kernel/Abstractions/observer.hpp> and
  <source-link|Data/Observers|src/Data/Observers>). When an elementary modification is applied to
  a tree, its observers are first <em|announced> the modification, then
  <em|notified> of the precise change (<cpp|notify_assign>,
  <cpp|notify_insert>, <abbr|etc.>), the modification is performed, and the
  observers are finally informed that it is <em|done>. Several observers can
  be combined into lists. The most important ones are:

  <\itemize>
    <item>The <em|inverse path> observer (<source-link|ip_observer.cpp|src/Data/Observers/ip_observer.cpp>), which
    allows each subtree of <cpp|the_et> to know its own location. Inverse
    paths are also stored in the boxes; they are the basis for the
    correspondence between the document and its typeset form.

    <item>The <em|editor> observer (<source-link|edit_observer.cpp|src/Data/Observers/edit_observer.cpp>), which
    forwards the modifications to the editor. The editor updates the cursor
    position and notifies the typesetter, which invalidates the corresponding
    bridges, so that the modified parts will be typeset again during the next
    repaint.

    <item>The <em|undo> observer (<source-link|undo_observer.cpp|src/Data/Observers/undo_observer.cpp>), which
    records the modifications into the <em|archiver> of the buffer (see
    <source-link|Data/History/archiver.hpp|src/Data/History/archiver.hpp>). The archiver stores the history as
    <em|patches> (<source-link|Data/History/patch.hpp|src/Data/History/patch.hpp>), which can be inverted
    in order to undo or redo changes.

    <item>Tree pointers, tree positions and links, which keep track of
    locations in the document while it is being edited.
  </itemize>

  <subsection|The flow of a modification>

  A typical modification goes through the following steps:

  <\enumerate>
    <item>An input event, such as a key press, is transmitted by the
    graphical user interface to the editor, which calls the <scheme>
    function <scm|keyboard-press>. The keyboard bindings (defined with
    <scm|kbd-map> in the <scheme> code) or a menu entry then trigger an
    action, such as <scm|make-fraction>, which is ultimately implemented by a
    <c++> routine like <cpp|edit_math_rep::make_fraction>.

    <item>All modifications which this action makes to the edit tree break
    down into elementary modifications, which are applied using
    <cpp|apply>.

    <item>Before and after performing each elementary modification, the
    observers of the affected subtree are notified as explained above. In
    particular, all editors which view the same buffer are informed, the
    typesetter invalidates the modified parts, and the archiver records the
    modification.

    <item>Each user action like a keystroke or a mouse click is enclosed
    between calls of <cpp|start_editing> and <cpp|end_editing>. At the end,
    the pending modifications are confirmed as one step in the history; this
    determines the granularity of undo. Undo points can also be inserted
    explicitly using <cpp|mark_start>, <cpp|mark_end> and
    <cpp|archive_state>.

    <item>Finally, the event loop of the graphical user interface requests
    the typesetting of the invalid parts of the document and the repainting
    of the invalid regions of the screen.
  </enumerate>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
