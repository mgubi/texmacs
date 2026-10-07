<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Math font profiles, shipped fonts and the database>

  The <verbatim|MATH> table tells <TeXmacs> how to lay out formulas in a
  font, but not which font to use for the text around them, whether the
  letters of a formula should come from the math font or from the italic
  text face, under which name the font should appear in a menu, or where
  its file is when the font database has never seen it. This page
  describes the <em|profiles> which record that knowledge for the
  <name|OpenType> math fonts <TeXmacs> knows by name, the way a profile
  steers the choice of fonts in the smart font, the menus built from the
  profiles, the fonts shipped with <TeXmacs> and the changes made to the
  font database so that they are found everywhere: the order in which font
  files are looked for, the merge of the shipped database into the local
  one, and faster rescans.

  How a user picks these fonts, and the environment variables involved,
  is described in <hlink|selecting fonts|../fonts/font-selection.en.tm>;
  every shipped font is shown in <hlink|the fonts which come with
  <TeXmacs>|../../main/math/fonts/man-math-font-catalogue.en.tm>. The
  database itself is the subject of <hlink|the font database and font
  selection|font-database.en.tm>.

  <section|Profiles>

  A profile is declared in <scheme>, in
  <source-link|TeXmacs/progs/fonts/fonts-opentype.scm|TeXmacs/progs/fonts/fonts-opentype.scm>, with the macro
  <scm|define-math-font-profile>, which expands into a call of the glue
  routine <scm|math-font-profile-set>:

  <\scm-code>
    (define-math-font-profile "Libertinus Math"

    \ \ (file "LibertinusMath-Regular") (text "Libertinus")

    \ \ (sans "Libertinus") (mono "Libertinus")

    \ \ (letters "math") (menu "Libertinus") (group "Serif"))
  </scm-code>

  The name is the family of the math font as the font database names it.
  The module is loaded at boot with the other font modules
  (<source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>), so the twenty-six profiles are in place
  before the first document is typeset: twenty-five of math fonts and one
  of a text font without mathematics (see below).

  On the <c++> side (<source-link|Graphics/Fonts/math_font_profiles.cpp|src/Graphics/Fonts/math_font_profiles.cpp>) a
  profile is stored as a <cpp|tree>, a tuple of <verbatim|(key value)>
  pairs, in a table keyed by the family. Both tables of the file are
  function-local statics, since they are filled during the <scheme> boot,
  before the global statics of other units are guaranteed to be
  initialized. The routines, all exported to <scheme> by
  <source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>, are

  <\description-paragraphs>
    <item*|<cpp|math_font_profile_set (family, profile)>>Stores a profile
    and, when it names a <verbatim|text> companion which no earlier profile
    claimed, records the reverse association text <math|\<rightarrow\>>
    math. The first claimant wins, so the order of the declarations in
    <source-link|fonts-opentype.scm|TeXmacs/progs/fonts/fonts-opentype.scm> decides which math font a shared text
    family pulls in: <name|TeX Gyre Pagella Math> is declared before
    <name|Euler Math> and <name|Asana Math>, which name the same text font.

    <item*|<cpp|math_font_profile (family)>,
    <cpp|math_font_profile_families ()>>The profile of a family (an empty
    tuple if there is none) and the list of profiled families.

    <item*|<cpp|math_font_profile_attr (family, key)>>The value of one key,
    or the empty string. Values arrive from <scheme> as quoted strings and
    are unquoted.

    <item*|<cpp|math_family_for_text (text)>>The math font whose profile
    names <cpp|text> as its companion. A document may name one of the
    families of a master (<verbatim|KpRoman>) where the profile names the
    master (<verbatim|Kepler>), so the master of the family is tried as
    well (<cpp|font_database_master>).

    <item*|<cpp|text_family_for_math (math)>>The <verbatim|text> key.
  </description-paragraphs>

  <subsection|The keys>

  The keys and the places where they are read are:

  <\description-paragraphs>
    <item*|<verbatim|file>>The file name of the math font without suffix.
    It decides whether the font is installed (<scm|font-exists-in-tt?> in
    the menus, <cpp|tt_font_exists> in <cpp|profile_fix>) and where to find
    it when the database does not know it (<cpp|register_profiled_font>).

    <item*|<verbatim|text>>The text companion, named by its <em|master>,
    the second field of an entry of <source-link|TeXmacs/fonts/font-features.scm|TeXmacs/fonts/font-features.scm>,
    since that is what the <src-var|font> variable holds. Naming a family
    instead (<verbatim|Fira Sans> rather than <verbatim|Fira>) makes the
    font selection fall back on the feature distance.

    <item*|<verbatim|sans>, <verbatim|mono>>The companions used for sans
    serif and typewriter text and mathematics (<cpp|profile_variant_fix>).
    A value may list alternatives separated by commas, such as
    <verbatim|"Inconsolatazi4, TeX Gyre Cursor"> for the <name|TeX Gyre>
    profiles, <name|Euler Math>, <name|Garamond-Math>,
    <name|OldStandard-Math> and <name|GFS Neohellenic Math>: the first one
    of which the database knows a style (<cpp|font_database_styles>) is
    used, and the item is kept when none is.

    <item*|<verbatim|letters>><verbatim|math> or <verbatim|text>: whether
    the letters of formulas come from the math alphabets of the math font
    or from the italic text face. Read by the constructor of
    <cpp|smart_font_rep>, see below.

    <item*|<verbatim|text-file>>A file of the text companion, for a
    companion which the database may not know; its directory is then added
    to the database (<cpp|register_profiled_font>). The menus also require
    it to exist before they list the font.

    <item*|<verbatim|family>>The font family, <verbatim|rm> or
    <verbatim|ss>, in which the text is set (<verbatim|rm> when absent).
    Read on the <scheme> side only (<scm|opentype-font-family>), by
    <scm|init-opentype-font>, the check marks of the menus and
    <scm|opentype-math-companions>: <name|KpMath Sans>, <name|Noto Sans
    Math> and <name|IBM Plex Math> set <verbatim|font-family> to
    <verbatim|ss>.

    <item*|<verbatim|menu>, <verbatim|group>>The label of the font in the
    menus and its section: <verbatim|Serif>, <verbatim|Sans serif> or
    <verbatim|Other>. The labels follow the names <LaTeX> users know, so
    <name|TeX Gyre Termes Math> is <verbatim|Times>.

    <item*|<verbatim|bold-math>>Recorded in four profiles but <em|read
    nowhere>. Bold mathematics uses a real bold face when the database
    attaches a <verbatim|Bold> style to the master of the math font, which
    is the case for all four, so the key only documents that such a face
    exists.
  </description-paragraphs>

  <paragraph|Profiles with companions only.>A profile without
  <verbatim|file> is never offered in the menus
  (<scm|opentype-math-font-installed?> requires the file) and pulls in no
  mathematics, but its <verbatim|sans> and <verbatim|mono> keys still apply
  to the text font it is named after. The one such profile is
  <verbatim|Palatino>, the Palatino of <name|macOS>, which has no
  typewriter face: without it the typewriter text would be the closest
  monospaced font of the database, <name|Linux Libertine Mono>; with it,
  it is <name|Inconsolata>.

  <section|How a profile steers the choice of fonts>

  The family string of a smart font goes through a chain of rewritings in
  <cpp|smart_font_bis> (<source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>) before a
  base font is looked for: <cpp|tex_gyre_fix>, <cpp|kepler_fix>,
  <cpp|math_fix> and, last, <cpp|profile_fix>, which is where the profiles
  come in. For each item of the font sequence which carries no condition
  (no <verbatim|=>):

  <\enumerate>
    <item>The item and, if the item is a text font with a math companion,
    that companion are registered if needed (see below).

    <item>In a math shape (<verbatim|mathitalic>, <verbatim|mathupright>,
    <verbatim|mathshape>), a text family with an installed math companion
    is replaced by the companion, the <verbatim|sans> or <verbatim|mono>
    companion is substituted for the sans serif and typewriter variants,
    and the result is replaced by its master, since the font selection is
    driven by masters (the family <verbatim|KpMath> belongs to the master
    <verbatim|Kepler Math>).

    <item>In a text shape, a math family is replaced by its text companion
    (or the sans serif or typewriter one), and a text family with a math
    companion keeps itself but takes the variant companions of that math
    font. A math font which has no text face of its own, such as
    <name|Concrete Math> or <name|Euler Math>, thus sets its text in the
    family its profile declares. A text family with neither takes the
    variant companions of a profile of its own name, if there is one (the
    companion-only profiles above).
  </enumerate>

  This is why formulas follow the math companion of the text font and not
  <src-var|math-font>: <src-var|math-font> only counts while
  <src-var|font> is <verbatim|roman>. A pairing which is not the canonical
  one is written as a rule, <verbatim|math=Euler Math,TeX Gyre Pagella>,
  which <cpp|math_fix> resolves in math shapes; the condition makes the
  item invisible to <cpp|profile_fix>.

  <cpp|profile_variant_fix> splits the variant on <verbatim|->, as
  <cpp|variant_features> does, and looks for the key <verbatim|mono> when
  one of the parts is <verbatim|tt>, for <verbatim|sans> when one is
  <verbatim|ss>. This matters for prog mode (the inputs of sessions and
  the prompt of AI sessions), which asks for the variant
  <verbatim|rm-tt> in the shape <verbatim|mathupright>. In that math shape
  <cpp|kepler_fix> has already turned <verbatim|Kepler> into
  <verbatim|Kepler Math>, which names no profile, so the profile is looked
  up by <cpp|profile_family>: the family itself when it has a profile, else
  the first profiled family whose master (<cpp|font_database_master>) is
  the given name, here <verbatim|KpMath>. The answers are cached in a
  static <cpp|hashmap>. Without these two steps the inputs of sessions were
  set in the closest monospaced font, <name|Libertinus Mono>, for <name|Kp
  Fonts> and Palatino alike.

  A monospaced font may also give its space another width than its
  cells (the space of <name|KpMono> is 333 units for cells of 530): the
  constructor of <cpp|unicode_font_rep>
  (<source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>)
  therefore gives the space the advance of <verbatim|m> (stretchable
  between three quarters and one and a half of it) when <verbatim|m> and
  <verbatim|i> have the same advance, so that verbatim text lines up.

  <paragraph|Letters.>The constructor of <cpp|smart_font_rep> sets the
  flag <cpp|ot_math> when the base font is an <name|OpenType> math font
  (<cpp|math_type == MATH_TYPE_OPENTYPE>) whose profile does not say
  <verbatim|(letters "text")>. For the math italic shape it then adds the
  subfont <verbatim|("ot-italic")> with <cpp|REWRITE_MATH_ITALIC>, so that
  the letters of a formula are rewritten to the mathematical italic
  alphanumerics of the math font itself instead of being taken from the
  italic text face. The four <name|TeX Gyre> profiles say
  <verbatim|text>: their hand-tuned tables are made against the text
  italic.

  <paragraph|Registering fonts the database does not know.>The database
  only holds what the shipped database records and what a scan of the disk
  found. The math fonts of a <TeX> distribution are typically installed
  but unknown, and selecting such a family would silently fall back on
  the nearest text face. <cpp|register_profiled_font> therefore runs once
  per family (a static <cpp|hashset>): if the database has no style for
  the family, it looks for the file of the profile with
  <cpp|tt_font_find> and adds it with <cpp|font_database_extend_local>; if
  the profile has a <verbatim|text-file> and the text companion is
  unknown too, the directory of that file is added. Both print a line on
  the console.

  <section|Menus and commands>

  The menus are built from the profiles of the fonts which are installed
  (<scm|opentype-math-font-installed?>: the <verbatim|file> exists, and
  the <verbatim|text-file> if there is one).
  <scm|opentype-math-font-list> returns triples <verbatim|(label math
  text)> sorted by label, and <scm|opentype-math-font-group-list> those of
  one section. <scm|opentype-font-menu>, which the font button of the focus
  toolbar shows, has one section per group, each a menu without arguments
  (<scm|opentype-serif-font-menu> and the others), since a submenu is
  expanded after its parent and a menu with arguments has lost them by
  then. <scm|opentype-math-font-menu> appends the installed math fonts to
  <menu|Document|Font|Mathematical font>. The text fonts which bring no
  mathematics are listed apart, in
  <source-link|TeXmacs/progs/fonts/font-short-menu.scm|TeXmacs/progs/fonts/font-short-menu.scm>, declared with
  <scm|define-text-font> by master. <scm|text-font-list> leaves out the
  text fonts returned by <scm|opentype-math-companions>, which the menu
  offers with their mathematics: only the <verbatim|text> companions of
  the installed profiles whose <verbatim|family> is <verbatim|rm>. A
  profile set in sans serif names a master which also has a serif face,
  <verbatim|Noto> for <name|Noto Sans Math> and <verbatim|IBM Plex> for
  <name|IBM Plex Math>, and Noto Serif and IBM Plex Serif must stay in the
  menu of text fonts.

  The entries call

  <\explain>
    <scm|(init-opentype-font <var|math>)><explain-synopsis|text and
    mathematics from a profile>
  <|explain>
    Calls <scm|init-font> with the value computed by
    <scm|opentype-font-value>: the text companion itself when the math font
    is the one this text font pulls in (<scm|math-family-for-text>), and
    the rule <verbatim|math=<var|math>,<var|text>> otherwise. It then sets
    <verbatim|font-family> when the profile asks for another family than
    <verbatim|rm>.
  </explain>

  <scm|init-font> (<source-link|generic/document-edit.scm|TeXmacs/progs/generic/document-edit.scm>) sends the four
  <name|TeX Gyre> text fonts to the style packages of their hand-tuned
  mathematics (<verbatim|pagella-font> and the others), unless another math
  font is asked for (<scm|tex-gyre-font?>): <name|Euler Math> and
  <name|Asana Math> are set with <name|Pagella> text, and the package would
  replace them by <name|Pagella Math>. It also no longer adds the packages
  of <verbatim|fira-font> and <verbatim|libertine-font> when a profiled
  math font is given, since those packages take their large operators from
  <name|TeX Gyre Pagella> and are older than the math fonts of the same
  design. The check marks of the menus (<scm|test-opentype-font?>) look at
  the style package for the <name|TeX Gyre> fonts and at the initial values
  of <src-var|font> and <src-var|font-family> otherwise.

  <section|The shipped fonts>

  The directory <source-link|TeXmacs/fonts/truetype|TeXmacs/fonts/truetype> holds, besides the fonts
  shipped before (the first <name|STIX> fonts, the <name|TeX Gyre> math
  fonts, <name|Linux Libertine>, <name|OpenDyslexic>), one subdirectory per
  family with the math font, its text faces, a <verbatim|README.md> and the
  license: <verbatim|lm> (<name|Latin Modern>), <verbatim|newcm> (<name|New
  Computer Modern>, regular and bold math, <name|New Computer Modern Sans
  Math>, the <verbatim|NewCM10>, <verbatim|NewCMSans10> and
  <verbatim|NewCMMono10> faces), <verbatim|stix2>, <verbatim|kp>
  (<name|KpMath>, <name|KpMath Sans> and the <name|Kp> text faces),
  <verbatim|fira> (<name|Fira Math>), <verbatim|libertinus>,
  <verbatim|erewhon>, <verbatim|xcharter>, <verbatim|concrete>
  (<name|Concrete Math> with the <name|CM Unicode> Concrete faces),
  <verbatim|euler>, <verbatim|letesans> (<name|Lete Sans Math>, regular
  and bold), <verbatim|garamond> (<name|Garamond-Math> and <name|EB
  Garamond>), <verbatim|oldstandard>, <verbatim|gfsneohellenic>,
  <verbatim|plex> (<name|IBM Plex Math> and the <name|Plex Sans>,
  <name|Serif> and <name|Mono> faces) and <verbatim|noto> (<name|Noto Sans
  Math>, <name|Noto Sans> and <name|Noto Sans Mono>). The directory
  <verbatim|inconsolata> holds <name|Inconsolatazi4>, the typewriter
  companion of several profiles. Of the profiled math fonts, only <name|TeX
  Gyre DejaVu Math>, <name|XITS Math> and <name|Asana Math> are not
  shipped; they are used when a <TeX> distribution or the system has
  them.

  All their faces are entered in the shipped database
  (<source-link|TeXmacs/fonts/font-database.scm|TeXmacs/fonts/font-database.scm>,
  <source-link|font-features.scm|TeXmacs/fonts/font-features.scm> and <source-link|font-characteristics.scm|TeXmacs/fonts/font-characteristics.scm>), so
  they work in a fresh installation without a scan. A few entries are
  arranged by hand: <name|KpMath Sans>, whose name table calls its family
  <verbatim|KpMath> with the style <verbatim|Sans>, is also listed as a
  family <verbatim|KpMathSans>, a master of its own, so that a sans serif
  document finds it rather than the <name|KpSans> text faces. The bold
  math faces whose name tables call their families otherwise are attached
  by hand: the bold <name|Erewhon Math> is entered in the database as the
  <verbatim|Bold> style of <verbatim|Erewhon Math>, and the families
  <verbatim|XCharter-Math-Bold> and <verbatim|Concrete> (the bold
  <name|Concrete Math>) are given the masters <verbatim|XCharter Math> and
  <verbatim|Concrete Math> in <source-link|font-features.scm|TeXmacs/fonts/font-features.scm>.
  <name|Noto Sans Math> is a master of its own, as <name|Fira Math> is,
  rather than a family of the master <verbatim|Noto>, and the <name|IBM
  Plex> entries list the regular file before the medium one, which a scan
  files as the regular style too.

  The smart font also uses one shipped file directly: when the main font
  has a <verbatim|MATH> table and a symbol it lacks could only be emulated
  as a bitmap, <cpp|resolve_shipped_math> takes the glyph from
  <verbatim|STIXTwoMath-Regular> (the macro <cpp|SHIPPED_MATH_FONT>); see
  <hlink|inspecting the font system|opentype-tools.en.tm>.

  <section|Finding the font files>

  <cpp|tt_font_find_sub> (<source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>) now
  looks for <verbatim|.otf>, <verbatim|.ttf> and <verbatim|.ttc> before
  <verbatim|.pfb>, and <verbatim|.dfont> last. A <TeX> distribution ships
  many families in both forms, and the <name|Type 1> file carries the
  encoding of the <TeX> world (in <verbatim|XCharter-Roman.pfb> the code of
  <verbatim|a> is the pound sign), whereas <TeXmacs> asks for characters by
  Unicode and needs the <verbatim|cmap> of the sfnt file; the <name|PDF>
  writer also gets real glyph indices that way. Since the answers of
  <cpp|tt_font_find> are cached in the home directory
  (<verbatim|font_cache.scm>), which outlives an upgrade, the cache key was
  changed from <verbatim|ttf:<var|name>> to <verbatim|sfnt:<var|name>>: a
  cache written when <name|Type 1> came first would otherwise keep naming
  the <verbatim|.pfb> file, without features or <verbatim|MATH> table. The
  comment in the code asks to change the prefix again whenever the order
  changes.

  The font path on <name|macOS> no longer lists the <TeX> Live
  directories year by year: <cpp|texlive_font_dirs> looks for every
  installation under <verbatim|/usr/local/texlive>,
  <verbatim|/usr/share/texlive>, <verbatim|/opt/texlive> and
  <verbatim|$HOME/texlive> and adds the <verbatim|opentype> and
  <verbatim|truetype> directories of their <verbatim|texmf-dist/fonts>.

  <section|Merging the shipped database>

  The local database (<verbatim|$TEXMACS_HOME_PATH/fonts>) used to be
  derived from the shipped one only when it was empty, so a home directory
  written by an older version never saw the fonts a newer version
  registers, and a character only those fonts draw came out as its name in
  red. <cpp|font_database_load> (<source-link|Graphics/Fonts/font_database.cpp|src/Graphics/Fonts/font_database.cpp>)
  now keeps a stamp in <verbatim|$TEXMACS_HOME_PATH/fonts/shipped-stamp.scm>
  of two lines:

  <\enumerate>
    <item>the dates and sizes of the three shipped files and the value of
    <verbatim|$TEXMACS_PATH> (<cpp|shipped_fonts_stamp>);

    <item>the number of entries of the local database when this
    installation left it.
  </enumerate>

  The shipped database, features and characteristics are loaded, filtered
  against the files of this machine and saved again, merged with the local
  entries, when the first line differs from the current stamp
  (<cpp|shipped_fonts_changed>, which also holds when the stamp is
  missing) or when the local database has fewer entries than the second
  line records (<cpp|shipped_fonts_shrunk>). The installation is part of
  the stamp because merging only keeps the fonts installed on this machine,
  and the shipped fonts are installed in the directory of the installation
  which merges: a home directory shared by several installations would
  otherwise keep the fonts of whichever ran last. A console line says when
  a merge happens.

  <section|Faster rescans>

  <cpp|font_database_build> used to read and parse every font file on the
  path, which takes minutes with a <TeX> Live installation on it. It now
  builds once an index of the <verbatim|"name size"> pairs already recorded
  (<cpp|font_database_init_scanned>, a static <cpp|hashset>) and skips those
  files, counting new and skipped files for a summary line printed by
  <cpp|font_database_build_local>.
  <cpp|font_database_build_characteristics> likewise skips the styles
  whose characteristics are known unless <cpp|force> is set. The per-file
  messages (<verbatim|Process>, <verbatim|Analyzing>) are only printed with
  the verbose debug flag, on <cpp|debug_fonts>.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Fonts/math_font_profiles.cpp|src/Graphics/Fonts/math_font_profiles.cpp>>The profile
    tables and their accessors.

    <item*|<source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>><cpp|profile_fix>,
    <cpp|profile_variant_fix>, <cpp|profile_family>, <cpp|register_profiled_font>, the
    <cpp|ot_math> flag and the math italic subfont,
    <cpp|resolve_shipped_math>.

    <item*|<source-link|Graphics/Fonts/font_database.cpp|src/Graphics/Fonts/font_database.cpp>>The shipped stamp,
    the merge and the index of scanned files.

    <item*|<source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>>The order of the
    suffixes, the cache key, the <TeX> Live directories.

    <item*|<source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>>The
    space of monospaced fonts.

    <item*|<source-link|TeXmacs/progs/fonts/fonts-opentype.scm|TeXmacs/progs/fonts/fonts-opentype.scm>>The profiles,
    the menus, <scm|init-opentype-font>.

    <item*|<source-link|TeXmacs/progs/fonts/font-short-menu.scm|TeXmacs/progs/fonts/font-short-menu.scm>>The text fonts
    of the focus toolbar.

    <item*|<source-link|TeXmacs/progs/generic/document-edit.scm|TeXmacs/progs/generic/document-edit.scm>,
    <source-link|document-menu.scm|TeXmacs/progs/generic/document-menu.scm>><scm|init-font> and the
    <menu|Document|Font> menus.

    <item*|<source-link|TeXmacs/fonts/truetype/|TeXmacs/fonts/truetype>, <verbatim|TeXmacs/fonts/*.scm>>The
    shipped fonts and database.
  </description-paragraphs>

  <section|Pitfalls>

  <\itemize>
    <item>A profile names its companions by master, not by family; a wrong
    name is only noticed when a document asks for it, since the profile
    test (<hlink|tests|opentype-tools.en.tm>) checks the math font, its
    family name and its <verbatim|MATH> table, not the companions.

    <item>The order of the profiles matters when two of them name the same
    text font. The companion-only <verbatim|Palatino> profile is declared
    last, after the menus.

    <item>The <verbatim|bold-math> key has no effect.

    <item>No key says which mathematical alphabets a font really has: a
    partial alphabet (the script letters of most fonts) is silently
    completed with emulated glyphs.

    <item><cpp|register_profiled_font> writes to the local database the
    first time a profiled family is asked for, which may happen while a
    document is typeset.

    <item>The font selection still knows some families by name:
    <cpp|is_math_family> lists the traditional math families and
    <cpp|tex_gyre_fix> and <scm|tex-gyre-font?> the four hand-tuned
    <name|TeX Gyre> fonts, which have no profile to consult.
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
