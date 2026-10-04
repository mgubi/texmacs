<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Text and mathematics>

  <section|Text mode>

  Text mode is implemented in <verbatim|progs/text/>. Its predicate
  <scm|in-text?> holds when the environment variable <verbatim|mode> is
  <verbatim|text> and the cursor is not in a graphics; many definitions
  are further restricted to the standard styles (<scm|in-std-text?>) or to
  a language (<scm|in-french?>, <scm|in-cyrillic?>, ...).

  <paragraph|Tag groups.><verbatim|text/text-drd.scm> declares the groups
  which the editing routines test: <verbatim|section-tag> and
  <verbatim|section*-tag>, <verbatim|list-tag> with its subgroups
  <verbatim|itemize-tag>, <verbatim|enumerate-tag> and
  <verbatim|description-tag>, <verbatim|enunciation-tag>
  (<verbatim|theorem-tag>, <verbatim|definition-tag>, ...),
  <verbatim|doc-title-tag> and <verbatim|author-data-tag>,
  <verbatim|frame-tag>, the <verbatim|variant-tag> and
  <verbatim|numbered-tag> groups used by <scm|variant-circulate> and
  <scm|numbered-toggle>, and others. New list environments defined by a
  document are registered at run time with
  <scm|tm-register-new-list-tag>, which adds them to the
  <verbatim|new-list-tag> group.

  <paragraph|Editing routines.><verbatim|text/text-edit.scm> defines, for
  each kind of markup, a context predicate, the commands used by the menus
  and keyboard, and the redefinitions of the generic hooks:

  <\description>
    <item*|Titles and authors>The <markup|doc-data> and
    <markup|author-data> blocks: <scm|make-doc-data>,
    <scm|make-doc-data-element>, <scm|make-author-data-element>, the
    activation and deactivation of hidden title fields
    (<scm|doc-data-activate-all>, ...). <key|return> in a title adds an
    author, in an author name adds an affiliation, and in the keyword and
    classification fields of an abstract adds a new field.

    <item*|Sections><scm|section-context?>, <scm|make-section> (which
    turns a one paragraph selection into the title),
    <scm|make-unnamed-section>, <scm|go-to-section-title>. <key|return> in
    a section title leaves the title. <verbatim|text/text-structure.scm>
    contains the routines which analyse the section structure of a
    document, used for instance to split a document into parts.

    <item*|Lists><scm|list-context?>, <scm|make-tmlist>, <scm|make-item>.
    <key|return> in a list inserts a new item (of the right kind for
    itemize, enumerate or description lists), <key|S-return> a plain
    paragraph; <scm|numbered-toggle> switches between itemize and
    enumerate; <scm|standard-parameters> and <scm|parameter-choice-list>
    give the parameters of the focus menu (item tags, levels).

    <item*|Enunciations, algorithms, notes, frames, floats>Context
    predicates (<scm|enunciation-context?>, <scm|algorithm-context?>,
    <scm|frame-context?>, <scm|float-context?>, ...), toggles for names,
    numbers and titles (<scm|algorithm-toggle-name>,
    <scm|titled-toggle-name>, <scm|frame-toggle-title>, ...), the
    conversion between floating and non floating forms
    (<scm|turn-floating>, <scm|turn-non-floating>) and the
    <scm|customizable-parameters> of the decorated environments.

    <item*|Equations><scm|make-equation>, <scm|make-equation*>,
    <scm|make-eqnarray*> and the <scm|focus-label> of numbered
    equations; the editing inside equations belongs to math mode.

    <item*|Spaces>In text mode, <scm|kbd-space-bar> implements the
    <verbatim|text spacebar> preference: a second space is ignored, or
    turned into a wider space.
  </description>

  <paragraph|Keyboard and menus.><verbatim|text/text-kbd.scm> contains the
  text shortcuts: special symbols (quotes, dashes, ...), font dependent
  and Greek symbols, language specific blocks (conditioned on
  <scm|in-cyrillic?>, <scm|in-spanish?>, <scm|in-polish?>, ...), overrides
  for verbatim text and for references (<scm|in-verbatim?>,
  <scm|in-variants-disabled?>) and paragraph alignment shortcuts; the input
  methods for Chinese, Cyrillic and Vietnamese are in the subdirectories
  of <verbatim|text/>. <verbatim|text/text-menu.scm> defines
  <scm|text-format-menu> (in a full and a compressed form, according to the
  preferences), <scm|text-icons> and <scm|text-format-icons>, the menus for
  sections, lists, enunciations, floats and notes, and redefinitions of the
  focus menus for document titles, authors, abstracts, sections and floats.
  <verbatim|text/text-speech*.scm> define speech commands.

  <section|Mathematics>

  Math mode is implemented in <verbatim|progs/math/>. Besides
  <scm|in-math?>, the main modes are <scm|in-math-or-hybrid?>,
  <scm|in-math-not-hybrid?> (the <markup|hybrid> command line, entered with the backslash key,
  behaves partially as math),
  <scm|in-math-in-session?>, the language variants
  (<scm|in-math-english?>, <scm|in-math-french?>, ...) and the semantic
  mode <scm|in-sem-math?>.

  <paragraph|Tag groups.><verbatim|math/math-drd.scm> declares
  <verbatim|fraction-tag>, <verbatim|vertical-script-tag> (<markup|above>,
  <markup|below>), <verbatim|textual-operator-tag> and the
  <verbatim|math-annotation-tag> group of the semantic annotations
  (<markup|math-relation>, <markup|math-ordinary>, ...), and declares the
  alternates <markup|wide>/<markup|wide*> and
  <markup|around>/<markup|around*>.

  <paragraph|Keyboard.><verbatim|math/math-kbd.scm> is the largest keyboard
  file of <TeXmacs> (about 2600 lines). It consists of <scm|kbd-map> blocks
  conditioned on <scm|in-math?>, <scm|in-math-or-hybrid?>,
  <scm|in-math-not-hybrid?> and the language modes, and binds ordinary characters and their variants (<key|a> followed by
  <key|tab> gives <math|\<alpha\>>), brackets, scripts, fractions, big
  operators and symbols, often through symbolic prefixes such as
  <verbatim|math:small>, <verbatim|math:left> or <verbatim|math:greek>,
  which are resolved by the wildcards of
  <verbatim|texmacs/keyboard/prefix-kbd.scm>. Before the
  keymaps, it redefines <scm|disable-pre-edit?> and
  <scm|downgrade-pre-edit> for math mode, so that accented characters
  produced by the input method of the system are turned into plain
  letters.

  <paragraph|Editing routines.><verbatim|math/math-edit.scm> contains

  <\description>
    <item*|Spaces and insertion><scm|kbd-space-bar> implements the
    <verbatim|math spacebar> preference; <scm|kbd-insert> removes a space
    typed just before an infix, postfix or closing symbol.

    <item*|Formulas and equations><key|return> in <markup|math>,
    <markup|equation> and <markup|equation*> leaves the formula; the
    variants of formulas and equations (<scm|variant-formula>,
    <scm|variant-equation>, and <scm|variant-circulate> on <markup|math>
    and equations) switch between inline and displayed mathematics, and
    <scm|equation-\<gtr\>eqnarray> and <scm|eqnarray-\<gtr\>equation> between
    single equations and equation arrays.

    <item*|Scripts, roots, accents>Context predicates and variants for
    <markup|lsub>, <markup|rsup>, ... (<scm|script-context?>), for roots
    (<scm|sqrt-toggle>) and for wide accents.

    <item*|Brackets><scm|math-bracket-open>, <scm|math-bracket-close> and
    <scm|math-separator> are bound to the bracket keys. With the
    <verbatim|automatic brackets> preference (the default), opening a
    bracket inserts a matching pair (<markup|around> or <markup|around*>,
    according to the <verbatim|use large brackets> preference), closing a
    bracket fills a missing bracket of an adjacent pair or moves after it,
    and a selection is wrapped in the new pair. Without it, single
    <markup|left>, <markup|mid> and <markup|right> brackets are inserted
    and <scm|brackets-refresh> rebuilds the pairs around the cursor.
    <scm|variant-circulate> changes the shape of a bracket and
    <scm|geometry-vertical> its size.

    <item*|Correction><scm|math-correct-all> and
    <scm|math-correct-manually> apply the kernel routine
    <scm|manual-correct> (see <verbatim|Data/Tree/tree_correct.cpp>) to the
    selected formula or the whole document; the manual variant shows the
    changes as a version comparison.
  </description>

  <paragraph|Semantic editing.>When the preference <verbatim|semantic
  correctness> is <verbatim|on>, the module
  <verbatim|math/math-sem-edit.scm> is loaded (lazily, for the mode
  <scm|in-sem-math?>). It wraps <scm|kbd-insert>, <scm|make>,
  <scm|kbd-backspace>, <scm|kbd-delete>, the space keys and several insertion
  commands with <scm|former>, so that each edit is checked against the
  mathematical grammar:

  <\enumerate>
    <item>the formula around the cursor is tested with
    <scm|packrat-correct?> on the grammar <verbatim|std-math>, using the
    rule <verbatim|Main> for displayed formulas, <verbatim|Cell> in table
    cells and <verbatim|Strict> otherwise;

    <item>the macro <scm|try-correct> tries a list of alternative ways of
    performing the edit, each inside <scm|try-modification>, and keeps the
    first one which leaves the formula correct;

    <item>missing operands are represented by <markup|suppressed>
    placeholders (<scm|(suppressed (tiny-box))>), which are added and removed
    automatically around the cursor.
  </enumerate>

  <paragraph|Menus and other files.><verbatim|math/math-menu.scm> defines
  <scm|math-format-menu>, <scm|math-icons>, <scm|math-insert-icons> and the
  focus menus for formulas, equations and scripts.
  <verbatim|math/math-speech.scm> and the language files
  <verbatim|math-speech-en.scm>, <verbatim|math-speech-fr.scm>,
  <verbatim|math-adjust-en.scm> and <verbatim|math-adjust-fr.scm> implement
  speech input for formulas; <verbatim|math/math-stats.scm> analyses the
  statistical properties of the formulas of a document. The
  <markup|math> versus text decision for a single tree is made by the
  <abbr|DRD> (<scm|tree-child-env>, see <hlink|the DRD from
  Scheme|drd-scheme.en.tm>).

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
