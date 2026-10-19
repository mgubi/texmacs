<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Pitfalls of tree-sitter>

  The pitfalls which concern all the features of program code (builds
  without a feature, providers, folds, layers) are in <hlink|the pitfalls
  of the features of program code|code-features-pitfalls.en.tm>. Those
  below are particular to tree-sitter.

  <section|Positions and encodings>

  <\itemize>
    <item>A column of tree-sitter is a byte of the <TeXmacs> string of the
    line, not a character: <verbatim|\<less\>alpha\<gtr\>> spans seven
    columns. This is what paths need, so the points of the glue go into
    paths unchanged; a column on the screen is <cpp|code_column> of it
    (an entity is one column, a tab goes to its stop), as the indentation
    computes it. The lexers and the scanners, which see decoded code
    points, count an entity as one character (<scm|lexer-column>), and a
    tab <verbatim|\<less\>#9\<gtr\>> is the character 9 for them.

    <item>The predicates of the queries (<verbatim|#eq?>,
    <verbatim|#any-of?>, <verbatim|#match?>) compare the undecoded text: a
    query of <TeXmacs> writes <verbatim|"\<less\>less\<gtr\>">, and the
    regular expressions of upstream queries see Cork bytes and entities,
    not <abbr|UTF-8>.

    <item><scm|tree-sitter-parse-sexp> and <scm|tree-sitter-query-captures>
    parse their string argument as <abbr|UTF-8>, without the Cork decoding
    of the regions; the tests compare a region with them after
    <scm|cork-\<gtr\>utf8>.

    <item>The colour codes of the lines carry the bit <cpp|TS_TOKEN_START>
    (<math|2<rsup|24>>) at the start of each token. Code which reads the
    stored colours of a line of tree-sitter must mask it
    (<cpp|TS_COLOR_MASK>), as <cpp|get_color> and
    <scm|tree-sitter-line-colors> do; colour codes must stay below the
    bit.
  </itemize>

  <section|Regions and lines>

  <\itemize>
    <item>The region of a line is found from its ip, in the edited tree. A
    line without ip, or in a detached tree (a copy typeset for an export,
    a test which typesets a detached tree), is parsed alone: a string or a
    comment over several lines is then coloured line by line. So is a
    string inside markup in a line.

    <item>A region is identified by its root tree, not by its content: a
    root replaced by an equal copy is a new region, parsed from scratch,
    and the old one stays until it is the least recently used.

    <item>At most 256 regions and 32 MB of text are kept; beyond that the
    least recently used region is dropped and its next request parses it
    in full. A test which makes many regions calls
    <scm|syntax-region-forget>.

    <item>At most 32 languages can be loaded in a session
    (<cpp|TSDYN_MAX_LANGUAGES>: the lex functions are called without the
    language) and 32 languages can have a scanner. The registry loads a
    grammar once, but <scm|tree-sitter-language-new> (and
    <scm|ts-c-language> in the tests) loads a new language at each call,
    which is never freed.

    <item>A serialized scanner state longer than 1024 bytes (the buffer of
    tree-sitter) is dropped, the scanner then starting again from the
    initial state there; and a state must not be modified once
    serialized, since its text is mapped back to the same value.
  </itemize>

  <section|Highlighters which come late>

  <\itemize>
    <item>The wrapper is registered when the list of the program languages
    is loaded. A program language made by <cpp|prog_language> before
    that, or not listed then, keeps its usual language for the session:
    the languages are made once per name.

    <item>When highlighting by tree-sitter is off, no highlighter is
    installed (the preference) or those which are installed are ignored
    (<scm|tree-sitter-set-highlighting>), and
    <scm|syntax-cursor-language> is <scm|#f>: the editing by the tree is
    off too, not only the colours. The tests of the editing functions need
    the tables of their grammars and the highlighter installed.

    <item>A highlighter which arrives after the region was coloured (the
    tables of an injected language generated in the background) is seen
    only after <scm|tree-sitter-reset-highlights>: the epoch makes every
    wrapped language take a new highlight code. The colours stored under
    the old codes stay on the lines until they are modified.

    <item>Lines whose colours change without being modified are typeset
    again only when the editor is idle (<scm|treesit-schedule-updates>):
    their stored colours are new at once, their boxes only after that
    update.

    <item>The queries are compiled once per language and session (the
    highlighters, and those of <source-link|ts-edit.scm|TeXmacs/progs/treesit/ts-edit.scm>); an edited query
    file is read at the next start. Only <verbatim|highlights.scm> follows
    a first line <verbatim|; inherits:>.
  </itemize>

  <section|Tables and caches>

  <\itemize>
    <item>A change of the generator or of the exported text needs a new
    <scm|treesit-generator-version>: the cache is keyed by it and by the
    grammar, and old tables would otherwise be read. The shipped tables
    must then be made again (<source-link|ship-tables.sh|misc/tree-sitter/ship-tables.sh>), or every grammar
    is generated at the first start; the suite <verbatim|treesit> fails
    while they do not match.

    <item>The generation in the background runs another <TeXmacs>: the
    <verbatim|bin/texmacs.bin> of <verbatim|$TEXMACS_PATH>, the binary of
    the macOS bundle, or else the <verbatim|texmacs> found in the path,
    which may be another version. The whole asynchronous path needs a
    running loop (<verbatim|-headless -X>), so the suite calls
    <scm|treesit-generate-cache> directly; Lean is tested only when its
    tables are in the cache.

    <item>The suite writes tables into the cache of
    <verbatim|$TEXMACS_HOME_PATH>, and needs those of <abbr|JSON>,
    <scheme> and C at least; corpus and capture checks against tree-sitter
    are the scripts of <source-link|misc/tree-sitter|misc/tree-sitter>, not
    the suite.

    <item>The suites run in one <TeXmacs>: a key typed in program code
    loads the keyboard of programs and its modules, which changed the
    behaviour of later suites. The tests call the editing functions
    instead of typing keys.
  </itemize>

  <section|<scheme> of the generator and the scanners>

  <\itemize>
    <item>Under S7, <scm|call/cc> is costly: scanners leave a scan with
    <scm|call-with-exit>. Under femtolisp the escape must not be called
    <scm|return>, a special form there, and a global macro wins over a
    local function of the same name.

    <item>Under S7, a <scm|do> counter stored with <scm|ahash-set!> ends as
    its last value; the loops which store their counter are named lets.

    <item>A call of a public function of another module costs about as
    much as the glue call behind it: the hot scanners keep the lookahead
    in a variable of their module, read once after each advance, and use
    the loops of <source-link|ts-scan.scm|TeXmacs/progs/treesit/ts-scan.scm>, which run in C.
  </itemize>

  <section|Orders of magnitude>

  Measured on an Apple M-series machine and recorded in
  <source-link|src/docs/tree-sitter.md|docs/tree-sitter.md>: generating the tables takes 0.2 s for
  <scheme>, 0.5 s for Python, 0.7 s for C and 6.5 s for C++ under S7 in
  <TeXmacs>, and Lean about a minute and gigabytes, hence the shipped
  tables (unpacked in about 0.1 s). A file of 100 KB of Python parses in
  about 40 ms; a scanner called at nearly every token costs more (254 KB
  of Bash in about 0.3 s). Typing in a C++ file of 13000 lines costs about 6
  ms per edit, most of it the incremental parse, since only the lines out
  of date are coloured again.

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
