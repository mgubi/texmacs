<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Spell checking and grammar checking>

  Spell checking is organized in four layers, from the external engine up
  to the user interface:

  <\enumerate>
    <item>a low level interface to an external spell checker, with three
    implementations;

    <item>a cache and session layer in <verbatim|System/Language/language.cpp>;

    <item>the spell checking of whole trees in
    <verbatim|Data/Tree/tree_spell.cpp>;

    <item>the <scheme> tools: continuous spell checking, the spell tool and
    toolbar, and grammar checking with <name|LanguageTool>.
  </enumerate>

  <section|Spell checking engines>

  The low level interface is declared in <verbatim|Plugins/Ispell/ispell.hpp>:

  <\cpp-code>
    string ispell_start (string lan);

    tree \ \ ispell_check (string lan, string s);

    void \ \ ispell_accept (string lan, string s);

    void \ \ ispell_insert (string lan, string s);

    void \ \ ispell_done (string lan);
  </cpp-code>

  Languages are <TeXmacs> language names; the <name|Aspell> and
  <name|Hunspell> backends translate them into dictionary names with
  <cpp|language_to_locale> (for instance
  <verbatim|fr_FR>). Words are passed in the Cork encoding and converted to
  <name|UTF-8> for the engine. <cpp|ispell_start> returns
  <verbatim|"ok"> or a message starting with <verbatim|"Error: ">.
  <cpp|ispell_check> returns <verbatim|"ok"> for a correct word, a
  message <verbatim|"Error: ..."> if the engine is not available, and
  otherwise a tuple whose first element is the <em|number> of suggestions,
  followed by the suggestions. <cpp|ispell_accept> accepts a word for the
  rest of the session, <cpp|ispell_insert> adds it to the personal
  dictionary of the engine, and <cpp|ispell_done> saves the personal
  dictionary.

  There are three implementations, chosen at compile time:

  <\description>
    <item*|<name|macOS> spell service>When <verbatim|MACOSX_EXTENSIONS> is
    defined, <verbatim|language.cpp> and <verbatim|edit_spell.cpp> map the
    five functions to <cpp|mac_spell_start>, ... in
    <verbatim|Plugins/MacOS/mac_spellservice.mm>, which use
    <cpp|NSSpellChecker>.

    <item*|<name|Aspell> library>Otherwise, if <verbatim|USE_ASPELL> is set
    (the <name|CMake> option of the same name, on by default, which is
    effective only if the library is found), <verbatim|ispell.cpp> links
    with <verbatim|libaspell>. It looks for dictionaries in
    <verbatim|$TEXMACS_PATH/aspell-0.60> if that directory exists (as in
    bundled distributions), and otherwise uses the system ones.

    <item*|External process>Otherwise <verbatim|ispell_exe.cpp> starts
    <verbatim|hunspell -a -i utf-8 -d <em|locale>> or, failing that,
    <verbatim|aspell -a --encoding=utf-8 --language-tag=<em|locale>>
    through a pipe (on <name|Windows> it also looks in the usual
    installation directories), and speaks the <verbatim|ispell -a>
    protocol: <verbatim|^<em|word>> to check a word, <verbatim|@<em|word>>
    to accept it, <verbatim|*<em|word>> to insert it and <verbatim|#> to
    save the personal dictionary. A dictionary is considered available if
    the process answers with its <verbatim|@(#)> banner.
  </description>

  In all cases there is one engine instance per language, stored as a
  resource (<cpp|ispeller> in the two <name|Aspell>/<name|Hunspell>
  implementations).

  <section|The session and cache layer>

  The rest of <TeXmacs> does not call the engine directly but the
  functions at the end of <verbatim|language.cpp>, exported to <scheme>
  as follows:

  <\description>
    <item*|<scm|multi-spell-start>, <scm|multi-spell-done>><cpp|spell_start
    ()> and <cpp|spell_done ()> open and close a <em|session> in which
    engines are kept running. Outside a session, every check is
    wrapped in <cpp|spell_start> and <cpp|spell_done>, so that the
    personal dictionary of the engine is saved after every word, which is
    slow.

    <item*|<scm|single-spell-start>, <scm|single-spell-done>>Start and stop
    the engine for one language.

    <item*|<scm|spell-check>><cpp|spell_check (lan, s)> returns the
    engine's answer. Words are checked in lower case, unless they are
    capitalized; for words in capitals the suggestions are converted to
    capitals. If no engine can be started, the answer is <verbatim|"ok">,
    so that documents in languages without a dictionary are simply not
    checked.

    <item*|<scm|spell-check?>><cpp|check_word (lan, s)> returns a boolean,
    using a cache <cpp|spell_cache> indexed by
    <verbatim|<em|lan>:<em|word>>.

    <item*|<scm|spell-accept>, <scm|spell-var-accept>><cpp|spell_accept
    (lan, s, permanent)> marks a word as correct in the cache (for the
    current session only unless <cpp|permanent>) and tells the engine.

    <item*|<scm|spell-insert>><cpp|spell_insert> adds a word to the
    personal dictionary of the engine and clears the cache of
    <verbatim|tree_spell.cpp>.

    <item*|<scm|spell-notify-insert>><cpp|spell_notify_insert> only marks a
    word as correct in the cache; it is used by the grammar tool for words
    which it stores in its own personal dictionary (see below).
  </description>

  The first time a language is used, <cpp|spell_initialize> calls the
  <scheme> function <scm|spell-user-words>, which returns the words of the
  <TeXmacs> personal dictionary
  <verbatim|$TEXMACS_HOME_PATH/langs/natural/spell/<em|lan>.scm>, and
  marks them as correct.

  <section|Spell checking trees>

  <verbatim|Data/Tree/tree_spell.cpp> finds all misspelled words of a tree
  and returns them as a <cpp|range_set> (a flat array of start and end
  paths). The front ends, exported as <scm|tree-spell>, <scm|tree-spell-at>
  and <scm|tree-spell-selection>, take the language, the tree, its path and
  a maximal number of hits; <scm|tree-spell-at> starts at a given position
  and proceeds outwards, so that the errors closest to the cursor are found
  first.

  The traversal follows the <abbr|DRD>: it only descends into accessible
  children (and the children of <markup|hidden>, and in source mode all
  children except those of <markup|raw-data>), and it tracks the
  <verbatim|mode> and <verbatim|language> variables through
  <cpp|get_env_child>, so that only text is checked, each part in its own
  language. The tags <markup|abbr>, <markup|name>, <markup|bib-list> and
  <markup|explain-macro> are skipped. Within a string, words are separated
  at spaces; leading and trailing non letters are stripped, words which
  begin or end with a digit and contain no lower case letters (such as
  postal codes) are accepted, and if a
  word with punctuation inside fails, its letter runs are checked
  separately.

  <scm|tree-spell*> (<cpp|spell_with_cache>) adds a cache indexed by
  language and subtree. For a document, each paragraph is looked up
  separately, so that only modified paragraphs are rechecked. The cache is
  renewed every five minutes, keeping the previous generation for one more
  period.

  <section|Continuous spell checking>

  When the preference <verbatim|continuous spell checking> is on, the
  editor calls the <scheme> function <scm|continuous-spell-check>
  (<verbatim|tools/spell/spell-edit.scm>) from
  <cpp|edit_interface_rep::apply_changes> whenever the tree or the
  environment changed. After 100 ms of idle time, this function runs
  <scm|tree-spell*> on the whole buffer in the language of the document and
  stores the result as the alternative selection
  <verbatim|"spell-errors">, notifying <cpp|THE_SPELL_ERRORS>. The editor
  then computes the rectangles of the errors, restricted to about one
  hundred errors around the visible part of the document, and draws them
  (<verbatim|Edit/Interface/edit_repaint.cpp>).

  <section|The spell tool>

  The <menu|Edit|Spell> command, <scm|interactive-spell>
  (<verbatim|generic/spell-widgets.scm>), computes the errors from the
  cursor position (or in the selection), and then opens either a toolbar
  at the bottom of the window (preference <verbatim|toolbar spell>, the
  default) or a spell tool in the side tools or in a dialog. While it is
  open, a spell session is active (<scm|multi-spell-start>). The tool
  shows the current error, the suggestions and buttons to replace, accept
  (<scm|spell-accept-word>, <scm|spell-keep-word>) or insert the word into
  the engine's personal dictionary (<scm|spell-insert-word>). The
  replacement is typed in an embedded <TeXmacs> buffer
  <verbatim|tmfs://aux/spell>.

  The editor also contains an older, key driven spell checking mode
  (<verbatim|Edit/Replace/edit_spell.cpp>, <scm|spell-start> and
  <scm|key-press-spell>): it walks through the document with
  <cpp|ispell_check> directly, bypassing the cache layer, and asks for an
  action in the footer (<verbatim|a> accept, <verbatim|r> replace,
  <verbatim|i> insert, a digit for a suggestion). It is no longer reachable
  from the menus.

  <section|Grammar checking with <name|LanguageTool>>

  Grammar checking uses an external <name|LanguageTool> server, either a
  local one (by default <verbatim|http://localhost:8081>; the script
  <verbatim|src/TeXmacs/misc/scripts/languagetool-server.ps1> starts one on
  <name|Windows>, by default on port 8085 rather than 8081, so the
  preference must be adapted) or the public service with an optional premium account.
  It is enabled by the preference <verbatim|grammar checking>; the
  <verbatim|languagetool> plug-in (<verbatim|src/plugins/languagetool/>)
  only declares the preferences <verbatim|languagetool server>,
  <verbatim|languagetool premium>, <verbatim|languagetool username>,
  <verbatim|languagetool API key> and <verbatim|languagetool use widgets>,
  and the corresponding preferences widget.

  <paragraph|Checking.><menu|Edit|Check grammar> (<scm|lantool-check> in
  <verbatim|tools/spell/spell-lantool.scm>) is shown when
  <scm|supports-lantool?> succeeds, that is, when a test query to the server
  returns a valid answer (the result is cached for the session). It starts
  an asynchronous <em|process> (<verbatim|utils/library/process.scm>) which
  visits the paragraphs of the document, or of the selection, one by one.
  For each paragraph:

  <\enumerate>
    <item>The paragraph is converted to a <em|compressed> <name|HTML>
    string with <scm|compress-html> (<verbatim|Data/Convert/AI/compress.cpp>):
    text is kept, and every piece of non textual markup is replaced by an
    opaque identifier <verbatim|x<em|n>> in an <verbatim|\<less\>a
    id\<gtr\>> or <verbatim|\<less\>div id\<gtr\>> element, so that the
    server only sees text.

    <item>The string is posted to <verbatim|<em|server>/v2/check> with
    <scm|async-http-post-query>, with the locale of the paragraph's language
    and the rule <verbatim|UPPERCASE_SENTENCE_START> disabled. Answers are
    cached by language and string.

    <item>The <name|JSON> answer is merged into the string by
    <scm|lantool-correct> (<verbatim|Data/Convert/AI/lantool.cpp>): every
    match whose text is not part of the markup becomes a
    <markup|spell-error> element with the original text, the message and up
    to nine replacements. Before it is inserted, the <scheme> function
    <scm|spell-replace-cached> may resolve it at once, if the user already
    chose a replacement for the same error or accepted the word.

    <item>The result is decompressed with <scm|decompress-html> and, if
    the paragraph has not changed in the meantime and contains at least one
    error, replaces the paragraph.
  </enumerate>

  <paragraph|Correcting.>The <markup|spell-error> tag (defined in
  <verbatim|src/TeXmacs/packages/standard/std-fold.ts>, and grouped as
  <scm|spell-tag> in <verbatim|version/version-drd.scm>) is rendered by the
  <scheme> function <scm|ext-spell-error> as the original text with a
  balloon listing the message and the proposals. <verbatim|spell-edit.scm>
  provides the navigation between errors (<scm|spell-go-to-next>, ...), the
  resolution of an error by one of its alternatives (<scm|spell-retain>) or
  by a typed replacement (<scm|spell-replace>), the personal dictionary of
  the grammar tool (<scm|spell-retain-permanent>, stored in
  <verbatim|$TEXMACS_HOME_PATH/langs/natural/spell/<em|lan>.scm>), and the
  removal of all remaining <markup|spell-error> tags
  (<menu|Edit|Terminate grammar>, <scm|spell-terminate>). The correction
  tool <verbatim|tools/spell/correct-widgets.scm> (<menu|Edit|Correct
  grammar>, <scm|open-correct>) shows the current error with its message
  and proposals, either in a bottom toolbar or in a widget.

  <section|Pitfalls>

  <\itemize>
    <item>The <name|Aspell> library backend returns the checked word, not
    the number of suggestions, as the first element of the tuple
    (<verbatim|Plugins/Ispell/ispell.cpp:122>). The <scheme> tools ignore
    this element, but the old key driven mode
    (<verbatim|Edit/Replace/edit_spell.cpp:182>) reads it as a number: with
    this backend a digit key accepts the word instead of choosing a
    suggestion.

    <item>In the external process backend, <cpp|ispell_check> uses the
    local handle <cpp|sc> after calling <cpp|ispell_start> without
    retrieving it again (<verbatim|Plugins/Ispell/ispell_exe.cpp:270-275>);
    if the resource did not exist yet, it dereferences a nil handle. The
    usual call path through <cpp|spell_check> always starts the engine
    first, which hides the problem.

    <item>There are two personal dictionaries: the engine's (written by
    <scm|spell-insert>) and the <TeXmacs> one of the grammar tool (written
    by <scm|spell-retain-permanent>). Words added to the second are known
    to the spell checker through <scm|spell-user-words>, but words added to
    the first are unknown to the grammar tool.

    <item>When an engine cannot be started, <cpp|spell_check> also clears
    the global session flag (<verbatim|language.cpp:521>), which ends a
    session opened by another tool.

    <item><verbatim|spell-lantool.scm> declares its dependencies without
    the <scm|:use> keyword (<verbatim|tools/spell/spell-lantool.scm:13-16>),
    so <scm|texmacs-module> ignores them; the module only works because
    <verbatim|spell-kbd.scm>, which is loaded at startup, imports
    <verbatim|spell-edit.scm>.

    <item>The tables of compressed markup in <verbatim|compress.cpp> are
    global and are never emptied, so they grow during a session.

    <item>The locale codes used to select dictionaries and the
    <name|LanguageTool> language are partly wrong; see <hlink|locales|language-translation.en.tm>.
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
