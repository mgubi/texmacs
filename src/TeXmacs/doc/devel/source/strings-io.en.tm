<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Where text is converted>

  The general rule is that strings inside <TeXmacs> are in the universal
  encoding, and that every module which exchanges text with the outside
  world converts at its own boundary. This page lists the main boundaries.
  There is no central place which enforces the rule, and some interfaces
  carry <name|UTF-8> or bytes of the operating system instead; these are
  pointed out below and in <hlink|string utilities and
  pitfalls|strings-utils.en.tm>.

  <section|Documents>

  <\description>
    <item*|<verbatim|.tm> files>The labels of the tree are written and read
    as they are: a <verbatim|.tm> file contains Cork bytes and universal
    symbols, not <name|UTF-8>. The serialization only escapes the
    characters which have a meaning in the file syntax (see <hlink|default
    serialization|../format/basics/tm-tm.en.tm>). This is why <verbatim|.tm>
    files edited by hand must not contain raw <name|UTF-8>: the bytes would
    be read as several Cork characters.

    <item*|Other formats>The converters of <verbatim|Data/Convert/> and
    <verbatim|progs/convert/> convert explicitly: <name|XML> and
    <name|HTML> use <scm|cork-\<gtr\>utf8> and <scm|utf8-\<gtr\>cork> (and
    named entities), <LaTeX> uses its own tables and the <name|T2A>
    encoding for Cyrillic, and the <scheme> serialization (<verbatim|stm>)
    keeps the universal encoding. See <hlink|converters to other data
    formats|conversions.en.tm> and <hlink|the <LaTeX> and <name|HTML>
    converters|convert.en.tm>.

    <item*|Plain text>Import and export of verbatim text
    (<verbatim|Data/Convert/Verbatim/verbatim.cpp>) follow the preferences
    <verbatim|verbatim-\<gtr\>texmacs:encoding> and
    <verbatim|texmacs-\<gtr\>verbatim:encoding>. On import,
    <verbatim|auto> uses <cpp|western_to_cork> (see <hlink|converters
    between encodings|strings-converters.en.tm>), <verbatim|utf-8> uses
    <cpp|utf8_to_cork>, <verbatim|SourceCode> uses <cpp|sourcecode_to_cork>,
    and <verbatim|iso-8859-1> as well as any other value only apply
    <cpp|tm_encode>. On export, <verbatim|auto> is first replaced by the
    character set of the locale (<cpp|get_locale_charset>);
    <verbatim|iso-8859-1> then applies <cpp|tm_decode>, <verbatim|cork>
    applies nothing, <verbatim|SourceCode> applies
    <cpp|cork_to_sourcecode> line by line, and any other value applies
    <cpp|cork_to_utf8> line by line (<cpp|var_cork_to_utf8>). On Windows,
    line ends are then converted to <verbatim|CR LF>.

    <item*|Selections in other languages>Copying or pasting text in
    Croatian, Czech, Hungarian, Polish, Slovak or Slovene with the verbatim
    encoding set to <verbatim|iso-8859-2> converts with <cpp|cork_to_il2>
    and <cpp|il2_to_cork>; Spanish and German text goes through
    <cpp|spanish_to_ispanish> and <cpp|german_to_igerman> and back
    (<verbatim|Edit/Replace/edit_select.cpp>).
  </description>

  <section|Keyboard and input methods>

  Keys reach the editor as strings in a small language of their own (see
  <hlink|the event loop|server.en.tm>): <verbatim|a>, <verbatim|S-a>,
  <verbatim|C-x>, <verbatim|return>, <verbatim|alpha>, ... A character
  typed on the keyboard is therefore converted to the universal encoding
  <em|without> its angular brackets.

  <\description>
    <item*|<name|Qt>><cpp|QTMKeyboardEvent::computeUnicodeToCork>
    (<verbatim|Plugins/Qt/QTMKeyboardEvent.cpp>) converts the text of the
    key event to <name|UTF-8> and then with <cpp|utf8_to_cork>. A result of
    the form <verbatim|\<less\><em|name>\<gtr\>> (but not
    <verbatim|\<less\>#...\<gtr\>>) loses its brackets, and
    <verbatim|less> and <verbatim|gtr> become <verbatim|\<less\>> and
    <verbatim|\<gtr\>>. Dead keys and combining accents are mapped to the
    key names <verbatim|grave>, <verbatim|acute>, <verbatim|hat>,
    <verbatim|umlaut> and <verbatim|tilde>. Text committed by an input
    method (<cpp|QTMWidget::inputMethodEvent>) is converted with
    <cpp|from_qstring>, that is, from <name|UTF-8> to Cork.

    <item*|<name|X11>><cpp|Xutf8LookupString> is followed by
    <cpp|utf8_to_cork> (<verbatim|Plugins/X11/x_loop.cpp>).

    <item*|Editor>When it shows keyboard shortcuts or handles pre-edit
    text, the editor converts back with <cpp|cork_to_utf8>
    (<verbatim|Edit/Interface/edit_keyboard.cpp>).
  </description>

  <section|Widgets and the clipboard>

  <\description>
    <item*|<cpp|to_qstring (s)>>Converts a string for display in a
    <name|Qt> widget (<verbatim|Plugins/Qt/qt_utilities.cpp>). Since many
    callers pass <name|UTF-8> (file names, titles computed in <scheme>)
    while most pass Cork, it <em|guesses>: a string which decodes as
    <name|UTF-8> and is neither pure <abbr|ASCII> nor a universal string is
    taken as <name|UTF-8>, anything else is converted with
    <cpp|cork_to_utf8>. The comment in the code calls this a hack. Code
    which knows that its string is <name|UTF-8> should call
    <cpp|utf8_to_qstring> instead.

    <item*|<cpp|from_qstring (q)>>The converse: <name|UTF-8> from
    <name|Qt>, then <cpp|utf8_to_cork>. <cpp|from_qstring_utf8> stops after
    the <name|UTF-8> step.

    <item*|Clipboard>When pasting, <cpp|qt_gui_rep::get_selection>
    (<verbatim|Plugins/Qt/qt_gui.cpp>) takes the native <TeXmacs> format if
    available, and otherwise <name|HTML> or plain text as <name|UTF-8>
    bytes, and passes them to the <scheme> converters (for instance from
    <verbatim|verbatim-snippet> or <verbatim|html-snippet> to
    <verbatim|texmacs-snippet>). When copying,
    <cpp|qt_gui_rep::set_selection> receives the bytes produced by the
    <scheme> converters and wraps them with <cpp|QString::fromUtf8> or
    <cpp|QString::fromLatin1>, depending on the target format and on the
    encoding preferences.
  </description>

  <section|Programs, files and <scheme>>

  <\description>
    <item*|Plug-in sessions>Output of a plug-in is converted line by line
    in <verbatim|Data/Convert/Generic/input.cpp>: in <verbatim|utf8> mode
    with <cpp|utf8_to_cork> (<cpp|texmacs_input_rep::utf8_flush>), in
    <verbatim|verbatim> mode with the <verbatim|auto> guess of
    <cpp|western_to_cork> (<cpp|verbatim_flush>); structured output in
    <TeXmacs> or <scheme> syntax is expected in the universal encoding. See
    <hlink|sessions and connections|plugins-sessions.en.tm>.

    <item*|File names and commands><abbr|URL>s and file names are kept in
    the encoding of the operating system (in practice <name|UTF-8>), and
    shell commands are passed to the system as they are; see <hlink|the
    system layer|system.en.tm>. Where file names are shown in the document
    or the interface, they are converted with <cpp|utf8_to_cork> (for
    instance for the default <verbatim|title> metadata in
    <cpp|edit_main_rep::get_metadata>) or through the heuristic of
    <cpp|to_qstring>.

    <item*|<scheme> strings>The glue passes strings between <c++> and
    <scheme> as byte sequences (<cpp|string_to_tmscm>,
    <cpp|tmscm_to_string> in <verbatim|Scheme/Guile/guile_tm.cpp>; see
    <hlink|the <scheme> interpreter and the glue|scheme-bridge.en.tm>), so
    <scheme> code sees Cork bytes and universal symbols, and must use
    <scm|tmstring-length> and friends rather than <scm|string-length> to
    count characters. Both supported dialects use
    <cpp|scm_from_locale_stringn> and <cpp|scm_to_locale_stringn>
    (<verbatim|Scheme/Guile/guile_tm.hpp>). With <name|Guile> 1.8
    (<verbatim|GUILE_C>, which includes the default embedded interpreter)
    they copy bytes unchanged; with <name|Guile> 2 and 3
    (<verbatim|GUILE_D>) they decode and encode according to the locale,
    so Cork bytes above 127 are not guaranteed to survive the round trip.

    <item*|Fonts>Universal characters are mapped to glyphs by the fonts;
    Unicode fonts convert symbols and escapes to code points (see
    <hlink|smart fonts|smart-fonts.en.tm>).

    <item*|<name|PDF>>Text strings in <name|PDF> files (outline and
    metadata) use <cpp|utf8_to_pdf_hex_string>.
  </description>

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
