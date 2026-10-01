<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|From documents to prompts and back>

  Language models only understand text, while a <TeXmacs> selection is a
  tree with arbitrary markup. Two strategies are used to bridge this gap:

  <\itemize>
    <item>for <em|corrections and translations>, the markup must survive
    untouched: everything which is not natural language text is replaced by
    an opaque identifier, the model is asked to work on an <name|HTML>
    document which only contains text, paragraphs and these identifiers,
    and the identifiers are expanded again in the answer
    (<verbatim|compress.cpp>);

    <item>for <em|sessions>, the prompt is sent as <LaTeX> and the model is
    asked to answer with a <LaTeX> document, which is imported with the
    <LaTeX> converter (<cpp|ai_latex_output> in <verbatim|ai.cpp>).
  </itemize>

  <section|Compression of markup>

  <paragraph|Compressing trees.><cpp|compress_tree (t)> walks the tree:

  <\itemize>
    <item>atomic trees (text) are kept;

    <item><markup|document> and <markup|concat> nodes are kept, and their
    children are compressed recursively;

    <item>every other node is replaced by a node
    <verbatim|(compressed <em|code> <em|c1> ... <em|cn>)>. The children
    <em|ci> are the compressed versions of the children of the original node
    which contain natural language text in the language of the
    surrounding text: children which are accessible, in <verbatim|text>
    mode and which do not change the <verbatim|language>, according to the
    current <abbr|DRD> <cpp|the_drd> (children of <markup|bib-list> are
    never considered text). All other children stay in the original node,
    which is remembered under <em|code> with placeholders
    <verbatim|(decompressed)> for the extracted children.
  </itemize>

  For instance, a formula or an image becomes a node without children,
  while <markup|strong> or <markup|section> becomes a node whose single
  child is its text. The code is <verbatim|x<em|n>>; it is allocated in the
  static tables <cpp|compressed> (code to key) and <cpp|compressed_code>
  (key to code), where the key consists of the original node with
  placeholders, a <em|space mode> recording whether the node was preceded
  or followed by a space in its <markup|concat>, and the path of the node in
  the compressed tree. The same markup at the same position therefore
  always gets the same code during a session.

  <paragraph|The <name|HTML> encoding.><cpp|compressed_to_html (c, mode)>
  writes a compressed tree in a small subset of <name|HTML>:

  <\itemize>
    <item>each paragraph of a <markup|document> becomes
    <verbatim|\<less\>p\<gtr\>...\<less\>/p\<gtr\>> (followed by a newline
    if <cpp|mode> contains <verbatim|COMPRESS_LINE_FEEDS>, value 1);

    <item>a compressed node without children becomes
    <verbatim|\<less\>a id="x<em|n>"\<gtr\>>;

    <item>a compressed node with children becomes
    <verbatim|\<less\>div id="x<em|n>"\<gtr\><em|c1>\<less\>/div\<gtr\>>,
    followed by <verbatim|\<less\>div id="cont-x<em|n>"\<gtr\><em|ci>\<less\>/div\<gtr\>>
    for the further children;

    <item>text is converted to <name|UTF-8> and <verbatim|&>,
    <verbatim|\<less\>> and <verbatim|\<gtr\>> are escaped;

    <item>the whole is wrapped in <verbatim|\<less\>body\<gtr\>> unless
    <cpp|mode> contains <verbatim|COMPRESS_SNIPPET> (value 2).
  </itemize>

  <cpp|compress_html (t, mode)> combines both steps. The inverse
  <cpp|decompress_html (s, mode)> parses this subset (tolerating some
  inconsistent quoting produced by models), rejects identifiers which
  appear at a position that is not below the position where they were
  created (the parse stops there), and calls <cpp|decompress_tree>, which
  substitutes the stored nodes, puts the answered texts in the
  placeholders, and restores the spaces around the nodes from their space
  mode. <cpp|compressed_contains_text (t)> tells whether there is any text
  outside the codes; it is used to skip paragraphs without text.

  The same encoding is used by the <name|LanguageTool> spell checker
  (<verbatim|tools/spell/spell-lantool.scm>): <cpp|lantool_correct (s,
  out)> (<verbatim|lantool.cpp>) takes the compressed <name|HTML> that was
  sent and the <name|JSON> reply of the server, and inserts a
  <markup|spell-error> node for every match with replacements, skipping
  matches which fall inside the <name|HTML> markup or the codes.

  <section|Corrections and translations>

  <cpp|ai_correct (t, lan, model, chat)> and <cpp|ai_translate (t, from,
  into, model, chat)> (<scheme>: <scm|cpp-ai-correct>,
  <scm|cpp-ai-translate>) work as follows:

  <\enumerate>
    <item>the tree is compressed with <cpp|compress_html>;

    <item>an agent description is built:

    <\itemize>
      <item>for corrections, a request to correct the spelling and grammar
      of the text in the given language without explanations; for
      <name|Albert>, instructions to act as a native speaker who corrects
      <name|HTML> documents, preserves the tags, adds the instructions of
      the selected <em|corrector agent>, and puts explanations in
      <name|HTML> comments at the end;

      <item>for translations, a request to translate <name|HTML> documents
      without explanations, to which <name|Albert> adds the instructions of
      the selected <em|translator agent>;
    </itemize>

    <item><cpp|ai_chat> sends the request synchronously and splits the
    answer into the <verbatim|\<less\>body\<gtr\>> part and what surrounds
    it;

    <item>the body is decompressed. For corrections the result is a
    <markup|tuple> whose first element is the corrected tree and whose
    other elements are the decompressed comments
    (<verbatim|\<less\>!--...--\<gtr\>>) found after the body.
  </enumerate>

  The asynchronous translation of <scm|ai-translate*> uses the lower level
  routines directly (see <hlink|sessions, menus and
  agents|ai-interface.en.tm>).

  <section|<LaTeX> answers>

  Sessions use <LaTeX> in both directions:

  <\description>
    <item*|The prompt>The session input is serialized by
    <scm|ai-serialize> (<verbatim|tools/ai/ai-batch.scm>): a document with a
    single paragraph is unwrapped, plain text is sent as <name|UTF-8>, and
    anything else is converted to a <LaTeX> snippet with <name|UTF-8>
    encoding.

    <item*|The request><cpp|ai_latex_command> and <cpp|ai_latex_request>
    call <cpp|ai_command> with history enabled and with an agent
    description asking for an untitled <LaTeX> document; for <name|Albert>
    also without comments, with <name|SVG> pictures embedded in
    <verbatim|filecontents*> environments, plus the instructions of the
    selected <em|interlocutor agent>.

    <item*|The answer><cpp|ai_latex_output (reply, model, chat)> extracts
    the answer with <cpp|ai_output>, keeps the part between
    <verbatim|\\begin{document}> and <verbatim|\\end{document}>, removes
    <verbatim|\\maketitle>, turns <verbatim|lstlisting> into
    <verbatim|verbatim>, turns literal <verbatim|\\n> sequences into
    newlines (<cpp|un_escape_cr>, which leaves macros such as
    <verbatim|\\neq> or <verbatim|\\noindent> alone), imports the result
    with the <verbatim|latex-snippet> converter, embeds the pictures and
    wraps everything in <verbatim|(with "mode" "text" ...)>. An answer
    without a <verbatim|document> environment is returned unchanged, as a
    string.
  </description>

  Pictures in <name|Albert> answers are handled in <cpp|albert_output>,
  before the import:

  <\itemize>
    <item><cpp|replace_tikz_by_pdf> copies the preamble of the answer and
    its first <verbatim|tikzpicture> into a <verbatim|standalone> document
    in the temporary directory, compiles it with <verbatim|pdflatex> and
    replaces the picture by an <verbatim|\\includegraphics> of the
    resulting <abbr|PDF>; it repeats this for the following pictures. If
    <verbatim|pdflatex> is not installed or fails, the pictures are left as
    they are (with a warning).

    <item><cpp|extract_svg> saves the contents of each
    <verbatim|filecontents*> (or <verbatim|filecontent*>) environment to a
    file of the given name in the temporary directory and makes the
    <verbatim|\\includesvg> and <verbatim|\\includegraphics> commands
    refer to it.
  </itemize>

  After the import, <cpp|embed_images> replaces the file name of every
  <markup|image> by the contents of the file (looking also in the
  temporary directory, with an <verbatim|.svg> suffix), so that the session
  output does not depend on temporary files.

  <section|Pitfalls>

  <\itemize>
    <item><cpp|ai_correct> and <cpp|ai_translate> call <verbatim|ai_post
    (r, u)> with the <em|answer string> <cpp|r> instead of the original
    tree <cpp|t> (<verbatim|ai.cpp:815> and <verbatim|851>). Since a string
    is never a <markup|document>, <cpp|ai_post> never removes the trailing
    empty paragraphs it was written to remove.

    <item>The compression tables are global and never cleared, so they grow
    during a session, and codes are only meaningful in the session which
    created them: pasting with <menu|Tools|External AI|Paste> an answer
    produced from a copy made in another session restores the text but
    loses all the markup that was replaced by codes.
    Identifiers which the model invents or garbles are silently dropped by
    <cpp|decompress_tree> (they decompress to the empty string), together
    with the markup they stood for.

    <item>The compression depends on <cpp|the_drd>, that is, on the
    current buffer; compressing a tree of another buffer may classify its
    children incorrectly.
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
