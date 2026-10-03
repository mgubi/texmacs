<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|AI integration>

  <section|Introduction>

  <TeXmacs> can use large language models in three ways:

  <\itemize>
    <item>as <em|sessions>: the plug-in <verbatim|ai> declares one session
    type per supported engine (<name|ChatGPT>, <name|Gemini>, <name|Ollama>,
    <name|Mistral> and <name|Albert>), in which the user types a question and
    receives a typeset answer;

    <item>as <em|editing tools>: <menu|Tools|Correct> and
    <menu|Tools|Translate> send the selection to the engine chosen in
    <menu|Tools|AI engine> and replace it by the corrected or translated
    text, keeping the non textual markup intact;

    <item>as a <em|clipboard format>: <menu|Tools|External AI|Copy> puts the
    selection on the clipboard in a compact <name|HTML> form that can be
    pasted into the web interface of any chatbot, and
    <menu|Tools|External AI|Paste> converts the answer back.
  </itemize>

  This chapter describes how these features are implemented: how requests
  are built for each engine and sent over the network, how documents are
  turned into prompts and answers into documents, and how the user
  interface and the plug-in are wired together. The user level description,
  including how to obtain keys and install the command line tools, is the
  documentation of the plug-in itself
  (<verbatim|src/plugins/ai/doc/ai.en.tm> and
  <verbatim|ai-setup.en.tm>).

  The integration reuses three general mechanisms which are documented
  elsewhere: the plug-in links behind the <scm|:cmdline> and
  <scm|:request> options (<hlink|the plug-in machinery|plugin-machinery.en.tm>), the
  user database in which the AI <em|agents> are stored (<hlink|the database
  and bibliographies|database.en.tm>) and the <LaTeX> import used to read
  answers (<hlink|the <LaTeX> and <name|HTML> converters|convert.en.tm>).

  <section|Overview>

  The code is organized in four layers:

  <\description>
    <item*|User interface>The <menu|Tools> menu and the preferences
    (<verbatim|texmacs/menus/tools-menu.scm>,
    <verbatim|preferences-widgets.scm>), the <scheme> commands of
    <verbatim|tools/ai/>, and the plug-in <verbatim|ai>
    (<verbatim|src/plugins/ai/progs/init-ai.scm>), which configures the
    sessions and their preferences.

    <item*|Engines>The file <verbatim|Data/Convert/AI/ai.cpp> knows the
    individual engines. For a prompt it builds a <em|command tree>
    describing the request, and for the raw reply of the engine it
    extracts the answer. It also implements the higher level operations
    (chat, correction, translation, <LaTeX> answers).

    <item*|Document conversion>The file <verbatim|Data/Convert/AI/compress.cpp>
    turns a <TeXmacs> tree into a small subset of <name|HTML> in which all
    markup that is not natural language text is replaced by short
    identifiers, and converts such <name|HTML> back. It is shared with the
    <name|LanguageTool> spell checker. <verbatim|json.cpp> is a small
    <name|JSON> parser and printer.

    <item*|Transport>A command tree is executed either as a shell command
    (<cpp|eval_system>, mostly calls of <verbatim|curl>) or as an
    <abbr|HTTP> <verbatim|POST> of a <name|JSON> document
    (<cpp|http_post_json>), synchronously or asynchronously. In sessions,
    the same commands go through the plug-in links
    <cpp|cmdline_link_rep> and <cpp|request_link_rep>.
  </description>

  A request thus flows as follows, for instance for a correction:

  <\verbatim-code>
    selection tree

    \ \ --compress_html--\<gtr\> \ \ HTML with \<less\>a id="x3"\<gtr\>, \<less\>div id="x5"\<gtr\>...\<less\>/div\<gtr\>

    \ \ --ai_command--\<gtr\> \ \ \ \ command tree (eval_system "curl ...") or (http_post url headers json)

    \ \ --ai_eval_command--\<gtr\> raw reply of the engine (JSON or text)

    \ \ --ai_output--\<gtr\> \ \ \ \ \ answer text (HTML)

    \ \ --decompress_html--\<gtr\> tree with the original markup restored
  </verbatim-code>

  For sessions, the prompt is serialized as a <LaTeX> snippet and the
  answer is requested as a <LaTeX> document, which is imported with the
  <LaTeX> converter.

  <section|Source files>

  <\description-paragraphs>
    <item*|<verbatim|Data/Convert/AI/ai.cpp>>Engine dispatch, command
    trees, transport helpers, extraction of answers, conversation history,
    <LaTeX> answers (including <name|TikZ> and <name|SVG> pictures), chat,
    correction and translation.

    <item*|<verbatim|Data/Convert/AI/compress.cpp>>Compression of
    <TeXmacs> trees into text with placeholders, and the <name|HTML>
    encoding of compressed trees.

    <item*|<verbatim|Data/Convert/AI/json.cpp>>The <name|JSON> parser
    <cpp|json_to_tree>, the printer <cpp|tree_to_json> and
    <cpp|json_get>.

    <item*|<verbatim|Data/Convert/AI/lantool.cpp>>Merging the matches
    returned by a <name|LanguageTool> server into compressed
    <name|HTML> (<cpp|lantool_correct>), used by
    <verbatim|tools/spell/spell-lantool.scm>.

    <item*|<verbatim|Data/Convert/convert.hpp>>The declarations of all the
    above, with their default arguments.

    <item*|<verbatim|System/Files/web_files.hpp>, <verbatim|web_files.cpp>,
    <verbatim|Plugins/Qt/qt_http.cpp>>The <abbr|HTTP> <verbatim|POST>
    routines (<name|Qt> network classes in <name|Qt> 6 builds,
    <verbatim|curl> otherwise).

    <item*|<verbatim|System/Misc/sys_utils.cpp>>Synchronous and
    asynchronous execution of shell commands (<cpp|eval_system>,
    <cpp|async_eval_system>, <cpp|async_eval_pending>).

    <item*|<verbatim|System/Link/cmdline_link.cpp>,
    <verbatim|request_link.cpp>>The plug-in links used by the AI sessions.

    <item*|<verbatim|Scheme/Glue/build-glue-basic.scm>>The glue routines
    <scm|cpp-ai-...>, <scm|compress-html>, <scm|decompress-html>,
    <scm|json-\<gtr\>tree>, <scm|lantool-correct>, ...

    <item*|<verbatim|src/plugins/ai/progs/init-ai.scm>>The plug-in: one
    <scm|plugin-configure> per engine, the preferences and the
    preferences widget, <name|Ollama> model discovery. (The build copies the
    plug-in to <verbatim|TeXmacs/plugins/ai>.)

    <item*|<verbatim|tools/ai/ai-batch.scm>>Serialization of session
    input, the session callbacks, synchronous correction and translation,
    copy and paste for external chatbots.

    <item*|<verbatim|tools/ai/ai-translate.scm>>Asynchronous translation
    of a whole document, paragraph by paragraph.

    <item*|<verbatim|database/ai-agents-db.scm>,
    <verbatim|ai-agents-menu.scm>>The registry of AI agents.

    <item*|<verbatim|texmacs/menus/tools-menu.scm>>The <menu|Tools|AI
    engine>, <menu|Correct>, <menu|Translate> and <menu|External AI>
    entries.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Engines, requests and transport|ai-engines.en.tm>

    <branch|From documents to prompts and back|ai-documents.en.tm>

    <branch|Sessions, menus and agents|ai-interface.en.tm>
  </traverse>

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
