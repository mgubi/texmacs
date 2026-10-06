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
    (<source-link|texmacs/menus/tools-menu.scm|TeXmacs/progs/texmacs/menus/tools-menu.scm>,
    <source-link|preferences-widgets.scm|TeXmacs/progs/texmacs/menus/preferences-widgets.scm>), the <scheme> commands of
    <verbatim|tools/ai/>, and the plug-in <verbatim|ai>
    (<source-link|src/plugins/ai/progs/init-ai.scm|plugins/ai/progs/init-ai.scm>), which configures the
    sessions and their preferences.

    <item*|Engines>The file <source-link|Data/Convert/AI/ai.cpp|src/Data/Convert/AI/ai.cpp> knows the
    individual engines. For a prompt it builds a <em|command tree>
    describing the request, and for the raw reply of the engine it
    extracts the answer. It also implements the higher level operations
    (chat, correction, translation, <LaTeX> answers).

    <item*|Document conversion>The file <source-link|Data/Convert/AI/compress.cpp|src/Data/Convert/AI/compress.cpp>
    turns a <TeXmacs> tree into a small subset of <name|HTML> in which all
    markup that is not natural language text is replaced by short
    identifiers, and converts such <name|HTML> back. It is shared with the
    <name|LanguageTool> spell checker. <source-link|json.cpp|src/Data/Convert/AI/json.cpp> is a small
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
    <item*|<source-link|Data/Convert/AI/ai.cpp|src/Data/Convert/AI/ai.cpp>>Engine dispatch, command
    trees, transport helpers, extraction of answers, conversation history,
    <LaTeX> answers (including <name|TikZ> and <name|SVG> pictures), chat,
    correction and translation.

    <item*|<source-link|Data/Convert/AI/compress.cpp|src/Data/Convert/AI/compress.cpp>>Compression of
    <TeXmacs> trees into text with placeholders, and the <name|HTML>
    encoding of compressed trees.

    <item*|<source-link|Data/Convert/AI/json.cpp|src/Data/Convert/AI/json.cpp>>The <name|JSON> parser
    <cpp|json_to_tree>, the printer <cpp|tree_to_json> and
    <cpp|json_get>.

    <item*|<source-link|Data/Convert/AI/lantool.cpp|src/Data/Convert/AI/lantool.cpp>>Merging the matches
    returned by a <name|LanguageTool> server into compressed
    <name|HTML> (<cpp|lantool_correct>), used by
    <source-link|tools/spell/spell-lantool.scm|TeXmacs/progs/tools/spell/spell-lantool.scm>.

    <item*|<source-link|Data/Convert/convert.hpp|src/Data/Convert/convert.hpp>>The declarations of all the
    above, with their default arguments.

    <item*|<source-link|System/Files/web_files.hpp|src/System/Files/web_files.hpp>, <source-link|web_files.cpp|src/System/Files/web_files.cpp>,
    <source-link|Plugins/Qt/qt_http.cpp|src/Plugins/Qt/qt_http.cpp>>The <abbr|HTTP> <verbatim|POST>
    routines (<name|Qt> network classes in <name|Qt> 6 builds,
    <verbatim|curl> otherwise).

    <item*|<source-link|System/Misc/sys_utils.cpp|src/System/Misc/sys_utils.cpp>>Synchronous and
    asynchronous execution of shell commands (<cpp|eval_system>,
    <cpp|async_eval_system>, <cpp|async_eval_pending>).

    <item*|<source-link|System/Link/cmdline_link.cpp|src/System/Link/cmdline_link.cpp>,
    <source-link|request_link.cpp|src/System/Link/request_link.cpp>>The plug-in links used by the AI sessions.

    <item*|<source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>>The glue routines
    <scm|cpp-ai-...>, <scm|compress-html>, <scm|decompress-html>,
    <scm|json-\<gtr\>tree>, <scm|lantool-correct>, ...

    <item*|<source-link|src/plugins/ai/progs/init-ai.scm|plugins/ai/progs/init-ai.scm>>The plug-in: one
    <scm|plugin-configure> per engine, the preferences and the
    preferences widget, <name|Ollama> model discovery. (The build copies the
    plug-in to <verbatim|TeXmacs/plugins/ai>.)

    <item*|<source-link|tools/ai/ai-batch.scm|TeXmacs/progs/tools/ai/ai-batch.scm>>Serialization of session
    input, the session callbacks, synchronous correction and translation,
    copy and paste for external chatbots.

    <item*|<source-link|tools/ai/ai-translate.scm|TeXmacs/progs/tools/ai/ai-translate.scm>>Asynchronous translation
    of a whole document, paragraph by paragraph.

    <item*|<source-link|database/ai-agents-db.scm|TeXmacs/progs/database/ai-agents-db.scm>,
    <source-link|ai-agents-menu.scm|TeXmacs/progs/database/ai-agents-menu.scm>>The registry of AI agents.

    <item*|<source-link|texmacs/menus/tools-menu.scm|TeXmacs/progs/texmacs/menus/tools-menu.scm>>The <menu|Tools|AI
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
