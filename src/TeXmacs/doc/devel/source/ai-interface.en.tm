<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Sessions, menus and agents>

  This page describes the <scheme> side of the AI integration: the plug-in
  which declares the AI sessions, the commands behind the <menu|Tools>
  menu, and the registry of AI agents.

  <section|The plug-in <verbatim|ai>>

  The plug-in lives in <verbatim|src/plugins/ai> (copied to
  <verbatim|TeXmacs/plugins/ai> by the build). Its file
  <verbatim|progs/init-ai.scm> declares one plug-in per engine, whose name
  is the model name:

  <\scm-code>
    (plugin-configure albert

    \ \ (:require (has-albert?))

    \ \ (:request ,ai-request ,ai-result)

    \ \ (:preferences #t)

    \ \ (:session "Albert")

    \ \ (:serializer ,ai-serialize))
  </scm-code>

  The other engines (<verbatim|chatgpt>, <verbatim|gemini>,
  <verbatim|ollama>, <verbatim|open-mistral-7b>) use <scm|(:cmdline
  ,ai-cmdline ,ai-result)> instead of <scm|:request>. The availability
  tests are <scm|has-chatgpt?> (the <verbatim|openai> tool is in the path
  and <verbatim|OPENAI_API_KEY> is set), <scm|has-gemini?> and
  <scm|has-open-mistral-7b?> (the key variable is set), <scm|has-ollama?>
  (the <verbatim|ollama> tool is in the path) and <scm|has-albert?> (the
  preference <verbatim|albert api key> is not empty). The same predicates
  decide which engines are offered in <menu|Tools|AI engine>.

  The file also declares the preferences of the engines (see below), the
  <scm|plugin-preferences-widget> shown for these plug-ins in the plug-in
  page of the preferences, and the helpers <scm|ollama-models>,
  <scm|ollama-model-variants> and <scm|ollama-default-model>, which parse
  the output of <verbatim|ollama list>. At load time it copies
  <verbatim|ALBERT_API_KEY> into the preference <verbatim|albert api key>
  if the variable is set.

  <section|How a session request is processed>

  The generic machinery for <scm|:cmdline> and <scm|:request> plug-ins is
  described in <hlink|the plug-in machinery|plugin-machinery.en.tm>. For the AI
  sessions, the steps are:

  <\enumerate>
    <item>The input field is serialized by the plug-in serializer
    <scm|ai-serialize> (<verbatim|tools/ai/ai-batch.scm>), as plain text or
    as a <LaTeX> snippet.

    <item>The link calls the <scheme> function <scm|connection-cmdline> or
    <scm|connection-request> (<verbatim|kernel/texmacs/tm-plugins.scm>)
    with the plug-in name, the chat name and the serialized input, which
    calls the function given in the configuration:

    <\description>
      <item*|<scm|ai-cmdline>>returns <scm|(cpp-ai-latex-command cmd name
      chat)>, a shell command. <cpp|cmdline_link_rep::write> runs it with
      <verbatim|/bin/sh -c> in a child process, with standard error
      discarded.

      <item*|<scm|ai-request>>returns <scm|(cpp-ai-latex-request cmd name
      chat)>, the request tree written as a <scheme> string.
      <cpp|request_link_rep::write> parses it and starts an asynchronous
      <verbatim|http_post> (the only form it accepts).
    </description>

    <item>When the command or the request has finished, the collected
    output goes to <cpp|texmacs_input_rep::cmdline_flush>
    (<verbatim|Data/Convert/Generic/input.cpp>), which calls
    <scm|connection-result>, hence <scm|ai-result>, that is,
    <scm|cpp-ai-latex-output>, and writes the resulting tree as the output
    of the session.
  </enumerate>

  The preference <verbatim|<em|name>-text-input> (the <verbatim|Textual
  input> toggle of the plug-in preferences, on by default) makes the input
  fields of the session text rather than mathematics
  (<scm|session-text-input?> in <verbatim|dynamic/session-edit.scm>).

  <section|The Tools menu>

  The AI entries of <verbatim|texmacs/menus/tools-menu.scm> are:

  <\description>
    <item*|<menu|AI engine>>Sets or resets the preference
    <verbatim|"ai"> to one of the available model names.

    <item*|<menu|Correct>>Shown when an engine is chosen and there is a
    selection; calls <scm|(ai-correct (get-preference "ai"))>.

    <item*|<menu|Translate>>Same condition; a submenu with one entry per
    supported language, which calls <scm|(ai-translate <em|lan>
    (get-preference "ai"))>.

    <item*|<menu|External AI>><menu|Copy> and <menu|Cut> put
    <scm|(compress-html (selection-tree) 1)> on the clipboard,
    <menu|Paste> inserts <scm|(decompress-html <em|clipboard> 1)>. This lets
    the user work with the web interface of any chatbot while keeping the
    markup.
  </description>

  These commands are defined in <verbatim|tools/ai/ai-batch.scm>, which is
  loaded at startup by <verbatim|init-texmacs.scm>:

  <\description>
    <item*|<scm|ai-correct>>Takes the selection, cuts it to the
    <verbatim|primary> clipboard, calls <scm|cpp-ai-correct> with the
    language of the selection, and inserts the result. If the preference
    <verbatim|ai-correct show differences> is on, it inserts instead the
    differences between the original and the corrected version, as computed
    by <scm|compare-versions> (see <hlink|versioning|collab-versioning.en.tm>).
    If <verbatim|ai-correct explain> is on and the model gave explanations,
    they are shown in the auxiliary buffer \PComments from AI about
    corrections\Q. Both preferences are set in the <verbatim|AI> tab of the
    <verbatim|Convert> page of the preferences.

    <item*|<scm|ai-translate>>The same for <scm|cpp-ai-translate>, from the
    language of the selection into the chosen one.
  </description>

  Both are synchronous: the editor is blocked until the answer has
  arrived.

  <section|Asynchronous translation>

  <verbatim|tools/ai/ai-translate.scm> implements <scm|(ai-translate*
  <em|lan>)>, which translates the selection, or the whole document if
  there is no selection, paragraph by paragraph and in the background, and
  <scm|ai-abort-translate>, which stops it. Both are declared lazily in
  <verbatim|init-texmacs.scm>; no menu entry calls them at present.

  The work is organized with the process utilities of
  <verbatim|utils/library/process.scm>: <scm|make-process> splits the
  document into paragraphs and calls the processing function on each of
  them in turn, through tree pointers so that the user may keep editing.
  For each paragraph, <scm|translate-process-one> compresses it, skips it
  if it contains no text, builds a request with <scm|cpp-ai-command> and an
  agent asking for an <name|HTML> translation, and starts it with
  <scm|cpp-ai-async-eval-command>. When the reply arrives,
  <scm|translate-processed-one> extracts the answer, decompresses it and
  replaces the paragraph, but only if the paragraph has not been modified
  meanwhile, and then continues with the next paragraph. The model is the
  preference <verbatim|"ai">.

  <section|AI agents>

  An <em|agent> is a named set of instructions which is added to the
  requests of a given kind. Agents are stored in the user database of kind
  <verbatim|"ai-agents"> (<verbatim|database/ai-agents-db.scm>), with three
  entry types, <verbatim|corrector>, <verbatim|interlocutor> and
  <verbatim|translator>, each with a single field
  <verbatim|instructions>. They are edited like bibliographies, through
  <menu|Data|Open AI agents>, which opens
  <verbatim|tmfs://db/ai-agents/global> (see <hlink|the user interface of
  the database|database-ui.en.tm>).

  For each engine, the preferences <verbatim|<em|engine> ai-agents
  corrector>, <verbatim|interlocutor> and <verbatim|translator> select an
  agent by name, or <verbatim|default>. <scm|ai-agents-get-corrector>,
  <scm|ai-agents-get-interlocutor> and <scm|ai-agents-get-translator>
  return the instructions of the selected agent; the defaults are a short
  built-in description for the corrector and nothing for the others. If
  the selected agent no longer exists, a warning is issued and the
  preference is reset to <verbatim|default>. <verbatim|ai.cpp> calls these
  functions only for <name|Albert>, and only the <name|Albert> preferences
  widget offers the choice.

  <section|Preferences>

  <\description>
    <item*|<verbatim|ai>>The engine used by <menu|Tools|Correct>,
    <menu|Translate> and <scm|ai-translate*>.

    <item*|<verbatim|ollama server>, <verbatim|ollama port>,
    <verbatim|ollama model>>The <name|Ollama> server and model.

    <item*|<verbatim|albert api key>, <verbatim|albert model>,
    <verbatim|albert chat history size>>The <name|Albert> key, the model
    variant (<verbatim|openweight-large>, <verbatim|-medium>,
    <verbatim|-small>) and the number of earlier exchanges sent with each
    session request.

    <item*|<verbatim|albert ai-agents corrector>, <verbatim|...
    interlocutor>, <verbatim|... translator>>The selected agents.

    <item*|<verbatim|<em|name>-text-input>>Text input in the sessions of
    the plug-in <em|name>.

    <item*|<verbatim|ai-correct show differences>, <verbatim|ai-correct
    explain>>The presentation of corrections.

    <item*|<verbatim|http request timeout>>The timeout of <abbr|HTTP>
    requests in <name|Qt> 6 builds.
  </description>

  <section|Pitfalls>

  <\itemize>
    <item><strong|All sessions of an engine share one conversation.> The
    links always pass the chat name <verbatim|"default">
    (<verbatim|System/Link/cmdline_link.cpp:191>,
    <verbatim|request_link.cpp:130> and
    <verbatim|Data/Convert/Generic/input.cpp:478>), so the <name|Albert>
    history is shared by all <name|Albert> sessions and is never reset.

    <item><strong|A failed correction removes the selection.>
    <scm|ai-correct> and <scm|ai-translate> cut the selection before the
    request is made (<verbatim|tools/ai/ai-batch.scm:62> and <verbatim|84>);
    if the request fails, the answer is empty and the selection is replaced
    by nothing. The original is still in the <verbatim|primary> clipboard.

    <item><strong|Asynchronous translation with <name|Albert> hangs.>
    <scm|cpp-ai-async-eval-command> refuses <name|Albert> requests (see
    <hlink|engines, requests and transport|ai-engines.en.tm>) without
    calling the callback, so the translation process never moves to the
    next paragraph and stays registered as running.

    <item><strong|Input is flattened in command line sessions.>
    <cpp|cmdline_link_rep::write> replaces the newlines of the serialized
    input by spaces before building the command, so the <LaTeX> sent to
    the model is on a single line and line breaks, for instance in
    verbatim blocks, are lost. On <name|Windows>, the same function does not run
    anything at all, so the command line engines do not work there.

    <item><strong|Keys and prompts on disk.> The <name|Albert> key taken
    from <verbatim|ALBERT_API_KEY> is stored in the preferences file in
    plain text, and the plug-in preferences widget displays it. Every
    <name|ChatGPT> prompt is written to
    <verbatim|$TEXMACS_HOME_PATH/system/tmp/chatgpt.txt>.
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
