<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Engines, requests and transport>

  This page describes <verbatim|Data/Convert/AI/ai.cpp> and
  <verbatim|json.cpp>: how a request for a given engine is built, how it is
  sent, and how the answer is extracted from the reply.

  <section|Engines and models>

  An AI request is always made for a <em|model> name, which is also the
  name of the corresponding plug-in session (<verbatim|"chatgpt">,
  <verbatim|"gemini">, <verbatim|"ollama">, <verbatim|"open-mistral-7b">,
  <verbatim|"albert">) and the value of the preference <verbatim|"ai">
  set by <menu|Tools|AI engine>. <cpp|ai_engine (model)> maps it to an
  <em|engine> by prefix: <verbatim|chatgpt>, <verbatim|gemini>,
  <verbatim|ollama>, <verbatim|open-mistral> (engine
  <verbatim|"mistral">) and <verbatim|albert>; any other name gives
  <verbatim|"unknown">, for which no request is built.

  The engines differ in how they are reached, where their key comes from
  and how their reply is read:

  <\description>
    <item*|<name|ChatGPT>>The prompt is written to
    <verbatim|$TEXMACS_HOME_PATH/system/tmp/chatgpt.txt> and passed to the
    command line tool <verbatim|openai -k 5000 complete
    <em|file>> (from the <name|Python> package <verbatim|openai-cli>),
    which reads the key from <verbatim|OPENAI_API_KEY>. The output of the
    tool is the answer.

    <item*|<name|Gemini>>A <verbatim|curl> command posting to the
    <verbatim|generateContent> endpoint of the model
    <verbatim|gemini-2.0-flash> (the model name is fixed in the code), with
    the key from <verbatim|GEMINI_API_KEY> in an
    <verbatim|X-goog-api-key> header. The answer is the value of the first
    <verbatim|"text"> field, located by string search.

    <item*|<name|Ollama>>A <verbatim|curl> command posting to
    <verbatim|http://<em|server>:<em|port>/api/generate> with
    <verbatim|"stream": false>. Server, port and model come from the
    preferences <verbatim|ollama server> (default <verbatim|localhost>),
    <verbatim|ollama port> (<verbatim|11434>) and <verbatim|ollama model>;
    the model <verbatim|default> is resolved by the <scheme> function
    <scm|ollama-default-model>, which runs <verbatim|ollama list> and
    prefers a model whose name starts with <verbatim|llama>. The answer is
    the <verbatim|"response"> field, located by string search.

    <item*|<name|Mistral>>A <verbatim|curl> command posting to
    <verbatim|https://api.mistral.ai/v1/chat/completions>, with the model
    name as given (<verbatim|open-mistral-7b>) and the key from
    <verbatim|MISTRAL_API_KEY> in a bearer authorization header. The
    answer is the first <verbatim|"content"> field.

    <item*|<name|Albert>>The French government service
    <verbatim|albert.api.etalab.gouv.fr>. This is the only engine whose
    request is a structured <abbr|HTTP> <verbatim|POST> rather than a shell
    command. The key comes from <verbatim|ALBERT_API_KEY> or the preference
    <verbatim|albert api key>, the model from the preference
    <verbatim|albert model> (default <verbatim|openweight-large>). The
    request is an <name|OpenAI> style chat completion with a
    <verbatim|system> message (the <em|agent> description, see below), the
    conversation history and the prompt. The reply is parsed as
    <name|JSON> and the answer is
    <verbatim|choices[0].message.content>.
  </description>

  Apart from <name|Albert>, the agent description (the instructions which
  say what kind of answer is expected) is simply prepended to the prompt.

  <section|Command trees>

  <cpp|ai_command (s, model, agent, chat, history)> does not perform the
  request; it returns a tree describing it, in one of two forms:

  <\description>
    <item*|<verbatim|(eval_system <em|cmd>)>>run the shell command
    <em|cmd> and use its standard output as the reply;

    <item*|<verbatim|(http_post <em|url> (tuple <em|h1> <em|v1> ...)
    <em|data>)>>post the <name|JSON> document <em|data> (a tree, see
    below) to <em|url> with the given header names and values.
  </description>

  Separating the description from the execution lets the same request be
  executed in four ways:

  <\description>
    <item*|<cpp|ai_eval_command (cmd)>>Synchronously, with
    <cpp|eval_system> or <cpp|http_post_json>; the call blocks the user
    interface until the reply has arrived.

    <item*|<cpp|ai_async_eval_command (cmd, callback)>>Asynchronously, with
    <cpp|async_eval_system> or <cpp|async_http_post_json>; the <scheme>
    <cpp|callback> is called with the reply from
    <cpp|async_eval_pending>, that is, from the interpose handler of the
    server. Following the convention of <cpp|async_eval_system>, it returns
    <cpp|true> on <em|failure>.

    <item*|As a shell command>The static <cpp|to_shell_command> renders
    either form as a shell command (an <verbatim|http_post> becomes a
    <verbatim|curl --silent -X POST> with <verbatim|--data-binary>);
    <cpp|ai_latex_command> uses it for the <scm|:cmdline> sessions.

    <item*|As a <scheme> string><cpp|ai_latex_request> returns the tree
    printed with <cpp|tree_to_scheme>; the <scm|:request> link reads it
    back with <cpp|scheme_to_tree> and executes it (see
    <hlink|sessions, menus and agents|ai-interface.en.tm>).
  </description>

  The prompt is inserted into the shell commands with <cpp|ai_quote>, which
  escapes double quotes, backslashes and newlines for the <name|JSON>
  string and turns single quotes into <verbatim|'\\''> for the
  surrounding single quoted shell argument. <cpp|ai_unquote> undoes the
  <name|JSON> escapes when the answer is extracted.

  <section|Transport>

  <cpp|eval_system> and <cpp|async_eval_system>
  (<verbatim|System/Misc/sys_utils.cpp>) run a command through the shell
  (<verbatim|popen> in a detached thread for the asynchronous variant,
  with <verbatim|2\<gtr\> /dev/null> appended). The <abbr|HTTP> routines
  of <verbatim|System/Files/web_files.hpp> have two implementations:

  <\itemize>
    <item>in <name|Qt> 6 builds, <verbatim|Plugins/Qt/qt_http.cpp> uses a
    shared <cpp|QNetworkAccessManager>; the synchronous
    <cpp|qt_http_post> runs a nested event loop until the reply has
    arrived, the asynchronous variant connects a <cpp|QTMHTTPHandler> to
    the <verbatim|finished> signal. The transfer timeout is the preference
    <verbatim|http request timeout> (in seconds, default 10), set in the
    <verbatim|AI> tab of the <verbatim|Convert> page of the preferences
    (<scm|ai-preferences-widget> in <verbatim|preferences-widgets.scm>). <cpp|http_from_json> uses
    <cpp|QJsonDocument>;

    <item>otherwise (<name|Qt> 5, <name|X11>), <verbatim|web_files.cpp>
    builds a <verbatim|curl> command line with the headers and the data and
    runs it with <cpp|system> or <cpp|async_eval_system>, without any
    timeout; <cpp|http_from_json> is the <cpp|json_to_tree> of
    <verbatim|json.cpp>.
  </itemize>

  <section|Extracting the answer>

  <cpp|ai_output (reply, model, chat)> dispatches to
  <cpp|chatgpt_output>, <cpp|gemini_output>, <cpp|ollama_output>,
  <cpp|mistral_output> or <cpp|albert_output>, which return the answer as
  a string (empty if the reply could not be understood). Except for
  <name|Albert>, the reply is not parsed: the answer is located by
  searching for a fixed key, cut at a fixed terminator and unquoted; a few
  <name|JSON> escapes such as <verbatim|\\u003c> are then replaced by
  hand. <cpp|albert_output> in addition

  <\itemize>
    <item>records the prompt and the answer in the conversation history;

    <item>renders <name|TikZ> pictures and extracts embedded <name|SVG>
    files (see <hlink|from documents to prompts and
    back|ai-documents.en.tm>);

    <item>replaces the narrow no-break space <name|U+202F> by an ordinary
    space.
  </itemize>

  <cpp|ai_chat (s, model, agent, chat)> combines <cpp|ai_command>,
  <cpp|ai_eval_command> and <cpp|ai_output>; the variant with
  <cpp|pre> and <cpp|post> arguments also splits the answer with
  <cpp|ai_get_body>, which returns the part between
  <verbatim|\<less\>body\<gtr\>> and <verbatim|\<less\>/body\<gtr\>> (both
  included) and what precedes and follows it.

  <section|Conversation history>

  Requests carry a <em|chat> name which identifies a conversation. Only
  <name|Albert> uses it: when <cpp|ai_command> is called with
  <cpp|history> set (which <cpp|ai_latex_request> and
  <cpp|ai_latex_command> do), the request includes the last prompts and
  answers of the conversation <verbatim|<em|model>-<em|chat>>, at most
  the value of the preference <verbatim|albert chat history size> (default
  3) of each. They are stored in static tables of <verbatim|ai.cpp>, so the
  history lives as long as the program and is not saved. A second
  mechanism, <cpp|ai_get_continuation> and <cpp|ai_set_continuation>,
  remembers the <verbatim|"id"> of the last answer and asks the model to
  follow up on it; it is only enabled for model names starting with
  <verbatim|none>, which no engine accepts, so it is currently dead code.

  <section|<name|JSON>>

  <verbatim|json.cpp> represents <name|JSON> values as trees: objects are
  <markup|attr> trees with alternating keys and values, arrays are
  <markup|tuple> trees and strings are atomic trees. Depending on the
  <cpp|mode> bits <verbatim|JSON_NULL>, <verbatim|JSON_BOOLEAN> and
  <verbatim|JSON_NUMBER>, <verbatim|null>, booleans and numbers are either
  plain strings or the compound trees <verbatim|json-null>,
  <verbatim|json-boolean> and <verbatim|json-number>. The functions are

  <\description>
    <item*|<cpp|json_object>, <cpp|json_array>>Constructors, with
    convenience overloads for a few arguments.

    <item*|<cpp|json_to_tree (s, mode)>>A tolerant parser: characters which
    cannot start a value are skipped.

    <item*|<cpp|tree_to_json (t, mode)>>A pretty printer; short arrays and
    objects are printed on one line.

    <item*|<cpp|json_get (t, key, mode)>>The value of a key in an object.
  </description>

  They are exported as <scm|json-\<gtr\>tree> and <scm|tree-\<gtr\>json>.

  <section|Pitfalls>

  <\itemize>
    <item><strong|Keys on command lines.> The <name|Gemini> and
    <name|Mistral> keys are part of the <verbatim|curl> command line, and
    so is the <name|Albert> key in builds without <name|Qt> 6. They are
    visible to other users of the machine in the process list. The
    <verbatim|curl> based <cpp|http_post> also prints the whole command,
    key included, as an error when it fails, and when <verbatim|-debug-io>
    is active.

    <item><strong|Asynchronous requests to <name|Albert> always fail.>
    <cpp|ai_async_eval_command> only accepts an <verbatim|http_post> whose
    data is atomic, but the data built by <cpp|albert_command> is a
    <name|JSON> tree; the function reports \Pwrong command\Q and returns
    <cpp|true> without ever calling the callback.

    <item><strong|Shell quoting.> The <name|Ollama> server, port and model
    preferences are inserted into the shell command without quoting.
    <cpp|ai_quote> does not escape tabs and other control characters, which
    are not allowed in <name|JSON> strings. In <cpp|gemini_command>, the
    line <verbatim|-X POST \\> is not followed by a newline, so an escaped
    space is passed to <verbatim|curl> as an extra argument.

    <item><strong|The <name|JSON> parser is incomplete.>
    <verbatim|\\u<em|xxxx>> escapes are not decoded (the backslash is
    dropped and the letter <verbatim|u> and the digits are kept), <verbatim|\\f> is turned
    into a backspace, and numbers with a sign or an exponent are not
    recognized (the minus sign is skipped). This matters for <name|Albert>
    answers in builds without <name|Qt> 6, which use this parser.

    <item>The extraction of answers for <name|Gemini>, <name|Ollama> and
    <name|Mistral> by string search breaks as soon as the services change
    the layout of their replies.
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
