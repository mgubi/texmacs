<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Setting up AI support inside <TeXmacs>>

  Let us now detail how to install the support for individual chatbots. If
  you wish to automate the process, in order to permanently support various
  chatbots, then you should put the appropriate commands in your personal
  startup file for shell sessions, such as <hgroup|<verbatim|~/.bashrc>> or
  <verbatim|~/.profile>.

  Instead of an environment variable, the key of a chatbot can be given in
  <menu|Insert|Session|Preferences>, with the model to use. When the wallet
  of <TeXmacs> is on (<menu|Edit|Preferences|Security>), the key is kept
  there, encrypted, rather than in the preferences. This is also how keys
  are given in a web browser, which has no environment variables. Once the
  key is given, <with|font-series|bold|Update the list of models> asks the
  chatbot which models it offers to this key, and proposes them in
  <with|font-series|bold|Model>. A session begins with the name of the model
  which it asks; in a web browser the answer is shown as it comes (in grey),
  set by <TeXmacs> as far as it can be: up to the last environment, group
  or formula which is not closed yet. It is replaced by the whole answer
  when it is complete.

  A question of a session is sent with the conversation above it in the
  session, as its context: the last questions and answers (10 by default,
  <with|font-series|bold|Context> in the preferences of the chatbot). The
  context is the one of the document: it is there again when the document
  is opened again, and follows the changes made to it.

  The pictures of an answer are shown: a <name|TikZ> picture
  (<verbatim|tikzpicture>, <verbatim|tikzcd>, <verbatim|circuitikz>)
  becomes an executable fold of the <name|TikZ> plug-in, made at once, with
  the libraries and packages which the answer asks for; an <name|SVG>
  picture becomes an image. Each answer ends with a folded copy of the
  answer as it came, to see what the chatbot wrote (it is also what is sent
  back as the context); <with|font-series|bold|Show the answer as it came>
  in the preferences removes it.

  The chatbot is told how to write its answers: as a <LaTeX> document which
  <TeXmacs> takes well (sections, lists, mathematics, <name|TikZ> or
  <name|SVG> pictures, nothing which only matters for printing). These
  instructions can be changed for each chatbot:
  <with|font-series|bold|Instructions> <with|font-series|bold|Edit> in its
  preferences opens them as a text file, whose changes are used once it is
  saved; <with|font-series|bold|Default> comes back to the instructions of
  <TeXmacs>.

  The key of an API is not that of a subscription: <name|ChatGPT Plus> or
  <name|Claude Pro> do not include the use of the API, which is paid apart,
  according to use. In the console of the service, set a limit to the
  spending, and make a key for <TeXmacs> alone, which you can revoke without
  the others. Never put a key in a document. All the
  chatbots are asked by HTTP requests (with <verbatim|curl> when <TeXmacs>
  is not built with <name|Qt>, by the browser itself in a web browser).

  <subsection*|ChatGPT>

  Please follow the following instructions for setting up <name|ChatGPT> for
  use inside <TeXmacs>.

  <\itemize>
    <item>Create an account on
    <hlink|platform.openai.com|https://platform.openai.com> (not the site of
    <name|ChatGPT> itself), add credit in <with|font-series|bold|Billing>, and
    create a key in <with|font-series|bold|API keys>. Keys typically start
    with <verbatim|sk->.

    <item>In your terminal, set the <verbatim|OPENAI_API_KEY> environment
    variables with your key:

    <\shell-code>
      export OPENAI_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item>When launching <TeXmacs>, you should now be able to use
    <name|ChatGPT>.
  </itemize>

  <subsection*|Claude>

  Please follow the following instructions in order to setup <name|Claude>
  for use inside <TeXmacs>.

  <\itemize>
    <item>Create an account on the console of <name|Anthropic>,
    <hlink|console.anthropic.com|https://console.anthropic.com>, add credit
    in <with|font-series|bold|Billing>, and create a key in
    <with|font-series|bold|API Keys>. Keys typically start with
    <verbatim|sk-ant->.

    <item>In your terminal, set the <verbatim|ANTHROPIC_API_KEY> environment
    variable with your key, or give it in the preferences:

    <\shell-code>
      export ANTHROPIC_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item>When launching <TeXmacs>, you should now be able to use
    <name|Claude>.
  </itemize>

  <subsection*|Gemini>

  Please follow the following instructions in order to setup <name|Gemini>
  for use inside <TeXmacs>.

  <\itemize>
    <item>Obtain a key in <name|Google AI Studio>,
    <hlink|aistudio.google.com|https://aistudio.google.com> (<with|font-series|bold|Get
    API key>). It has a free tier, which is enough to try.

    <item>In your terminal, set the <verbatim|GEMINI_API_KEY> environment
    variables with your key:

    <\shell-code>
      export GEMINI_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item>When launching <TeXmacs>, you should now be able to use
    <name|Gemini>.
  </itemize>

  <subsection*|Llama>

  <name|Llama> has the advantage that the models can be run on your own
  computer, without any internet connection. Our preferred way to do this is
  through <verbatim|ollama>:

  <\itemize>
    <item>Install <verbatim|ollama> on your computer following the
    instructions from <hlink|here|https://ollama.com/>.

    <item>Download the model that you wish to use, <abbr|e.g.>
    <verbatim|llama3>:

    <\shell-code>
      ollama pull llama3
    </shell-code>

    <TeXmacs> also supports <verbatim|llama4>.

    <item>When launching <TeXmacs>, you should now be able to use <name|Llama
    3>.
  </itemize>

  In a web browser, <verbatim|ollama> answers the page only if it allows
  the address of the page, for instance for <TeXmacs> on
  <verbatim|mgubi.github.io>:

  <\shell-code>
    OLLAMA_ORIGINS=https://mgubi.github.io ollama serve
  </shell-code>

  The server and the model are chosen in <menu|Insert|Session|Preferences>.

  <subsection*|Mistral>

  Please follow the following instructions for setting up <name|Mistral> for
  use inside <TeXmacs>.

  <\itemize>
    <item>Create an account on
    <hlink|console.mistral.ai|https://console.mistral.ai> and obtain a
    key.

    <item>In your terminal, set the <verbatim|MISTRAL_API_KEY> environment
    variables with your key:

    <\shell-code>
      export MISTRAL_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item>When launching <TeXmacs>, you should now be able to use
    <name|Mistral>.
  </itemize>

  <subsection*|Albert (by DINUM, for French administrations only)>

  The server of <name|Albert> does not answer the requests of a web page:
  <name|Albert> cannot be used in a web browser.

  Please follow the following instructions for setting up <name|Albert> for
  use inside <TeXmacs>.

  <\itemize>
    <item>Create an account for <name|Albert> and obtain a key at
    <slink|https://albert.playground.etalab.gouv.fr>

    <item>Go to menu <menu|Insert|Session|Manual key>. Enter \Palbert\Q
    (without the quotes) for the key name and then copy the key.

    <item>Alternatively, in your terminal, set the <verbatim|ALBERT_API_KEY>
    environment variables with your key

    <\shell-code>
      export ALBERT_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    When launching <TeXmacs>, you should now be able to use <name|Albert>.
  </itemize>

  <tmdoc-copyright|2025|Joris van der Hoeven|Marc Lalaude-Labayle|Robin Wils>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>