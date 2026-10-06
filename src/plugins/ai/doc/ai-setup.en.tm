<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Setting up AI support inside <TeXmacs>>

  <subsection*|Keys>

  Most chatbots need the key of their API, which you obtain from the
  service (see the instructions for each of them below). Give it in
  <menu|Insert|Session|Preferences>, in the panel of the chatbot,
  <with|font-series|bold|API key>. A key is looked for in the wallet of
  <TeXmacs>, then in the preferences, then in the environment variable of
  the chatbot (<verbatim|OPENAI_API_KEY>, <verbatim|ANTHROPIC_API_KEY>,
  <verbatim|GEMINI_API_KEY>, <verbatim|MISTRAL_API_KEY>,
  <verbatim|OPENROUTER_API_KEY>, <verbatim|ALBERT_API_KEY>). The environment
  is read when <TeXmacs> starts: a <TeXmacs> started from the <name|Finder>,
  the <name|Dock> or a menu of the desktop does not see the variables of
  <verbatim|~/.bashrc> or <verbatim|~/.profile>, so the preferences are the
  simplest place.

  When the wallet of <TeXmacs> is on, the key is kept there, encrypted,
  rather than in the preferences, and the panel shows
  <with|font-series|bold|(in the wallet)>. On the desktop, the wallet needs
  <name|GnuPG> and the experimental feature
  <with|font-series|bold|Encryption> (<menu|Edit|Preferences|Other>); it is
  then set up in <menu|Edit|Preferences|Security>. In a web browser, which
  has no environment variables, the browser encrypts the wallet, opened with
  a passphrase or a passkey. A key given while the wallet is there but
  closed opens it first (its passphrase is asked), to keep the key in it;
  if it is not opened, the key is kept in the preferences, not encrypted.
  <menu|Insert|Session|Manual key> also keeps a key in the preferences, not
  encrypted.

  The chatbots are listed in <menu|Insert|Session|AI> before they have a
  key; a session without a key asks for it (see <hlink|Chatting with a
  chatbot|ai-sessions.en.tm>).

  The key of an API is not that of a subscription: <name|ChatGPT Plus> or
  <name|Claude Pro> do not include the use of the API, which is paid apart,
  according to use. In the console of the service, set a limit to the
  spending, and make a key for <TeXmacs> alone, which you can revoke without
  the others. Never put a key in a document.

  <subsection*|Preferences of a chatbot>

  The panel of each chatbot in <menu|Insert|Session|Preferences> has:

  <\description>
    <item*|API key>its key (see above);

    <item*|Model>the model of the new sessions and of the tools of
    <menu|Tools>; <with|font-series|bold|Update the list of models> asks the
    chatbot which models it offers to your key, and proposes them;

    <item*|Context>the number of former questions and answers sent with a
    question (10 by default);

    <item*|Reasoning>how much the models which reason do so
    (<name|ChatGPT>, <name|Claude>, <name|Gemini>, <name|OpenRouter> and
    <name|Ollama>);

    <item*|Instructions>how the chatbot is told to write its answers
    (<with|font-series|bold|Edit>, <with|font-series|bold|Default>);

    <item*|Textual input>the input of the sessions as plain text.
  </description>

  and, for all the chatbots, <with|font-series|bold|Show the answer as it
  came>, <with|font-series|bold|Show the reasoning> and
  <with|font-series|bold|Show the tokens and the cost>.

  <subsection*|Network>

  All the chatbots are asked by HTTP requests: with <verbatim|libcurl> on
  the desktop (or with <name|Qt> in the versions built with <name|Qt 6>),
  by the browser itself in a web browser. Behind a proxy, the requests go
  through the one of the system: the settings of <name|macOS> (with their
  exceptions and their automatic configuration) and <name|Windows>, the
  variables <verbatim|https_proxy>, <verbatim|http_proxy>,
  <verbatim|all_proxy> and <verbatim|no_proxy> of the environment elsewhere
  (and before the settings of <name|macOS>). Another one is given in
  <menu|Edit|Preferences|Convert|AI>, <with|font-series|bold|Proxy>:
  <verbatim|host:port>, <verbatim|socks5://host:port>, or
  <verbatim|direct> for none. The same tab has the
  <with|font-series|bold|Network timeout in seconds> of the versions built
  with <name|Qt>.

  <subsection*|ChatGPT>

  <\itemize>
    <item>Create an account on
    <hlink|platform.openai.com|https://platform.openai.com> (not the site of
    <name|ChatGPT> itself), add credit in <with|font-series|bold|Billing>,
    and create a key in <with|font-series|bold|API keys>. Keys typically
    start with <verbatim|sk->.

    <item>Give it in the preferences of <name|ChatGPT>, or set the
    <verbatim|OPENAI_API_KEY> environment variable:

    <\shell-code>
      export OPENAI_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>
  </itemize>

  <subsection*|Claude>

  <\itemize>
    <item>Create an account on the console of <name|Anthropic>,
    <hlink|console.anthropic.com|https://console.anthropic.com>, add credit
    in <with|font-series|bold|Billing>, and create a key in
    <with|font-series|bold|API Keys>. Keys typically start with
    <verbatim|sk-ant->.

    <item>Give it in the preferences of <name|Claude>, or set the
    <verbatim|ANTHROPIC_API_KEY> environment variable:

    <\shell-code>
      export ANTHROPIC_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>
  </itemize>

  <subsection*|Gemini>

  <\itemize>
    <item>Obtain a key in <name|Google AI Studio>,
    <hlink|aistudio.google.com|https://aistudio.google.com>
    (<with|font-series|bold|Get API key>). It has a free tier, which is
    enough to try.

    <item>Give it in the preferences of <name|Gemini>, or set the
    <verbatim|GEMINI_API_KEY> environment variable:

    <\shell-code>
      export GEMINI_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>
  </itemize>

  <subsection*|Ollama>

  <name|Ollama> runs models on your own computer, without any internet
  connection and without a key: <name|Llama>, <name|Mistral>,
  <name|Gemma>, <name|DeepSeek> and many others.

  <\itemize>
    <item>Install <verbatim|ollama> on your computer following the
    instructions from <hlink|ollama.com|https://ollama.com/>. On the
    desktop, <TeXmacs> offers <name|Ollama> when the program
    <verbatim|ollama> is in the path.

    <item>Download the models that you wish to use, <abbr|e.g.>
    <verbatim|llama3>:

    <\shell-code>
      ollama pull llama3
    </shell-code>
  </itemize>

  The preferences of <name|Ollama> give its server
  (<verbatim|localhost> by default) and its port (<verbatim|11434>), and
  the model, among the models installed (on the desktop, those which
  <verbatim|ollama list> gives). The default model is the first installed
  model whose name starts with <verbatim|llama>, else the first one.

  In a web browser, <verbatim|ollama> answers the page only if it allows
  the address of the page, for instance for <TeXmacs> on
  <verbatim|mgubi.github.io>:

  <\shell-code>
    OLLAMA_ORIGINS=https://mgubi.github.io ollama serve
  </shell-code>

  <subsection*|Mistral>

  <\itemize>
    <item>Create an account on
    <hlink|console.mistral.ai|https://console.mistral.ai> and obtain a key.

    <item>Give it in the preferences of <name|Mistral>, or set the
    <verbatim|MISTRAL_API_KEY> environment variable:

    <\shell-code>
      export MISTRAL_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>
  </itemize>

  <subsection*|OpenRouter>

  <name|OpenRouter> gives the models of many providers (<name|OpenAI>,
  <name|Anthropic>, <name|Google>, <name|DeepSeek>, <name|Meta>...) with a
  single key, named <verbatim|provider/model>, such as
  <verbatim|anthropic/claude-sonnet-4.5>; <verbatim|openrouter/auto> chooses
  one for each question.

  <\itemize>
    <item>Create an account on <hlink|openrouter.ai|https://openrouter.ai>,
    add credit in <with|font-series|bold|Credits> (the models whose name
    ends with <verbatim|:free> need none, with limits), and create a key in
    <with|font-series|bold|Keys>. Keys typically start with
    <verbatim|sk-or->.

    <item>Give it in the preferences of <name|OpenRouter>, or set the
    <verbatim|OPENROUTER_API_KEY> environment variable:

    <\shell-code>
      export OPENROUTER_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item><with|font-series|bold|Update the list of models> lists all its
    models (several hundreds); a model whose name says <verbatim|image>
    (<verbatim|google/gemini-2.5-flash-image>,
    <verbatim|openai/gpt-5-image>...) draws images.
  </itemize>

  <subsection*|Albert (by DINUM, for French administrations only)>

  The server of <name|Albert> does not answer the requests of a web page:
  <name|Albert> cannot be used in a web browser.

  <\itemize>
    <item>Create an account for <name|Albert> and obtain a key at
    <slink|https://albert.playground.etalab.gouv.fr>

    <item>Give it in the preferences of <name|Albert>, or set the
    <verbatim|ALBERT_API_KEY> environment variable:

    <\shell-code>
      export ALBERT_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>
  </itemize>

  The preferences of <name|Albert> choose its model
  (<verbatim|openweight-large>, <verbatim|-medium> or <verbatim|-small>),
  the number of former questions and answers sent with a question
  (<with|font-series|bold|Chat history size>) and its agents (see
  <hlink|Correcting, translating and asking about a
  document|ai-tools.en.tm>).

  <tmdoc-copyright|2025--2026|Joris van der Hoeven|Marc Lalaude-Labayle|Robin
  Wils|Massimiliano Gubinelli>

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
