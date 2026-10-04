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
  <menu|Edit|Preferences|Plug-ins>, with the model to use. When the wallet
  of <TeXmacs> is on (<menu|Edit|Preferences|Security>), the key is kept
  there, encrypted, rather than in the preferences. This is also how keys
  are given in a web browser, which has no environment variables. All the
  chatbots are asked by HTTP requests (with <verbatim|curl> when <TeXmacs>
  is not built with <name|Qt>, by the browser itself in a web browser).

  <subsection*|ChatGPT>

  Please follow the following instructions for setting up <name|ChatGPT> for
  use inside <TeXmacs>.

  <\itemize>
    <item>Create an account for <name|ChatGPT> and obtain a key. Keys
    typically start with <verbatim|sk->.

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
    <item>Create an account on the console of <name|Anthropic> and obtain an
    API key. Keys typically start with <verbatim|sk-ant->.

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
    <item>Create an account for <name|Gemini> and obtain a key.

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

  The server and the model are chosen in <menu|Edit|Preferences|Plug-ins>.

  <subsection*|Mistral>

  Please follow the following instructions for setting up <name|Mistral> for
  use inside <TeXmacs>.

  <\itemize>
    <item>Create an account for <name|Mistral> and obtain a key.

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