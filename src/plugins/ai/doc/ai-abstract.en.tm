<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Introduction to AI tools inside <TeXmacs>>

  <TeXmacs> contains experimental support for various chatbots:
  <name|ChatGPT>, <name|Claude>, <name|Gemini>, <name|Mistral>, the models
  of many providers through <name|OpenRouter>, <name|Llama> and other models
  which run on your own computer through <name|Ollama>, and <name|Albert>.
  For conversations with programs such as <TeXmacs>, most chatbots require
  you to generate a private key, the key of their API (some give such keys
  for free). The key is given in <menu|Insert|Session|Preferences>, or in an
  environment variable; when the wallet of <TeXmacs> is on, it is kept
  there, encrypted. The <hlink|setup|ai-setup.en.tm> explains how to obtain
  and give the key of each chatbot. Everything also works in the version of
  <TeXmacs> which runs in a web browser, except <name|Albert>.

  Once your chatbot, say <name|ChatGPT>, has its key, you may use it in the
  following ways:

  <\enumerate>
    <item>For direct chats, inside a session, using
    <menu|Insert|Session|AI|ChatGPT>, or in an executable fold whose answer
    becomes part of your document (<menu|Insert|Fold|Executable|AI>). You
    may directly put mathematical formulas in your questions, and the
    answers, with their formulas and pictures, can be copied into your
    documents. See <hlink|Chatting with a chatbot|ai-sessions.en.tm>.

    <item>For questions about your document, for translations into another
    language, and for correcting the spelling and grammar of a text: choose
    the chatbot in <menu|Tools|AI engine>, then use <menu|Tools|Ask about
    the selection>, <menu|Tools|Ask about the document>,
    <menu|Tools|Translate> or <menu|Tools|Correct>. See <hlink|Correcting,
    translating and asking about a document|ai-tools.en.tm>.
  </enumerate>

  If setting up a chatbot for <TeXmacs> is too much work, or if you wish to
  use an unsupported service, then we provide one final possibility to
  interact with structured <TeXmacs> documents. Assume for instance that we
  wish to translate a piece of <TeXmacs> text from English into French using
  <name|Google Translate>:

  <\enumerate>
    <item>First copy this piece of text using <menu|Tools|External AI|Copy>
    (or <menu|Tools|External AI|Cut>). Now paste the text into <name|Google
    Translate>. (The text is copied as an HTML document, in which all
    non-textual content is replaced by unique codes for internal use by
    <TeXmacs>.)

    <item>Next use <name|Google Translate> to translate the selected text
    into a language of your choosing.

    <item>Finally paste the translation back into <TeXmacs> using
    <menu|Tools|External AI|Paste>. The structure of the original selection
    should be recovered automatically.
  </enumerate>

  The <menu|Tools> menu is shown with the detailed menus (<menu|Details in
  menus> in <menu|Edit|Preferences|General>).

  <tmdoc-copyright|2025--2026|Joris van der Hoeven|Massimiliano Gubinelli>

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
