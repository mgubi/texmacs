<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Introduction to AI tools inside <TeXmacs>>

  <TeXmacs> contains experimental support for various chatbots:
  <name|ChatGPT>, <name|Claude>, <name|Gemini>, <name|Mistral>, the models
  of many providers through <name|OpenRouter>, <name|Llama> and other models
  through <name|Ollama>, and <name|Albert>. For
  conversations with programs such as <TeXmacs>, most chatbots require you to
  generate a private key, the key of their API (some give such keys for
  free). The key is given in <menu|Insert|Session|Preferences>, or in an
  environment variable; when the wallet of <TeXmacs> is on, it is kept there,
  encrypted. Below, you will find specific instructions how to setup various
  chatbots for communication with <TeXmacs>. They also work in the version of
  <TeXmacs> which runs in a web browser, except <name|Albert>.

  Assuming that your chatbot, say <name|ChatGPT> is recognized by <TeXmacs>,
  you may use it the following ways:

  <\enumerate>
    <item>For direct chats, inside a session, using
    <menu|Insert|Session|ChatGPT>. In that case, <TeXmacs> allows you to
    directly put mathematical formulas in your queries and output with
    mathematical formulas can directly be cut and pasted into your documents.

    <item>For translations into another language. In that case, you first
    have to select your favorite engine via <menu|Tools|AI engine>. Next, you
    may simply select a piece of text and translate it to another language
    using <menu|Tools|Translate>. Note that chatbots are typically fairly
    slow, so you need to be a little bit patient, especially when selecting a
    large piece of text.

    <item>For correcting the spelling and grammar of a text. This works in a
    similar way as translation, except that you should now do
    <menu|Tools|Correct>.
  </enumerate>

  If setting up a chatbot for <TeXmacs> is too much work, or if you wish to
  use an unsupported service, then we provide one final possibility to
  interact with structured <TeXmacs> documents. Assume for instance that we
  wish to translate a piece of <TeXmacs> text from English into French using
  <name|Google Translate>:

  <\enumerate>
    <item>irst copy this piece of text using <menu|Tools|External AI|Copy>.
    Now paste the text into <name|Google Translate>. (This results your text
    to be pasted as an HTML document, while replacing all non-textual content
    by unique codes for internal use by <TeXmacs>.)

    <item>Next use <name|Google Translate> to translate the selected text
    into a language of your choosing.

    <item>Finally paste the translation back into <TeXmacs> using
    <menu|Tools|External AI|Paste>. The structure of the original selection
    should be recovered automatically.
  </enumerate>

  <tmdoc-copyright|2025|Joris van der Hoeven>

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