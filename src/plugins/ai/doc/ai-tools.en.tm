<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Correcting, translating and asking about a document>

  The tools of the menu <menu|Tools> use the chatbot chosen in
  <menu|Tools|AI engine> (<menu|Off> by default): <name|Albert>, <name|Chat
  GPT>, <name|Claude>, <name|Gemini>, <name|Ollama>, <name|Mistral> or
  <name|OpenRouter>, with the model of its preferences
  (<menu|Insert|Session|Preferences>). They appear once a chatbot is
  chosen.

  <subsection*|Questions about a document>

  With a selection, <menu|Tools|Ask about the selection> puts a session of
  the chatbot after the paragraph of the selection, with the selection in
  its input: type your question after it. Without a selection,
  <menu|Tools|Ask about the document> puts a session at the cursor which
  sends the whole document with each question (see <hlink|Chatting with a
  chatbot|ai-sessions.en.tm>).

  <subsection*|Translating and correcting>

  Select a piece of text, then use <menu|Tools|Translate> and choose the
  language into which it is translated (from the language of the text), or
  <menu|Tools|Correct> to correct its spelling and grammar. The selection
  is replaced by the answer once it has come; an error, or a missing key,
  is said on the status bar, and the selection is kept. On the desktop,
  <TeXmacs> waits for the answer, which takes some time for a large
  selection; in a web browser, it comes in the background.

  Two preferences in <menu|Edit|Preferences|Convert|AI> change the
  corrections: <with|font-series|bold|Show differences after text
  corrections> shows the corrected text as a comparison with the original
  (the changes can then be accepted or rejected as for the versioning tool),
  and <with|font-series|bold|Explain text corrections> opens a window
  <with|font-shape|italic|Comments from AI about corrections> when the
  chatbot explains its corrections (the corrector of <name|Albert> does).

  <subsection*|The agents of Albert>

  The instructions given to <name|Albert> can be chosen among
  <em|agents>: named instructions for correcting, for chatting (an
  interlocutor) or for translating. They are kept in a database: enable
  <menu|Tools|Database tool>, then <menu|Data|Open AI agents> opens them;
  <menu|Data|New entry> adds an agent of one of the three kinds, with its
  name and its instructions, and <menu|Data|Confirm entry> stores it. The
  preferences of <name|Albert> choose the <with|font-series|bold|Corrector
  agent>, the <with|font-series|bold|Interlocutor agent> and the
  <with|font-series|bold|Translator agent> (<verbatim|default> is the agent
  of <TeXmacs>), and the focus bar of a session of <name|Albert> changes
  its interlocutor. The agents only apply to <name|Albert>.

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
