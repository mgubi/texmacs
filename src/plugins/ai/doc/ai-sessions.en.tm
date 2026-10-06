<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Chatting with a chatbot>

  <subsection*|Sessions>

  <menu|Insert|Session|AI> lists the chatbots: <name|Albert>,
  <name|ChatGPT>, <name|Claude>, <name|Gemini>, <name|Mistral>,
  <name|Ollama> and <name|OpenRouter> (<name|Ollama> only when it is
  installed). A session of a chatbot begins with its name and the model
  which it asks, the model of the preferences of the chatbot. Type your
  question in the input field (with formulas, if you wish) and press
  <key|Return>; <with|font-series|bold|Textual input> in the preferences
  (<menu|Insert|Session|Preferences>) or in <menu|Focus|Input options> makes
  the input plain text.

  A session without the key of its chatbot asks for it: it opens the
  wallet of <TeXmacs> if it is closed (the key may be there), and otherwise
  the preferences of the chatbot; a question asked without a key says so,
  and nothing is sent. See <hlink|Setting up AI support|ai-setup.en.tm>.

  The answer is shown as it comes (in grey), set by <TeXmacs> as far as it
  can be: up to the last environment, group or formula which is not closed
  yet. It is replaced by the whole answer when it is complete. A model which
  reasons first shows <with|font-series|bold|Thinking...> with the end of
  its reasoning. <menu|Focus|Interrupt execution> (or the stop icon of
  the focus bar) stops an answer.

  Each session has its own model, kept in the document. The menu of the
  model in the focus bar of the session changes it for the next questions
  of this session; it lists the models of the chatbot (the list of the
  preferences, or the one given by <with|font-series|bold|Update the list
  of models>, which asks the chatbot which models it offers to your key),
  and <with|font-series|bold|Other model> asks for the name of another one.

  <subsection*|The conversation>

  A question of a session is sent with the conversation above it in the
  session, as its context: the last questions and answers (10 by default,
  <with|font-series|bold|Context> in the preferences of the chatbot). The
  context is the one of the document: it is there again when the document
  is opened again, and follows the changes made to it.

  <with|font-series|bold|Send the document as context>, in the menu of the
  model, also sends the whole document with each question (as <LaTeX>,
  without the sessions of chatbots, and at most 400000 characters).
  <name|Claude> keeps the document in its cache, so that the next questions
  about it cost less.

  The chatbot is told how to write its answers: as a <LaTeX> document which
  <TeXmacs> takes well (sections, lists, mathematics, <name|TikZ> or
  <name|SVG> pictures, nothing which only matters for printing). These
  instructions can be changed for each chatbot:
  <with|font-series|bold|Instructions> <with|font-series|bold|Edit> in its
  preferences opens them as a text file, whose changes are used once it is
  saved; <with|font-series|bold|Default> comes back to the instructions of
  <TeXmacs>. An answer which is not a <LaTeX> document (for instance in
  Markdown) is shown as plain text.

  <subsection*|Pictures>

  The pictures of an answer are shown: a <name|TikZ> picture
  (<verbatim|tikzpicture>, <verbatim|tikzcd>, <verbatim|circuitikz>)
  becomes an executable fold of the <name|TikZ> plug-in, made at once, with
  the libraries and packages which the answer asks for (on the desktop, it
  is made by <LaTeX>, which must be installed; in a web browser, by
  <name|TikZJax>, as soon as the picture is complete); an <name|SVG>
  picture becomes an image. An answer cut in the middle of a picture says
  so.

  Images in <name|PNG> or <name|JPEG> (a painting, an artistic rendition
  of an idea) come from the models which draw: those of <name|Gemini> whose
  name says <verbatim|image> (<verbatim|gemini-2.5-flash-image>...), and
  <verbatim|gpt-image-1> or <verbatim|dall-e-3> of <name|ChatGPT>, and
  those of <name|OpenRouter> whose name says <verbatim|image>; choose one as
  the model of the session (<with|font-series|bold|Update the list of
  models> lists them). Their images are put in the answer; the conversation
  sent again holds only a mention of them. The other models do not make
  images (<name|Claude> among them): they draw in <name|TikZ> or
  <name|SVG>.

  <subsection*|Reasoning, tokens and costs>

  The models which reason before they answer keep their reasoning folded
  before their answer (<with|font-series|bold|Show the reasoning> in the
  preferences removes it). How much they reason is chosen with
  <with|font-series|bold|Reasoning> in the preferences of <name|ChatGPT>,
  <name|Claude>, <name|Gemini>, <name|OpenRouter> and <name|Ollama>, or in
  the focus bar of a session, which changes the same setting:
  <with|font-series|bold|Default> lets the model decide,
  <with|font-series|bold|Low>, <with|font-series|bold|Medium> and
  <with|font-series|bold|High> ask for more and more (and cost more).

  Each answer is followed by its tokens: those of the question with its
  context (and the part read from the cache of the engine), those of the
  answer (and those of its reasoning), and its cost: the one which
  <name|OpenRouter> gives, else an estimate from the prices of the model
  (<name|ChatGPT>, <name|Claude>, <name|Gemini>, <name|Mistral>: those
  which <name|OpenRouter> lists for them, fetched once from
  <verbatim|openrouter.ai>), said <with|font-series|bold|about>.
  <name|Ollama> and <name|Albert> have no cost. The menu of the model gives
  the sum for the session (<with|font-series|bold|This session>), or the
  tokens of the answer of a fold, and the sum for the answers of the
  document. <with|font-series|bold|Show the tokens and the cost> in the
  preferences removes them.

  Each answer ends with a folded copy of the answer as it came, to see what
  the chatbot wrote (it is also what is sent back as the context);
  <with|font-series|bold|Show the answer as it came> in the preferences
  removes it. <with|font-series|bold|Insert answer> in the focus bar puts
  the answer at the cursor (else the last one) after the session, as
  paragraphs of the document, without its reasoning and its tokens.

  <subsection*|Executable folds>

  A chatbot can also answer in an executable fold
  (<menu|Insert|Fold|Executable|AI>), whose answer becomes part of the
  document. A fold asks its question alone, without the questions and
  answers around it, so that it always asks the same; it has its model, its
  reasoning and <with|font-series|bold|Send the document as context> in its
  focus bar, as a session. Its answer is the answer alone (its tokens are
  said on the status bar). It is kept: unfolding the fold again, or
  unfolding all the folds of the document, shows it without asking again;
  <key|Return> in its question, or <with|font-series|bold|Ask again> in its
  focus bar, asks again. When its question was changed since its answer,
  its focus bar says <with|font-series|bold|Question changed>.

  <subsection*|Errors>

  An error of the chatbot is shown as the answer, starting with
  <with|font-series|bold|Error>. When an engine says that it has too many
  requests, or that it is overloaded, the question is asked again after a
  few seconds (as long as the engine says, if it says it, up to a minute),
  three times at most; the session says so while it waits. An exhausted
  quota or credit is not asked again.

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
