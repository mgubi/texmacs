<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing program code>

  Program code, in a document (<menu|Insert|Program|Block of code>) or in a source
  file opened in <TeXmacs>, is read as a syntax tree while you type, for C,
  C++, Python, Scheme, Java, <name|JavaScript>, <name|TypeScript>, R, Julia,
  Shell, Go, Rust, Lua, Lean, Haskell, <name|OCaml>, HTML, CSS, TOML, YAML,
  JSON and <LaTeX>. The editor uses this tree for the colors, the
  indentation and the editing commands below. The other languages are
  colored and indented as before.

  <paragraph*|Colors>

  Keywords, types, functions, strings, comments, numbers and operators get
  the colors of the program languages (see the preferences of the colors of
  each language). The scripts and styles of HTML are colored as
  <name|JavaScript> and CSS, as is the code in some strings (a template
  <verbatim|css`...`> of <name|JavaScript>). A bracket without its partner
  is shown in the color of errors.

  <paragraph*|Brackets, quotes and closing keywords>

  Brackets and quotes stay balanced as you type: an opening bracket comes
  with its closing one, a closing bracket is typed over the one which is
  already there, and deleting a bracket of an empty pair deletes both; on a
  bracket of a pair which has content, <key|backspace> or <key|delete>
  first selects the pair, and a second key deletes it. In strings and
  comments, brackets are ordinary characters. When a line opens a block,
  <key|return> adds the keyword which closes it on the next line
  (<verbatim|end>, <verbatim|fi>, <verbatim|done>...), or the
  <verbatim|\\end> of an environment of <LaTeX>; in HTML, the end tag comes
  with its start tag.

  <paragraph*|Indentation>

  <key|return> and <shortcut|(program-indent #f)> indent a line by the
  structure of the code: inside a block, after an open bracket, aligned with
  the arguments of a call, and back to the level of its block for a line
  which closes it (<verbatim|}>, <verbatim|end>, <verbatim|else>...).

  <paragraph*|Moving and selecting>

  <shortcut|(kbd-select-enlarge)> selects the smallest part of the code
  around the cursor, then the part around it, and so on. The keys of
  structured movement go from part to part:
  <shortcut|(kbd-select-if-active traverse-right)> and
  <shortcut|(kbd-select-if-active traverse-left)> to the next and previous
  part, <shortcut|(kbd-select-if-active traverse-up)> to the part around,
  <shortcut|(kbd-select-if-active traverse-down)> into it, and
  <shortcut|(kbd-select-if-active traverse-next)> and
  <shortcut|(kbd-select-if-active traverse-previous)> to the next and
  previous definition. <menu|Focus|Definitions> lists the definitions of
  the code, <menu|Focus|Go to definition> goes to the definition of the
  name at the cursor, and <menu|Focus|Select definition> selects the
  definition around the cursor.

  <paragraph*|Structural edits>

  <menu|Focus|Structure> edits whole parts of the code, which keeps it well
  formed: <menu|Delete node> (also <shortcut|(structured-remove-right)>,
  and <shortcut|(structured-remove-left)> for the part before the cursor),
  <menu|Swap with next> and <menu|Swap with previous>, <menu|Raise node>
  (replace the part around by the one at the cursor), <menu|Remove
  brackets> (also <shortcut|(program-remove-structure)>), <menu|Wrap in
  parentheses> and the like, and <menu|Comment or uncomment node>.
  <menu|Focus|Toggle comment> comments or uncomments the lines of the
  selection.

  <paragraph*|Folds>

  <shortcut|(program-fold)> folds the block at the cursor: its lines are
  hidden, but the first one, which ends with <verbatim|...>, and the
  closing line of the block. <shortcut|(program-unfold)> unfolds it.
  <menu|Focus|Folds> also folds all the definitions, and unfolds all. Folds
  change what is shown, not the document: the hidden lines are saved,
  printed, copied and searched as usual, and moving the cursor into a fold
  opens it.

  <paragraph*|Preferences>

  In <menu|Edit|Preferences>, tab <menu|Other>, <menu|Balanced brackets in
  code with a syntax tree> turns the balancing of brackets and the closing
  keywords on or off, and <menu|Program coloring by syntax trees> the use of
  the syntax trees for the colors (taken into account at the next start of
  <TeXmacs>).

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
