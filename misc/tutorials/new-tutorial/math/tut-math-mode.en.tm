<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Entering and leaving mathematical mode>

  <TeXmacs> distinguishes between ordinary text and mathematical formulas.
  Inside formulas, letters are typeset in italic, the spacing around the
  symbols follows the mathematical conventions, and the keyboard behaves
  differently, so that symbols can be typed quickly. In order to type a
  formula, you therefore first have to enter <em|mathematical mode>.

  <paragraph*|Inline formulas>

  Assume that you have typed \PHere comes a formula:\Q and that you now
  want to type a small formula inside the same line. Select
  <menu|Insert|Mathematics|Inline formula>, or simply press the
  <key|$>-key:

  <\big-envbox>
    Here comes a formula: <math|<small-focus|<math-cursor>>>
  </big-envbox>

  Several things happened: the cursor became purple, an empty formula
  surrounded by a cyan box was inserted, the left part of the status bar now
  indicates <verbatim|math> instead of <verbatim|text>, and the mode toolbar
  shows icons with mathematical symbols. Whatever you type now goes into the
  formula, until you leave it. The simplest way to leave the formula is to
  move the cursor out of it with <key*|right> once you are at its end.

  <paragraph*|Displayed formulas>

  Larger formulas are better displayed on a line of their own, like

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>.
  </equation*>

  Such <em|displayed formulas> are inserted with
  <menu|Insert|Mathematics|Displayed formula> or
  <shortcut|(make-equation*)>. You may give a displayed formula a number with
  <shortcut|(numbered-toggle (focus-tree))>, and turn an inline formula into
  a displayed one (or back) with <shortcut|(variant-circulate (focus-tree)
  #t)>.

  In the next sections, we will see how to type the contents of formulas.

  <tmdoc-copyright|1998\U2026|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
