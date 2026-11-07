<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The cursor and the structure>

  Consider the following sentence, in which the words \Pbold blue\Q have
  been typed in bold (with <shortcut|(toggle-bold)>) and then in blue (with
  <menu|Format|Color>):

  <\quote-env>
    Some <with|font-series|bold|<with|color|blue|bold blue>> text.
  </quote-env>

  The words \Pbold blue\Q are inside two pieces of markup: one for the bold
  font, and one for the blue color. Where does a letter go, when you type it
  next to these words? <TeXmacs> answers this question by giving the cursor
  several positions at the border of the markup, which look the same on the
  screen but are different in the document.

  Put the cursor after \PSome \Q, and press <key*|right> several times while
  you watch the status bar and the boxes around the cursor:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Cursor>|<cell|Position>>|<row|<cell|Some
  <text-cursor><with|font-series|bold|<with|color|blue|bold blue>>
  text.>|<cell|outside the markup>>|<row|<cell|Some
  <with|font-series|bold|<small-focus|<text-cursor><with|color|blue|bold
  blue>>> text.>|<cell|inside the bold font>>|<row|<cell|Some
  <with|font-series|bold|<small-envbox|<small-focus|<with|color|blue|<text-cursor>bold
  blue>>>> text.>|<cell|inside the bold font and the blue
  color>>|<row|<cell|Some <with|font-series|bold|<with|color|blue|b<text-cursor>old
  blue>> text.>|<cell|after the letter <verbatim|b>>>>>>>
    Moving the cursor into \Pbold blue\Q.
  </big-table>

  The first presses of <key*|right> do not move the cursor on the screen,
  but move it inside the markup, one level at a time. When you type a letter
  at one of these positions, it gets the font and the color of that
  position: in the first row an ordinary black letter, in the second one a
  bold black letter, and in the third one a bold blue letter. The same
  happens at the end of \Pbold blue\Q.

  As we have seen, the innermost piece of markup around the cursor is
  surrounded by a cyan box, and the outer ones by light grey boxes; the
  right part of the status bar lists them all. With a little practice,
  these boxes allow you to see at a glance where your next letter will go.

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
