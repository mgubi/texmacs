<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Writing, saving, printing and loading a simple text>

  <paragraph*|Typing>

  Once you have given your document a name, you may start typing. Just type
  your text as on a typewriter: <TeXmacs> automatically breaks the lines
  when they become too long, and also justifies them, that is, it adjusts
  the spaces between the words so that all lines have the same width. You
  only have to press <key|return> in order to start a new paragraph. The
  following text is a simple exercise:

  <\big-figure>
    <\big-envbox>
      Hey you there!

      Would you be so kind to enroll the gnus and gnats around you to try
      <TeXmacs> once?<text-cursor>
    </big-envbox>
  <|big-figure>
    A simple text typed with <TeXmacs> (<verbatim|examples/test.tm>).
  </big-figure>

  If you make a mistake, <key*|backspace> removes the character before the
  cursor, and <menu|Edit|Undo> (<shortcut|(undo 0)>) undoes your last
  changes. In case your text contains characters which are not on your
  keyboard, such as accented letters, you may type them using the methods
  of your system; we will see later how to type mathematical symbols.

  <paragraph*|Saving>

  When you are done with typing, save your document with <menu|File|Save>,
  with the <menu|Save> entry of the <icon|tm_save.png> icon menu, or with <shortcut|(save-buffer)>. A
  message on the status bar confirms that the document has been saved.

  For your safety, <TeXmacs> also <em|autosaves> your document every two
  minutes, in a separate backup file. If you forget to save your document
  before closing <TeXmacs>, or if your computer stops for some reason, then
  <TeXmacs> will propose you to recover the unsaved changes the next time
  you open the document.

  <paragraph*|Printing and exporting>

  You will usually want to print your document, or to send it to people who
  do not use <TeXmacs>. You print your document with <menu|File|Print|Print buffer>,
  using the printers of your system. You may also turn it into a PDF file,
  which can be read on any computer, with <menu|File|Export|Pdf>. The size
  of the paper (A4 in Europe, Letter in the U.S.A.) and the margins are
  set in <menu|Document|Page>.

  <paragraph*|Loading>

  In order to retrieve your text after closing <TeXmacs>, use
  <menu|File|Load>, the <menu|Load> entry of the <icon|tm_open.png> icon
  menu, or the shortcut
  <shortcut|(open-document)>. A file chooser appears, in which you select
  the file <verbatim|test.tm> and confirm, or simply double-click on it.
  Your recent documents are also listed in <menu|File|Recent>.

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
