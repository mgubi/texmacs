<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Understanding the <TeXmacs> window>

  The following figure shows the main parts of a <TeXmacs> window. We will
  frequently refer to them in this tutorial.

  <\big-figure>
    <tabular|<tformat|<twith|table-width|0.9par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-hyphen|n>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|-1|-1|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-lsep|1em>|<cwith|1|-1|1|-1|cell-valign|c>|<cwith|1|1|1|1|cell-background|pastel
    blue>|<cwith|1|1|1|1|cell-halign|c>|<cwith|2|5|1|1|cell-background|light
    grey>|<cwith|2|5|1|1|cell-bborder|0.5ln>|<cwith|6|6|1|1|cell-height|6em>|<cwith|6|6|1|1|cell-vmode|exact>|<cwith|6|6|1|1|cell-valign|t>|<cwith|6|6|1|1|cell-tsep|1em>|<cwith|6|6|1|1|cell-lsep|1.5em>|<cwith|7|7|1|1|cell-background|light
    grey>|<cwith|7|7|1|1|cell-tborder|0.5ln>|<table|<row|<cell|<with|font-family|ss|test.tm>>|<cell|<em|title
    bar>>>|<row|<cell|<with|font-size|0.84|<menu-item|File><space|0.5em><menu-item|Edit><space|0.5em><menu-item|Insert><space|0.5em><menu-item|Focus><space|0.5em><menu-item|Format><space|0.5em><menu-item|Document><space|0.5em><menu-item|View><space|0.5em><menu-item|Go><space|0.5em><menu-item|Tools><space|0.5em><menu-item|Help>>>|<cell|<em|menu
    bar>>>|<row|<cell|<icon|tm_new.png> <icon|tm_open.png>
    <icon|tm_save.png> <icon|tm_print.png> \ <icon|tm_copy.png>
    <icon|tm_undo.png>>|<cell|<em|main toolbar>>>|<row|<cell|<with|font-family|ss|<em|tools
    for the current mode (text, mathematics...)>>>|<cell|<em|mode
    toolbar>>>|<row|<cell|<with|font-family|ss|<em|tools for the tag at the
    cursor>>>|<cell|<em|focus toolbar>>>|<row|<cell|<text-cursor>Hello>|<cell|<em|canvas>
    with the <em|cursor>>>|<row|<cell|<with|font-family|ss|text roman 10<htab|1em>start>>|<cell|<em|status bar>>>>>>
  <|big-figure>
    The main parts of the <TeXmacs> window.
  </big-figure>

  The <em|title bar> displays the name of your document. As long as you did
  not give your document a name, it is called <verbatim|No name [1]> (or
  <verbatim|No name [2]> for the next new document, and so on). At the
  sides of the title bar, the <em|window decorations> allow you to minimize,
  maximize or close the window. They are drawn by your operating system, not
  by <TeXmacs>, so that their appearance depends on your system.

  Your text itself is displayed in the <em|canvas>, which shows your
  document as it will be printed. If the text does not fit into the window,
  then you may use the <em|scrollbars>, the wheel of your mouse or your
  touchpad in order to scroll through it. The <em|cursor> (also called the
  <em|caret>) indicates the position in the text where you are typing; it is
  drawn in red in text and in purple in mathematical formulas. You may move
  it with the arrow keys or by clicking with the mouse.

  The <em|menus> on the <em|menu bar> give access to all the features of
  <TeXmacs>. For instance, the <menu|File> menu allows you to create, load,
  save, export and print documents, the <menu|Insert> menu to insert new
  material such as sections, lists, formulas, tables and pictures, the
  <menu|Format> menu to change the font or the color of your text, and the
  <menu|Document> menu to change the global properties of your document,
  such as its style, its language or its page size. On <name|macOS>, the
  menu bar is located at the top of the screen instead of the top of the
  window.

  The <em|icons> on the <em|toolbars> give a faster access to the most
  frequent actions. The main toolbar hardly changes; the <em|mode
  toolbar> changes when you go from text to mathematics, and the <em|focus
  toolbar> changes with the piece of markup in which the cursor is located,
  as we will see in the chapter on structured text. Many icons are actually
  <em|icon menus>: a menu appears when you click on them. You may hide or
  show the toolbars using the <menu|View> menu.

  The <em|status bar> at the bottom of the window displays relevant
  information about your document. Its left part tells you whether you are
  typing text or mathematics, and which font and size are used at the
  position of the cursor. Its
  right part describes the position of the cursor in the document: here,
  the cursor is at the <verbatim|start> of a paragraph; later on, it will
  show you which character you just typed and in which pieces of markup the
  cursor is. The status bar is also used to display messages, for instance
  when a document has been saved.

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
