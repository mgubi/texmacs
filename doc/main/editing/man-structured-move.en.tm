<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Structured cursor movement>

  <TeXmacs> implements the three main mechanisms for <em|structured cursor
  movement>:

  <\enumerate>
    <item>Traversal of the entire structure of the document.

    <item>Traversal of tags which are similar to the innermost tag.

    <item>Movements inside the innermost tag.
  </enumerate>

  Most keyboard shortcuts for structured cursor movements can be used in
  combination with the<nbsp><prefix|S->-key so as to similtaneously
  select text while moving around.

  <todo|customizing the behaviour>

  <paragraph|Structured traversal of the document>

  The <shortcut|(traverse-left)>, <shortcut|(traverse-right)>, <shortcut|(traverse-up)> and
  <shortcut|(traverse-down)> keys are used for the structured traversal of the entire
  document. Inside plain text, <shortcut|(traverse-left)> and <shortcut|(traverse-right)>
  allow you to move in a word-by-word manner, while <shortcut|(traverse-up)> and
  <shortcut|(traverse-down)> correspond to paragraph-by-paragraph motion.

  In the presence of other markup, the <shortcut|(traverse-left)> and
  <shortcut|(traverse-right)> keys allow you to visit all accessible cursor positions
  of the document, except that we keep moving in a word-by-word manner inside
  plain text. The behaviour of the <shortcut|(traverse-up)> and <shortcut|(traverse-down)>
  keys is more context-dependent. Inside matrices, they typically allow you
  to move one row up or down.

  <paragraph|Traversal of tags which are similar to the innermost tag>

  This type of cursor movement allows you to quickly visit all other tags in
  the document which are <em|similar> to the innermost tag. The
  <shortcut|(traverse-previous)> and <shortcut|(traverse-next)> keys allow you move to the
  previous or next similar tags, whereas <shortcut|(traverse-first)> and
  <shortcut|(traverse-last)> directly jump to the first or last similar tags.

  For instance, when you are inside a section title, you may move to the
  previous sectional title (which could also be the title of a subsection or
  a chapter, for instance) using <shortcut|(traverse-previous)>. Notice that you may
  use <key|C-�> to jump to the previous section title.

  <paragraph|Movements inside the innermost tag>

  It is also possible to quickly move inside the innermost tag without
  quitting it, using <key|H-left>, <key|H-right>, etc. We recall
  that the <prefix|H-> prefix corresponds to the <key|hyper>-key, which does not
  exist on most keyboards. In order to compose <prefix|H->-based shortcuts, you
  may either <hyper-link|configure your keyboard|../config/man-config-kbd-modkeys.en.tm>
  so as to map another key to <prefix|H->, or simulate the hyper key using
  <key|escape escape escape> or <key|C-escape>.

  In general, <key|H-left>, <key|H-right>, <key|H-home> and
  <key|H-end> provide a way to move to the previous, next, first or
  last argument of the innermost tag. Furthermore, the shortcuts <key|H-(>
  and <key|H-)> may be used to exit the innermost tag on the left or on the
  right.

  This default behaviour may be overridden in special contexts. For instance,
  inside tables or trees, they rather correspond to cell-by-cell or
  node-by-node cursor movement. In addition, these cases associate vertical
  cursor movements to <key|H-up>, <key|H-down>,
  <key|H-pageup> and<nbsp><key|H-pagedown>.

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>