<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Greek letters and mathematical symbols>

  Inside a formula, you obtain most mathematical symbols with short
  sequences of keys. While you type such a sequence, the status bar shows
  the keys typed so far and the symbol they produce.

  <paragraph*|Greek letters>

  A Greek letter is obtained by typing the corresponding Latin letter
  followed by <key|var>. For instance, in order to type the formula
  <math|\<lambda\>*ln x>, proceed as follows:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Keys>|<cell|Formula>|<cell|Remark>>|<row|<cell|<key|l>>|<cell|<math|l<math-cursor>>>|<cell|an
  ordinary letter <math|l>>>|<row|<cell|<key|var>>|<cell|<math|\<lambda\><math-cursor>>>|<cell|the
  <math|l> becomes a <math|\<lambda\>>>>|<row|<cell|<key|*>>|<cell|<math|\<lambda\>*<math-cursor>>>|<cell|an
  invisible multiplication>>|<row|<cell|<key|l n>>|<cell|<math|\<lambda\>*ln<math-cursor>>>|<cell|the
  name of the logarithm>>|<row|<cell|<key|space>>|<cell|<math|\<lambda\>*ln
  <math-cursor>>>|<cell|a function application>>|<row|<cell|<key|x>>|<cell|<math|\<lambda\>*ln
  x<math-cursor>>>|<cell|the argument>>>>>>
    Typing the formula <math|\<lambda\>*ln x>.
  </big-table>

  Notice that the multiplication typed with <key|*> and the function
  application typed with <key|space> are invisible, but they tell <TeXmacs>
  what your formula means. Pressing <key|var> several times gives the
  variants of the letter: <key|e var> yields <math|\<varepsilon\>>,
  <key|e var var> yields the constant <math|\<mathe\>> and <key|e var var
  var> yields <math|\<epsilon\>>. Capital Greek letters are
  obtained in the same way: <key|G var> yields <math|\<Gamma\>>.

  <paragraph*|Arrows and other symbols>

  Most symbols are obtained by \Pdrawing\Q them with the keyboard. For
  instance, <key|- \<gtr\>> yields <math|\<rightarrow\>>, <key|= \<gtr\>>
  yields <math|\<Rightarrow\>>, <key|\<less\> => yields
  <math|\<leqslant\>> and <key|= /> yields <math|\<neq\>>. In general,
  <key|/> after a symbol <em|negates> it, and <key|@> before a symbol puts it
  into a circle: <key|@ +> yields <math|\<oplus\>> and <key|@ x> yields
  <math|\<otimes\>>.

  <paragraph*|Variants>

  Just as for the Greek letters, the <key|var> key cycles through the
  variants of a symbol. For instance, starting with <key|\<less\>> and
  pressing <key|var> several times, you successively obtain:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Keys>|<cell|Symbol>>|<row|<cell|<key|\<less\>>>|<cell|<math|\<less\>>>>|<row|<cell|<key|\<less\>
  var>>|<cell|<math|\<in\>>>>|<row|<cell|<key|\<less\> var
  var>>|<cell|<math|\<subset\>>>>|<row|<cell|<key|\<less\> var var
  var>>|<cell|<math|\<prec\>>>>|<row|<cell|<key|\<less\> var var var
  =>>|<cell|<math|\<preccurlyeq\>>>>|<row|<cell|<key|\<less\> var var var =
  var>>|<cell|<math|\<preceq\>>>>|<row|<cell|<key|\<less\> var var var = var
  />>|<cell|<math|\<npreceq\>>>>>>>>
    Variants of the symbol <math|\<less\>>.
  </big-table>

  If you went too far, <key|S-var> goes back to the previous variant.

  <paragraph*|Symbols by name>

  If you know the <LaTeX> name of a symbol, you may also type it after the
  <key|\\> key. For instance, typing <key|\\ h e a r t s u i t> and then
  <key|return> yields <math|\<heartsuit\>>. While you type the name, the
  status bar tells you which symbol will be inserted when you press
  <key|return>.

  <paragraph*|Symbol menus>

  Finally, all symbols are also available in the icon menus of the mode
  toolbar, and in <menu|Insert|Symbol>. When you leave the mouse pointer on
  a symbol in these menus, a tooltip shows the keyboard shortcut for it, so
  that you can learn the shortcuts of the symbols which you use often.

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
