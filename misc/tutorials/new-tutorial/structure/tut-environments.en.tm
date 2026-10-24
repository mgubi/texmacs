<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Theorems, proofs and lists>

  Mathematical texts are made of definitions, theorems, proofs, remarks,
  and so on. In <TeXmacs>, each of them is an <em|environment>, found in
  <menu|Insert|Enunciation>: <TeXmacs> numbers them and displays them
  according to the style of the document. Let us type the following text,
  which is also available as <verbatim|examples/ramsey.tm>:

  <\big-envbox>
    <\theorem>
      Among six people, there are always three people who mutually know
      each other or three people who mutually do not know each other.
    </theorem>

    <\proof>
      Let <math|P> be an arbitrary person among the six. Then either one of
      the following cases holds:

      <\description>
        <item*|Case 1><math|P> knows at least three other persons, which
        may be called <math|Q>, <math|R> and <math|S>.

        <item*|Case 2><text-cursor>
      </description>
    </proof>
  </big-envbox>

  <paragraph*|Creating the environments>

  Select <menu|Insert|Enunciation|Theorem> and type the statement of the
  theorem; the word \PTheorem\Q and its number are added by <TeXmacs>. Then
  leave the theorem by moving the cursor beyond its end with <key*|right>,
  or with <shortcut|(structured-exit-right)>, and select
  <menu|Insert|Enunciation|Proof>. Inside the proof, a description list is
  started with <menu|Insert|Description|Default>: each time you press
  <key|return>, a new item is created, whose label you type before its
  text.

  <paragraph*|The context of the cursor>

  At the position of the cursor in the example above, the right part of the
  status bar shows <verbatim|proof description item*>: the cursor is in an
  item of a description, inside a proof. This <em|context> also determines
  the contents of the <menu|Focus> menu and of the focus toolbar, which
  offer the actions which make sense for the innermost environment.

  <paragraph*|Changing an environment>

  Since <TeXmacs> knows that the statement is a theorem, it is easy to
  change your mind. Put the cursor inside the theorem and press
  <shortcut|(variant-circulate (focus-tree) #t)>, or use the focus toolbar:
  the theorem becomes a proposition, a lemma, a corollary... while its text
  stays the same. In the same way, <shortcut|(numbered-toggle
  (focus-tree))> removes or restores the number of the theorem.

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
