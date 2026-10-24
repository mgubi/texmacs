<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Emphasizing text>

  Assume that you want to write the sentence

  <\quote-env>
    A <em|gnu> is a hairy animal, which is not a clone of a unix.
  </quote-env>

  in which the word \Pgnu\Q is emphasized. Rather than switching to an
  italic font, you tell <TeXmacs> that the word is <em|emphasized>, by
  putting it inside the <markup|em> tag. There are several ways to do this.

  <paragraph*|Using the menus>

  Type \PA \Q and select <menu|Insert|Content tag|Emphasize>. An empty
  emphasized region appears, surrounded by a cyan box, and the status bar
  indicates that the cursor is inside <verbatim|em>:

  <\big-envbox>
    A <em|<small-focus|<text-cursor>>>
  </big-envbox>

  Type the word \Pgnu\Q: it appears in italic, since it is inside the
  emphasized region.

  <\big-envbox>
    A <em|<small-focus|gnu<text-cursor>>>
  </big-envbox>

  In order to continue with ordinary text, press <key*|right>: the cursor
  leaves the emphasized region (the cyan box disappears) without moving on
  the screen. Now type the rest of the sentence.

  <paragraph*|Using a hybrid command>

  Instead of using the menu, you may also type the name of the tag after the
  <key|\\> key. Type <key|\\ e m>: the status bar now says that pressing
  <key|return> will emphasize the text. Press <key|return>, and you are in
  the same situation as above. This works for every tag, provided you know
  its name, and also for the <LaTeX> commands which have an equivalent in
  <TeXmacs>; for instance <key|\\ s e c t i o n> creates a section title.

  <paragraph*|Using the selection>

  Finally, if the text has already been typed, select the word \Pgnu\Q with
  the mouse and then choose <menu|Insert|Content tag|Emphasize>: the
  selected text is put inside the tag.

  <paragraph*|Why not simply use italic?>

  Emphasized text usually looks like italic text, but it is the style which
  decides. For instance, inside a theorem, which is already in italic,
  emphasized text is typeset upright, so that it remains visible:

  <\quote-env>
    <\theorem>
      A <em|gnu> is a hairy animal.
    </theorem>
  </quote-env>

  In the same way, the <markup|strong> tag
  (<menu|Insert|Content tag|Strong>) marks <strong|important> text, and the
  <markup|name> tag (<menu|Insert|Content tag|Name>) marks names of persons
  like <name|Joris>. If you later decide that important text should be
  displayed in red rather than in bold, you only have to change the style,
  and not the many places where <markup|strong> was used.

  The example of this section is in <verbatim|examples/hairy.tm>.

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
