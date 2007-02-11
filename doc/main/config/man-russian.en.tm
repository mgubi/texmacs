<TeXmacs|1.0.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Notes for users of Cyrillic languages>

  In order to type Russian (and similarly for other Cyrillic languages) text,
  you have several options:

  <\itemize>
    <item>Select Russian as your default language in
    <menu|Edit|Preferences|Language|Russian>. If \ <TeXmacs> starts with
    Russian menus, then this is done automatically if the Russian locale is
    set.

    <item>Select Russian for an entire document using
    <menu|Document|Language|Russian>.

    <item>Select Russian for a portion of text in another document using
    <menu|Format|Language|Russian>.
  </itemize>

  If your X server uses the xkb extension, and is instructed to switch
  between the Latin and Russian keyboard modes, you need not do anything
  special. Just switch your keyboard to the Russian mode, and go ahead. All
  the software needed for this is included in modern Linux distributions, and
  the xkb extension is enabled by default in
  <with|font-family|tt|XF86Config>. With the xkb extension, keysyms are
  2-byte, and Russian letters are at 0x6??. The keyboard is configured by
  <with|font-family|tt|setxkbmap>. When X starts, it issues this command with
  the system-wide <with|font-family|tt|Xkbmap> file (usually living in
  <with|font-family|tt|/etc/X11/xinit>), if it exists; and then with the
  user's <with|font-family|tt|~/.Xkbmap>, if it exists. A typical
  <with|font-family|tt|~/.Xkbmap> may look like

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  This means that the keyboard mode is toggled by <key|<localize|l-shift>
  <localize|r-shift>>. Other popular choices are <key|<key-control>
  <key-shift>> or <key|<key-control> <key-alternate>>, see
  <with|font-family|tt|/usr/X11R6/lib/X11/xkb/> for more details. This is the
  preferred keyboard setup for modern Linux systems, if you plan to use
  Russian often.

  In older Linux systems, the xkb extension is often disabled. Keysyms are
  1-byte, and are configured by <with|font-family|tt|xmodmap>. When X starts,
  it issues this command with the system-wide <with|font-family|tt|Xmodmap>
  (usually living in <with|font-family|tt|/etc/X11/xinit>), if it exists; and
  then with the user's <with|font-family|tt|~/.Xmodmap>, if it exists. You
  can configure the mode toggling key combination, and use a 1-byte Russian
  encoding (such as koi8-r) in the Russian mode. It is easier to download the
  package <with|font-family|tt|xruskb>, and just run

  <verbatim| \ \ \ xrus jcuken-koi8>

  at the beginning of your X session. This sets the layout jcuken (see below)
  and the encoding koi8-r for your keyboard in the Russian mode. If you use
  such keyboard setup, you should select Options
  <with|mode|math|\<rightarrow\>> international keyboard
  <with|mode|math|\<rightarrow\>> russian <with|mode|math|\<rightarrow\>>
  koi8-r.

  It is also possible to use the Windows cp1251 encoding instead of koi8-r,
  though this is rarely done in UNIX. If you do use <with|font-family|tt|xrus
  jcuken-cp1251>, select cp1251 instead of koi8-r.

  All the methods described above require some special actions to ``russify''
  the keyboard. This is not difficult, see the Cyrillic-HOWTO or, better, its
  updated version

  <verbatim|http://www.inp.nsk.su/<with|font-family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Also, all of the above methods globally affect all X applications: text
  editors (emacs, nedit, kedit...), xterms, <TeXmacs> etc.

  If you need to type Russian only once, or very rarely, a proper keyboard
  setup may be more trouble than it's worth. For the benefit of such
  occasional users, <TeXmacs> has methods of Russian input which require no
  preliminary work. Naturally, such methods affect only <TeXmacs>, and no
  other application.

  The simplest way to type some Russian on the standard US-style keyboard
  with no software setup is to select <menu|Edit|Preferences|Keyboard|Cyrillic
  input method|translit>. Then, typing a Latin letter will produce ``the most
  similar'' Russian one. In order to get some Russian letters, you have to
  type 2- or 3-letter combinations:<vspace|0.5fn>

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Shorthand>|<cell|for>|<cell|Shorthand(s)>|<cell|for>>|<row|<cell|<kbd-text|"
  e>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<kbd-text|"
  E>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|j
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|J
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|e
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|E
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|�>>>>>>|Typing Cyrillic text
  on a Roman keyboard.>

  If you want to get, e.g., ``<with|language|russian|font|cyrillic|��>'', and
  not ``<with|language|russian|font|cyrillic|�>'', you have to type <key|s /
  h>. Of course, the choice of ``optimal'' mapping of Latin letters to
  Russian ones in not unique. You can investigate the mapping supplied with
  <TeXmacs> and, if you don't like something, override it in your
  <with|font-family|tt|~/.TeXmacs/progs/my-init-texmacs.scm>.

  If you select jcuken instead of translit, you get the ``official'' Russian
  typewriter layout. It is so called because the keys ``qwerty'' produce
  ``<with|language|russian|<with|font|cyrillic|������>''. This input method
  is most useful> when you have a Russian-made keyboard, which has additional
  Russian letters written on the key caps in red, in the jcuken layout (a
  similar effect can be achieved by attaching transparent stickers with red
  Russian letters to caps of a US-style keyboard). It is also useful if you
  are an experienced Russian typist, and your fingers remember this layout.

  Those who have no Russian letters indicated at the key caps often prefer
  the yawerty layout, where the keys ``qwerty'' produce
  ``<with|language|russian|font|cyrillic|������''. Each Latin letter is
  mapped into a ``similar'' Russian one; some additional Russian letters are
  produced by ><key|<key-shift>>-digits. <TeXmacs> comes with a slightly
  modified yawerty layout, because it does not redefine the keys <key|$>,
  <key|�>, <key|<with|mode|math|\<backslash\>>>, which are important for
  <TeXmacs>, are not redefined. The corresponding Russian letters are
  produced by some <key|<key-shift>>-digit combinations instead.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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