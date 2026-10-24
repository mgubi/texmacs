<TeXmacs|2.1.5>

<style|<tuple|article|doc>>

<\body>
  <\hide-preamble>
    <assign|tcmd|<macro|x|<move|<verbatim|<arg|x>>|0fn|0fn>>>
  </hide-preamble>

  <doc-data|<doc-title|<TeXmacs> in 60 minutes>|<doc-running-title|<TeXmacs>
  in 60 minutes>|<doc-author|<author-data|<author-name|Andreas
  Seidl>>>|<doc-misc|Updated for <TeXmacs> 2.1.5 in 2026.>>

  This document will enable you to quickly understand what you can do with
  <TeXmacs>, how you can install it, and how to get started using it. For the
  official source of information see <hlink|www.texmacs.org|https://www.texmacs.org>.

  <\description>
    <item*|Step 1>Features and current issues
    (section<nbsp><reference|sec-features>).

    <item*|Step 2>Download and install (section<nbsp><reference|sec-install>).

    <item*|Step 3>Hands-on tutorial (section<nbsp><reference|sec-tutorial>).
  </description>

  <section|Features and current issues>

  <label|sec-features>To know about the features and issues of a software
  plays a key role in deciding whether it is sensible to start working with
  it: are the bugs small and the workarounds painless enough to justify the
  benefits?

  <subsection|Features>

  The following is an incomplete list of features, just to give an idea of
  what you can do with <TeXmacs>.

  <\itemize>
    <item><strong|Creating and editing structured text.>

    <\itemize>
      <item>The usual constructs from <LaTeX>: title, author, abstract,
      sections, are available.

      <item>The same holds for environments like tables and figures, as well
      as for mathematical environments like definition, theorem, proof.

      <item>Mathematical content can be input as known from <LaTeX> (type
      <tcmd|\\alpha>, <tcmd|\\frac>, <tcmd|\\sum>...), with keyboard
      shortcuts, with the menus and with the mathematical toolbar. What you
      see on the screen is the typeset formula.

      <item>Inclusion of images (<name|Pdf>, <name|Svg>, <name|Png>,
      <name|Jpeg>..., and <name|Eps> or <name|PostScript> when
      <name|Ghostscript> is installed), and drawing with the integrated
      graphics editor (<menu|Insert|Image|Draw image>).

      <item>Labels for sections, equations, theorems, tables and figures, and
      references to them, also to their page numbers.

      <item>Citations and bibliographies based on <BibTeX> files (an
      integrated <BibTeX> engine is included).

      <item>Automatically generated (and updated) table of contents,
      bibliography, index, glossary.

      <item>Laptop presentations (style <tmstyle|beamer>), posters, letters,
      books.
    </itemize>

    <item><strong|Importing and exporting> (<menu|File|Import> and
    <menu|File|Export>).

    <\itemize>
      <item><LaTeX> (import and export, also import of <LaTeX> classes)

      <item><name|Html>

      <item><TeXmacs> <name|Xml>

      <item><BibTeX>

      <item><TeXmacs> Scheme, verbatim text and source code

      <item><name|Pdf>: export, also with the <TeXmacs> document embedded in
      the <name|Pdf>; such a <name|Pdf> can be imported back
      (<menu|File|Import|Pdf with embedded document>)

      <item><name|PostScript> (export only); a selection can also be exported
      as an image.
    </itemize>

    <item><strong|Plug-ins: interfaces to external programs.> With
    <menu|Insert|Session> you open an interactive session inside your
    document, whose results are typeset by <TeXmacs>. There are plug-ins for
    computer algebra systems (<name|Maxima>, <name|Giac>, <name|FriCAS>,
    <name|Reduce>, <name|Sage>, <name|Mathematica>, <name|Maple>,
    <name|Pari>, <name|Macaulay2>...), numerical systems (<name|Octave>,
    <name|Scilab>, <name|R>...), programming languages (<name|Python>,
    <name|Scheme>, the <name|Shell>...) and graphics tools (<name|Gnuplot>,
    <name|Asymptote>, <name|Graphviz>, <name|TikZ>...). The program itself
    has to be installed on your computer (<name|TikZ> needs a <LaTeX>
    installation): <TeXmacs> only shows the plug-ins which it finds.

    <item><strong|Customization.> Macros, style files and the <name|Scheme>
    extension language make it possible to adapt <TeXmacs> to your needs.
  </itemize>

  <subsection|Current issues>

  <TeXmacs> runs natively on <name|Windows>, <name|macOS> and <name|Linux>:
  the issues of the <name|Cygwin> versions of 2004 (X server, keyboard,
  clipboard, image converters) are gone. The remaining limitations are mostly
  the following:

  <\itemize>
    <item>The plug-ins need the corresponding programs: install for instance
    <name|Maxima> or <name|Gnuplot> before using them from <TeXmacs>.

    <item>The conversions to and from <LaTeX> and <name|Html> work well for
    usual documents, but complex <LaTeX> macros or packages may need some
    manual work after an import.

    <item>Some image formats need an external program to be displayed:
    <name|Ghostscript> for <name|Eps> and <name|PostScript>, a converter for
    <name|Xfig>.
  </itemize>

  Problems with the official <TeXmacs> are reported to its developers, see
  <hlink|www.texmacs.org|https://www.texmacs.org> (mailing lists and bug
  tracker); problems with the <TeXmacs> Fixes builds go to the <hlink|issues
  of github.com/mgubi/texmacs|https://github.com/mgubi/texmacs/issues>, not
  to the official <TeXmacs>. Please include the version of <TeXmacs> (shown
  by <tcmd|texmacs --version>, or on <name|macOS> by the <em|About> entry of
  the application menu) and of your operating system.

  <section|Download and installation>

  <label|sec-install>Installing <TeXmacs> takes three steps:

  <\enumerate>
    <item><strong|Download.> Ready-made packages of <TeXmacs> for
    <name|Windows>, <name|macOS> and the usual <name|Linux> distributions are
    available from the download page of <hlink|www.texmacs.org|https://www.texmacs.org>.
    The <TeXmacs> Fixes version used in this tutorial, with the corrections
    collected since the last official release, is available as packages for
    <name|Windows>, <name|macOS> and <name|Linux> from
    <hlink|mgubi.github.io/texmacs/fixes.html|https://mgubi.github.io/texmacs/fixes.html>
    (the files of the pre-release <verbatim|fixes-latest> of
    <hlink|github.com/mgubi/texmacs|https://github.com/mgubi/texmacs/releases>).

    <item><strong|Install.> Official packages: on <name|Windows>, run the
    installer; on <name|macOS>, open the disk image and drag the application
    to the <verbatim|Applications> folder; on <name|Linux>, install the
    package with the package manager of your distribution. <TeXmacs> Fixes
    builds: on <name|Windows>, run the installer, or unpack the zip archive
    and start <TeXmacs> from the unpacked folder; on <name|Linux>, the build
    is an <name|AppImage>: make it executable (<verbatim|chmod +x> on the
    file) and run it; on <name|macOS>, the application is not notarized, so
    the first time open it with a right-click (or Control-click) on it and
    <em|Open>, or remove the quarantine flag with <verbatim|xattr -dr
    com.apple.quarantine> followed by the path of the application. Nothing
    else is needed: no <name|Cygwin>, no X server, no <TeX> installation.

    <item><strong|Start> <TeXmacs> like any other application. The first
    time, a <em|Welcome> document (<menu|Help|Welcome>) shows links to the
    documentation.
  </enumerate>

  <section|Hands-on tutorial>

  <label|sec-tutorial>The keyboard shortcuts shown below are those of the
  system on which this document was typeset; the menus show the shortcuts of
  your own system next to their entries. Instead of the menus, you may also
  use the icons of the toolbars; the third toolbar, the <em|focus toolbar>,
  always shows what can be done with the structure under the cursor.

  <subsection|Part 1: Title, author, abstract, sections and the style of a
  document>

  <\enumerate>
    <item>Start <TeXmacs>. Select <menu|Insert|Title|Insert title> from the
    menu.

    <item>Type \PMy little Paper\Q and hit <key|return>.

    <item>Type some name and hit <key|return>.

    <item>Type in some address, with <key|return> between the lines.
    Meanwhile your document should look like:

    <image|images/title-author.png|0.8par|||>

    <item>Hit <key|return> and choose <menu|Insert|Link|Hyperlink> from the
    menu. Type in a URL, then hit the <key|right> key and retype it. You
    should see something like:

    <image|images/hyperlink-edit.png|0.8par|||>

    <item>Hit <key|return> to finish the definition of the hyperlink:

    <image|images/hyperlink-done.png|0.8par|||>

    <item>Hit <key|end> and then the <key|right> key: you leave the address
    and the whole title block. (The cyan box shows the structure in which the
    cursor is; the focus toolbar and the right side of the footer show it
    too.)

    <item>Choose <menu|Insert|Title|Abstract> from the menu. Enter some text.

    <image|images/abstract.png|0.8par|||>

    <item>Hit the <key|right> key, and notice how you leave the abstract. Hit
    <key|return>, this brings you to the beginning of a new paragraph.

    <item>Choose <menu|Insert|Section|Section> from the menu (or type
    <shortcut|(make-section 'section)>). Type \PThe First Section\Q. Hit
    <key|return>, type some text and hit <key|return> again.

    <item>Choose <menu|Insert|Section|Subsection> from the menu (or
    <shortcut|(make-section 'subsection)>). Type \PThe First Subsection of
    the First Section\Q. You see now something like:

    <image|images/sections.png|0.8par|||>

    <item>The sections and subsections are numbered: a new document has the
    <tmstyle|generic> style. Select <menu|Document|Style|article> from the
    menu: the title and the sections take the look of an article.

    <item>Hit <key|return> to get to a new line. There is a second
    (<LaTeX>-style) way to enter sections. Type \P<tcmd|\\section>\Q. You
    see:

    <image|images/latex-section-command.png|0.8par|||>

    <item>Hit <key|return>. Now you have achieved the same as just before by
    selecting <menu|Insert|Section|Section> from the menu, but without
    removing your hands from the keyboard! Type in \PThe Second Section\Q and
    hit <key|return>. Type some text.

    <item>Save your file with <menu|File|Save> (<shortcut|(save-buffer)>),
    for instance as <verbatim|hot1.tm>.
  </enumerate>

  This finishes the first part of the hands-on tutorial.

  <subsection|Part 2: Tables and figures>

  Proceed with your file from part 1.

  <\enumerate>
    <item>Put the cursor at the end of the heading of the first section and
    hit <key|return> to create a new line after it.

    <item>Select <menu|Insert|Table|Big table> from the menu. (There is an
    icon <icon|tm_table.xpm> on the second toolbar as well, it says \PInsert
    a table\Q if you move the mouse over it.) This inserts a numbered table
    with a caption, containing a table without borders (a <em|tabular>). To
    get the cell borders, click on the name <em|Tabular> in the focus toolbar
    (or use <menu|Focus|Tabular>) and choose the variant <em|Block>; the wand
    icon of the focus toolbar, or <shortcut|(variant-circulate (focus-tree)
    #t)>, goes through the variants too.

    <image|images/table-variant.png|0.8par|||>

    <item>By pressing <shortcut|(structured-insert-right)> three times, you
    get three more columns. Accordingly, by pressing
    <shortcut|(structured-insert-down)> twice you get two more rows. On
    <name|macOS>, these shortcuts are taken by default by <name|Mission
    Control>: use instead the icons of the focus toolbar which insert a
    column to the right and a row below (the round icons with a cross, at the
    left of the focus toolbar). Navigate with the cursor keys and enter
    something in all the cells.

    <image|images/table-filled.png|0.8par|||>

    <item>Mark the four topmost cells by clicking inside the leftmost cell,
    holding the mouse button pressed, and moving to the rightmost cell (works
    as well in the other direction). Then click on the icon
    <icon|tm_cell_background.xpm> of the focus toolbar (\PSet background
    color of cell\Q) and pick yellow:

    <image|images/table-selection-color.png|0.8par|||>

    <item>Click after \PTable 1.\Q and enter some caption. The result looks
    like:

    <no-page-break*><image|images/table-yellow-caption.png|0.8par|||>

    <item>Create a new line after the heading of the first subsection.

    <item>Select <menu|Insert|Image|Big figure>, and then
    <menu|Insert|Image|Insert image> from the menu. (There is an icon
    <icon|tm_image.xpm> as well, it says \PInsert a picture\Q if you move the
    mouse over it.) You get a dialog, in which you choose an image file:
    <name|Pdf>, <name|Svg>, <name|Png>, <name|Jpeg>, <name|Gif>... are all
    accepted (<name|Eps> too if <name|Ghostscript> is installed), and the
    dialog shows a preview. Select any image (we used a drawing of two
    circles, <verbatim|circ.svg>):

    <image|images/figure-big.png|0.8par|||>

    <item>This is too big. To edit the size, look at the focus toolbar while
    the cursor is just after the image: it shows the file name and the fields
    <em|Width> and <em|Height>. Type for instance <verbatim|6cm> in the
    <em|Width> field (or a relative width like <verbatim|0.4par>, 40% of the
    width of a paragraph) and hit <key|return>. The image is smaller now:

    <image|images/figure-resized.png|0.8par|||>

    <item>Save your file as <verbatim|hot2.tm> (<menu|File|Save as>).
  </enumerate>

  This finishes the second part of the hands-on tutorial.

  <subsection|Part 3: Mathematical content>

  Proceed with your file from part 2.

  <\enumerate>
    <item>Go somewhere in the text of section 2 of the document and type:
    <tcmd|$\\alpha_1><key|right><tcmd|,\\ldots><key|return><tcmd|,\\alpha_23>.
    (A <LaTeX>-like command is confirmed with <key|return>, or with
    <key|space>, <verbatim|_> or <verbatim|^>, which then also do their usual
    job.) While you are inside the formula, the mathematical toolbar replaces
    the text toolbar:

    <image|images/math-subscript.png|0.8par|||>

    <item>Type <key|right> to leave the index, <key|right> again to leave the
    formula, and hit <key|space> to insert a space before the word \Psome\Q.

    <item>Go to the end of the paragraph and hit <key|return> to make a new
    line.

    <item>Type: <tcmd|\\theorem><key|return><verbatim|For a natural number
    ><tcmd|$n\\in><key|return> and select from the icon <icon|tm_bbb.xpm> of
    the mathematical toolbar the blackboard bold version of N (or type <key|N
    N>, i.e. two capital N):

    <image|images/blackboard-bold.png|0.8par|||>

    <item>Type <key|right> to leave the formula, then type: \Pit holds:\Q

    <item>Now we want to insert a displayed formula. Select
    <menu|Insert|Mathematics|Displayed formula> from the menu (or faster, hit
    <shortcut|(make-equation*)>).

    <item>Type: <tcmd|\\sum><key|return><verbatim|_i=1><key|right><verbatim|^n><key|end><tcmd|n=\\frac><key|return><verbatim|n*(n-1)><key|down><verbatim|2>.
    Notice that \P<verbatim|*>\Q results in a little space and not in an
    explicit multiplication sign (it is an invisible multiplication). If you
    press <key|tab> after \P<verbatim|*>\Q, then the visible symbol is
    chosen. Typing \P<verbatim|(>\Q inserts both brackets.

    <image|images/theorem-equation.png|0.8par|||>

    <item>Press <key|right> three times and then <key|return>: you leave the
    formula and the theorem, and get to a new line.

    <item>Type: <tcmd|\\proof><key|return><verbatim|Done by Gauss in
    elementary school.>

    <item>Save your file as <verbatim|hot3.tm>.
  </enumerate>

  This finishes the third part of the hands-on tutorial.

  <subsection|Part 4: References>

  Proceed with your file from part 3.

  <\enumerate>
    <item>Move the cursor just before the text \PFor a natural number\Q, and
    enter: <tcmd|\\label><key|return><verbatim|theorem:gauss> (or use
    <menu|Insert|Link|Label>, <shortcut|(make-label)>).

    <image|images/label-theorem.png|0.8par|||>

    <item>Hit <key|return> to commit the label. You see a little flag, where
    the otherwise invisible label sits. This flag won't be printed. If you go
    behind this flag and hit <key|backspace>, then you can edit the label
    again (or edit its name in the focus toolbar). Note: the label's name is
    not important, you could have used \Pcat\Q or \Pdog\Q, but mentioning the
    environment (theorem, section, etc.) helps later when a reference is
    made.

    <image|images/label-flag.png|0.8par|||>

    <item>Go to the beginning of the first line after the heading of section
    2 and type <tcmd|\\label><key|return><verbatim|sec:math>. (A label typed
    inside the heading itself may get a wrong number, because the heading is
    repeated in the page header.)

    <image|images/label-section.png|0.8par|||>

    <item>Hit <key|return> to commit the label.

    <item>Do a similar thing at the beginning of the caption of the table:
    <tcmd|\\label><key|return><verbatim|tab:foo>, and hit <key|return>.

    <image|images/label-table.png|0.8par|||>

    <item>Only numbered equations can be referred to. Put the cursor inside
    the displayed formula, but outside the fraction and the sum (for instance
    just before the \P=\Q sign), so that the focus toolbar shows
    <em|Displayed formula>; then select <menu|Focus|Numbered> (or the icon
    <icon|tm_numbered.xpm> of the focus toolbar). The shortcut
    <shortcut|(numbered-toggle (focus-tree))> works from anywhere in the
    formula. The equation gets the number (1):

    <image|images/equation-numbered.png|0.8par|||>

    <item>Put a label at the end of the equation:
    <tcmd|\\label><key|return><verbatim|eq:gauss>, and hit <key|return>.

    <image|images/label-equation.png|0.8par|||>

    <item>After that many labels, we can finally refer to them! Go, e.g., to
    the end of the document and write: \PWe have seen in Section
    <tcmd|\\ref><key|return><verbatim|sec:math>\Q (or
    <menu|Insert|Link|Reference>):

    <image|images/reference-edit.png|0.8par|||>

    <item>Hit <key|return>, and go on by typing: \Pin the Theorem on page
    <tcmd|\\pageref><key|return><verbatim|theorem:gauss><key|return> the
    equation (<tcmd|\\ref><key|return><verbatim|eq:gauss><key|return>).\Q The
    page numbers are computed when the pages are made: if a page number shows
    as \P?\Q, use <menu|Document|Update|All> (<shortcut|(update-document
    "all")>). The result is:

    <image|images/references-done.png|0.8par|||>

    <item>Play around, insert some bogus theorems, sections and page breaks
    to see how the labels change automatically!

    <item>Save your file as <verbatim|hot4.tm>.
  </enumerate>

  This finishes the fourth part of the hands-on tutorial.

  <\with|par-mode|right>
    \<copyright\> Andreas Seidl 2003\U2004
  </with>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|page-type|a4>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|2|2>>
    <associate|auto-11|<tuple|3|2>>
    <associate|auto-12|<tuple|3|2>>
    <associate|auto-13|<tuple|3.1|3>>
    <associate|auto-14|<tuple|1|3>>
    <associate|auto-15|<tuple|5|3>>
    <associate|auto-16|<tuple|8|3>>
    <associate|auto-17|<tuple|10|3>>
    <associate|auto-18|<tuple|11|3>>
    <associate|auto-19|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|generic>|4>>
    <associate|auto-2|<tuple|1.1|1>>
    <associate|auto-20|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|generic>|4>>
    <associate|auto-21|<tuple|14|4>>
    <associate|auto-22|<tuple|15|4>>
    <associate|auto-23|<tuple|3.2|4>>
    <associate|auto-24|<tuple|2|4>>
    <associate|auto-25|<tuple|2|5>>
    <associate|auto-26|<tuple|7|6>>
    <associate|auto-27|<tuple|7|6>>
    <associate|auto-28|<tuple|9|6>>
    <associate|auto-29|<tuple|3.3|6>>
    <associate|auto-3|<tuple|<with|mode|<quote|math>|<rigid|\<circ\>>>|1>>
    <associate|auto-30|<tuple|6|8>>
    <associate|auto-31|<tuple|3.4|8>>
    <associate|auto-32|<tuple|1|8>>
    <associate|auto-33|<tuple|6|9>>
    <associate|auto-34|<tuple|8|9>>
    <associate|auto-35|<tuple|9|9>>
    <associate|auto-4|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|beamer>|1>>
    <associate|auto-5|<tuple|<with|mode|<quote|math>|\<bullet\>>|1>>
    <associate|auto-6|<tuple|<with|mode|<quote|math>|\<bullet\>>|1>>
    <associate|auto-7|<tuple|<with|mode|<quote|math>|<rigid|\<circ\>>>|1>>
    <associate|auto-8|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-9|<tuple|1.2|2>>
    <associate|sec-features|<tuple|1|1>>
    <associate|sec-install|<tuple|2|2>>
    <associate|sec-tutorial|<tuple|3|2>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Image>|<with|font-family|<quote|ss>|Draw
      image>>|<pageref|auto-3>>

      <tuple|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|beamer>>|<pageref|auto-4>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Import>>|<pageref|auto-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Export>>|<pageref|auto-6>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Import>|<with|font-family|<quote|ss>|Pdf
      with embedded document>>|<pageref|auto-7>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Session>>|<pageref|auto-8>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Help>|<with|font-family|<quote|ss>|Welcome>>|<pageref|auto-11>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Title>|<with|font-family|<quote|ss>|Insert
      title>>|<pageref|auto-14>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Hyperlink>>|<pageref|auto-15>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Title>|<with|font-family|<quote|ss>|Abstract>>|<pageref|auto-16>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Section>|<with|font-family|<quote|ss>|Section>>|<pageref|auto-17>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Section>|<with|font-family|<quote|ss>|Subsection>>|<pageref|auto-18>>

      <tuple|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|generic>>|<pageref|auto-19>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Style>|<with|font-family|<quote|ss>|article>>|<pageref|auto-20>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Section>|<with|font-family|<quote|ss>|Section>>|<pageref|auto-21>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Save>>|<pageref|auto-22>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Table>|<with|font-family|<quote|ss>|Big
      table>>|<pageref|auto-24>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Tabular>>|<pageref|auto-25>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Image>|<with|font-family|<quote|ss>|Big
      figure>>|<pageref|auto-26>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Image>|<with|font-family|<quote|ss>|Insert
      image>>|<pageref|auto-27>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>|<with|font-family|<quote|ss>|Save
      as>>|<pageref|auto-28>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Mathematics>|<with|font-family|<quote|ss>|Displayed
      formula>>|<pageref|auto-30>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Label>>|<pageref|auto-32>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Numbered>>|<pageref|auto-33>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Reference>>|<pageref|auto-34>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Update>|<with|font-family|<quote|ss>|All>>|<pageref|auto-35>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Features
      and current issues> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|1tab>|1.1<space|2spc>Features
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2>>

      <with|par-left|<quote|1tab>|1.2<space|2spc>Current issues
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Download
      and installation> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-10><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Hands-on
      tutorial> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-12><vspace|0.5fn>

      <with|par-left|<quote|1tab>|3.1<space|2spc>Part 1: Title, author,
      abstract, sections and the style of a document
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-13>>

      <with|par-left|<quote|1tab>|3.2<space|2spc>Part 2: Tables and figures
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-23>>

      <with|par-left|<quote|1tab>|3.3<space|2spc>Part 3: Mathematical content
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-29>>

      <with|par-left|<quote|1tab>|3.4<space|2spc>Part 4: References
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-31>>
    </associate>
  </collection>
</auxiliary>