<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Studying the \Pmycas\Q example>

  The best way to start implementing a new interface with <TeXmacs> is to
  take a look at the sample \Pcomputer algebra system\Q <verbatim|mycas>,
  which is shipped as a plug-in in the directory
  <verbatim|$TEXMACS_PATH/plugins/mycas> (in the source code of <TeXmacs>,
  this is <verbatim|src/plugins/mycas>). The file
  <source-link|src/mycas.cpp|plugins/mycas/src/mycas.cpp> of this plug-in, which is listed at the end of
  this section, contains a very simple program which can be interfaced
  with <TeXmacs>. In order to test the program, you should compile it
  using

  <\shell-code>
    g++ mycas.cpp -o mycas
  </shell-code>

  and move the binary <verbatim|mycas> to some location in your path. The
  configuration file <source-link|progs/init-mycas.scm|plugins/mycas/progs/init-mycas.scm> of the plug-in
  contains

  <\scm-code>
    (plugin-configure mycas

    \ \ (:require (url-exists-in-path? "mycas"))

    \ \ (:launch "mycas --texmacs")

    \ \ (:session "Mycas"))
  </scm-code>

  so that, when starting up <TeXmacs> (you may need to use
  <menu|Tools|Update|Plugins> in order to force the detection of the new
  binary), you should then have a <menu|Mycas> entry in the
  <menu|Insert|Session> menu.

  <\remark>
    The file <source-link|mycas.cpp|plugins/mycas/src/mycas.cpp> dates from 2001 and includes the
    pre-standard header <verbatim|\<less\>iostream.h\<gtr\>>, which is
    rejected by modern <c++> compilers. In order to compile it, replace
    this line by <verbatim|#include \<less\>iostream\<gtr\>> followed by
    <verbatim|using namespace std;>, as in the listing below.
  </remark>

  <section|Studying the source code step by step>

  Let us study the source code of <verbatim|mycas> step by step. First, all
  communication takes place via standard input and output, using pipes. In
  order to make it possible for <TeXmacs> to know when the output from your
  system has finished, all output needs to be encapsulated in blocks, using
  three special control characters:

  <\cpp-code>
    #define DATA_BEGIN \ \ ((char) 2)

    #define DATA_END \ \ \ \ ((char) 5)

    #define DATA_ESCAPE \ ((char) 27)
  </cpp-code>

  The <verbatim|DATA_ESCAPE> character followed by any other character
  <math|c> may be used to produce <math|c>, even if <math|c> is one of the
  three control characters. An illustration of how to use
  <verbatim|DATA_BEGIN> and <verbatim|DATA_END> is given by the startup
  banner:

  <\cpp-code>
    int

    main () {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";

    \ \ cout \<less\>\<less\> "Welcome to my test computer algebra system
    for TeXmacs\\n";

    \ \ cout \<less\>\<less\> "This software comes with no warranty
    whatsoever\\n";

    \ \ cout \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";

    \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";

    \ \ next_input ();

    \ \ cout \<less\>\<less\> DATA_END;

    \ \ fflush (stdout);
  </cpp-code>

  The first line of <verbatim|main> says that the startup banner will be
  printed in the \Pverbatim\Q format. The <verbatim|next_input> function,
  which is called after outputting the banner, is used for printing a prompt
  and will be detailed later. The final <verbatim|DATA_END> closes the
  startup banner block and tells <TeXmacs> that <verbatim|mycas> is waiting
  for input. Don't forget to flush the standard output, so that <TeXmacs>
  will receive the whole message.

  The main loop starts by asking for input from the standard input:

  <\cpp-code>
    \ \ while (1) {

    \ \ \ \ char buffer[100];

    \ \ \ \ cin.getline (buffer, 100, '\\n');

    \ \ \ \ if (strcmp (buffer, "quit") == 0) break;
  </cpp-code>

  The output which is sent back should again be enclosed in a
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block.

  <\cpp-code>
    \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    \ \ \ \ cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer
    \<less\>\<less\> "\\n";
  </cpp-code>

  Inside such a block you may recursively send other blocks, which may be
  specified in different formats. For instance, the following code will
  send a <LaTeX> formula:

  <\cpp-code>
    \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula: ";

    \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:"
    \<less\>\<less\> "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;

    \ \ \ \ cout \<less\>\<less\> "\\n";
  </cpp-code>

  For certain purposes, it may be useful to directly send output in
  <TeXmacs> format using a <scheme> representation:

  <\cpp-code>
    \ \ \ \ cout \<less\>\<less\> "And finally a fraction ";

    \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:"
    \<less\>\<less\> "(frac \\"a\\" \\"b\\")" \<less\>\<less\> DATA_END;

    \ \ \ \ cout \<less\>\<less\> ".\\n";
  </cpp-code>

  In order to finish, we should again output the matching
  <verbatim|DATA_END> and flush the standard output:

  <\cpp-code>
    \ \ \ \ next_input ();

    \ \ \ \ cout \<less\>\<less\> DATA_END;

    \ \ \ \ fflush (stdout);

    \ \ }

    \ \ return 0;

    }
  </cpp-code>

  Notice that you should never output more than one
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block. As soon as the first
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block has been received by
  <TeXmacs>, it is assumed that your system is waiting for input. If you
  want to send several <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks,
  then they should be enclosed in one main block.

  A special \Pchannel\Q is used in order to send the input prompt. In
  <source-link|mycas.cpp|plugins/mycas/src/mycas.cpp>, the channel is selected using a special
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block in the
  <verbatim|channel> format, which redirects the remainder of the enclosing
  block to the <verbatim|prompt> channel:

  <\cpp-code>
    static int counter= 0;

    \;

    void

    next_input () {

    \ \ counter++;

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "channel:prompt"
    \<less\>\<less\> DATA_END;

    \ \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";

    }
  </cpp-code>

  Since <verbatim|next_input> is always called at the end of the output,
  this works fine. In new code, it is simpler and more robust to use a
  block of the form <verbatim|DATA_BEGIN prompt# ... DATA_END>, as
  explained in the section on <hlink|output channels, prompts and default
  input|../interface/interface-channels.en.tm>. Inside the prompt channel,
  you may again use <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks in a
  nested way. This allows you for instance to use a formula as a prompt.
  There are four standard channels:

  <\description>
    <item*|<verbatim|output>>The default channel for normal output.

    <item*|<verbatim|prompt>>For sending input prompts.

    <item*|<verbatim|input>>For specifying a default value for the next
    input.

    <item*|<verbatim|error>>For error messages. All output on the standard
    error of the application is sent to this channel.
  </description>

  <section|Graphical output>

  It is possible to send <name|PostScript> graphics as output. Assume for
  instance that you have a picture <verbatim|picture.ps> in your home
  directory. Then inserting the lines

  <\cpp-code>
    \ \ \ \ cout \<less\>\<less\> "A little picture:\\n";

    \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "ps:";

    \ \ \ \ fflush (stdout);

    \ \ \ \ system ("cat $HOME/picture.ps");

    \ \ \ \ cout \<less\>\<less\> DATA_END;

    \ \ \ \ cout \<less\>\<less\> "\\n";
  </cpp-code>

  at the appropriate place in the main loop will display your image in the
  middle of the output. Alternatively, you may send the name of an image
  file in the <verbatim|png>, <verbatim|eps>, <verbatim|pdf> or
  <verbatim|svg> format using a block of the form <verbatim|DATA_BEGIN
  file:/path/to/picture.png DATA_END>; see the <hlink|internals of the
  plug-in system|plugin-internals.en.tm> for the complete list of
  formats.

  <section|The complete listing>

  Here follows the complete listing of <source-link|mycas.cpp|plugins/mycas/src/mycas.cpp>, with the
  include lines adapted to modern <c++> compilers:

  <\cpp-code>
    #include \<less\>stdio.h\<gtr\>

    #include \<less\>stdlib.h\<gtr\>

    #include \<less\>string.h\<gtr\>

    #include \<less\>iostream\<gtr\>

    using namespace std;

    \;

    #define DATA_BEGIN \ \ ((char) 2)

    #define DATA_END \ \ \ \ ((char) 5)

    #define DATA_ESCAPE \ ((char) 27)

    \;

    static int counter= 0;

    \;

    void

    next_input () {

    \ \ counter++;

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "channel:prompt"
    \<less\>\<less\> DATA_END;

    \ \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";

    }

    \;

    int

    main () {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";

    \ \ cout \<less\>\<less\> "Welcome to my test computer algebra system
    for TeXmacs\\n";

    \ \ cout \<less\>\<less\> "This software comes with no warranty
    whatsoever\\n";

    \ \ cout \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";

    \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";

    \ \ next_input ();

    \ \ cout \<less\>\<less\> DATA_END;

    \ \ fflush (stdout);

    \;

    \ \ while (1) {

    \ \ \ \ char buffer[100];

    \ \ \ \ cin.getline (buffer, 100, '\\n');

    \ \ \ \ if (strcmp (buffer, "quit") == 0) break;

    \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    \ \ \ \ cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer
    \<less\>\<less\> "\\n";

    \;

    \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula: ";

    \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:"
    \<less\>\<less\> "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;

    \ \ \ \ cout \<less\>\<less\> "\\n";

    \;

    \ \ \ \ cout \<less\>\<less\> "And finally a fraction ";

    \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:"
    \<less\>\<less\> "(frac \\"a\\" \\"b\\")" \<less\>\<less\> DATA_END;

    \ \ \ \ cout \<less\>\<less\> ".\\n";

    \;

    \ \ \ \ next_input ();

    \ \ \ \ cout \<less\>\<less\> DATA_END;

    \ \ \ \ fflush (stdout);

    \ \ }

    \ \ return 0;

    }
  </cpp-code>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
