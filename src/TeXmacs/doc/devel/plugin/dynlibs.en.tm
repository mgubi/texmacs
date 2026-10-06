<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Linking your system as a dynamic library>

  Instead of connecting your system to <TeXmacs> using a pipe, it is also
  possible to connect it as a dynamically linked library. Although
  communication through pipes is usually easier to implement, more robust
  and compatible with gradual output, the second option is faster.

  <\warning>
    Dynamic linking is only available if <TeXmacs> was compiled with the
    macro <cpp|TM_DYNAMIC_LINKING> defined (the <name|autotools>
    configuration script defines it when <cpp|dlopen> is available), and it
    is not supported under <name|Windows>. Otherwise, the connection fails
    with the message <verbatim|Dynamic linking not implemented>. A more
    recent description of dynamic linking, with a complete example, can be
    found in the section on <hlink|dynamic
    libraries|../interface/interface-dynlibs.en.tm>.
  </warning>

  <section|Connections via dynamically linked libraries>

  Let us now describe the steps you have to go through in order to link
  your system as a dynamic library.

  <\enumerate>
    <item>Modify the architecture of your system in such a way that the main
    part of it can be linked as a shared library; your binary should
    typically become a very small program, which handles verbatim input and
    output, and which is linked with your shared library at runtime.

    <item>Include the header file <verbatim|$TEXMACS_PATH/include/TeXmacs.h>
    in the source of your system and write the input/output routines as
    required by the <TeXmacs> communication protocol, as explained below.

    <item>Include a line of the form

    <\scm-code>
      (:link "libmyplugin.so" "myplugin_exports" "init")
    </scm-code>

    in the <scm|plugin-configure> instruction of your file
    <verbatim|init-myplugin.scm>. Here <verbatim|libmyplugin.so> is the
    corresponding shared library, which is searched in
    <verbatim|$LD_LIBRARY_PATH> (the <verbatim|lib> subdirectory of your
    plug-in is automatically added to this path),
    <verbatim|myplugin_exports> is the name of the exported data structure
    which will be used by <TeXmacs> in order to link your system, and
    <verbatim|init> some initialization string for your package.

    <item>Proceed in a similar way as in the case of communication by
    pipes.
  </enumerate>

  In older versions of <TeXmacs>, the configuration used the instructions
  <verbatim|package-declare> and <verbatim|package-format>; these no longer
  exist.

  <section|The <TeXmacs> communication protocol>

  The <TeXmacs> communication protocol is used for linking libraries
  dynamically to <TeXmacs>. The file <verbatim|$TEXMACS_PATH/include/TeXmacs.h>
  contains the declarations of all data structures used by the protocol.
  In principle, a succession of different protocols is foreseen. Each of
  these protocols has the abstract data structures
  <cpp|TeXmacs_exports> and <cpp|package_exports> in common, with
  information about the versions of the protocol, <TeXmacs> and your
  package.

  The <math|n>-th concrete version of the communication protocol should
  provide two data structures <cpp|TeXmacs_exports_n> and
  <cpp|package_exports_n>. The first structure contains all routines and
  data of <TeXmacs>, which may be necessary for the package. The second
  structure contains all routines and data of your package, which should
  be visible inside <TeXmacs>. Only the first version of the protocol has
  been implemented so far.

  When a session is started, <TeXmacs> (see
  <source-link|System/Link/dyn_link.cpp|src/System/Link/dyn_link.cpp>) opens the library, looks up the
  symbol given in the <scm|:link> option using <cpp|dlsym>, and
  interprets it as a pointer to a structure of type
  <cpp|package_exports_1>. Notice that the comments at the beginning of
  <source-link|TeXmacs.h|TeXmacs/include/TeXmacs.h>, which mention a function <cpp|get_my_package> and
  a <scheme> command <verbatim|package_declare>, describe an older
  mechanism which is not used anymore.

  <section|Version 1 of the <TeXmacs> communication protocol>

  In the first version of the <TeXmacs> communication protocol, your
  package should export an instance of the following data structure:

  <\cpp-code>
    typedef struct package_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_package;

    \ \ char* (*install) (TeXmacs_exports_1* TeXmacs,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ char* options, char** errors);

    \ \ char* (*evaluate) (char* what, char* session, char** errors);

    } package_exports_1;
  </cpp-code>

  The string <verbatim|version_protocol> should contain <verbatim|"TeXmacs
  communication protocol 1"> and the string <verbatim|version_package> the
  version of your package.

  The routine <verbatim|install> will be called once by <TeXmacs> in order
  to initialize your system with options <verbatim|options> (the
  initialization string of the <scm|:link> option). It communicates the
  routines exported by <TeXmacs> to your system in the form of
  <verbatim|TeXmacs>. The routine should return a status message like

  <\verbatim-code>
    "yourcas-version successfully linked to TeXmacs"
  </verbatim-code>

  If installation failed, then you should set <verbatim|*errors> to an
  error message: as soon as <verbatim|*errors> is not <verbatim|NULL>, the
  installation is considered to have failed.

  The routine <verbatim|evaluate> is used to evaluate the expression
  <verbatim|what> inside a <TeXmacs>-session with name <verbatim|session>.
  The input <verbatim|what> is serialized in the same way as for pipes. The
  routine should return the result of the evaluation, encoded in the same
  way as the output of an application which communicates through pipes,
  that is, using <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks (for
  instance <verbatim|"\\2verbatim:Hello\\5">). If <verbatim|NULL> is
  returned, then the contents of <verbatim|*errors> is used instead (or the
  string <verbatim|"Error"> if <verbatim|*errors> is <verbatim|NULL>).

  <\remark>
    <TeXmacs> copies the strings returned by the routines
    <verbatim|install> and <verbatim|evaluate> and the error messages, but
    it does not free them (see <source-link|System/Link/dyn_link.cpp|src/System/Link/dyn_link.cpp>). The
    package is therefore responsible for the memory management of these
    strings, for instance by reusing a static buffer, as in the
    <verbatim|dynlink> example plug-in. The comments in
    <source-link|TeXmacs.h|TeXmacs/include/TeXmacs.h> which state that these strings are freed by
    <TeXmacs> are not accurate.
  </remark>

  The first version of the <TeXmacs> communication protocol also requires
  <TeXmacs> to export an instance of the data structure

  <\cpp-code>
    typedef struct TeXmacs_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_TeXmacs;

    } TeXmacs_exports_1;
  </cpp-code>

  The string <verbatim|version_protocol> contains the version
  <verbatim|"TeXmacs communication protocol 1"> of the protocol and
  <verbatim|version_TeXmacs> the current version of <TeXmacs>.

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
