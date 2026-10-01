<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Example of a plug-in with Python code>

  <paragraph*|The <verbatim|pyminimal> plug-in>

  Consider the example of the <verbatim|pyminimal> plug-in in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  It consists of the following files:

  <\verbatim>
    \ \ \ \ <example-plugin-link|pyminimal/progs/init-pyminimal.scm>

    \ \ \ \ <example-plugin-link|pyminimal/src/minimal.py>
  </verbatim>

  In order to try the plug-in, you first have to recursively copy the
  directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/pyminimal
  </verbatim>

  to <verbatim|$TEXMACS_PATH/plugins> or <verbatim|$TEXMACS_HOME_PATH/plugins>.

  When relaunching <TeXmacs>, the plug-in should now be automatically
  recognized.

  <paragraph*|How it works: The Scheme Part>

  The <verbatim|pyminimal> plug-in demonstrates a minimal interface between
  <TeXmacs> and an extern program in python. The initialization file
  <verbatim|init-pyminimal.scm> essentially contains the following code:

  <\scm-code>
    (define (python-launcher)

    \ \ (if (url-exists? "$TEXMACS_HOME_PATH/plugins/pyminimal")

    \ \ \ \ \ \ (string-append "python \\""

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (getenv "TEXMACS_HOME_PATH")

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "/plugins/pyminimal/src/minimal.py\\"")

    \ \ \ \ \ \ (string-append "python \\""

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (getenv "TEXMACS_PATH")

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "/plugins/pyminimal/src/minimal.py\\"")))

    \;

    (plugin-configure pyminimal

    \ \ (:require (url-exists-in-path? "python"))

    \ \ (:launch ,(python-launcher))

    \ \ (:session "PyMinimal"))
  </scm-code>

  The <scm|:require> option checks whether <shell|python> indeed exists in
  the path (so this will fail if you did not have python installed). The
  <scm|:launch> option specifies how to launch the extern program. The
  <verbatim|:session> option indicates that it will be possible to create
  sessions for the <verbatim|pyminimal> plug-in using
  <menu|Insert|Session|PyMinimal>.

  The <scm|python-launcher> function will be evaluated and return the proper
  command to launcher the extern program. If
  <shell|$TEXMACS_HOME_PATH/plugins/pyminimal> exists, it would be

  <\shell-code>
    python "$TEXMACS_HOME_PATH/plugins/pyminimal/src/minimal.py"
  </shell-code>

  otherwise,

  <\shell-code>
    python "$TEXMACS_PATH/plugins/pyminimal/src/minimal.py"
  </shell-code>

  The environment variables will be replaced in runtime. Sometimes,
  <shell|$TEXMACS_HOME_PATH> and <shell|$TEXMACS_PATH> may contain spaces, as
  a result, we quote the path using the double quotes.

  <paragraph|How it works: The Python Part>

  Using the Python interpreter, we do not need to compile the code. And most
  of the time, python code can be interpreted without any modification under
  multiple platforms. The built-in python libraries are really helpful and
  handy.

  Many <TeXmacs> built-in plugins are written in Python, and we have
  carefully organized the code and reused the common part named <name|tmpy>.

  <\python-code>
    import os

    import sys

    from os.path import exists

    tmpy_home_path = os.environ.get("TEXMACS_HOME_PATH") + "/plugins/tmpy"

    if (exists (tmpy_home_path)):

    \ \ \ \ sys.path.append(os.environ.get("TEXMACS_HOME_PATH") +
    "/plugins/")

    else:

    \ \ \ \ sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")
  </python-code>

  The first part of the code just add <shell|$TEXMACS_HOME_PATH/plugins> or
  <shell|$TEXMACS_PATH/plugins> to the python path for importing the
  <name|tmpy> package.

  <\python-code>
    from tmpy.protocol \ \ \ \ \ \ \ import *

    from tmpy.compat \ \ \ \ \ \ \ \ \ import *

    \;

    flush_verbatim ("Hi there!")

    \;

    while True:

    \ \ \ \ line = tm_input()

    \ \ \ \ if not line:

    \ \ \ \ \ \ \ \ pass

    \ \ \ \ else:

    \ \ \ \ \ \ \ \ flush_verbatim ("You typed " + line)
  </python-code>

  <python|flush_verbatim> is provided by <python|tmpy.protocol> which is a
  subpackage for interaction with <TeXmacs> server in Python. It writes a
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block in the <verbatim|utf8>
  format on the standard output and flushes it. The same module also
  provides <python|flush_prompt>, <python|flush_command>,
  <python|flush_scheme>, <python|flush_latex>, <python|flush_file>,
  <python|flush_ps> and <python|flush_err> (the latter writes to the
  standard error, whose contents are displayed as error output), as well as
  the constants <python|DATA_BEGIN>, <python|DATA_END>,
  <python|DATA_ESCAPE> and <python|DATA_COMMAND>.

  <python|tm_input> is provided by <python|tmpy.compat> which is a subpackage
  for compatibility within Python 2 and 3.

  <paragraph|Choosing the <name|Python> interpreter>

  The example uses the command <shell|python> both in its <scm|:require>
  test and in its launcher. On systems where only <shell|python3> is
  installed, the plug-in will therefore not be detected. The plug-ins which
  are shipped with <TeXmacs> rather use the function
  <scm|(python-command)> (defined in <verbatim|kernel/library/base.scm>),
  which returns the first of <shell|python3>, <shell|python> and
  <shell|python2> which can be found in the path (or the empty string if
  there is none). For instance, the configuration of the
  <verbatim|python> plug-in (in <verbatim|plugins/python/progs/init-python.scm>)
  reads

  <\scm-code>
    (plugin-configure python

    \ \ (:winpath "python*" ".")

    \ \ (:winpath "Python*" ".")

    \ \ (:winpath "Python/Python*" ".")

    \ \ (:require (python-command))

    \ \ (:launch ,(python-launcher))

    \ \ (:preferences (supports-jupyter?))

    \ \ (:tab-completion #t)

    \ \ (:serializer ,python-serialize)

    \ \ (:session "Python")

    \ \ (:scripts "Python"))
  </scm-code>

  where <scm|python-launcher> starts the script
  <verbatim|plugins/tmpy/session/tm_python.py> with the interpreter
  returned by <scm|(python-command)> and the option <verbatim|-X utf8>.
  Notice that the empty string returned by <scm|(python-command)> when no
  interpreter is found is a true value in <scheme>, so this particular
  <scm|:require> test always succeeds; a more robust test would be
  <scm|(!= (python-command) "")>.

  <paragraph|Comparison with <c++>>

  This demo plugin is a Python implementation of the well-documented
  <hlink|minimal plugin|plugin-binary.en.tm> written in <c++>. Here is the
  summary of the differences:

  <\itemize>
    <item><verbatim|pyminimal> requires the <name|Python> interpreter,
    <verbatim|minimal> needs to be compiled and linked

    <item><verbatim|pyminimal> is easier to install than <verbatim|minimal>

    <item><verbatim|pyminimal> reuses the common part related to <TeXmacs>

    <item><verbatim|pyminimal> has better compatibility for <name|Linux>,
    <name|Windows> and <name|macOS>
  </itemize>

  <tmdoc-copyright|2019|Darcy Shen>

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
