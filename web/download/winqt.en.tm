<TeXmacs|2.1.5>

<style|<tuple|tmweb2|old-spacing|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Compiling <TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  <section|Download and build the environment><label|install>

  <\itemize-dot>
    <item>In order to compile <TeXmacs> under <name|Windows>, you need to
    <hlink|install MSYS2|https://www.msys2.org/>.

    <item>On Windows Intel/AMD, open the terminal named \QMSYS2 MINGW64\Q. On
    Windows ARM, open the terminal named \PMSYS2 CLANGARM64\Q.

    <item>Update MSYS2 :\ 

    <\shell-code>
      pacman -Syu
    </shell-code>

    <em|<item>Note: You may need to relaunch the terminal after the update.>

    <item>Install subversion :\ 

    <\shell-code>
      pacman -S subversion
    </shell-code>

    <item>Download the builder :\ 

    <\shell-code>
      svn co svn://svn.savannah.gnu.org/texmacs/trunk/misc/builder
    </shell-code>

    <item>Run the builder (this may take a few hours) :\ 

    <\shell-code>
      cd builder

      ./script/build
    </shell-code>

    <item>The builder will create a /windows-qt6 directory with all the built
    dependencies.
  </itemize-dot>

  <section|Build and run <TeXmacs>>

  <\itemize-dot>
    <item>Source the environment

    <\shell-code>
      cd /windows-qt6

      source set-devel-paths
    </shell-code>

    <item>If you wish to reconfigure, rebuild <TeXmacs> entierly, and make a
    package, run :\ 

    <\shell-code>
      mingw32-make texmacs
    </shell-code>

    <\itemize-minus>
      <item><name|texmacs/distr> contains all the final installers and
      binaries

      <item><name|texmacs/src> contains the sources
    </itemize-minus>

    <item>If you edited some sources, and want to build texmacs, go to the
    source directory and run a make :\ 

    <\shell-code>
      cd texmacs/src

      mingw32-make
    </shell-code>

    <item>Inside the source directory, you can also get and build the latest
    commit using :\ 

    <\shell-code>
      svn up

      mingw32-make
    </shell-code>

    <item>You can test the compiled binaries by runnig :

    <\shell-code>
      ./TeXmacs/bin/texmacs.bin
    </shell-code>
  </itemize-dot>

  <tmdoc-copyright|2026|Liza Belos, David Michel|Massimiliano Gubinelli|Joris
  van der Hoeven, Denis Raux>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>