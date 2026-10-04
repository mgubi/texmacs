<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Notification and download of updates>

  Since <name|svn> revision 7196, <TeXmacs> supports automatic notification
  of available downloads from a repository and their installation using the
  <name|Sparkle> framework for <name|MacOS> and <name|WinSparkle> under
  <name|Windows>.

  In order to guarantee the origin of releases, these must be signed with a
  <name|DSA> key, whose public part will be bundled with the application. On
  the server side a so-called <with|font-shape|italic|appcast> must be
  updated for each release. It is an <name|XML> file containing information
  about available downloads, their contents and their digital signatures,
  following the specification for <name|Sparkle>/<name|WinSparkle>. For the
  moment we refer to <name|Sparkle>'s documentation for more details.

  In principle it should be easy for anyone to release their custom versions
  of <TeXmacs> and let their users autoupdate them. For this they only need
  to provide the public key and the <abbr|URL> of the appcast (using the
  <verbatim|--with-appcast> configuration option, see below).

  <subsection|Operating system specifics>

  Under <name|MacOS> the process of creation of the appcast is partially
  automated through the <name|make> build rule <verbatim|MACOS_RELEASE>.
  Calling <verbatim|make MACOS_RELEASE> will compile and bundle <TeXmacs>,
  then zip and finally digitally sign the resulting
  <verbatim|.zip> archive of the bundle with the script
  <verbatim|misc/admin/sign_update>. In order for this to work, one has to
  set the environment variable <verbatim|TEXMACS_PRIVATE_DSA> to point to the
  location of the private <name|DSA> key used to sign releases. At the end of
  the build process a chunk of <name|XML> is printed that can be pasted in
  the <verbatim|appcast.xml> file. This rule is only provided by the
  <name|autotools> build (<verbatim|Makefile.in>), not by the <name|CMake>
  build.

  Under <name|Windows> digital signatures are not yet supported by
  <name|WinSparkle> and as such will be ignored (Aug. 2013).

  There is no support for automatic notification of releases under
  <name|Linux> yet. Automatic download and installation is unlikely to happen
  due to the way packaging systems work for most distributions.

  <subsection|Client side interface>

  The <c++> side of the updater lives in <verbatim|src/src/Plugins/Updater/>:
  the abstract class <cpp|tm_updater> (<verbatim|tm_updater.hpp>) has the
  implementations <cpp|tm_sparkle> (<name|MacOS>) and <cpp|tm_winsparkle>
  (<name|Windows>). Support is only compiled in when <TeXmacs> is configured
  with <verbatim|--with-sparkle>; the <abbr|URL> of the appcast is fixed at
  configuration time using <verbatim|--with-appcast=<em|url>> (see
  <verbatim|misc/m4/sparkle.m4>), and is stored as <verbatim|SUFeedURL> in
  the <name|MacOS> bundle's <verbatim|Info.plist>, <abbr|resp.> in the
  <name|Windows> resource file. The following glued routines are available
  from <scheme>:

  <\explain>
    <scm|(updater-supported?)><explain-synopsis|is the updater available?>
  <|explain>
    Returns <scm|#t> if <TeXmacs> was compiled with support for
    <name|Sparkle> or <name|WinSparkle>. All other routines do nothing and
    return <scm|#f> (or zero) otherwise.
  </explain>

  <\explain>
    <scm|(updater-check-background)><explain-synopsis|check for updates in
    the background>
  <|explain>
    Start a background check for updates. A dialog box pops up only if
    there is an update. Returns <scm|#f> if the check could not be started
    (for instance, because a check is already running, or, under
    <name|Windows>, because automatic checks are disabled).
  </explain>

  <\explain>
    <scm|(updater-check-foreground)><explain-synopsis|check for updates in
    the foreground>
  <|explain>
    Start a check for updates immediately, popping up a dialog with the
    progress. This call is non-blocking, since <name|Sparkle> and
    <name|WinSparkle> run in separate threads.
  </explain>

  <\explain>
    <scm|(updater-set-interval <scm-arg|hours>)><explain-synopsis|set the
    update interval>
  <|explain>
    Sets the interval in hours to wait between automatic checks. The value
    is clamped between 24 hours and 31 days; under <name|Windows>, a value
    of zero disables automatic checks. Note that this does <em|not> alter
    the value of the preference <scm|"updater:interval">, whose use is
    preferred.
  </explain>

  <\explain>
    <scm|(updater-running?)>

    <scm|(updater-last-check)><explain-synopsis|updater status>
  <|explain>
    Test whether a check is currently in progress, <abbr|resp.> return the
    time of the last check (in seconds since the epoch, or zero if there was
    no check yet).
  </explain>

  At startup, <verbatim|init-texmacs.scm> loads the module
  <verbatim|utils/misc/updater.scm> if <scm|(updater-supported?)> holds, and
  calls <scm|(updater-initialize)> after a short delay. This routine reads
  the following preference:

  <\explain>
    <scm|("updater:interval" <scm-arg|hours>)><explain-synopsis|preference>
  <|explain>
    The number of hours between automatic checks, as a string. Its default
    value <scm|"null"> means that automatic checks are disabled. If the
    value is a number, then <scm|updater-initialize> calls
    <scm|updater-set-interval> with this value and starts a background
    check. In the preferences dialog (<menu|Edit|Preferences|Other>), the
    possible choices are <scm|"0"> (never), <scm|"24">, <scm|"168"> and
    <scm|"720"> (once a day, week or month).
  </explain>

  The older interface (<scm|check-updates-background>,
  <scm|check-updates-foreground>, <scm|check-updates-interval> and the
  preferences <scm|"updater:appcast">, <scm|"updater:automatic-checks">,
  <scm|"updater:check-interval"> and <scm|"updater:public-dsa-key">) no
  longer exists.

  <tmdoc-copyright|2013|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>