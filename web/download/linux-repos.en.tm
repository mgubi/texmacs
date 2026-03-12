<TeXmacs|2.1.5>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|GNU/<name|Linux> repositories
  for <TeXmacs>|<tmweb-download-links>>

  On certain <name|Linux> distributions, it is possible to add a <TeXmacs>
  repository to your package manager. This makes it possible to manage
  <TeXmacs> in the same way as any other packages on your system. In
  particular, you can automatically check for updates and install new
  versions as soon as they become available. We implemented <TeXmacs>
  repositories for the following distributions:

  <\itemize>
    <item><hlink|Debian and Raspbian|#debian>.

    <item><hlink|Ubuntu|#ubuntu>.
  </itemize>

  <section*|Instructions><label|debian>

  Download and import the <TeXmacs> public key :

  <\shell-code>
    wget -qO- --no-check-certificate https://ftp.texmacs.org/TeXmacs/tmftp/repos/apt/apt-texmacs.asc
    \| sudo tee /usr/share/keyrings/texmacs-keyring.asc \<gtr\> /dev/null
  </shell-code>

  Add the texmacs repository:

  <\shell-code>
    echo "deb [signed-by=/usr/share/keyrings/texmacs-keyring.asc]
    http://ftp.texmacs.org/TeXmacs/tmftp/repos/apt/ $(lsb_release -cs) main"
    \| sudo tee /etc/apt/sources.list.d/texmacs.list \<gtr\> /dev/null
  </shell-code>

  Update the package list:

  <\shell-code>
    apt-get update
  </shell-code>

  Install TeXmacs:

  <\shell-code>
    apt-get install texmacs
  </shell-code>

  <tmdoc-copyright|1999\U2018|Denis Raux|Joris van der Hoeven, Liza Belos>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>