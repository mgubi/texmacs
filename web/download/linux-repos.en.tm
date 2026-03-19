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
  repositories for Debian, Raspbian, and Ubuntu.

  <section*|Instructions for Debian, Raspbian and Ubuntu><label|debian>

  Download and import the <TeXmacs> public key :

  <\shell-code>
    wget -qO- --no-check-certificate https://ftp.texmacs.org/TeXmacs/tmftp/repos/apt/apt-texmacs.asc
    \| gpg --dearmor \| sudo tee /usr/share/keyrings/texmacs-keyring.gpg
    \<gtr\> /dev/null
  </shell-code>

  Add the texmacs repository:

  <\shell-code>
    echo "deb [signed-by=/usr/share/keyrings/texmacs-keyring.gpg]
    http://ftp.texmacs.org/TeXmacs/tmftp/repos/apt/ $(lsb_release -cs) main"
    \| sudo tee /etc/apt/sources.list.d/texmacs.list \<gtr\> /dev/null
  </shell-code>

  Update the package list:

  <\shell-code>
    apt-get update
  </shell-code>

  Install <TeXmacs>:

  <\shell-code>
    apt-get install texmacs
  </shell-code>

  <section*|Instructions for Ubuntu-based distributions><label|debian>

  Distributions that are Ubuntu-based (like Linux Mint) are compatible with
  the <TeXmacs> apt repository.

  Download and import the <TeXmacs> public key :

  <\shell-code>
    wget -qO- --no-check-certificate https://ftp.texmacs.org/TeXmacs/tmftp/repos/apt/apt-texmacs.asc
    \| gpg --dearmor \| sudo tee /usr/share/keyrings/texmacs-keyring.gpg
    \<gtr\> /dev/null
  </shell-code>

  Add the texmacs repository :

  <\shell-code>
    echo "deb [signed-by=/usr/share/keyrings/texmacs-keyring.gpg]
    http://ftp.texmacs.org/TeXmacs/tmftp/repos/apt/ $(cat /etc/os-release \|
    grep UBUNTU_CODENAME \| cut -d"=" -f2) main" \| sudo tee
    /etc/apt/sources.list.d/texmacs.list \<gtr\> /dev/null
  </shell-code>

  Update the package list:

  <\shell-code>
    apt-get update
  </shell-code>

  Install <TeXmacs>:

  <\shell-code>
    apt-get install texmacs
  </shell-code>

  <section*|Troubleshooting><label|debian>

  <subsection*|How to force Ubuntu to prioritize the official <TeXmacs>
  repository>

  Run this if Ubuntu keeps trying to install an older version from its own
  repositories:

  <\shell-code>
    echo -e "Package: texmacs\\nPin: origin ftp.texmacs.org\\nPin-Priority:
    1001" \| sudo tee /etc/apt/preferences.d/texmacs \<gtr\> /dev/null
  </shell-code>

  <tmdoc-copyright|1999\U2018|Denis Raux|Joris van der Hoeven, Liza Belos>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>