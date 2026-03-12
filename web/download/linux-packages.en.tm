<TeXmacs|2.1.5>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|Binary GNU/<name|Linux> packages
  for <TeXmacs>|<tmweb-download-links>>

  We provide our own binary <TeXmacs> packages for the following
  GNU/<name|Linux> distributions:

  <\itemize>
    <item><hlink|CentOS|#centos>.

    <item><hlink|Debian|#debian>.

    <item><hlink|Fedora|#fedora>.

    <item><hlink|Scientific Linux|#scientificlinux>.

    <item><hlink|Open Suse|#suse>.

    <item><hlink|Ubuntu|#ubuntu>.

    <item><hlink|Raspbian|#raspbian>.
  </itemize>

  Further distributions may be added progressively
  <hlink|here|ftp://ftp.texmacs.org/TeXmacs/tmftp/Linux/>.

  <strong|Important>: In some cases you need to manually install the
  additional guile package. You can download it
  <hlink|here|ftp://ftp.texmacs.org/TeXmacs/tmftp/Linux/>.

  <section*|CentOS><label|centos>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    CentOS and your processor:

    <\itemize>
      <item><TeXmacs> 2.1.5 (Qt5) for CentOS-8 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_8/TeXmacs-2.1.5.x86_64.rpm>

      <item><TeXmacs> 2.1.4 (Qt5) for CentOS-7 : <hlink|64
      bits|https://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_7/TeXmacs-2.1.4.x86_64.rpm>
    </itemize>

    <\itemize>
      <item><TeXmacs> 2.1.1 (Qt4) for CentOS-6 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_6/|TeXmacs-2.1.1.i686.rpm>>
      or <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_6/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><hlink|Other versions|https://www.texmacs.org/Download/ftp/tmftp/Linux/>
    </itemize>

    <item>Install the package using

    <\shell-code>
      yum install <merge|<TeXmacs-version-release|devel>|-*.rpm>
    </shell-code>
  </enumerate>

  <section*|Debian><label|debian>

  For Debian-based systems, we recommend using our <hlink|official APT
  repository|linux-repos.en.tm#debian>. To install TeXmacs manually using
  .deb packages, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><TeXmacs> 2.1.5 (Qt6) for Debian 13 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_13/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_13/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt6) for Debian 12 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_12/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_12/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Debian 11 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_11/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_11/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Debian 10 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_10/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_10/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Debian 9 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_9.0/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_9.0/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.1 for Debian 8 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_8.0/|TeXmacs-2.1.1.i386.deb>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_8.0/|TeXmacs-2.1.1.amd64.deb>>

      <item><TeXmacs> 2.1.1 for Debian 7 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_7.0/|TeXmacs-2.1.1.i386.deb>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_7.0/|TeXmacs-2.1.1.amd64.deb>>\ 

      <item><hlink|Other versions|https://www.texmacs.org/Download/ftp/tmftp/Linux/>
    </itemize>

    <item>As <verbatim|root>, install the package using

    <\shell-code>
      sudo apt-get install <merge|<TeXmacs-version-release|devel>|-*.deb>
    </shell-code>

    If you get complaints about missing dependencies, then run

    <\shell-code>
      sudo apt --fix-broken install
    </shell-code>
  </enumerate>

  <section*|Fedora><label|fedora>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><TeXmacs> 2.1.5 (Qt6) for Fedora 42 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_42/TeXmacs-2.1.5.x86_64.rpm>,
      <hlink|ARM|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_42/TeXmacs-2.1.5.aarch64.rpm>

      <item><TeXmacs> 2.1.5 (Qt6) for Fedora 41 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_41/TeXmacs-2.1.5.x86_64.rpm>,
      <hlink|ARM|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_41/TeXmacs-2.1.5.aarch64.rpm>

      <item><TeXmacs> 2.1.5 (Qt6) for Fedora 40 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_40/TeXmacs-2.1.5.x86_64.rpm>,
      <hlink|ARM|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_40/TeXmacs-2.1.5.aarch64.rpm>

      <item><TeXmacs> 2.1.5 (Qt6) for Fedora 39 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_39/TeXmacs-2.1.5.x86_64.rpm>,
      <hlink|ARM|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_39/TeXmacs-2.1.5.aarch64.rpm>

      <item><TeXmacs> 2.1.5 (Qt5) for Fedora 38 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_38/TeXmacs-2.1.5.x86_64.rpm>,
      <hlink|ARM|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_38/TeXmacs-2.1.5.aarch64.rpm>

      <item><TeXmacs> 2.1.5 (Qt5) for Fedora 37 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_37/TeXmacs-2.1.5.x86_64.rpm>,
      <hlink|ARM|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_37/TeXmacs-2.1.5.aarch64.rpm>

      <item><TeXmacs> 2.1.5 (Qt5) for Fedora 36 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_36/TeXmacs-2.1.5.x86_64.rpm>

      <item><TeXmacs> 2.1.2 for Fedora 35 : <hlink|64
      bits|https://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_35/TeXmacs-2.1.2.x86_64.rpm>

      <item><TeXmacs> 2.1.2 for Fedora 34 : <hlink|64
      bits|https://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_34/TeXmacs-2.1.2.x86_64.rpm>

      <item><TeXmacs> 2.1.2 for Fedora 33 : <hlink|64
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_33/|TeXmacs-2.1.2.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Fedora 32 : <hlink|64
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_32/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Fedora 31 : <hlink|64
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_31/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Fedora 30 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_30/|TeXmacs-2.1.1.i686.rpm>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_30/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Fedora 29 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_29/|TeXmacs-2.1.1.i686.rpm>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_29/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Fedora 28 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_28/|TeXmacs-2.1.1.i686.rpm>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_28/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Fedora 27 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_27/|TeXmacs-2.1.1.i686.rpm>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_27/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><hlink|Other versions|https://www.texmacs.org/Download/ftp/tmftp/Linux/>
    </itemize>

    <item>Install the package using

    <\shell-code>
      dnf install <merge|<TeXmacs-version-release|devel>|-*.rpm>
    </shell-code>
  </enumerate>

  <section*|Scientific Linux><label|scientificlinux>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><TeXmacs> 2.1.4 for Scientific Linux 7 : <hlink|64
      bits|https://www.texmacs.org/Download/ftp/tmftp/Linux/ScientificLinux_7/TeXmacs-2.1.4.x86_64.rpm>

      <item><TeXmacs> 2.1.1 for Scientific Linux 6 : \ <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ScientificLinux_6/|TeXmacs-2.1.1.i386.rpm>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ScientificLinux_6/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><hlink|Other versions|https://www.texmacs.org/Download/ftp/tmftp/Linux/>
    </itemize>

    <item>Install the package using

    <\shell-code>
      yum install <merge|<TeXmacs-version-release|devel>|-*.rpm>
    </shell-code>
  </enumerate>

  <section*|Open Suse><label|suse>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><TeXmacs> 2.1.5 (Qt5) for Open Suse Leap 15.6 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/15.6/TeXmacs-2.1.5.x86_64.rpm>

      <item><TeXmacs> 2.1.5 (Qt5) for Open Suse Leap 15.5 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/15.5/TeXmacs-2.1.5.x86_64.rpm>

      <item><TeXmacs> 2.1.5 (Qt5) for Open Suse Leap 15.4 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/15.4/TeXmacs-2.1.5.x86_64.rpm>

      <item><TeXmacs> 2.1.2 (Qt5) for Open Suse Leap 15.3 : <hlink|64
      bits|https://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_15.3/TeXmacs-2.1.2.x86_64.rpm>
    </itemize>

    <\itemize>
      <item><TeXmacs> 2.1.1 for Open Suse Leap 15.2 : <hlink|64
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_15.2/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Open Suse Leap 15.1 : <hlink|64
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_15.1/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Open Suse Leap 42.2 : <hlink|64
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_42.2/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Open Suse Leap 42.3 : <hlink|64
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_42.3/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><TeXmacs> 2.1.1 for Open Suse Tumbleweed : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Tumbleweed/|TeXmacs-2.1.1.i586.rpm>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Tumbleweed/|TeXmacs-2.1.1.x86_64.rpm>>

      <item><hlink|Other versions|https://www.texmacs.org/Download/ftp/tmftp/Linux/>
    </itemize>

    <item>Install the package using

    <\shell-code>
      dpkg -i <merge|<TeXmacs-version-release|devel>|-*.deb>
    </shell-code>
  </enumerate>

  <section*|Ubuntu><label|ubuntu>

  For Ubuntu-based systems, we recommend using our <hlink|official APT
  repository|linux-repos.en.tm#debian>. To install TeXmacs manually using
  .deb packages, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Ubuntu and your processor:

    <\itemize>
      <item><TeXmacs> 2.1.5 (Qt6) for Ubuntu 25.10 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_25.10/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt6) for Ubuntu 25.04 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_25.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt6) for Ubuntu 24.10 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_24.10/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt6) for Ubuntu 24.04 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_24.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt6) for Ubuntu 23.10 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_23.10/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt6) for Ubuntu 23.04 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_23.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Ubuntu 22.10 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_22.10/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Ubuntu 22.04 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_22.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Ubuntu 21.10 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_21.10/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Ubuntu 21.04 : <hlink|64
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_21.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Ubuntu 20.04 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_20.04/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_20.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt5) for Ubuntu 18.04 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_18.04/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_18.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt4) for Ubuntu 16.04 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_16.04/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_16.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.5 (Qt4) for Ubuntu 14.04 : <hlink|32
      bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_14.04/TeXmacs-2.1.5.i386.deb>,
      <hlink|64 bits|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_14.04/TeXmacs-2.1.5.amd64.deb>

      <item><TeXmacs> 2.1.1 (Qt4) for Ubuntu 12.04 : <hlink|32
      bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_12.04/|TeXmacs-2.1.1.i386.deb>>,
      <hlink|64 bits|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_12.04/|TeXmacs-2.1.1.amd64.deb>>

      <item><hlink|Other versions|https://www.texmacs.org/Download/ftp/tmftp/Linux/>
    </itemize>

    <item>As <verbatim|root>, install the package using

    <\shell-code>
      sudo apt-get install <merge|<TeXmacs-version-release|devel>|-*.deb>
    </shell-code>

    If you get complaints about missing dependencies, then run

    <\shell-code>
      sudo apt --fix-broken install
    </shell-code>
  </enumerate>

  <section*|Raspbian><label|raspbian>

  For Raspbian-based systems, we recommend using our <hlink|official APT
  repository|linux-repos.en.tm#debian>. To install TeXmacs manually using
  .deb packages, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Raspbian and your processor:

    <\itemize>
      <item><TeXmacs> 2.1.5 (Qt5) for Raspbian 11 :
      <hlink|ARMv7|http://www.texmacs.org/Download/ftp/tmftp/Linux/Raspbian_11/TeXmacs-2.1.5.armhf.deb>

      <item><hlink|Other versions|https://www.texmacs.org/Download/ftp/tmftp/Linux/>
    </itemize>

    <item>As <verbatim|root>, install the package using

    <\shell-code>
      sudo apt-get install <merge|<TeXmacs-version-release|devel>|-*.deb>
    </shell-code>

    If you get complaints about missing dependencies, then run

    <\shell-code>
      sudo apt --fix-broken install
    </shell-code>
  </enumerate>

  <tmdoc-copyright|1999\U2018|Denis Raux|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>