<TeXmacs|2.1.5>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|Installing <TeXmacs> for
  <name|Linux>|<tmweb-download-links>>

  \;

  <center|<center|<image|../images/Download-TeXmacs.png|600px|||>>>

  <tabular|<tformat|<cwith|1|3|1|1|cell-hyphen|n>|<cwith|1|-1|2|2|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-width|>|<cwith|1|-1|1|1|cell-hmode|auto>|<cwith|1|-1|2|2|cell-lsep|1spc>|<cwith|1|-1|2|2|cell-bsep|1em>|<cwith|1|-1|2|2|cell-tsep|1em>|<cwith|2|2|1|1|cell-hyphen|n>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|2|2|1|1|cell-width|>|<cwith|2|2|1|1|cell-hmode|auto>|<cwith|2|2|2|2|cell-lsep|1spc>|<cwith|2|2|2|2|cell-bsep|1em>|<cwith|2|2|2|2|cell-tsep|1em>|<table|<row|<cell|<item-pic|../images/appimage.png>>|<\cell>
    <strong|An AppImage for <TeXmacs>><vspace|0.5fn>

    The recommended generic way to install <TeXmacs> on GNU/<name|Linux>
    distributions with no dedicated <TeXmacs> packages is to use an
    <hlink|<name|AppImage>|https://appimage.org/>. For this, you should first
    download the <hlink|<TeXmacs> 2.1.5 (Qt6) <name|AppImage> for 64 bit
    GNU/<name|Linux> distributions|http://www.texmacs.org/Download/ftp/tmftp/generic/TeXmacs-2.1.5-x86_64.AppImage>.<vspace|0.5fn>

    After downloading, simply move the <name|AppImage> to your desktop or any
    other convenient location and double click on it to launch
    <TeXmacs>.<vspace|0.5fn>

    On some older versions of GNU/<name|Linux>, you may need to give your
    system the permissions to execute the <name|AppImage>. In that case, open
    a terminal and type the following command in the directory that contains
    the <name|AppImage>:

    <\shell-code>
      chmod a+x <merge|<TeXmacs-version-release|devel>|.x86_64.AppImage>
    </shell-code>

    After that, double clicking on <name|AppImage> should launch <TeXmacs>.

    <hlink|Older AppImage versions are available
    here.|https://www.texmacs.org/Download/ftp/tmftp/generic/>
  </cell>>|<row|<cell|<item-pic|../images/install-method.png>>|<\cell>
    <strong|Packages for specific GNU/<name|Linux>
    distributions><vspace|0.5fn>

    Depending on your GNU/<name|Linux> distribution, you may choose between
    the following installation methods:

    <\itemize>
      <item>For Debian, Ubuntu and Raspbian, we recommend installing TeXmacs
      <hlink|from our official APT repository|linux-repos.en.tm#debian>. This
      ensures TeXmacs updates automatically alongside your system.

      <item>We provide ready-to-install packages for
      <hlink|CentOS|linux-packages.en.tm#centos>,
      <hlink|Debian|linux-packages.en.tm#debian>,
      <hlink|Fedora|linux-packages.en.tm#fedora>, <hlink|Scientific
      Linux|linux-packages.en.tm#scientificlinux>, <hlink|Open
      Suse|linux-packages.en.tm#suse>, <hlink|Ubuntu|linux-packages.en.tm#ubuntu>,
      <hlink|Raspbian|linux-packages.en.tm#raspbian>.

      <item>Some distributions (such as <hlink|Gentoo|http://www.gentoo.org/>)
      actively support <TeXmacs>, in which case you may directly install
      <TeXmacs> using the standard tools of your system.
    </itemize>
  </cell>>|<row|<cell|<item-pic|../images/Book_icon_1.png>>|<\cell>
    <strong|Learning <TeXmacs>>

    <\itemize>
      <item>Get started by watching our introductory
      <hlink|videos|../home/videos.en.tm>.

      <item>Or by reading one of the <TeXmacs>
      <hlink|tutorials|../help/tutorial.en.tm>.

      <item>For more information, please consult the <hlink|user
      manual|../help/book.en.tm>.
    </itemize>
  </cell>>|<row|<cell|<item-pic|../images/FAQ_icon.svg.png>>|<\cell>
    <strong|Any questions?>

    <\itemize>
      <item><hlink|Frequently asked questions|../help/faq.en.tm>.

      <item>Ask questions on the <hlink|<verbatim|texmacs-users> mailing
      list|../home/ml.en.tm#tmusers>.
    </itemize>
  </cell>>|<row|<cell|<item-pic|../images/Crystal_Project_money.png>>|<\cell>
    <strong|Donate>

    <\itemize>
      <item>If you like <TeXmacs>, then please consider
      <hlink|donating|../contribute/donations.en.tm> money or services to us.
    </itemize>
  </cell>>>>>

  <tmdoc-copyright|1999\U2019|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>