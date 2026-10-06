<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The URL system>

  <TeXmacs> uses a tree representation for urls. This allows us to generalize
  the concept of an url and allow paths and patterns to be regarded as urls
  too. There are three main types of urls:

  <\itemize-dot>
    <item>rootless urls, like a/b/c. These urls are mainly used in
    computations. For example, they can be appended to another url.

    <item>Standard rooted urls, like file:///usr or https://www.texmacs.org.
    These are the same as those used on the web.

    <item>System urls, characterized by a \Pdefault\Q root. These urls are
    similar to standard rooted urls, but they behave in a slightly different
    way with respect to concatenation. For instance
    https://www.texmacs.org/Web * file:///tmp would yield file:///tmp,
    whereas https://www.texmacs.org/Web * /tmp yields
    https://www.texmacs.org/tmp.
  </itemize-dot>

  There are several formats for parsing (and printing) urls:

  <\itemize-dot>
    <item>System format: the usual format on your operating system. On unix
    systems \P/usr/bin:/usr/local/bin\Q would be a valid url representing a
    path and on windows systems \Pc:\\windows;c:\\TeXmacs\Q would be OK.

    <item>Unix format: this format forces unix-like notation even for other
    systems like Windows. This is convenient for url's in the source code.
    The home directory <verbatim|~> and environment variables like
    <verbatim|$TEXMACS_PATH> can also be part of the url.

    <item>Standard format: the format which is used on the web. Notice that
    ftp://www.texmacs.org/pub and ftp://www.texmacs.org/pub/ represent
    different urls. The second one is represented by concating on the right
    with an empty name.
  </itemize-dot>

  When an explicit operation on urls need to be performed, like reading a
  file, the url is first \Presolved\Q into a single url with a unique name
  (modulo symbolic links) for the resource (first match). Next, the url is
  \Pconcretized\Q as a system-specific file name which is understood by the
  operating system. Note that for remote urls this may involve downloading a
  file. Concretized urls should be used quickly and not memorized, since such
  names may be the names of temporary files, which can be destroyed
  afterwards.

  From <scheme>, urls are represented by a special data type; the routines
  below also accept strings, which are converted using the <name|Unix>
  format (the system format on <name|Windows>), so that <verbatim|~>,
  environment variables and the separators <verbatim|/> and <verbatim|:>
  may be used in them.
  The <c++> implementation can be found in <source-link|System/Classes/url.cpp|src/System/Classes/url.cpp>
  and <source-link|System/Files/file.cpp|src/System/Files/file.cpp>, and the glue definitions in
  <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>.

  <subsection|Construction and conversion>

  <\explain>
    <scm|(string-\<gtr\>url <scm-arg|s>)>

    <scm|(system-\<gtr\>url <scm-arg|s>)>

    <scm|(unix-\<gtr\>url <scm-arg|s>)><explain-synopsis|parse a url>
  <|explain>
    Convert a string into a url, using the standard, the system and the unix
    format respectively. The routine <scm|(root-\<gtr\>url <scm-arg|r>)>
    creates a url with root (protocol) <scm-arg|r>, and <scm|(url-unix
    <scm-arg|dir> <scm-arg|name>)> the concatenation of two strings in the
    unix format. The routine <scm|url-\<gtr\>url> returns its argument (it
    converts a string into a url).
  </explain>

  <\explain>
    <scm|(url-\<gtr\>string <scm-arg|u>)>

    <scm|(url-\<gtr\>system <scm-arg|u>)>

    <scm|(url-\<gtr\>unix <scm-arg|u>)><explain-synopsis|print a url>
  <|explain>
    Convert a url into a string, using the standard, the system and the unix
    format respectively. The routine <scm|(url-\<gtr\>stree <scm-arg|u>)>
    returns the internal tree representation of <scm-arg|u>.
  </explain>

  <\explain>
    <scm|(url-append <scm-arg|u1> <scm-arg|u2>)>

    <scm|(url-or <scm-arg|u1> <scm-arg|u2>)><explain-synopsis|concatenation
    and alternatives>
  <|explain>
    Concatenate two urls (<abbr|i.e.> <scm-arg|u2> relative to
    <scm-arg|u1>), <abbr|resp.> build the url which stands for either
    <scm-arg|u1> or <scm-arg|u2>. The routine <scm|(url-ref <scm-arg|u>
    <scm-arg|i>)> returns the child <scm-arg|i> (<scm|1> or <scm|2>) of a
    concatenation or of an alternative: since <verbatim|a:b:c> is stored as
    <verbatim|a> or (<verbatim|b> or <verbatim|c>), its second child is
    <verbatim|b:c>. Use <scm|(url-\<gtr\>list <scm-arg|u>)> and
    <scm|(list-\<gtr\>url <scm-arg|l>)> to convert between alternatives and
    lists.
  </explain>

  <\explain>
    <scm|(url-none)>

    <scm|(url-any)>

    <scm|(url-wildcard <scm-arg|pattern>)>

    <scm|(url-parent)>

    <scm|(url-ancestor)>

    <scm|(url-pwd)><explain-synopsis|special urls>
  <|explain>
    Respectively the url which matches nothing (tested by <scm|url-none?>),
    the url which matches anything, a file name pattern with <verbatim|*>
    wildcards (such as <scm|"*.tm">), the parent directory <verbatim|..>, an
    arbitrary ancestor directory <verbatim|...>, and the current working
    directory.
  </explain>

  <\explain>
    <scm|(url-temp)>

    <scm|(url-temp-dir)>

    <scm|(url-scratch <scm-arg|prefix> <scm-arg|postfix>
    <scm-arg|i>)><explain-synopsis|temporary urls>
  <|explain>
    Return a fresh temporary file name, the directory for temporary files,
    and the first url <scm-arg|prefix><scm-arg|j><scm-arg|postfix> with
    <scm-arg|j>\<geqslant\><scm-arg|i> which does not exist in the
    <TeXmacs> scratch directory <verbatim|$TEXMACS_HOME_PATH/texts/scratch>
    (created if needed) (this is used for the names of new unnamed
    buffers, which satisfy <scm|url-scratch?>).
  </explain>

  <subsection|Navigation>

  <\explain>
    <scm|(go-to-url <scm-arg|u> . <scm-arg|opt-from>)><explain-synopsis|Jump
    to the url @u>
  <|explain>
    Opens a new buffer with the contents of the resource at <scm-arg|u>. This
    can be either a full <abbr|URL> or a file path, absolute or relative to
    the current <scm|buffer-master>. Both types of argument accept
    parameters. The second, optional argument, is an optional path for the
    cursor history.

    You can pass parameters in <scm-arg|u> in two ways: appending a hash
    <tt|#> and some text, like in <verbatim|some/path/some-file.tm#blah> will
    open the file and jump to the first label of name <tt|blah> found, if
    any; this works for <TeXmacs>, generic and <name|Html> files. The other
    possibility is the usual way in the web: append a question mark <tt|?>
    followed by pairs <tt|parameter=value>. The parameters <tt|line>,
    <tt|column> and <tt|select>, which respectively jump to the chosen
    location and select the given text at that line, are supported for the
    source code formats (<scheme>, <c++>, <name|Java>, <name|Scala>,
    <name|Python> and <name|Julia>; see <scm|source-file-post> in
    <source-link|link-navigate.scm|TeXmacs/progs/link/link-navigate.scm>).
    The parameters are ignored for <verbatim|tmfs> urls.
  </explain>

  <subsection|Predicates>

  <\explain>
    <scm|(url-concat? <scm-arg|u>)><explain-synopsis|Returns #t if @u
    contains multiple subdirs>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-concat? "a/b")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-concat? "file.ext")
      <|unfolded-io>
        #f
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-or? <scm-arg|u>)><explain-synopsis|#t if the url contains an
    alternative>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-or? "a/b:c")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-or? "a/b")
      <|unfolded-io>
        #f
      </unfolded-io>
    </session>

    On unix systems, the colon separates alternatives in the system format;
    on <name|Windows>, the semicolon is used instead. See also
    <scm|url-atomic?>, which tests whether a url consists of a single name,
    and <scm|url-expand> below.
  </explain>

  <\explain>
    <scm|(url-rooted? <scm-arg|u>)><explain-synopsis|Test whether @u is
    absolute>
  <|explain>
    Return <scm|#t> if the url is absolute. Absolute urls may be for instance
    full paths in the file system or internet <abbr|URL>s starting with a
    protocol specification like <verbatim|ftp> or <verbatim|http>. The
    <verbatim|tmfs> urls are also understood to be rooted. See also
    <scm|url-rooted-tmfs?>, <scm|url-rooted-web?> and
    <scm|url-rooted-protocol?>.
  </explain>

  <\explain>
    <scm|(url-rooted-tmfs? <scm-arg|u>)><explain-synopsis|Test whether @u
    belongs to the <TeXmacs> file system>
  <|explain>
    Return <scm|#t> if <scm-arg|u> is an <abbr|URL> of the form
    <verbatim|tmfs://...>. Such <abbr|URL>s designate documents generated by
    <scheme> handlers; see <hlink|the <TeXmacs> file
    system|tmfs/tmfs.en.tm>. The variant <scm|(url-rooted-tmfs-protocol?
    <scm-arg|u> <scm-arg|class>)> also checks that the first component after
    <verbatim|tmfs://> is <scm-arg|class>.
  </explain>

  <\explain>
    <scm|(url-descends? <scm-arg|u> <scm-arg|base>)><explain-synopsis|Test
    whether @u lies below @base>
  <|explain>
    Test whether <scm-arg|u> equals or descends from <scm-arg|base>, in a
    purely syntactic way (the urls are not resolved). If <scm-arg|base> is
    an alternative, it suffices that <scm-arg|u> descends from one of them;
    if <scm-arg|u> is an alternative, all of them must descend from
    <scm-arg|base>. The predicate <scm|(url-secure? <scm-arg|u>)> checks
    whether <scm-arg|u> descends from the <verbatim|$TEXMACS_SECURE_PATH>.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-descends? "/a/b/c.tm" "/a")
      <|unfolded-io>
        #t
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-regular? <scm-arg|u>)><explain-synopsis|Test whether the url
    refers to regular file>
  <|explain>
    Applies only to filesystem urls. Returns <scm|#t> if the url is a regular
    file, <scm|#f> otherwise. See also <scm|url-directory?> and
    <scm|url-link?>.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-regular? "$TEXMACS_PATH/LICENSE")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(url-directory? <scm-arg|u>)><explain-synopsis|Test whether the url
    refers to a directory>
  <|explain>
    Applies only to filesystem urls. Returns <scm|#t> if the url is a
    directory and it exists, <scm|#f> otherwise.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-directory? "/tmp")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-directory? "/tmp_not_exist")
      <|unfolded-io>
        #f
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-directory? "$TEXMACS_PATH/LICENSE")
      <|unfolded-io>
        #f
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(url-link? <scm-arg|u>)><explain-synopsis|Test whether the url
    refers to a symbolic link>
  <|explain>
    Applies only to filesystem urls. Returns <scm|#t> if the url is a
    symbolic link, <scm|#f> otherwise.
  </explain>

  <\explain>
    <scm|(url-test? <scm-arg|u> <scm-arg|filter>)><explain-synopsis|Test the
    type and permissions of a file>
  <|explain>
    The <scm-arg|filter> is a string of letters, all of which must be
    satisfied: <verbatim|f> (regular file), <verbatim|d> (directory),
    <verbatim|l> (symbolic link), <verbatim|r>, <verbatim|w>, <verbatim|x>
    (readable, writable, executable). Other letters are ignored, and the
    empty filter always succeeds.
  </explain>

  <\explain>
    <scm|(url-exists? <scm-arg|u>)>

    <scm|(url-exists-in-path? <scm-arg|u>)><explain-synopsis|Test whether a
    file exists>
  <|explain>
    Test whether <scm-arg|u> resolves to an existing file, <abbr|resp.>
    whether an executable of name <scm-arg|u> exists in the
    <verbatim|$PATH>.
  </explain>

  <\explain>
    <scm|(url-newer? <scm-arg|u1> <scm-arg|u2>)>

    <scm|(url-last-modified <scm-arg|u>)>

    <scm|(url-size <scm-arg|u>)><explain-synopsis|File attributes>
  <|explain>
    Test whether <scm-arg|u1> was modified more recently than <scm-arg|u2>;
    return the time of the last modification of <scm-arg|u> (in seconds
    since the epoch); return the size of the file <scm-arg|u> in bytes.
  </explain>

  <subsection|Operations>

  <\explain>
    <scm|(url-head <scm-arg|u>)><explain-synopsis|Return the directory part
    of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-head "/tmp")
      <|unfolded-io>
        \<less\>url /\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-head "/tmp/a.out")
      <|unfolded-io>
        \<less\>url /tmp\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-tail <scm-arg|u>)><explain-synopsis|Return the file name
    without path of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-tail "/tmp")
      <|unfolded-io>
        \<less\>url tmp\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-tail "/tmp/hello.tm")
      <|unfolded-io>
        \<less\>url hello.tm\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-format <scm-arg|u>)><explain-synopsis|Returns the file format
    of @u>
  <|explain>
    Determine the file format of <scm-arg|u> (such as <scm|"texmacs">,
    <scm|"latex"> or <scm|"generic">) from its suffix; for a <verbatim|tmfs>
    url, the format is given by the handler of its class (see
    <scm|tmfs-format>).
  </explain>

  <\explain>
    <scm|(url-suffix <scm-arg|u>)><explain-synopsis|Returns the suffix
    (extension) of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-suffix "/tmp/hello.tm")
      <|unfolded-io>
        "tm"
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-basename <scm-arg|u>)><explain-synopsis|Return the basename as
    string for @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-basename "/tmp")
      <|unfolded-io>
        "tmp"
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-basename "/tmp/hello.tm")
      <|unfolded-io>
        "hello"
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-glue <scm-arg|u> <scm-arg|s>)><explain-synopsis|Returns \ @u
    with suffix @s appended>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-glue (url-basename (current-buffer)) ".new")
      <|unfolded-io>
        \<less\>url url.en.new\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-unglue <scm-arg|u> <scm-arg|n>)><explain-synopsis|Removes @n
    characters from the suffix of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-unglue (current-buffer) 3) ;output edited
      <|unfolded-io>
        \<less\>url (...)/src/TeXmacs/doc/devel/scheme/api/url.en\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-relative <scm-arg|base> <scm-arg|u>)><explain-synopsis|Prepends
    the head of \ @base to @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-relative "/a/b/c.tm" "d.tm")
      <|unfolded-io>
        \<less\>url /a/b/d.tm\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-delta <scm-arg|base> <scm-arg|u>)><explain-synopsis|Computes
    the change in url from @base to @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-delta "/a/b/c/file.tm" "/a/b")
      <|unfolded-io>
        \<less\>url ../../b\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-root <scm-arg|u>)><explain-synopsis|Returns the root (protocol)
    of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-root (current-buffer))
      <|unfolded-io>
        "default"
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-root "https://www.texmacs.org")
      <|unfolded-io>
        "https"
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-unroot <scm-arg|u>)><explain-synopsis|Removes the root
    (protocol) of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-unroot "/a/b/c")
      <|unfolded-io>
        \<less\>url a/b/c\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-unroot "https://www.texmacs.org")
      <|unfolded-io>
        \<less\>url www.texmacs.org\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-expand <scm-arg|u>)>

    <scm|(url-factor <scm-arg|u>)><explain-synopsis|Distribute concatenation
    over alternatives>
  <|explain>
    The routine <scm|url-expand> rewrites <verbatim|a/(b:c)> into
    <verbatim|a/b:a/c>, and also expands the ancestor pattern
    <verbatim|...>. The routine <scm|url-factor> is the inverse operation;
    it also sorts the alternatives.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-expand (url-append "a" (url-or "b" "c")))
      <|unfolded-io>
        \<less\>url a/b:a/c\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  The <c++> routines <cpp|reroot> (change the protocol of a url) and
  <cpp|sort> (order the alternatives of a url) are not exported to
  <scheme>.

  <subsection|File operations>

  <\explain>
    <scm|(string-load <scm-arg|u>)>

    <scm|(string-save <scm-arg|s> <scm-arg|u>)>

    <scm|(string-append-to-file <scm-arg|s> <scm-arg|u>)><explain-synopsis|read
    and write files>
  <|explain>
    Return the contents of the file <scm-arg|u> as a string, <abbr|resp.>
    write the string <scm-arg|s> into <scm-arg|u>, or append it at its end.
    Saving to a <verbatim|tmfs> url goes through the handler of its class
    (see <hlink|the <TeXmacs> file system|tmfs/tmfs.en.tm>), and appending to
    a <verbatim|tmfs> url is a fatal error.
  </explain>

  <\explain>
    <scm|(system-copy <scm-arg|from> <scm-arg|to>)>

    <scm|(system-move <scm-arg|from> <scm-arg|to>)>

    <scm|(system-remove <scm-arg|u>)>

    <scm|(system-mkdir <scm-arg|u>)>

    <scm|(system-rmdir <scm-arg|u>)>

    <scm|(system-rmdir-recursive <scm-arg|u>)><explain-synopsis|operations on
    files and directories>
  <|explain>
    Copy, move or remove files, create or remove a directory (which must be
    empty for <scm|system-rmdir>), and remove a directory with all its
    contents. The routine <scm|(url-remove <scm-arg|u>)> removes a file,
    through the handler of its class for a <verbatim|tmfs> url.
  </explain>

  <\explain>
    <scm|(url-autosave <scm-arg|u> <scm-arg|suffix>)>

    <scm|(url-wrap <scm-arg|u>)>

    <scm|(url-backup <scm-arg|u>)>

    <scm|(url-backup? <scm-arg|u>)><explain-synopsis|auxiliary files of a
    document>
  <|explain>
    The routine <scm|url-autosave> returns the url where <scm-arg|u> is
    autosaved, <scm-arg|u> followed by <scm-arg|suffix>, or <scm|#f> when
    the file cannot be autosaved (for a <verbatim|tmfs> url, the handler
    decides). The routine <scm|url-wrap> returns the url of the file wrapped
    by a <verbatim|tmfs> url, if any. The routine <scm|url-backup> returns
    the name of the backup of <scm-arg|u> in
    <verbatim|$TEXMACS_HOME_PATH/texts/backup>, and <scm|url-backup?> tests
    whether a url is such a backup. These routines are defined in
    <source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>
    and <source-link|file.cpp|src/System/Files/file.cpp>.
  </explain>

  <\explain>
    <scm|(url-cache-invalidate <scm-arg|u>)><explain-synopsis|forget a
    downloaded file>
  <|explain>
    Remove the remote url <scm-arg|u> from the cache of downloaded files, so
    that it is downloaded again the next time it is used.
  </explain>

  <subsection|Resolution>

  The following routines take urls with alternatives and wildcards and look
  up the matching files. Here <scm-arg|filter> is a string as for
  <scm|url-test?>; the default filter in <c++> is <verbatim|"fr"> (existing
  readable regular files).

  <\explain>
    <scm|(url-complete <scm-arg|u> <scm-arg|filter>)><explain-synopsis|Find
    all matches>
  <|explain>
    Return the alternative of all files which match <scm-arg|u> and satisfy
    <scm-arg|filter>. For instance, <scm|(url-read-directory <scm-arg|dir>
    <scm-arg|pattern>)> is implemented using <scm|url-complete> and returns
    the list of files in <scm-arg|dir> matching the wildcard
    <scm-arg|pattern>:

    <\scm-code>
      (url-read-directory "$TEXMACS_PATH/styles" "*.ts")
    </scm-code>
  </explain>

  <\explain>
    <scm|(url-resolve <scm-arg|u> <scm-arg|filter>)><explain-synopsis|Find
    the first match>
  <|explain>
    Like <scm|url-complete>, but only return the first match, or
    <scm|(url-none)> if there is none. The variant
    <scm|(url-resolve-in-path <scm-arg|u>)> looks for an executable in the
    <verbatim|$PATH>.
  </explain>

  <\explain>
    <scm|(url-search-upwards <scm-arg|dir> <scm-arg|name>
    <scm-arg|stops>)><explain-synopsis|Find a file in a directory tree>
  <|explain>
    Look for a file called <scm-arg|name> in <scm-arg|dir> and its
    subdirectories; if there is none, look in the parent directory, and so
    on. The search goes up only while an ancestor directory has a name in
    the list of strings <scm-arg|stops>, and never beyond such a directory.
    The result is <scm|(url-none)> if nothing is found.
  </explain>

  <\explain>
    <scm|(url-grep <scm-arg|what> <scm-arg|u>)><explain-synopsis|Files which
    contain a string>
  <|explain>
    Return the alternative of the files matching <scm-arg|u> which contain
    the string <scm-arg|what> (the results are cached).
  </explain>

  <\explain>
    <scm|(url-resolve-pattern <scm-arg|u>)>

    <scm|(url-exists-in-tex? <scm-arg|u>)><explain-synopsis|Special search
    paths>
  <|explain>
    Find a background pattern, also in the directories of
    <verbatim|$TEXMACS_PATTERN_PATH>, <abbr|resp.> test whether a file can be
    found by the <TeX> tools (<verbatim|kpsewhich>).
  </explain>

  <\explain>
    <scm|(url-concretize <scm-arg|u>)>

    <scm|(url-materialize <scm-arg|u> <scm-arg|filter>)><explain-synopsis|System
    file name for a url>
  <|explain>
    The routine <scm|url-concretize> returns the system specific file name
    for a resolved url; for remote urls this may involve downloading the
    file into a temporary file. The routine <scm|url-materialize> first
    resolves <scm-arg|u> using <scm-arg|filter> and then concretizes the
    result. The variant <scm|url-concretize*> returns a url instead of a
    string, <scm|url-sys-concretize> returns a string which is escaped for
    use in shell commands, and <scm|system-url-\<gtr\>string> is the same
    as <scm|url-sys-concretize>.
  </explain>

  <tmdoc-copyright|2013\U2021|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>