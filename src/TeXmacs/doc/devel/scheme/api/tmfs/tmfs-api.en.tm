<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <verbatim|tmfs> <scheme> <abbr|API>>

  <section|Introduction>

  This page documents the <scheme> interface of the <TeXmacs> file system.
  Unless stated otherwise, the functions and macros are defined in the module
  <scm|(kernel texmacs tm-file-system)>, file
  <verbatim|src/TeXmacs/progs/kernel/texmacs/tm-file-system.scm>, and are
  available everywhere. In the descriptions, <scm-arg|class> is the first
  component of a <verbatim|tmfs> <abbr|URL> and <scm-arg|name> the rest of
  the <abbr|URL> after <verbatim|tmfs://<scm-arg|class>/>. See the <hlink|primer|tmfs-basics.en.tm>
  for an introduction and <hlink|the internals|tmfs-internals.en.tm> for the
  way these functions are used by the rest of <TeXmacs>.

  <section|Defining handlers>

  <\explain>
    <scm|(tmfs-handler <scm-arg|class> <scm-arg|action>
    <scm-arg|handle>)><explain-synopsis|register a handler>
  <|explain>
    Register the procedure <scm-arg|handle> for the operation
    <scm-arg|action> on the <abbr|URL>s of the class <scm-arg|class>.
    <scm-arg|class> is a string, or <scm|#t> for the default handler of the
    operation. <scm-arg|action> is one of the symbols <scm|load>,
    <scm|save>, <scm|autosave>, <scm|remove>, <scm|wrap>, <scm|date>,
    <scm|title>, <scm|permission?>, <scm|master> and <scm|format>; the
    arguments of <scm-arg|handle> are those of the corresponding macros
    below. Registering a handler for a class and action which already have
    one replaces the old handler.
  </explain>

  The following macros are more convenient. Each of them takes a head
  <scm|(<scm-arg|class> <scm-arg|arg1> ...)>, where <scm-arg|class> is a
  symbol and the other elements are the names of the variables to which the
  arguments of the handler are bound, followed by the body of the handler.

  <\explain>
    <scm|(tmfs-load-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|define how to load documents>
  <|explain>
    The body returns the document <verbatim|tmfs://<scm-arg|class>/<scm-arg|name>>,
    either as a <scheme> tree (<scm|(document (TeXmacs ...) (style ...) (body
    ...))>) or as a string in the format returned by <scm|tmfs-format>. It
    should never return the empty string, which is interpreted as a failure.
    This is the only mandatory handler of a class.
  </explain>

  <\explain>
    <scm|(tmfs-save-handler (<scm-arg|class> <scm-arg|name> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define how to save documents>
  <|explain>
    The body saves the document <scm-arg|doc>, given as a <scheme> tree.
    The save command of <TeXmacs> refuses to save the buffer unless the
    permission handler grants <verbatim|"write"> access. The result of the
    handler is ignored.
  </explain>

  <\explain>
    <scm|(tmfs-title-handler (<scm-arg|class> <scm-arg|name> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define window titles>
  <|explain>
    The body returns a string to be used as the title of the buffer; the
    loaded document <scm-arg|doc> is provided so that the title may be
    extracted from it. Without a title handler, the complete <abbr|URL> is
    used as title.
  </explain>

  <\explain>
    <scm|(tmfs-permission-handler (<scm-arg|class> <scm-arg|name>
    <scm-arg|kind>) <scm-arg|body>)><explain-synopsis|define access
    permissions>
  <|explain>
    The body returns a boolean which tells whether the access
    <scm-arg|kind>, either <verbatim|"read"> or <verbatim|"write">, is
    granted. Read permission is also what <scm|url-exists?> tests. Without a
    permission handler, only read access is granted, unless the class has a
    wrap handler, in which case the permissions of the wrapped file are used.
  </explain>

  <\explain>
    <scm|(tmfs-master-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|define the master of documents>
  <|explain>
    The body returns the <abbr|URL> to be used as the master of the buffer
    (see <scm|buffer-set-master>) when the document is loaded. Without a
    master handler, the buffer is its own master.
  </explain>

  <\explain>
    <scm|(tmfs-format-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|define the file format of documents>
  <|explain>
    The body returns the format of the document as a string, such as
    <verbatim|"texmacs"> or <verbatim|"png">. The format determines how the
    string produced by the load handler is parsed, and how the buffer is
    serialized before saving. Without a format handler, the format is
    <verbatim|"stm">, the <scheme> representation of <TeXmacs> documents.
    For instance, the <verbatim|revision> handler uses the format of the
    underlying file:

    <\scm-code>
      (tmfs-format-handler (revision name)

      \ \ (with u (tmfs-string-\<gtr\>url (tmfs-cdr name))

      \ \ \ \ (url-format u)))
    </scm-code>
  </explain>

  <\explain>
    <scm|(tmfs-wrap-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|declare the underlying file>
  <|explain>
    The body returns the <abbr|URL> of an ordinary file of which the
    document is a different presentation, or <scm|#f>. The wrapped file is
    used by the default permission, date, removal and autosave handlers,
    and by <scm|buffer-last-save>.
  </explain>

  <\explain>
    <scm|(tmfs-autosave-handler (<scm-arg|class> <scm-arg|name>
    <scm-arg|suf>) <scm-arg|body>)><explain-synopsis|define autosave
    locations>
  <|explain>
    The body returns the <abbr|URL> where an autosave copy of the document
    with suffix <scm-arg|suf> (<verbatim|"~"> or <verbatim|"#">) should be
    stored, or <scm|#f> if autosaving is not supported.
  </explain>

  <\explain>
    <scm|(tmfs-date-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|define modification dates>
  <|explain>
    The body returns the date of last modification of the document, in the
    same unit as <scm|url-last-modified>, or <scm|#f>.
  </explain>

  <\explain>
    <scm|(tmfs-remove-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|define removal>
  <|explain>
    The body removes the document and should return <scm|#t> on success.
  </explain>

  <\explain>
    <scm|(lazy-tmfs-handler <scm-arg|module> <scm-arg|class1> ...
    <scm-arg|classn>)><explain-synopsis|register handlers lazily>
  <|explain>
    Declare that the handlers of the classes <scm-arg|class1>, ...,
    <scm-arg|classn> (symbols) are defined in <scm-arg|module> (a list of
    symbols such as <scm|(part part-tmfs)>). None of the arguments is
    evaluated. The module is loaded when one of these classes is used for
    the first time.
  </explain>

  <\explain>
    <scm|(lazy-tmfs-force <scm-arg|class>)><explain-synopsis|load the module
    of a class>
  <|explain>
    Load the module registered for <scm-arg|class> (a string or a symbol),
    if it has not been loaded yet. All the operations below call this
    function before looking up a handler.
  </explain>

  <section|Operations on <verbatim|tmfs> <abbr|URL>s>

  The following functions take an <abbr|URL> <scm-arg|u>, as a string or an
  <abbr|URL> object, decompose it with <scm|tmfs-decompose-name> and call
  the appropriate handler, or a default behavior. Most code should use the
  generic functions for files and buffers instead, which call these
  functions for <verbatim|tmfs> <abbr|URL>s.

  <\explain>
    <scm|(tmfs-load <scm-arg|u>)><explain-synopsis|load a document>
  <|explain>
    Return the document at <scm-arg|u> as a string. Called by the <c++>
    routine <cpp|get_from_server>.
  </explain>

  <\explain>
    <scm|(tmfs-save <scm-arg|u> <scm-arg|what>)><explain-synopsis|save a
    document>
  <|explain>
    Save the string <scm-arg|what>, which is converted from
    <verbatim|stm> into a <scheme> tree before being passed to the save
    handler. Called by the <c++> routine <cpp|save_to_server>.
  </explain>

  <\explain>
    <scm|(tmfs-title <scm-arg|u> <scm-arg|doc>)><explain-synopsis|title of
    a document>
  <|explain>
    Return the title of the document <scm-arg|doc> loaded from
    <scm-arg|u>. Called by the <c++> routine <cpp|propose_title>, and by the
    menu of recent files.
  </explain>

  <\explain>
    <scm|(tmfs-permission? <scm-arg|u> <scm-arg|kind>)><explain-synopsis|check
    access permissions>
  <|explain>
    Check whether the access <scm-arg|kind> (<verbatim|"read"> or
    <verbatim|"write">) to <scm-arg|u> is permitted. Called by the <c++>
    routine <cpp|is_of_type>, hence by <scm|url-test?> and
    <scm|url-exists?>.
  </explain>

  <\explain>
    <scm|(tmfs-master <scm-arg|u>)><explain-synopsis|master of a document>
  <|explain>
    Return the <abbr|URL> which should serve as the master of the buffer
    <scm-arg|u>; by default, <scm-arg|u> itself.
  </explain>

  <\explain>
    <scm|(tmfs-format <scm-arg|u>)><explain-synopsis|file format of a
    document>
  <|explain>
    Return the file format of <scm-arg|u>; by default <verbatim|"stm">.
    Called by the <c++> routine <cpp|file_format>, hence by
    <scm|url-format>.
  </explain>

  <\explain>
    <scm|(tmfs-wrap <scm-arg|u>)><explain-synopsis|underlying file>
  <|explain>
    Return the file wrapped by <scm-arg|u>, or <scm|#f>. The generic
    function <scm|url-wrap> in <verbatim|texmacs/texmacs/tm-files.scm>
    calls it for <verbatim|tmfs> <abbr|URL>s and returns <scm|#f> for other
    <abbr|URL>s.
  </explain>

  <\explain>
    <scm|(tmfs-date <scm-arg|u>)><explain-synopsis|date of last
    modification>
  <|explain>
    Return the date of last modification of <scm-arg|u>, or <scm|#f>. Used
    by <scm|url-last-modified> and <scm|url-newer?>, which are redefined in
    <verbatim|tm-files.scm> for <verbatim|tmfs> <abbr|URL>s.
  </explain>

  <\explain>
    <scm|(tmfs-remove <scm-arg|u>)><explain-synopsis|remove a document>
  <|explain>
    Remove <scm-arg|u> and return <scm|#t> on success. Used by
    <scm|url-remove>.
  </explain>

  <\explain>
    <scm|(tmfs-autosave <scm-arg|u> <scm-arg|suf>)><explain-synopsis|autosave
    location>
  <|explain>
    Return the <abbr|URL> where the autosave copy of <scm-arg|u> with suffix
    <scm-arg|suf> is stored, or <scm|#f>. Used by <scm|url-autosave>.
  </explain>

  <\explain>
    <scm|(tmfs-can-autosave? <scm-arg|u>)><explain-synopsis|whether
    autosaving is supported>
  <|explain>
    Return <scm|#t> if <scm|(tmfs-autosave <scm-arg|u> "~")> is not
    <scm|#f>. Called by the <c++> routine <cpp|get_from_server>.
  </explain>

  <\explain>
    <scm|(tmfs-remote? <scm-arg|u>)><explain-synopsis|whether the class has
    no local load handler>
  <|explain>
    Return <scm|#t> if no load handler is known for the class of
    <scm-arg|u>, even after the lazy loading of its module.
  </explain>

  <\explain>
    <scm|(tmfs-decompose-name <scm-arg|u>)><explain-synopsis|split an
    <abbr|URL> into class and name>
  <|explain>
    Return the list <scm|(<scm-arg|class> <scm-arg|name>)> of two strings.
    The prefix <verbatim|tmfs://> is optional; if there is no slash, the
    class is <verbatim|"file">.

    <\scm-code>
      (tmfs-decompose-name "tmfs://grep/type=doc&what=tmfs")

      ;; =\<gtr\> ("grep" "type=doc&what=tmfs")
    </scm-code>
  </explain>

  <section|Queries>

  <\explain>
    <scm|(query-ref <scm-arg|qry> <scm-arg|var>)><explain-synopsis|value of
    a query variable>
  <|explain>
    Given a query string <scm-arg|qry> of the form
    <verbatim|var1=val1&var2=val2>, return the value of the variable
    <scm-arg|var>, for instance <verbatim|"val1"> for <scm-arg|var> equal
    to <verbatim|"var1">. The empty string is returned if the variable does
    not occur. The value is decoded with <scm|tmstring-\<gtr\>string>.
  </explain>

  <\explain>
    <scm|(query-\<gtr\>list <scm-arg|qry>)><explain-synopsis|parse a query>
  <|explain>
    Return the association list of the variables and values of
    <scm-arg|qry>, with the escape <verbatim|%3A> replaced by a colon.
  </explain>

  <\explain>
    <scm|(list-\<gtr\>query <scm-arg|l>)><explain-synopsis|build a query>
  <|explain>
    Build a query string from an association list of strings, replacing
    colons by <verbatim|%3A>. For instance, <verbatim|doc/docgrep.scm> opens
    search results with

    <\scm-code>
      (with query (list-\<gtr\>query (list (cons "type" "doc") (cons "what" what)))

      \ \ (load-document (string-append "tmfs://grep/" query)))
    </scm-code>
  </explain>

  <section|Paths and embedded file names>

  <\explain>
    <scm|(tmfs-pair? <scm-arg|s>)><explain-synopsis|whether a name has
    several components>
  <|explain>
    Return a true value if the string <scm-arg|s> contains a slash.
  </explain>

  <\explain>
    <scm|(tmfs-car <scm-arg|s>)><explain-synopsis|first component>
  <|explain>
    Return the part of <scm-arg|s> before the first slash, or <scm|#f> if
    there is no slash.
  </explain>

  <\explain>
    <scm|(tmfs-cdr <scm-arg|s>)><explain-synopsis|remaining components>
  <|explain>
    Return the part of <scm-arg|s> after the first slash, or <scm|#f> if
    there is no slash.
  </explain>

  <\explain>
    <scm|(tmfs-\<gtr\>list <scm-arg|s>)><explain-synopsis|split a name at
    slashes>
  <|explain>
    Return the list of the components of <scm-arg|s>.
  </explain>

  <\explain>
    <scm|(list-\<gtr\>tmfs <scm-arg|l>)><explain-synopsis|join components>
  <|explain>
    Join a list of strings with slashes; inverse of
    <scm|tmfs-\<gtr\>list>.
  </explain>

  <\explain>
    <scm|(url-\<gtr\>tmfs-string <scm-arg|u>)><explain-synopsis|encode a
    file name>
  <|explain>
    Encode the <abbr|URL> <scm-arg|u> as a string which may be embedded in a
    <verbatim|tmfs> name: <verbatim|tm/...> for files under
    <verbatim|$TEXMACS_PATH>, <verbatim|file/...> for other local files,
    <verbatim|here/...> for relative names and
    <verbatim|<em|protocol>/...> otherwise.
  </explain>

  <\explain>
    <scm|(tmfs-string-\<gtr\>url <scm-arg|s>)><explain-synopsis|decode a
    file name>
  <|explain>
    Inverse of <scm|url-\<gtr\>tmfs-string>.
  </explain>

  <\explain>
    <scm|(strip-colon <scm-arg|s>)><explain-synopsis|remove the colon of a
    drive letter>
  <|explain>
    Turn <verbatim|c:/dir> into <verbatim|c/dir>; used by
    <scm|url-\<gtr\>tmfs-string> on <name|Windows>.
  </explain>

  <section|Documents>

  <\explain>
    <scm|(tmfs-document <scm-arg|t>)><explain-synopsis|complete a document>
  <|explain>
    Convert the tree <scm-arg|t> into a <scheme> tree and, if it is a
    <markup|document> without a <markup|TeXmacs> version tag, insert the
    version of <TeXmacs>. Useful to return imported documents from load
    handlers.
  </explain>

  <\explain>
    <scm|(object-\<gtr\>tmstring <scm-arg|obj>)><explain-synopsis|serialize
    a <scheme> object>
  <|explain>
    Serialize <scm-arg|obj> as a string; used by <scm|tmfs-load> to convert
    the documents returned by load handlers.
  </explain>

  The module <scm|(kernel gui gui-markup)> provides macros which simplify
  the creation of documents: <scm|$generic> and <scm|$tmdoc> build
  complete documents with the <verbatim|generic> and <verbatim|tmdoc>
  styles, and <scm|$tmfs-title> produces a title using the
  <markup|tmfs-title> markup. For instance, the <verbatim|history> handler
  is written as

  <\scm-code>
    (tmfs-load-handler (history name)

    \ \ (let* ((u (tmfs-string-\<gtr\>url name))

    \ \ \ \ \ \ \ \ \ (h (version-history u))

    \ \ \ \ \ \ \ \ \ ...)

    \ \ \ \ ($generic

    \ \ \ \ \ ($tmfs-title "History of " ...)

    \ \ \ \ \ ($when (not h)

    \ \ \ \ \ \ \ "This file is not under version control.")

    \ \ \ \ \ ...)))
  </scm-code>

  <section|Auxiliary buffers>

  <\explain>
    <scm|(aux-name <scm-arg|aux>)><explain-synopsis|name of an auxiliary
    buffer>
  <|explain>
    Return the <abbr|URL> <verbatim|tmfs://aux/<scm-arg|aux>>.
  </explain>

  <\explain>
    <scm|(aux-set-document <scm-arg|aux> <scm-arg|doc>)><explain-synopsis|set
    the contents of an auxiliary buffer>
  <|explain>
    Set the contents of the auxiliary buffer <scm-arg|aux> to
    <scm-arg|doc> and remember them in the table <scm|aux-buffers>, so that
    the buffer can be reloaded.
  </explain>

  <\explain>
    <scm|(aux-set-master <scm-arg|aux> <scm-arg|master>)><explain-synopsis|set
    the master of an auxiliary buffer>
  <|explain>
    Set the master of the auxiliary buffer <scm-arg|aux> and remember it in
    the table <scm|aux-masters>.
  </explain>

  <\explain>
    <scm|(open-auxiliary <scm-arg|aux> <scm-arg|body>
    [<scm-arg|master>])><explain-synopsis|open an auxiliary buffer>
  <|explain>
    Defined in <verbatim|texmacs/texmacs/tm-files.scm>. Set the contents and
    the master (by default the master of the current buffer) of the
    auxiliary buffer <scm-arg|aux> and switch to it.
  </explain>

  <section|<abbr|URL> predicates>

  <\explain>
    <scm|(url-rooted-tmfs? <scm-arg|u>)><explain-synopsis|test for
    <verbatim|tmfs> <abbr|URL>s>
  <|explain>
    Return <scm|#t> if <scm-arg|u> is a <verbatim|tmfs> <abbr|URL>. Glue for
    the <c++> function <cpp|is_rooted_tmfs>.
  </explain>

  <\explain>
    <scm|(url-rooted-tmfs-protocol? <scm-arg|u>
    <scm-arg|class>)><explain-synopsis|test for a given class>
  <|explain>
    Return <scm|#t> if <scm-arg|u> is a <verbatim|tmfs> <abbr|URL> of the
    class <scm-arg|class>, as in
    <scm|(url-rooted-tmfs-protocol? u "part")>.
  </explain>

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
