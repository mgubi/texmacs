<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|A <verbatim|tmfs> primer>

  <subsection|The <TeXmacs> file system>

  Many things in <TeXmacs> can be referenced through a <abbr|URL> with
  <verbatim|tmfs> as its protocol. Examples are help pages, search results,
  the history of a file under version control, auxiliary buffers used by
  dialogs, remote files and chat rooms. Internally, <TeXmacs> also uses
  <verbatim|tmfs> <abbr|URL>s as identifiers for views and windows. A
  <TeXmacs> file system <abbr|URL> has the format

  <center|<verbatim|tmfs://class/name>>

  The first component, <verbatim|class>, selects a <em|handler>; the remainder,
  <verbatim|name>, is passed as a string to that handler and is interpreted by
  it. A handler is a set of <scheme> procedures which implement the basic
  operations on the documents of its class: loading the content, saving it
  (if this makes sense), computing a title for the window and establishing
  access permissions are the most important ones; see <hlink|the <verbatim|tmfs>
  <scheme> <abbr|API>|tmfs-api.en.tm> for the complete list.

  Handlers which the user commonly encounters are <verbatim|help>,
  <verbatim|apidoc>, <verbatim|grep>, <verbatim|history> and
  <verbatim|revision>. The format of <verbatim|name> depends on the handler:

  <\itemize>
    <item>Some handlers take a <em|query> of the form
    <verbatim|variable1=value1&variable2=value2>, which is parsed with
    <scm|query-ref> or <scm|query-\<gtr\>list>. For instance,
    <verbatim|tmfs://apidoc/type=module&what=doc.apidoc> displays the
    documentation of the <scheme> module <scm|(doc apidoc)> and
    <verbatim|tmfs://grep/type=doc&what=tmfs> searches the documentation
    for the word <verbatim|tmfs>.

    <item>Other handlers take a path of components separated by slashes,
    which is decomposed with <scm|tmfs-car> and <scm|tmfs-cdr>. Ordinary
    file names are embedded in such paths using <scm|url-\<gtr\>tmfs-string>.
    For instance, <verbatim|tmfs://help/normal/tm/doc/main/man-manual.en.tm>
    opens the file <verbatim|$TEXMACS_PATH/doc/main/man-manual.en.tm> as a
    help page, and <verbatim|tmfs://history/file/home/joe/paper.tm> shows the
    version history of <verbatim|/home/joe/paper.tm>.
  </itemize>

  The <hlink|catalogue of handlers|tmfs-handlers.en.tm> describes the syntax
  accepted by each of the existing handlers.

  Situations where using this system makes more sense than regular documents
  are for instance documentation, which must be chosen from several languages
  and possibly be compiled on the fly from various sources (see the module
  <hlink|<verbatim|doc.apidoc>|tmfs://apidoc/type=module&what=doc.apidoc> and
  related modules), and automatically generated content, like the content
  resulting from the interaction with an external version control system
  (see the handlers <verbatim|history>, <verbatim|revision> and
  <verbatim|commit> in the module
  <hlink|<verbatim|version.version-tmfs>|tmfs://apidoc/type=module&what=version.version-tmfs>).

  <subsection|Implementing a handler>

  A handler is defined via <scm|tmfs-handler> or, more conveniently, with
  the macros <scm|tmfs-load-handler>, <scm|tmfs-save-handler>,
  <scm|tmfs-title-handler>, <scm|tmfs-permission-handler>,
  <scm|tmfs-master-handler> and a few others described in the <hlink|<abbr|API>
  reference|tmfs-api.en.tm>. Only the load handler is mandatory.

  Below we implement a basic handler named <verbatim|simple> which accepts
  queries with two variables, <verbatim|type> and <verbatim|what>. We use two
  procedures, one to create the document, another one to handle the
  requests.

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tm-define (simple-load header body)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document (section ,header) ,body))))
    <|folded-io>
      \;
    </folded-io>
  </session>

  As you can see, we do not do much other than creating a <TeXmacs> document
  in <scheme> form. The load handler is not complicated either. We only parse
  the query string with the help of <scm|query-ref> and then return one of
  three possible documents.

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tmfs-load-handler (simple qry)

      \ \ (let ((type (query-ref qry "type"))

      \ \ \ \ \ \ \ \ (what (query-ref qry "what")))

      \ \ \ \ (cond ((== type "very") (simple-load "Very simple" what))

      \ \ \ \ \ \ \ \ \ \ ((== type "totally") (simple-load "Totally simple"
      what))

      \ \ \ \ \ \ \ \ \ \ (else (simple-load "Error"

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (string-append
      "Query unknown: " what))))))
    <|folded-io>
      \;
    </folded-io>
  </session>

  A load handler may return either a document in <scheme> form, as above,
  or a string holding the serialized document. We can test the handler right
  away with:

  <\session|scheme|default>
    <\input>
      Scheme]\
    <|input>
      (load-buffer "tmfs://simple/type=very&what=example")
    </input>
  </session>

  or from a document using tags like <markup|hlink> and <markup|branch>:
  <hlink|click here to test it|tmfs://simple/type=very&what=example>. Notice
  that the <abbr|URL> is loaded anew each time the buffer is opened or
  reverted: there is no cache.

  By default, a document of a class with a load handler can only be read.
  You may grant other permissions by implementing a <em|permission handler>.
  Such a handler receives the name and the kind of access requested, which
  is <verbatim|"read"> or <verbatim|"write">:

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tmfs-permission-handler (simple name type)

      \ \ (display* "Name= " name "\\nType= " type "\\n")

      \ \ (in? type (list "read" "write")))
    <|folded-io>
      \;
    </folded-io>
  </session>

  Once write access is granted, <TeXmacs> lets the user save the buffer and
  calls the <em|save handler> with the document in <scheme> form. If there is
  no save handler, saving silently does nothing.

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tmfs-save-handler (simple qry doc)

      \ \ (display* "Saving " qry "\\n")

      \ \ (set-message "Nothing to save" "simple handler"))
    <|folded-io>
      \;
    </folded-io>
  </session>

  Finally, the title of the window displaying the buffer is computed by the
  <em|title handler>. Without a title handler, the full <abbr|URL> is used as
  title.

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tmfs-title-handler (simple qry doc)

      \ \ (string-append "Simple handler - " (query-ref qry "what")))
    <|folded-io>
      \;
    </folded-io>
  </session>

  <subsection|The main handler macros>

  <\explain>
    <scm|(tmfs-load-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|define the load handler for
    <scm-arg|class>>
  <|explain>
    A <em|load handler> for <scm-arg|class> is invoked when <TeXmacs> needs
    the contents of a <abbr|URL> of the form
    <verbatim|tmfs://<scm-arg|class>/<scm-arg|name>>. The <scm-arg|body> is
    evaluated with <scm-arg|name> bound to the string following the class
    (for instance a query to be parsed with <scm|query-ref>) and must return
    a complete <TeXmacs> document, either in <scheme> form or as a string.
    Consider the following example, which is actually defined in
    <verbatim|kernel/texmacs/tm-file-system.scm>:

    <\scm-code>
      (tmfs-load-handler (id what)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document ,what))))
    </scm-code>

    This will display the text <verbatim|whatever> when opening the
    <abbr|URL> <verbatim|tmfs://id/whatever>.

    The creation of the document may be simplified using the macros defined
    in the module <hlink|<verbatim|kernel.gui.gui-markup>|tmfs://apidoc/type=module&what=kernel.gui.gui-markup>,
    such as <scm|$generic>, <scm|$tmdoc> and <scm|$tmfs-title>.
  </explain>

  <\explain>
    <scm|(tmfs-save-handler (<scm-arg|class> <scm-arg|name> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define the save handler for
    <scm-arg|class>>
  <|explain>
    A <em|save handler> is invoked when the user saves a buffer whose name
    is <verbatim|tmfs://<scm-arg|class>/<scm-arg|name>>. The argument
    <scm-arg|doc> contains the document in <scheme> form. The save command
    refuses to save the buffer unless the permission handler grants
    <verbatim|"write"> access.
    The return value of the handler is ignored; errors should be reported
    by the handler itself.
  </explain>

  <\explain>
    <scm|(tmfs-title-handler (<scm-arg|class> <scm-arg|name> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define the title handler for
    <scm-arg|class>>
  <|explain>
    A <em|title handler> is invoked to build the title of a window
    displaying a buffer of the form <verbatim|tmfs://<scm-arg|class>/<scm-arg|name>>.
    The argument <scm-arg|doc> is the loaded document, which may be used to
    extract a title from its contents. The handler is expected to return a
    string, preferably translated into the language of the user.
  </explain>

  <\explain>
    <scm|(tmfs-permission-handler (<scm-arg|class> <scm-arg|name>
    <scm-arg|kind>) <scm-arg|body>)><explain-synopsis|define the permission
    handler for <scm-arg|class>>
  <|explain>
    A <em|permission handler> decides whether the document
    <verbatim|tmfs://<scm-arg|class>/<scm-arg|name>> may be accessed in the
    way specified by <scm-arg|kind>, which is either <verbatim|"read"> (the
    document may be loaded; this is also how existence is tested) or
    <verbatim|"write"> (the document may be saved). The handler returns a
    boolean. Without a permission handler, only <verbatim|"read"> access is
    granted.
  </explain>

  <\explain>
    <scm|(tmfs-master-handler (<scm-arg|class> <scm-arg|name>)
    <scm-arg|body>)><explain-synopsis|define the master handler for
    <scm-arg|class>>
  <|explain>
    A <em|master handler> returns the <abbr|URL> which should serve as the
    <em|master> of the buffer <verbatim|tmfs://<scm-arg|class>/<scm-arg|name>>.
    After loading, the buffer master is set to this <abbr|URL> (see
    <scm|buffer-set-master>); it is used for instance to resolve relative
    hyperlinks and inclusions. The <verbatim|part> handler uses it so that a
    part of a large document behaves like the file it was extracted from.
    Without a master handler, the buffer is its own master.
  </explain>

  <\explain>
    <scm|(query-ref <scm-arg|qry> <scm-arg|var>)><explain-synopsis|return
    the value of the variable <scm-arg|var> in the query <scm-arg|qry>>
  <|explain>
    Given a query string <scm-arg|qry> of the form
    <verbatim|variable1=value1&variable2=value2>, <scm|query-ref> returns
    <verbatim|"value1"> for the <scm-arg|var> <verbatim|"variable1">, and so
    on. The empty string is returned if the variable does not occur in the
    query.
  </explain>

  <subsection|Installing the handler>

  A handler becomes available as soon as the module which defines it is
  loaded. In order to make it available from any menu item or document upon
  startup without loading your code at startup, register it in
  <verbatim|my-init-texmacs.scm> (or, for handlers which are part of
  <TeXmacs>, in <verbatim|init-texmacs.scm>) using the macro
  <scm|lazy-tmfs-handler>. The module is then loaded the first time that a
  <abbr|URL> of one of the registered classes is accessed.

  <\explain>
    <scm|(lazy-tmfs-handler <scm-arg|module>
    <scm-arg|class1> ... <scm-arg|classn>)><explain-synopsis|lazily install
    <verbatim|tmfs> handlers>
  <|explain>
    Inform <TeXmacs> that the handlers for the classes <scm-arg|class1>, ...,
    <scm-arg|classn> are defined in the module <scm-arg|module>. The
    arguments are not evaluated: <scm-arg|module> is a list of symbols (like
    <scm|(doc tmdoc)>) naming the <scheme> module, and the classes are
    symbols. For instance, <verbatim|init-texmacs.scm> contains

    <\scm-code>
      (lazy-tmfs-handler (doc tmdoc) help)
    </scm-code>

    Only the listed classes trigger the loading of the module: if a module
    defines several handlers, list all of them.
  </explain>

  <\remark>
    The classes <verbatim|view> and <verbatim|window> must not be used as
    names of handlers, since <abbr|URL>s <verbatim|tmfs://view/...> and
    <verbatim|tmfs://window/...> are used internally by <TeXmacs> as
    identifiers for views and windows. The classes of the handlers listed in
    the <hlink|catalogue|tmfs-handlers.en.tm>, such as <verbatim|aux>,
    <verbatim|id> and <verbatim|import>, are of course also taken. Finally, a
    <abbr|URL> <verbatim|tmfs://foo> without any slash after the class is
    treated as a name in the class <verbatim|file>.
  </remark>

  More details about how handlers are dispatched and how the <c++> file
  layer delegates to them can be found in <hlink|the internals of
  <verbatim|tmfs>|tmfs-internals.en.tm>.

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
