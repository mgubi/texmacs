<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Scheme objects in C++>

  <section|The class <cpp|object>>

  <\explain>
    <cpp|class object><explain-synopsis|a <scheme> value seen from <c++>>
  <|explain>
    Declared in <verbatim|Scheme/scheme.hpp> as a <cpp|CONCRETE> handle;
    its representation <cpp|tmscm_object_rep> (<verbatim|Scheme/Scheme/object.hpp>)
    holds the <name|Guile> value. Objects are built from the usual
    <TeXmacs> types:

    <\description>
      <item*|<cpp|object ()>>The empty list <verbatim|()>, also returned by
      <cpp|null_object ()>.

      <item*|<cpp|object (bool)>, <cpp|object (int)>, <cpp|object
      (double)>>Booleans and numbers.

      <item*|<cpp|object (const char*)>, <cpp|object (string)>>A <scheme>
      <em|string>. Use <cpp|symbol_object (s)> for a symbol.

      <item*|<cpp|object (tree)>, <cpp|object (url)>, <cpp|object
      (modification)>, <cpp|object (patch)>>Black boxes.

      <item*|<cpp|object (path)>, <cpp|object (list\<less\>string\<gtr\>)>,
      <cpp|object (list\<less\>tree\<gtr\>)>, <cpp|object
      (array\<less\>double\<gtr\>)>>Lists of the converted elements.
    </description>

    The constructor <cpp|object (void*)> is declared but deliberately left
    undefined: without it, any pointer would silently be converted to
    <cpp|bool> and become <verbatim|#t>.

    Lists are built and taken apart with <cpp|cons>, <cpp|list_object> (one
    to three elements), <cpp|as_list_object (array\<less\>object\<gtr\>)>,
    <cpp|car>, <cpp|cdr> and the compositions up to <cpp|cadddr>. The
    operators <cpp|==> and <cpp|!=> use <scm|equal?>, <cpp|hash> calls the
    <scheme> function <scm|hash>, and printing an object on <cpp|cout> or
    <cpp|cerr> calls <scm|write> or <scm|write-err>.
  </explain>

  <section|Protection from the garbage collector>

  A <name|Guile> value referenced only from <c++> memory would be freed by
  the garbage collector. Every <cpp|tmscm_object_rep> therefore registers
  its value in the <scheme> list <scm|object-stack>, created by
  <cpp|initialize_scheme>: the constructor conses a cell <verbatim|(<em|value>)>
  onto the front of that list and keeps a pointer <cpp|handle> to the
  new link; <cpp|object_to_tmscm> returns the value through this handle.

  The destructor may run while the garbage collector is active, so it must
  not touch <scheme> data. It only pushes the handle on the <c++> list
  <cpp|destroy_list>. The next time an object is constructed, the pending
  handles are cleared (their value is replaced by <verbatim|()>) and the
  cleared links which follow them are removed from <scm|object-stack>.
  Values held by destroyed objects thus stay alive until the next object
  is created.

  <section|Predicates and conversions>

  The predicates <cpp|is_null>, <cpp|is_list>, <cpp|is_bool>,
  <cpp|is_int>, <cpp|is_double>, <cpp|is_string>, <cpp|is_symbol>,
  <cpp|is_tree>, <cpp|is_path>, <cpp|is_url>, <cpp|is_array_double>,
  <cpp|is_array_string>, <cpp|is_modification>, <cpp|is_patch> and
  <cpp|is_widget> test the type of the value.

  The conversions <cpp|as_<em|type>> are lenient: if the value has the
  wrong type, most of them return a neutral value instead of failing.

  <\description>
    <item*|Neutral value on mismatch><cpp|as_bool> (<cpp|false>),
    <cpp|as_int> (<cpp|0>), <cpp|as_double> (<cpp|0.0>), <cpp|as_string>
    and <cpp|as_symbol> (<verbatim|"">), <cpp|as_tree> (an empty tree),
    <cpp|as_list_string>, <cpp|as_list_tree>, <cpp|as_path> (empty),
    <cpp|as_url> (<verbatim|url ("")>), <cpp|as_modification> (an
    assignment of <verbatim|""> at the root), <cpp|as_patch> (an empty
    compound patch), <cpp|as_widget> (a nil widget).

    <item*|Assertion on mismatch><cpp|as_array_object> (the value must be
    a list), <cpp|as_array_double>, <cpp|as_array_string>.

    <item*|Other><cpp|as_scheme_tree> converts any value to a
    <cpp|scheme_tree> (see <hlink|the glue|scheme-bridge-glue.en.tm>);
    <cpp|as_command> and <cpp|as_promise_widget> wrap the value without
    checking it (see below).
  </description>

  <cpp|tree_to_stree>, <cpp|stree_to_tree>, <cpp|string_to_object> and
  <cpp|object_to_string> call the <scheme> functions <scm|tree-\<gtr\>stree>,
  <scm|stree-\<gtr\>tree>, <scm|string-\<gtr\>object> and
  <scm|object-\<gtr\>string>; <cpp|content_to_tree> converts directly in
  <c++>.

  <section|Evaluating and calling <scheme> code>

  <\description>
    <item*|<cpp|eval (string expr)>>Parses and evaluates a string (through
    <cpp|eval_scheme>). <cpp|eval (object expr)> instead calls the
    <scheme> function <scm|eval> on an expression which is already a
    <scheme> value.

    <item*|<cpp|eval_file (string)>, <cpp|exec_file (url)>>Load a file.
    <cpp|exec_file> is the one used for the initialization files of the
    server and of new views (<verbatim|Texmacs/Server/tm_server.cpp>,
    <verbatim|Texmacs/Data/new_view.cpp>).

    <item*|<cpp|call (fun, a1, ..., a4)>, <cpp|call (fun,
    array\<less\>object\<gtr\>)>>Apply a function. <cpp|fun> may be an
    <cpp|object> (a procedure) or a <cpp|string> or <cpp|const char*>. In
    the latter case the string is <em|evaluated> on each call (with
    <cpp|eval_scheme>), so it is usually the name of a function, but may be
    any expression which yields a procedure.

    <item*|<cpp|scheme_cmd (s)>>Turns a string or an expression into a
    procedure without arguments, <verbatim|(lambda () <em|s>)>; this is the
    usual argument of <cpp|exec_delayed>.
  </description>

  All of these go through the error catching of the back-end: if the
  <scheme> code fails, the error is printed on the console and the result
  is a pair <verbatim|(<em|key> . <em|args>)> (see <hlink|the
  interpreter|scheme-bridge-interpreter.en.tm>). Combined with the lenient
  conversions, a failed <cpp|as_int (call ("f"))> silently yields 0.

  <section|Commands and promises>

  <\explain>
    <cpp|command as_command (object fun)><explain-synopsis|a <c++> command
    which calls a <scheme> procedure>
  <|explain>
    Returns an <cpp|object_command_rep>, a <cpp|command_rep> holding the
    procedure. Applying the command without arguments calls the procedure
    without arguments; <cpp|apply (cmd, args)>
    (<verbatim|Kernel/Abstractions/command.hpp>) passes the elements of
    the list <cpp|args> as arguments. This is how <scheme> callbacks are
    passed to widgets, timers and dialogs.

    From <scheme>, <scm|object-\<gtr\>command> creates such a command,
    <scm|command-eval> and <scm|command-apply> run one, and commands
    created in <c++> are passed to <scheme> as black boxes.
  </explain>

  <cpp|as_promise_widget (fun)> similarly wraps a procedure without
  arguments as a <cpp|promise\<less\>widget\<gtr\>>, whose evaluation calls
  the procedure and fails if the result is not a widget.

  <section|Delayed execution>

  <cpp|exec_delayed (cmd)> appends a procedure without arguments to a
  queue which is processed by the event loop, in the order of insertion.
  With <name|Qt> the queue is <cpp|command_queue> in
  <verbatim|Plugins/Qt/qt_gui.cpp>; with the other toolkits it is the
  static queue of <verbatim|Scheme/Scheme/object.cpp>, processed by
  <cpp|exec_pending_commands> from the interpose handler. Both
  implementations behave in the same way:

  <\itemize>
    <item>a command scheduled with <cpp|exec_delayed> runs once, at the
    next pass;

    <item>a command scheduled with <cpp|exec_delayed_pause> also runs at
    the next pass but, if it returns an integer <math|n>, it is scheduled
    again to run <math|n> milliseconds later (0 means at the next pass);
    any other result removes it from the queue.
  </itemize>

  The <scheme> macro <scm|delayed> (<verbatim|kernel/texmacs/tm-dialogue.scm>)
  is built on the second form: options such as <scm|:pause>,
  <scm|:idle>, <scm|:every>, <scm|:while> or <scm|:refresh> compile to a
  procedure which returns the number of milliseconds left as long as its
  condition is not met. A comment in that file notes that <scm|:idle>, which
  waits until the user has been inactive for some time, does not work in
  headless mode. <cpp|clear_pending_commands> empties
  the queue; it is called when <TeXmacs> quits.

  <section|Protected calls>

  <cpp|protected_call (cmd)>, exported as <scm|protected-call>, is used
  by the menu code (<verbatim|kernel/gui/menu-widget.scm>,
  <verbatim|menu-convert.scm>) to run the action of a menu entry. It
  brackets the call with <cpp|before_menu_action> and
  <cpp|after_menu_action> of the current editor, and, if a <c++>
  exception is raised, calls <cpp|cancel_menu_action> and reports it with
  <cpp|handle_exceptions> (see <hlink|fatal errors and crash
  handling|server-startup.en.tm>).

  <section|Preferences>

  <cpp|get_preference>, <cpp|set_preference> and <cpp|notify_preference>
  are also declared in <verbatim|scheme.hpp>, because the preferences are
  managed in <scheme>. Before <cpp|notify_preferences_booted> has been
  called, they fall back on the <c++> table of user preferences
  (<cpp|get_user_preference>, <cpp|set_user_preference>), which is what
  allows the main program to read preferences before the interpreter
  runs.

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
