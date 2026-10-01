<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Function definition and contextual overloading>

  Conventional programming languages often provide some means to overload
  certain functions depending on the types of the arguments. <TeXmacs>
  provides additional context-based overloading mechanisms, which require the
  use of the <scm|tm-define> construct for function definitions (and
  <scm|tm-define-macro> for macro definitions). Definition with
  <scm|tm-define> also allows the specification of properties of the
  function/macro: arguments, synopsis, etc.

  Furthermore, one may use <scm|tm-property> for associating additional
  properties, such as interactivity or default values for the arguments, of a
  function <em|which is already defined>, specifically functions exported
  from <c++> code through the glue.

  <\explain>
    <scm|(tm-define <scm-arg|head> <scm-args|options>
    <scm-args|body>)><explain-synopsis|<TeXmacs> function definition>

    <scm|(tm-define-macro <scm-arg|head> <scm-args|options>
    <scm-args|body>)><explain-synopsis|<TeXmacs> macro definition>
  <|explain>
    <TeXmacs> function and macro declarations are similar to usual
    declarations based on <scm|define> and <scm|define-macro>, except for the
    additional list of <scm-arg|options> and the fact that all functions and
    macros defined using <scm|tm-define> and <scm|tm-define-macro> are
    public. Each option is of the form <scm|(:<scm-arg|kind>
    <scm-args|arguments>)> and the <scm-arg|body> starts at the first element
    of the list following <scm-arg|head> which is not of this form. Available
    options are <scm|:mode>, <scm|:require> and <scm|:applicable> (for
    contextual overloading), <scm|:type>, <scm|:synopsis>, <scm|:synopsis*>,
    <scm|:returns>, <scm|:note>, <scm|:argument>, <scm|:default>,
    <scm|:proposals>, <scm|:secure>, <scm|:check-mark>, <scm|:interactive>
    and <scm|:balloon>. The implementation can be found in
    <verbatim|kernel/texmacs/tm-define.scm>.
  </explain>

  <\explain>
    <scm|(tm-property <scm-arg|head> <scm-args|options>)><explain-synopsis|<TeXmacs>
    properties definition>
  <|explain>
    <scm|tm-property> allows the declaration of <TeXmacs> properties for
    functions which have already been defined, specifically for functions
    exported through the glue. Available options are the same as for
    <scm|tm-define>, except for the ones used for contextual overloading.
  </explain>

  <paragraph*|Contextual overloading>

  We will first describe the most important <scm|:require> option for
  contextual overloading, which was already discussed
  <hlink|before|../overview/overview-overloading.en.tm>.

  <\explain>
    <scm|(:require <scm-arg|cond>)><explain-synopsis|argument based
    overloading>
  <|explain>
    This option specifies that one necessary condition for the declaration to
    be valid is that the condition <scm-arg|cond> is met. This condition may
    involve the arguments of the function.

    As an example, let us consider the following definitions:

    <\scm-code>
      (tm-define (special t)

      \ \ (and-with p (tree-outer t)

      \ \ \ \ (special p)))

      \;

      (tm-define (special t)

      \ \ (:require (tree-is? t 'frac))

      \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0))))

      \;

      (tm-define (special t)

      \ \ (:require (tree-is? t 'rsub))

      \ \ (tree-set! t `(rsup ,(tree-ref t 0))))
    </scm-code>

    The default implementation of <scm|special> is to apply <scm|special> to
    the parent <scm|p> of <scm|t> as long as <scm|t> is not the entire
    document itself. The two overloaded cases apply when <scm|t> is either a
    fraction or a right subscript.

    Assuming that your cursor is inside a fraction inside a subscript,
    calling <scm|special> will swap the numerator and the denominator. On the
    other hand, if your cursor is inside a subscript inside a fraction, then
    calling <scm|special> will change the subscript into a superscript.

    When the conditions of several (re)declarations are met, then the last
    redeclaration will be used. Inside a redeclaration, one may also use the
    <scm|former> keyword in order to explicitly access the former value of
    the redefined symbol.
  </explain>

  <\explain>
    <scm|(:mode <scm-arg|mode>)><explain-synopsis|mode-based overloading>
  <|explain>
    This option is similar to <scm|(:require (<scm-arg|mode>))> and
    specifies that the definition is only valid when we are in a given
    <scm-arg|mode>. Here <scm-arg|mode> is a mode predicate such as
    <scm|in-math?> or <scm|in-prog-scheme?>. Contrary to conditions
    specified using <scm|:require>, mode conditions do not depend on the
    arguments of the function, and definitions for more specific modes
    automatically take precedence over definitions for more general
    modes. New modes are defined using <scm|texmacs-modes> and modes
    can inherit from other modes.
  </explain>

  <\explain>
    <scm|(texmacs-modes . <scm-arg|modedefs>)> <explain-synopsis|define new
    texmacs modes>
  <|explain>
    Use this macro to define new modes that you can use for contextual
    overloading, for instance in <scm|kbd-map>. Modes may be made dependent
    on other modes. This macro takes a variable number of definitions as
    arguments, each of the form <scm|(mode-name conditions . dependencies)>.
    End your <scm|mode-name> and any dependencies with one <scm|%>; the
    macro then defines a predicate whose name is obtained by replacing
    <scm|%> by <scm|?> (<abbr|e.g.> <scm|in-verbatim?> below). The
    <scm-arg|conditions> may be <scm|#t> for modes which only depend on
    other modes. For instance:

    <\scm-code>
      (texmacs-modes

      \ \ (in-verbatim% (inside? 'verbatim) in-text%)

      \ \ (in-tt% (inside? 'tt)))
    </scm-code>

    When creating new modes remember to place first the faster checks
    (against booleans, etc.) for speed.
  </explain>

  <paragraph*|Other options for function and macro declarations>

  Besides the contextual overloading options, the <scm|tm-define> and
  <scm|tm-define-macro> primitives admit several other options for attaching
  additional information to the function or macro. We will now describe these
  options and explain how the additional information attached to functions
  can be exploited.

  <\warning>
    A current limitation of the implementation is that functions overloaded
    using <scm|:require> and <scm|:mode> cannot have different options. This
    means in particular that you cannot specify different values for
    <scm|:synopsis> depending on the context.
  </warning>

  <\explain>
    <scm|(:synopsis <scm-arg|short-help>)><explain-synopsis|short
    description>
  <|explain>
    This option gives a short description of the function or macro, in the
    form of a string <scm-arg|short-help>. As a convention, <scheme>
    expressions may be encoded inside this string by using the
    <verbatim|@>-prefix. For instance:

    <\scm-code>
      (tm-define (list-square l)

      \ \ (:synopsis "Appends the list @l to itself")

      \ \ (append l l))
    </scm-code>

    The synopsis of a function is used for instance in order to provide a
    short help string for the function. In the future, we might also use it
    for help balloons describing menu items.
  </explain>

  <\explain>
    <scm|(:argument <scm-arg|var> <scm-arg|description>)>

    <scm|(:argument <scm-arg|var> <scm-arg|type>
    <scm-arg|description>)><explain-synopsis|argument description>
  <|explain>
    This option gives a short <scm-arg|description> of one of the arguments
    <scm-arg|var> to the function or macro. Such a description is used for
    instance for the prompts, when calling the function interactively. For
    these uses, the second format allows for the specification of a
    <scm-arg|type> (a string or a symbol) which changes how the
    widgets/prompts work. The default type is <scm|"string">. Types ending
    with <scm|file> (such as <scm|smart-file>) and the type
    <scm|"directory"> allow the user to choose a file or directory, and tab
    completion in the interactive prompt will traverse the file system. For
    instance, <scm|load-buffer> in <verbatim|texmacs/texmacs/tm-files.scm> is
    declared using <scm|(:argument name smart-file "File name")>.
  </explain>

  <\explain>
    <scm|(:default <scm-arg|var> <scm-args|body>)>

    <scm|(:proposals <scm-arg|var> <scm-args|body>)><explain-synopsis|default
    values and proposals for arguments>
  <|explain>
    When calling the function interactively, the argument <scm-arg|var> is
    proposed with the default value obtained by evaluating <scm-arg|body>,
    <abbr|resp.> with the list of proposals obtained by evaluating
    <scm-arg|body>.
  </explain>

  <\explain>
    <scm|(:interactive #t)><explain-synopsis|interactive functions>
  <|explain>
    Indicates that the function is interactive, <abbr|i.e.> that it may
    prompt the user for further input. In menus, the names of the
    corresponding entries are followed by dots.
  </explain>

  <\explain>
    <scm|(:check-mark <scm-arg|text> <scm-arg|pred?>)>

    <scm|(:balloon <scm-arg|fun>)><explain-synopsis|menu decorations>
  <|explain>
    The <scm|:check-mark> option specifies a check mark <scm-arg|text> (such
    as <scm|"*"> or <scm|"v">) and a predicate <scm-arg|pred?> which
    determines whether menu entries calling the function should be marked;
    the predicate is applied to the same arguments as the function. The
    <scm|:balloon> option specifies a function which is applied to the same
    arguments and which should return the text of the help balloon for menu
    entries calling the function. By default, the synopsis is used as the
    help balloon.
  </explain>

  <\explain>
    <scm|(:secure #t)><explain-synopsis|secure functions>
  <|explain>
    Declares the function to be secure, so that it can be called from
    untrusted documents (for instance through <markup|action> tags).
  </explain>

  <\explain>
    <scm|(:returns <scm-arg|description>)><explain-synopsis|return value
    description>
  <|explain>
    This option gives a short <scm-arg|description> of the return value of
    the function or macro.
  </explain>

  <\explain>
    <scm|(:type (-\<gtr\> <scm-arg|from> <scm-arg|to>))><explain-synopsis|type
    conversion description>
  <|explain>
    This option specifies that a function or macro performs a conversion from
    the data type <scm-arg|from> to the data type <scm-arg|to>.
  </explain>

  <tmdoc-copyright|2007--2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>