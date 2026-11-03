<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Logical programming extensions>

  The <scheme> kernel of <TeXmacs> contains a small <name|Prolog>-like
  engine for logical programming, which is implemented in the modules
  <verbatim|kernel/logic/logic-*.scm>. It is mainly used for storing
  declarative data, such as tables and groups of symbols, and for the
  inheritance relations between modes (see <hlink|contextual
  overloading|utils-overload.en.tm>). By convention, the names of logical
  predicates end with a <verbatim|%>.

  <paragraph|Rules and queries>

  <\explain>
    <scm|(logic-rules <scm-arg|rule> ...)><explain-synopsis|declare rules>
  <|explain>
    Each <scm-arg|rule> is a list <scm|(<scm-arg|head> <scm-arg|cond>
    ...)>, meaning that <scm-arg|head> holds if all conditions
    <scm-arg|cond> hold (facts are rules without conditions). Variables are
    written as quoted symbols <scm|'x>. A special rule <scm|(assume
    <scm-arg|cond> ...)> adds the given conditions to all subsequent rules
    in the same declaration. For instance (from
    <source-link|kernel/logic/logic-test.scm|TeXmacs/progs/kernel/logic/logic-test.scm>):

    <\scm-code>
      (logic-rules

      \ \ ((sun% Joris Piet))

      \ \ ((sun% Piet Opa))

      \ \ ((daughter% Geeske Opa))

      \ \ ((child% 'x 'y) (sun% 'x 'y))

      \ \ ((child% 'x 'y) (daughter% 'x 'y))

      \ \ ((descends% 'x 'y) (child% 'x 'y))

      \ \ ((descends% 'x 'z) (child% 'x 'y) (descends% 'y 'z)))
    </scm-code>

    The rules are quasi-quoted, so that values can be inserted using
    <scm|unquote>. The macro <scm|(logic-rule <scm-arg|head> <scm-arg|cond>
    ...)> declares a single rule.
  </explain>

  <\explain>
    <scm|(logic-query <scm-arg|goal> <scm-arg|extra> ...)>

    <scm|(query <scm-arg|goal> <scm-arg|extra> ...)><explain-synopsis|query
    the rule base>
  <|explain>
    Return the list of all solutions of <scm-arg|goal>; each solution is an
    association list with the values of the free variables of
    <scm-arg|goal>. The <scm-arg|extra> arguments are additional facts
    which are assumed to hold during the query. The macro
    <scm|logic-query> quotes its arguments, whereas the function
    <scm|query> evaluates them. With the above rules,
    <scm|(logic-query (descends% Joris 'x))> returns the two solutions with
    <scm|x> bound to <scm|Piet> and <scm|Opa>.
  </explain>

  <\explain>
    <scm|(logic-test? <scm-arg|name> <scm-arg|arg> ...)><explain-synopsis|test
    a relation>
  <|explain>
    Test whether the relation <scm|(<scm-arg|name> <scm-arg|arg> ...)>
    holds. The results are cached, which makes repeated tests cheap; the
    underlying function is <scm|logic-holds?>.
  </explain>

  <paragraph|Groups, tables and dispatchers>

  The following macros provide convenient interfaces for common kinds of
  declarative data.

  <\explain>
    <scm|(logic-group <scm-arg|name> <scm-arg|member> ...)>

    <scm|(logic-in? <scm-arg|x> <scm-arg|name>)><explain-synopsis|groups>
  <|explain>
    Declare <scm-arg|member> ... to belong to the group <scm-arg|name>,
    <abbr|resp.> test whether <scm-arg|x> belongs to the group. Groups may
    be extended by further <scm|logic-group> declarations. Elements of
    other groups can be included using rules of the form <scm|((<scm-arg|name>
    'x) (<scm-arg|other-group> 'x))> in <scm|logic-rules>.
  </explain>

  <\explain>
    <scm|(logic-table <scm-arg|name> (<scm-arg|key> <scm-arg|value>)
    ...)>

    <scm|(logic-ref <scm-arg|name> <scm-arg|key>)>

    <scm|(logic-ref-list <scm-arg|name> <scm-arg|key>)><explain-synopsis|tables>
  <|explain>
    Declare a table <scm-arg|name> which associates values to keys. A key
    of the form <scm|(:or <scm-arg|key-1> ... <scm-arg|key-n>)> associates
    the same value to several keys. The macro <scm|logic-ref> returns the
    unique value associated to <scm-arg|key> (or <scm|#f>), whereas
    <scm|logic-ref-list> returns the list of all associated values. For
    instance, <source-link|utils/base/environment.scm|TeXmacs/progs/utils/base/environment.scm> declares

    <\scm-code>
      (logic-table env-var-description%

      \ \ ("color" "Foreground colour")

      \ \ ("bg-color" "Background colour")

      \ \ ...)
    </scm-code>

    and <scm|(logic-ref env-var-description% "color")> returns
    <scm|"Foreground colour">.
  </explain>

  <\explain>
    <scm|(logic-dispatcher <scm-arg|name> (<scm-arg|key> <scm-arg|fun>)
    ...)>

    <scm|(logic-dispatch <scm-arg|name> <scm-arg|key> <scm-arg|arg>
    ...)><explain-synopsis|dispatchers>
  <|explain>
    A dispatcher is a table which associates functions to keys; contrary
    to <scm|logic-table>, the values <scm-arg|fun> are evaluated. Converters
    such as <source-link|convert/html/tmhtml.scm|TeXmacs/progs/convert/html/tmhtml.scm> use dispatchers to associate
    a conversion routine to each tag, which is then retrieved using
    <scm|logic-ref>. The macro <scm|logic-dispatch> calls the function
    associated to <scm-arg|key> on the arguments <scm-arg|arg> ...; it is
    currently not used in the <TeXmacs> sources and its behaviour when no
    argument <scm-arg|arg> is given is somewhat peculiar (the function
    associated to <scm|(car <scm-arg|key>)> is applied to <scm-arg|key>
    itself).
  </explain>

  <tmdoc-copyright|2005--2026|Joris van der Hoeven, the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
