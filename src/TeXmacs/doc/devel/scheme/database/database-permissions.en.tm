<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Users, groups, and permissions>

  The additional layer <verbatim|database/db-users.scm> allows you to specify
  read, write and administration permissions for database entries. There are
  two main types of users of <TeXmacs> databases: individual users (entries
  of type <scm|"user">) and groups (entries of type <scm|"group">). Groups
  may delegate permissions to individual users or other groups. In addition,
  the special user <scm|#t> stands for the super user, which has all
  permissions, and the special value <scm|"all"> in a permission field grants
  the corresponding permission to everybody.

  Users are identified by the identifiers of their entries in the user
  database <verbatim|$TEXMACS_HOME_PATH/users/users-master.tmdb>; routines
  such as <scm|(get-default-user)>, <scm|(pseudo-\<gtr\>user
  <scm-arg|pseudo>)> and <scm|(user-\<gtr\>pseudo <scm-arg|uid>)> allow for
  the conversion between identifiers and pseudos.

  <paragraph|Macros for context specification>

  <\explain>
    <scm|(with-user user . body)><explain-synopsis|specify current user>
  <|explain>
    Evaluate the <scm|body> using the permissions of the specified
    <scm|user>, which is either a user identifier, a list of user
    identifiers, or <scm|#t> (the default, meaning no restrictions).
  </explain>

  <paragraph|Special attributes>

  <\description>
    <item*|<scm|pseudo>>Specifies a pseudo for the user. Notice that
    <scm|name> should specify the full name.

    <item*|<scm|owner>, <scm|readable>, <scm|writable>>Ownership and
    read/write permissions for an entry. Notice that the wrappers below only
    check the <scm|owner> and <scm|readable> permissions: modifications
    require ownership. Under the encoding scheme <scm|:pseudos>, the values
    of these fields are represented by user pseudos instead of user
    identifiers.

    <item*|<scm|delegate-owner>, <scm|delegate-readable>,
    <scm|delegate-writable>>Delegate group permissions: a user (or group)
    listed in the <scm|delegate-readable> field of a group inherits the
    <scm|readable> permissions of that group, and similarly for the other
    attributes.
  </description>

  <paragraph|Affected routines of the database API>

  <\explain>
    <scm|(db-set-field id attr vals)><explain-synopsis|set values for a given
    field>

    <scm|(db-set-entry id l)><explain-synopsis|fill out a complete entry>

    <scm|(db-remove-entry id)><explain-synopsis|remove a complete entry>
  <|explain>
    The actions only succeed when the current user owns the entry.
  </explain>

  <\explain>
    <scm|(db-create-entry l)><explain-synopsis|create a new entry>
  <|explain>
    Unless the current user is <scm|#t>, the current user is added to the
    owners of the new entry. If the resulting list of owners is empty, then
    no entry is created and <scm|#f> is returned.
  </explain>

  <\explain>
    <scm|(db-get-field id attr)><explain-synopsis|get all values for a given
    field>

    <scm|(db-get-entry id)><explain-synopsis|retrieve a complete entry>

    <scm|(db-search q)><explain-synopsis|search for a list of fields>
  <|explain>
    Only values are returned for which the current user has appropriate
    permissions (ownership or read permission). In the case of
    <scm|db-search>, only entries owned by or readable for the current user
    are returned.
  </explain>

  <paragraph|Other useful routines>

  <\explain>
    <scm|(db-allow? id uid permission-attr)><explain-synopsis|check
    permissions>
  <|explain>
    Check whether a user with identifier <scm|uid> has a given permission
    <scm|permission-attr> (such as <scm|"owner"> or <scm|"readable">) for
    the entry with identifier <scm|id>. Group delegations are taken into
    account, and owners automatically have all other permissions.
  </explain>

  <\explain>
    <scm|(db-expand-user uid attr)><explain-synopsis|expand user by group
    membership>
  <|explain>
    Return the sorted list of the user <scm|uid> (a user identifier or a list
    of identifiers) and all groups from which <scm|uid> inherits the
    permission <scm|attr> through delegation, followed by <scm|"all">.
  </explain>

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>