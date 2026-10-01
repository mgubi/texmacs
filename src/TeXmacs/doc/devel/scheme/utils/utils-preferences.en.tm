<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|User preferences>

  Preferences are used to store any information you need to keep across
  different runs of <TeXmacs>, like window position and size, active menu
  bars, etc. Internally they are stored in the file
  <verbatim|$TEXMACS_HOME_PATH/system/preferences.scm> as a <scheme> list of
  items like <scm|("name" "value")> which therefore has in principle no
  structure. The <scheme> interface is defined in
  <verbatim|kernel/texmacs/tm-preferences.scm>, on top of the glued <c++>
  routines <scm|cpp-get-preference>, <scm|cpp-set-preference>,
  <scm|cpp-reset-preference>, <scm|cpp-has-preference?> and
  <scm|save-preferences>. However, a good practice to avoid conflicts is to
  prefix your options by the name of the plugin or module you are creating,
  like in <scm|"gui:help-window-position">.

  The first step in defining a new preference is adding it with
  <scm|define-preferences> and assigning a call-back function to handle
  changes in the preference. This is important for instance in menus, where a
  click on an item simply sets some preference to some value and it's up to
  the call-back to actually take the necessary actions.

  <\warning*>
    One may not store the boolean values <scm|#t>, <scm|#f> directly into
    preferences. Instead one should use the strings <scm|"on"> and
    <scm|"off">. This is due to the internal storage of default values for
    preferences using <scm|ahash-table>: a default value <scm|#f> cannot be
    distinguished from an undefined default. Preference values are stored as
    strings; other values are converted using <scm|object-\<gtr\>string>,
    and converted back by <scm|get-preference> only if the default value is
    not a string.
  </warning*>

  <\explain>
    <scm|(define-preferences <scm-arg|list>)><explain-synopsis|define new
    preferences with defaults and call-backs>
  <|explain>
    Each element of <scm-arg|list> is of the form <scm|("somename"
    default-value notify-procedure)> where <scm|notify-procedure> is a
    procedure taking two arguments like this:

    <scm|(define (notify-procedure property-name value) (do-things))>

    A default value is only set if no default value had been defined
    before. The call-back is also called once when the preference is
    defined.

    Remember to use the strings <scm|"on"> and <scm|"off"> instead of
    booleans <scm|#t>, <scm|#f>.

    <\unfolded-documentation>
      Example
    <|unfolded-documentation>
      <\session|scheme|default>
        <\input|Scheme] >
          (define (notify-test pref value)

          \ \ (display* "Hey! " pref " changed to " value) (newline))
        </input>

        <\input|Scheme] >
          (define-preferences ("test:pref" "off" notify-test))
        </input>

        <\unfolded-io|Scheme] >
          (get-preference "test:pref")
        <|unfolded-io>
          "off"
        </unfolded-io>

        <\input|Scheme] >
          (set-preference "test:pref" "on")
        </input>

        <\unfolded-io|Scheme] >
          (preference-on? "test:pref")
        <|unfolded-io>
          #t
        </unfolded-io>

        <\input|Scheme] >
          \;
        </input>
      </session>
    </unfolded-documentation>
  </explain>

  <\explain>
    <scm|(set-preference <scm-arg|name> <scm-arg|value>)><explain-synopsis|set
    user preference>
  <|explain>
    Save preference <scm|name> with value <scm|value>. If the value changed,
    then call the call-back associated to this preference, as defined in
    <scm|define-preferences>, and save the preferences to disk.

    Remember to use the strings <scm|"on"> and <scm|"off"> instead of
    booleans <scm|#t>, <scm|#f>.
  </explain>

  <\explain>
    <scm|(append-preference <scm-arg|name>
    <scm-arg|value>)><explain-synopsis|appends a value to the list for a
    preference>
  <|explain>
    This convenience function appends <scm|value> to the list of values of
    preference <scm|name>, or creates a list with one element in case the
    preference didn't exist. The call-back associated to this preference, as
    defined in <scm|define-preferences> is called once the modification is
    done.
  </explain>

  <\explain>
    <scm|(reset-preference <scm-arg|name>)><explain-synopsis|delete user
    preference>
  <|explain>
    Deletes preference <scm|name> from the user preferences, so that it
    reverts to its default value, and calls the associated call-back.
  </explain>

  <\explain>
    <scm|(get-preference <scm-arg|name>)><explain-synopsis|get user
    preference>
  <|explain>
    Returns the value of preference <scm|name>. If the user did not set the
    preference, then its default value from <scm|define-preferences> is
    returned, or the string <scm|"default"> if no default value was
    defined.
  </explain>

  <\explain>
    <scm|(preference-on? <scm-arg|name>)><explain-synopsis|test boolean user
    preference>
  <|explain>
    Returns <scm|#t> if the value of preference <scm|name> is <scm|"on">.
  </explain>

  <\explain>
    <scm|(toggle-preference <scm-arg|name>)><explain-synopsis|change value of
    boolean user preference>
  <|explain>
    Toggles the value of preference <scm|name> between <scm|"on"> and
    <scm|"off">.
  </explain>

  <\explain>
    <scm|(set-boolean-preference <scm-arg|name> <scm-arg|val>)>

    <scm|(get-boolean-preference <scm-arg|name>)><explain-synopsis|boolean
    preferences>
  <|explain>
    Set the preference <scm|name> to <scm|"on"> or <scm|"off"> depending on
    the boolean <scm|val>, <abbr|resp.> test whether it is <scm|"on">.
  </explain>

  <\explain>
    <scm|(define-preference-names <scm-arg|name> (<scm-arg|val>
    <scm-arg|pretty>) ...)>

    <scm|(set-pretty-preference <scm-arg|name> <scm-arg|pretty>)>

    <scm|(get-pretty-preference <scm-arg|name>)><explain-synopsis|human
    readable preference values>
  <|explain>
    The macro <scm|define-preference-names> associates human readable names
    <scm-arg|pretty> to the internal values <scm-arg|val> of the preference
    <scm-arg|name>. The routines <scm|set-pretty-preference> and
    <scm|get-pretty-preference> are variants of <scm|set-preference> and
    <scm|get-preference> which work with these human readable names; they
    are typically used in the preferences dialogue.
  </explain>

  <\explain>
    <scm|(notify-preference <scm-arg|name>)><explain-synopsis|call the
    call-back of a preference>
  <|explain>
    Call the call-back associated to the preference <scm|name> with its
    current value.
  </explain>

  <tmdoc-copyright|2012|Miguel de Benito Delgado>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>