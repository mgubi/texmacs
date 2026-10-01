<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Threat model and pitfalls>

  This page summarizes what the mechanisms of this chapter protect against,
  as they are implemented, and lists the known weaknesses and bugs. File
  names are relative to <verbatim|src/TeXmacs/progs/> for <scheme> files
  and to <verbatim|src/src/> for <c++> files.

  <section|What is protected>

  <\description>
    <item*|Untrusted documents>A document which does not lie below
    <verbatim|$TEXMACS_SECURE_PATH> should not be able to run arbitrary
    <scheme> code when it is opened, typeset or clicked, unless the user
    chose \Paccept all scripts\Q or confirmed a prompt. The protection
    relies entirely on the static checker <scm|secure?> and on the honesty
    of the <scm|:secure> declarations.

    <item*|Confidentiality at rest>Encrypted regions and encrypted
    documents are stored on disk only in encrypted form, as long as they are
    in their encrypted state when the file is written. Passphrases are
    passed to <verbatim|gpg> through pipes, never on the command line, and
    stored only in memory and, if the user wishes, in the wallet and the
    system keychain.
  </description>

  <section|What is not protected>

  <\itemize>
    <item><em|Trusted locations are trusted completely.> Everything below
    <verbatim|$TEXMACS_PATH> and <verbatim|$TEXMACS_HOME_PATH> is trusted,
    including the scratch directory in which new unnamed documents are
    created (<verbatim|$TEXMACS_HOME_PATH/texts/scratch>) and the temporary
    directory (<verbatim|$TEXMACS_HOME_PATH/system/tmp>). Material pasted
    into a new document therefore runs with full rights, and so do files
    which other programs store below the user's <TeXmacs> directory.

    <item><em|The trust status is computed once.> The environment of an
    editor takes its <cpp|secure> flag from the master of the buffer when
    the editor is created (<verbatim|Typeset/Env/env.cpp:37>);
    <cpp|edit_typeset_rep::typeset_prepare> updates the base file name but
    not the flag. A document keeps the trust status of its original
    location after <menu|Save as> (in both directions) until it is
    reopened.

    <item><em|Decrypted regions are plain text.> A region in its decrypted
    state is saved, autosaved, exported, copied and kept in the undo history
    in clear. Nothing encrypts regions automatically before saving; the user
    has to encrypt them explicitly.

    <item><em|No authenticity.> Regions are encrypted, not signed, and
    <scm|gpg-encrypt> uses <verbatim|--trust-model always>: anyone who has
    the public keys (which are stored in the document itself) can produce
    an encrypted region for the same recipients, and nothing checks that a
    key belongs to the person its user identifier names.

    <item><em|Metadata.> Encrypted regions list the fingerprints of their
    recipients in clear, and the attachment <verbatim|gpg> contains their
    user identifiers and public keys. Whole-document encryption hides
    everything except the fact that the file is encrypted.

    <item><em|Memory.> Decrypted documents, the passphrase table of
    encrypted buffers and, while it is on, the whole wallet are held in
    clear in the memory of the process.
  </itemize>

  <section|Known weaknesses and bugs>

  <paragraph|The script checker can be bypassed.><scm|secure-expr?>
  (<verbatim|kernel/texmacs/tm-secure.scm:76-94>) checks only the
  <em|head symbol> of each call. Calls whose head is a computed value or a
  variable bound by <scm|lambda> or <scm|with> are accepted as long as the
  sub-expressions are (lines 81 and 87), and symbols are accepted as
  expressions whatever they denote (line 88). Since any global value,
  including a procedure which is not declared secure, can be referred to
  by its name, this lets an untrusted expression reach functions that are
  not declared secure. <scm|set!> is also treated as a secure form (line
  103), so an untrusted expression can assign to global variables that are
  visible from the module in which it is evaluated. The checker should only accept calls whose
  head is a symbol with the <scm|:secure> property, refuse <scm|set!> and
  free variables that are not known constants, or be replaced by
  evaluation in a restricted environment.

  <paragraph|Secure functions with side effects.>Several functions of the
  encryption code are declared <scm|:secure> although they open dialogs or
  change persistent state: the dialogs which ask for passphrases or
  recipients (<verbatim|security/gpg/gpg-edit.scm>),
  <scm|gpg-set-default-key-fingerprint>, which sets a preference
  (<verbatim|security/gpg/gpg-widgets.scm:51>),
  <scm|tm-gpg-collect-public-keys-from-buffer>, which writes the file of
  collected keys (<verbatim|security/gpg/gpg-base.scm:225>), and
  <scm|gpg-get-default-key-fingerprint>, which reveals the fingerprint of the user's default key.
  Untrusted documents can call all of them.

  <paragraph|Encrypted documents may be written in clear.>When
  <scm|tree-export-encrypted> (<verbatim|security/gpg/gpg-edit.scm:506-524>)
  has no passphrase for the target file, or when <verbatim|gpg> fails, it
  shows an error but returns the <em|unencrypted> document, which
  <cpp|export_tree> (<verbatim|Texmacs/Data/new_buffer.cpp:535-545>) then
  writes. Passphrases are registered only for the file name of the buffer
  and its <verbatim|~> autosave file, so this happens for instance when an
  encrypted document is exported in <TeXmacs> format to another file name,
  and for the <verbatim|#> autosave file written in rescue mode
  (<verbatim|texmacs/texmacs/tm-files.scm:389>). The hook should refuse to
  write instead. Also, <cpp|export_tree> calls the hook whenever the
  variable <verbatim|encryption> is present, whatever its value.

  <paragraph|Deleting a public key deletes the secret key.>
  <scm|gpg-delete-public-key> builds its command with
  <scm|gpg-executable-delete-secret-and-public-key>
  (<verbatim|security/gpg/gpg-base.scm:429>), so deleting a public key in
  the key manager (<verbatim|security/gpg/gpg-widgets.scm:606>) also
  deletes the corresponding secret key, although the confirmation dialog
  only mentions public keys.

  <paragraph|Passphrase encrypted regions cannot be decrypted.>
  <scm|tm-gpg-dialogue-passphrase-decrypt> calls
  <scm|gpg-ask-ask-standalone-passphrase>
  (<verbatim|security/gpg/gpg-edit.scm:362>), which is not defined (the
  function is <scm|gpg-ask-standalone-passphrase>). All menu entries, icons
  and <scm|alternate-toggle> for passphrase encrypted regions end in an
  unbound variable error.

  <paragraph|Importing keys from a document.>In
  <scm|gpg-widget-import-public-keys-from-buffer>
  (<verbatim|security/gpg/gpg-edit.scm:400-404>) the <scm|for> loop has an
  empty body and the import call which follows it refers to the loop
  variable outside the loop, so the <verbatim|Ok> button fails.

  <paragraph|Smaller problems.>

  <\itemize>
    <item>The error path of
    <scm|system-security-delete-generic-password> on <name|Windows> refers
    to an unbound variable <scm|cmd>
    (<verbatim|security/keychain/win-security.scm:61>).

    <item>The wallet dialogs read the second form value (the \Premember
    passphrase\Q choice) even when that field is not shown because no
    system keychain is available (<verbatim|security/wallet/wallet-menu.scm:98>,
    <verbatim|142>, <verbatim|185>).

    <item>When the wallet is on, <scm|gpg-wallet-reinitialize> restores the
    old entries only in memory (<verbatim|security/gpg/gpg-wallet.scm:135-137>);
    the new <verbatim|table.gpg> stays empty until the next change of the
    wallet.

    <item><scm|generate-password> draws the characters with
    <name|GnuTLS> but shuffles them with the ordinary <scm|random>
    (<verbatim|security/password.scm:33>), and falls back to
    <scm|random> entirely without <name|GnuTLS> (line 26).

    <item>The file of collected public keys is computed once, when
    <verbatim|gpg-base.scm> is loaded (line 166), from the user who is the
    default user at that time.

    <item>The field <cpp|new_buffer_rep::secure>
    (<verbatim|Texmacs/Data/new_buffer.hpp:31>) is initialized but never
    read; the checks use the flag of the typesetting environment.
  </itemize>

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
