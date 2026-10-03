<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<name|GnuPG> encryption of documents>

  All encryption in <TeXmacs> is delegated to the external program
  <verbatim|gpg>. The <scheme> code in <verbatim|progs/security/gpg/>
  builds the command lines, sends the data and the passphrases to the
  program, and stores the results in documents. Unless stated otherwise,
  file names below are relative to <verbatim|src/TeXmacs/progs/>.

  <section|Enabling encryption>

  Encryption is an experimental feature. <scm|supports-gpg?>
  (<verbatim|security/gpg/gpg-base.scm>) is true only if

  <\itemize>
    <item>the preference <verbatim|experimental encryption> is
    <verbatim|on>;

    <item>the preference <verbatim|gpg executable> names a program which is
    found in the <verbatim|PATH> (the default is <verbatim|gpg>, or
    <verbatim|gpg2> if only that one exists);

    <item>a keyring exists in the <TeXmacs> key directory, or can be
    created there (<scm|gpg-make-homedir>).
  </itemize>

  The menus only show the encryption entries when encryption is enabled:
  the <menu|Insert|Fold|Encrypt> submenu
  (<scm|gpg-menu>, <verbatim|dynamic/fold-menu.scm>) and the
  <menu|Encryption> submenu of <menu|Document>
  (<scm|document-encryption-menu>, <verbatim|generic/document-menu.scm>).
  The key manager and the preferences are in
  <verbatim|security/gpg/gpg-widgets.scm>.

  <section|Keys and the key directory>

  <TeXmacs> does not use the user's ordinary <name|GnuPG> keyring. Each
  <TeXmacs> user (in the sense of the user database, see <hlink|the
  database|database.en.tm>) has its own key directory

  <\verbatim-code>
    $TEXMACS_HOME_PATH/users/<em|default-user>/gnupg
  </verbatim-code>

  (<scm|gpg-homedir>), which is created on demand and, except on
  <name|Windows>, made inaccessible to other users (<verbatim|chmod
  og-rwx>). Every command is run with <verbatim|--homedir> pointing to this
  directory, or to another directory given as an optional last argument
  (the wallet has its own, see <hlink|the wallet|security-wallet.en.tm>).

  <scm|gpg-base.scm> offers the usual operations on this keyring:

  <\description>
    <item*|Creation><scm|gpg-gen-key> creates an <name|RSA> key of 4096
    bits from a name, an e-mail address, an optional comment and a
    passphrase, by sending a parameter file to <verbatim|gpg --gen-key>.

    <item*|Listing><scm|gpg-public-keys> and <scm|gpg-secret-keys> parse
    the output of <verbatim|--with-colons>; each key is a list of rows
    (<verbatim|pub>/<verbatim|sec>, <verbatim|fpr>, <verbatim|uid>, ...).
    <scm|gpg-get-key-fingerprint>, <scm|gpg-get-key-user-id>,
    <scm|gpg-public-key-fingerprints>, <scm|gpg-secret-key-fingerprints>
    and the <scm|gpg-search-...-by-fingerprint> functions query these
    lists.

    <item*|Import, export, deletion><scm|gpg-import-public-keys>,
    <scm|gpg-import-secret-keys>, <scm|gpg-export-public-keys>,
    <scm|gpg-export-secret-keys>, <scm|gpg-delete-public-key>,
    <scm|gpg-delete-secret-and-public-key>.

    <item*|Default identity>The preference <verbatim|gpg default key
    fingerprint> (<scm|gpg-get-default-key-fingerprint>,
    <scm|gpg-set-default-key-fingerprint> in
    <verbatim|security/gpg/gpg-widgets.scm>), which is also stored as the
    user information <verbatim|gpg-key-fingerprint>. It is proposed as a
    recipient when a region is encrypted.

    <item*|Collected public keys>Public keys found in documents (see below)
    can be stored in <verbatim|collected-public-keys.scm> in the key
    directory, a list of entries <verbatim|(<em|fingerprint> <em|user-id>
    <em|armored-key>)> (<scm|gpg-collected-public-keys>,
    <scm|gpg-add-collected-public-keys>,
    <scm|gpg-import-public-key-from-collected>, ...).
  </description>

  <section|Running <verbatim|gpg>>

  All calls go through <scm|evaluate-system>, the glue for
  <cpp|evaluate_system> (<verbatim|src/src/System/Misc/sys_utils.cpp>). On
  <name|Unix> it starts the program with <cpp|posix_spawn>, without a
  shell (<cpp|unix_system>, <verbatim|src/src/Plugins/Unix/unix_sys_utils.cpp>),
  writes given strings to given file descriptors of the child and
  collects given output descriptors. An input whose descriptor is
  <math|-1> gets a fresh pipe, and the string <verbatim|$$<em|i>> in the
  arguments is replaced by its descriptor number. <TeXmacs> uses this to
  send passphrases with <verbatim|--passphrase-fd $$1>: passphrases never
  appear on the command line. The data to encrypt or decrypt are sent on
  standard input, and the result is read from standard output. The common
  options are

  <\verbatim-code>
    gpg --homedir <em|dir> --batch --no-tty --no-use-agent ...
  </verbatim-code>

  (<scm|gpg-executable-default>). On failure, <scm|gpg-error> reports the
  command line and the error output with <scm|report-system-error>.

  The two kinds of encryption are:

  <\description>
    <item*|Public key encryption><scm|gpg-encrypt data fingerprints>
    runs <verbatim|--encrypt> with one <verbatim|-r> per recipient,
    <verbatim|--trust-model always> and <verbatim|--armor>.
    <scm|gpg-decrypt data passphrase> runs <verbatim|--decrypt> with the
    passphrase of the secret key.

    <item*|Passphrase encryption><scm|gpg-passphrase-encrypt data
    passphrase> runs <verbatim|--symmetric> with the cipher given by the
    preference <verbatim|gpg cipher algorithm> (<verbatim|AES256> by
    default, or <verbatim|AES192>); <scm|gpg-passphrase-decrypt> uses the
    same command as <scm|gpg-decrypt>.
  </description>

  <scm|gpg-decryptable?> runs a decryption and only reports whether it
  succeeded; <scm|gpg-correct-passphrase?> encrypts a test string for a key
  and checks whether the passphrase decrypts it. Results are armored
  <name|ASCII> strings, so they can be stored in documents.
  <scm|gpg-encrypt-save-object> and <scm|gpg-load-decrypt-object> store a
  <scheme> object encrypted in a file; the wallet uses them.

  <section|Encrypted regions>

  A document may contain encrypted regions. The markup is defined in
  <verbatim|src/TeXmacs/packages/standard/std-security.ts>, which is part
  of <verbatim|std>:

  <\description-paragraphs>
    <item*|<markup|gpg-decrypted>, <markup|gpg-decrypted-block>>A region in
    clear, followed by the fingerprints of its recipients.

    <item*|<markup|gpg-encrypted>, <markup|gpg-encrypted-block>>The armored
    result of public key encryption, followed by the fingerprints.

    <item*|<markup|gpg-passphrase-decrypted>,
    <markup|gpg-passphrase-decrypted-block>>A region in clear, to be
    encrypted with a passphrase.

    <item*|<markup|gpg-passphrase-encrypted>,
    <markup|gpg-passphrase-encrypted-block>>The armored result of passphrase
    encryption.
  </description-paragraphs>

  The packages <verbatim|gpg-info-level-none>, <verbatim|-short> and
  <verbatim|-detailed> (<verbatim|packages/customize/encryption/>) set
  <verbatim|gpg-info-level>, which determines how much information on the
  recipients is shown around decrypted blocks.

  The operations are in <verbatim|security/gpg/gpg-edit.scm>:

  <\description>
    <item*|Insertion><scm|tm-gpg-dialogue-insert-decrypted> and the block
    variant ask for the recipients (<scm|gpg-widget-select-public-key-fingerprints>)
    and wrap the subtree at the cursor; <scm|tm-gpg-insert-passphrase-decrypted>
    and its block variant insert the tag with <scm|make>.

    <item*|Encryption><scm|tm-gpg-encrypt> serializes the body with
    <scm|serialize-texmacs>, encrypts it for the recipients and replaces the
    tag by its encrypted counterpart; <scm|tm-gpg-dialogue-passphrase-encrypt>
    first asks for a new passphrase (twice). Both then autosave the buffer.

    <item*|Decryption><scm|tm-gpg-dialogue-decrypt> keeps the recipients
    whose secret key is in the keyring and tries them one after the other
    (<scm|gpg-try-decrypt>): the passphrase of each key is taken from the
    wallet if possible, and asked for otherwise; a passphrase that works is
    stored in the wallet if the wallet is on persistently. The decrypted
    string is parsed back with <scm|parse-texmacs-snippet>.
    <scm|tm-gpg-dialogue-passphrase-decrypt> asks for the passphrase of a
    passphrase encrypted region.

    <item*|Toggling><scm|alternate-toggle> and the focus menus and icons of
    <verbatim|security/gpg/gpg-menu.scm> switch between the two forms;
    <verbatim|Recipients> changes the recipients of a decrypted region.
    Structured insertion and removal of children are disabled for all these
    tags.
  </description>

  So that the recipients of a document can encrypt for each other, the
  public keys of all recipients are stored in the document itself, in the
  attachment <verbatim|gpg>: a table from fingerprints to pairs
  <verbatim|(<em|user-id> <em|armored-key>)>
  (<scm|gpg-set-ahash-table-attachment>,
  <scm|gpg-get-ahash-table-attachment>). <scm|tm-gpg-get-key-user-id> uses
  it to display user identifiers, and the public keys can be imported into
  the keyring or into the collected keys from there.

  <section|Encrypted documents>

  A whole document can be encrypted with a passphrase
  (<menu|Document|Encryption|Passphrase encryption>,
  <scm|tm-gpg-dialogue-passphrase-buffer-set-encryption>). This stores
  the passphrase for the buffer, sets the initial environment variable
  <verbatim|encryption> to <verbatim|gpg-passphrase> and saves the buffer.

  <paragraph|Passphrases of buffers.>They are kept in memory in the table
  <scm|gpg-buffer-passphrase-table> of <verbatim|gpg-edit.scm>, under the
  concrete file name of the buffer <em|and> under the name of its autosave
  file (suffix <verbatim|~>), and also in the wallet if it is on
  (<scm|gpg-set-buffer-passphrase>). <scm|save-buffer-as-main> is overloaded
  for encrypted buffers to copy the passphrase to the new name.

  <paragraph|Saving.><cpp|export_tree>
  (<verbatim|src/src/Texmacs/Data/new_buffer.cpp>) checks whether a
  document in <TeXmacs> format has an initial variable
  <verbatim|encryption>; if so, it replaces the document by the result of
  the <scheme> function <scm|tree-export-encrypted> before writing it. This
  function serializes the document, encrypts it with the passphrase
  registered for the target file name, and returns a new document with
  style <verbatim|generic> whose body is a single
  <markup|gpg-passphrase-encrypted-buffer> holding the armored data. The
  same hook encrypts autosave files, since they are written by
  <scm|buffer-export>.

  <paragraph|Loading.>The file then loads like any other document;
  <scm|load-buffer-open> (<verbatim|texmacs/texmacs/tm-files.scm>) notices
  the <markup|gpg-passphrase-encrypted-buffer> tag and calls
  <scm|tm-gpg-dialogue-passphrase-decrypt-buffer>. If encryption is not
  enabled, a dialog explains how to enable it. Otherwise the passphrase is
  taken from the wallet, or asked for; the decrypted document replaces the
  buffer contents (<scm|buffer-set>) and the passphrase is registered for
  later saves.

  <paragraph|Disabling.><scm|tm-gpg-passphrase-buffer-unset-encryption>
  removes the variable, forgets the passphrase and saves the buffer in
  clear.

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
