;;; compile-inventory.scm -- The module whitelist the Chez build compiles.
;;;
;;; These seven tiers are the single source of truth for which TeXmacs modules
;;; the build compiles, in which order, and which of them the texmacs-user.so
;;; kernel snapshot aggregates. They used to be defined in compile-texmacs.scm.
;;;
;;; They live here for the same reason the other build libraries do: the driver
;;; is exec'd into the TeXmacs top-level environment, and everything that wants
;;; to know what the build covers -- the linters, the graph test, the macro
;;; scanners -- had to re-read the driver and eval its `define` forms to find
;;; out. One library, imported by all of them, replaces that scraping.
;;;
;;; Tier 1 is the kernel. Its modules are additionally re-imported into the
;;; interaction environment as soon as they load (register-module-loaded! in
;;; compile-texmacs.scm), because the bootstrap code that follows resolves
;;; their bindings at top level.
;;;
;;; Order matters: each list is compiled top to bottom, and a module may only
;;; rely on the ones above it (or on an explicit back-edge, which the compiler
;;; defers to a runtime module-provide).

(library (compile-inventory)
  (export *kernel-files*
          *tier2-files*
          *tier3-files*
          *tier4-files*
          *tier5-convert-files*
          *tier6-dynamic-files*
          *tier7-extended-files*
          *source-files*
          inventory-tiers
          inventory-tier-number
          inventory-tier-name
          inventory-tier-title
          inventory-tier-files)
  (import (chezscheme))

  (define *kernel-files*
    '(
      "kernel/boot/chez-compat.scm"
      "kernel/boot/compat.scm"
      "kernel/boot/abbrevs.scm"
      "kernel/boot/debug.scm"
      "kernel/boot/srfi.scm"
      "kernel/boot/ahash-table.scm"
      "kernel/boot/prologue.scm"
      "kernel/library/base.scm"
      "kernel/library/list.scm"
      "kernel/library/tree.scm"
      "kernel/library/content.scm"
      "kernel/library/patch.scm"
      "kernel/regexp/regexp-match.scm"
      "kernel/regexp/regexp-select.scm"
      "kernel/logic/logic-bind.scm"
      "kernel/logic/logic-unify.scm"
      "kernel/logic/logic-rules.scm"
      "kernel/logic/logic-query.scm"
      "kernel/logic/logic-data.scm"
      "kernel/texmacs/tm-define.scm"
      "kernel/texmacs/tm-preferences.scm"
      "kernel/texmacs/tm-modes.scm"
      "kernel/texmacs/tm-dialogue.scm"
      "kernel/texmacs/tm-plugins.scm"
      "kernel/texmacs/tm-secure.scm"
      "kernel/texmacs/tm-convert.scm"
      "kernel/texmacs/tm-language.scm"
      "kernel/texmacs/tm-file-system.scm"
      "kernel/texmacs/tm-states.scm"
      "kernel/gui/gui-markup.scm"
      "kernel/gui/menu-define.scm"
      "kernel/gui/kbd-define.scm"
      "kernel/gui/menu-widget.scm"
      "kernel/gui/speech-define.scm"
      "kernel/gui/kbd-handlers.scm"
      "kernel/gui/menu-test.scm"
      "kernel/old-gui/old-gui-widget.scm"
      "kernel/old-gui/old-gui-factory.scm"
      "kernel/old-gui/old-gui-form.scm"
      "kernel/old-gui/old-gui-test.scm"
     ))

  (define *tier2-files*
    '(
      "utils/library/cpp-wrap.scm"
      "utils/library/tree.scm"
      "utils/library/smart-table.scm"
      "utils/library/length.scm"
      "utils/library/cursor.scm"
      "utils/plugins/plugin-convert.scm"
      "convert/rewrite/tmtm-brackets.scm"
      "utils/misc/markup-funcs.scm"
      "utils/misc/artwork.scm"
      "utils/misc/tooltip.scm"
      "utils/handwriting/handwriting.scm"
      "utils/edit/variants.scm"
      "utils/base/environment.scm"
      "utils/library/ptrees.scm"
      "generic/document-edit.scm"
      "generic/generic-edit.scm"
      "generic/document-style.scm"
      "texmacs/texmacs/tm-server.scm"
      "texmacs/texmacs/tm-tools.scm"
      "texmacs/texmacs/tm-view.scm"
      "texmacs/texmacs/tm-files.scm"
      "texmacs/texmacs/tm-print.scm"
      "texmacs/keyboard/config-kbd.scm"
      "fonts/fonts-ec.scm"
      "fonts/fonts-adobe.scm"
      "fonts/fonts-x.scm"
      "fonts/fonts-math.scm"
      "fonts/fonts-foreign.scm"
      "fonts/fonts-misc.scm"
      "fonts/fonts-composite.scm"
      "fonts/fonts-truetype.scm"
      "bibtex/bib-utils.scm"
      "bibtex/bib-complete.scm"
      "database/title-transform.scm"
      "database/title-markup.scm"
      "source/macro-search.scm"
      "tools/ai/ai-batch.scm"
      "utils/misc/gui-utils.scm"
      "utils/misc/gui-keyboard.scm"
      "tools/spell/spell-lantool.scm"
      "debug/debug-widgets.scm"
      "language/natural.scm"
      "utils/library/process.scm"
      "utils/edit/selections.scm"
      "utils/edit/auto-close.scm"
      "utils/plugins/plugin-cmd.scm"
      "utils/plugins/plugin-eval.scm"
      "utils/cite/cite-sort.scm"
     ))

  (define *tier3-files*
    '(
      ;; Math editing:
      "math/math-edit.scm"
      "math/math-kbd.scm"
      "math/math-menu.scm"

      ;; Table editing:
      "table/table-edit.scm"
      "table/table-kbd.scm"
      "table/table-menu.scm"

      ;; Text editing:
      "text/text-drd.scm"
      "text/text-edit.scm"
      "text/text-kbd.scm"
      "text/text-menu.scm"

      ;; Graphics canvas:
      "graphics/graphics-drd.scm"
      "graphics/graphics-utils.scm"
      "graphics/graphics-env.scm"
      "graphics/graphics-markup.scm"
      "graphics/graphics-group.scm"
      "graphics/graphics-object.scm"
      "graphics/graphics-main.scm"
      "graphics/graphics-edit.scm"
      "graphics/graphics-menu.scm"
      "graphics/graphics-kbd.scm"
      "graphics/graphics-single.scm"
      "graphics/graphics-animate.scm"
      "math/math-drd.scm"
      "math/math-markup.scm"
      "table/table-widgets.scm"
      "table/table-tools.scm"
      "table/table-doc.scm"
      "text/text-structure.scm"
      "text/text-natbib.scm"
     ))

  (define *tier4-files*
    '(
      ;; Core desktop menus:
      "texmacs/menus/file-menu.scm"
      "texmacs/menus/edit-menu.scm"
      "texmacs/menus/view-menu.scm"
      "texmacs/menus/view-tools.scm"
      "texmacs/menus/view-widgets.scm"
      "texmacs/menus/tools-menu.scm"
      "texmacs/menus/preferences-menu.scm"
      "texmacs/menus/preferences-tools.scm"
      "texmacs/menus/preferences-widgets.scm"
      "texmacs/menus/print-widgets.scm"
      "texmacs/menus/developer-menu.scm"
      "texmacs/menus/main-menu.scm"

      ;; Generic interactive layer:
      "generic/generic-kbd.scm"
      "generic/format-drd.scm"
      "generic/format-edit.scm"
      "generic/format-geometry-edit.scm"
      "generic/format-menu.scm"
      "generic/format-tools.scm"
      "generic/format-widgets.scm"
      "generic/document-menu.scm"
      "generic/document-part.scm"
      "generic/document-tools.scm"
      "generic/document-widgets.scm"
      "generic/document-doc.scm"
      "generic/insert-menu.scm"
      "generic/generic-menu.scm"
      "generic/pattern-selector.scm"
      "generic/pattern-tools.scm"
      "generic/search-widgets.scm"
      "generic/spell-widgets.scm"
      "generic/generic-doc.scm"
      "generic/embedded-edit.scm"
      "generic/embedded-menu.scm"
      "generic/generic-speech.scm"
      "generic/generic-speech-en.scm"
      "generic/generic-speech-fr.scm"
      "source/source-drd.scm"
      "source/source-edit.scm"
      "source/source-kbd.scm"
      "source/source-menu.scm"
      "source/macro-edit.scm"
      "source/macro-menu.scm"
      "source/macro-widgets.scm"
     ))

  (define *tier5-convert-files*
    '(
      ;; Core converter tools:
      "convert/tools/output.scm"
      "convert/tools/tmconcat.scm"
      "convert/tools/tmcolor.scm"
      "convert/tools/tmlength.scm"
      "convert/tools/tmtable.scm"
      "convert/tools/stm.scm"
      "convert/tools/environment.scm"
      "convert/tools/sxml.scm"
      "convert/tools/css.scm"
      "convert/tools/tmpre.scm"
      "convert/tools/xmltm.scm"
      "convert/tools/sxhtml.scm"

      ;; Core format rewrite:
      "convert/rewrite/init-rewrite.scm"

      ;; Image formats:
      "convert/images/tmimage.scm"
      "convert/images/init-images.scm"

      ;; LaTeX subsystem:
      "convert/latex/latex-define.scm"
      "convert/latex/latex-drd.scm"
      "convert/latex/latex-command-drd.scm"
      "convert/latex/latex-symbol-drd.scm"
      "convert/latex/latex-texmacs-drd.scm"
      "convert/latex/latex-tools.scm"
      "convert/latex/latex-overload.scm"
      "convert/latex/texout.scm"
      "convert/latex/tmtex.scm"
      "convert/latex/tmtex-widgets.scm"
      "convert/latex/init-latex.scm"
      "convert/latex/tmtex-ams.scm"
      "convert/latex/tmtex-acm.scm"
      "convert/latex/tmtex-beamer.scm"
      "convert/latex/tmtex-elsevier.scm"
      "convert/latex/tmtex-ieee.scm"
      "convert/latex/tmtex-revtex.scm"
      "convert/latex/tmtex-springer.scm"

      ;; HTML subsystem:
      "convert/html/htmlout.scm"
      "convert/html/tmhtml.scm"
      "convert/html/htmltm.scm"
      "convert/html/tmhtml-expand.scm"
      "convert/html/init-html.scm"

      ;; TMML subsystem:
      "convert/tmml/tmtmml.scm"
      "convert/tmml/tmmltm.scm"
      "convert/tmml/tmmlout.scm"
      "convert/tmml/init-tmml.scm"

      ;; MathML subsystem:
      "convert/mathml/mathml-drd.scm"
      "convert/mathml/tmmath.scm"
      "convert/mathml/mathtm.scm"

      ;; BibTeX subsystem:
      "convert/bibtex/bibtextm.scm"
      "convert/bibtex/tmbibtex.scm"
      "convert/bibtex/bibtexout.scm"
      "convert/bibtex/init-bibtex.scm"

      ;; Coq subsystem:
      "convert/coq/scmcoqml.scm"
      "convert/coq/coqmlscm.scm"
      "convert/coq/coqmlout.scm"
      "convert/coq/vernacout.scm"
      "convert/coq/gallinatm.scm"
      "convert/coq/tmvernac.scm"
      "convert/coq/init-coqml.scm"
     ))

  (define *tier6-dynamic-files*
    '(
      "dynamic/dynamic-drd.scm"
      "dynamic/calc-drd.scm"
      "dynamic/calc-markup.scm"
      "dynamic/calc-edit.scm"
      "dynamic/calc-kbd.scm"
      "dynamic/calc-menu.scm"
      "dynamic/calc-table.scm"
      "dynamic/fold-markup.scm"
      "dynamic/fold-edit.scm"
      "dynamic/fold-kbd.scm"
      "dynamic/fold-menu.scm"
      "dynamic/session-drd.scm"
      "dynamic/session-edit.scm"
      "dynamic/session-menu.scm"
      "dynamic/session-doc.scm"
      "dynamic/program-drd.scm"
      "dynamic/program-edit.scm"
      "dynamic/program-menu.scm"
      "dynamic/program-doc.scm"
      "dynamic/animate-edit.scm"
      "dynamic/animate-menu.scm"
      "dynamic/scripts-drd.scm"
      "dynamic/scripts-edit.scm"
      "dynamic/scripts-kbd.scm"
      "dynamic/scripts-menu.scm"
      "dynamic/scripts-plot.scm"
     ))

  (define *tier7-extended-files*
    '(
      "bibtex/plain.scm"
      "bibtex/abbrv.scm"
      "bibtex/abstract.scm"
      "bibtex/acm.scm"
      "bibtex/alpha.scm"
      "bibtex/bib-widgets.scm"
      "bibtex/elsart-num.scm"
      "bibtex/ieeetr.scm"
      "bibtex/siam.scm"
      "bibtex/unsrt.scm"
      "convert/tmml/tmmltm-test.scm"
      "database/db-base.scm"
      "database/db-format.scm"
      "database/db-users.scm"
      "database/db-version.scm"
      "server/server-authentication.scm"
      "security/password.scm"
      "server/server-base.scm"
      "server/server-cache.scm"
      "server/server-cache-test.scm"
      "convert/tools/xmltm-test.scm"
      "convert/tools/tmlength-test.scm"
      "server/server-tmfs.scm"
      "server/server-notifications.scm"
      "server/server-chat.scm"
      "server/server-fixtures.scm"
      "server/server-tmfs-test.scm"
      "server/server-backup.scm"
      "server/server-backup-test.scm"
      "convert/tools/environment-test.scm"
      "convert/mathml/mathtm-test.scm"
      "prog/prog-format-test.scm"
      "utils/cite/cite-sort-test.scm"
      "convert/html/tmhtml-test.scm"
      "server/server-notifications-test.scm"
      "convert/html/htmltm-test.scm"
      "check/check-master.scm"
      "client/client-authentication.scm"
      "client/client-base.scm"
      "version/version-tmfs.scm"
      "client/client-db.scm"
      "client/client-tmfs.scm"
      "client/client-chat.scm"
      "database/db-edit.scm"
      "database/db-convert.scm"
      "client/client-db-sync.scm"
      "utils/relate/live-document.scm"
      "utils/relate/live-connection.scm"
      "utils/relate/live-view.scm"
      "client/client-live.scm"
      "client/client-markup.scm"
      "client/client-remote-config.scm"
      "client/client-sync.scm"
      "client/client-notifications.scm"
      "client/client-widgets.scm"
      "client/client-menu.scm"
      "convert/coq/coqmltest.scm"
      "convert/html/htmltest.scm"
      "convert/latex/test-tmtex.scm"
      "convert/latex/textest.scm"
      "convert/tools/old-tmtable.scm"
      "database/bib-db.scm"
      "database/bib-manage.scm"
      "database/db-tmfs.scm"
      "database/db-widgets.scm"
      "database/db-menu.scm"
      "database/bib-menu.scm"
      "database/bib-kbd.scm"
      "database/bib-local.scm"
      "database/db-markup.scm"
      "debug/debug-menu.scm"
      "doc/apidoc-collect.scm"
      "doc/apidoc-funcs.scm"
      "doc/apidoc-widgets.scm"
      "doc/apidoc-kbd.scm"
      "doc/apidoc-markup.scm"
      "prog/scheme-tools.scm"
      "doc/apidoc-menu.scm"
      "doc/apidoc.scm"
      "doc/help-funcs.scm"
      "doc/docgrep.scm"
      "doc/help-menu.scm"
      "doc/tmdoc-drd.scm"
      "doc/tmdoc-edit.scm"
      "doc/tmdoc-kbd.scm"
      "doc/tmdoc-markup.scm"
      "doc/tmdoc-menu.scm"
      "doc/tmdoc-search.scm"
      "doc/tmdoc.scm"
      "doc/tmweb.scm"
      "education/edu-drd.scm"
      "education/edu-edit.scm"
      "education/edu-kbd.scm"
      "education/edu-markup.scm"
      "education/edu-menu.scm"
      "fonts/font-sample.scm"
      "fonts/font-new-widgets.scm"
      "fonts/font-old-menu.scm"
      "kernel/gui/menu-convert.scm"
      "kernel/library/iterator.scm"
      "kernel/logic/logic-test.scm"
      "kernel/regexp/regexp-test.scm"
      "kernel/texmacs/tm-convert-test.scm"
      "language/minimal.scm"
      "language/std-symbols.scm"
      "language/std-math.scm"
      "link/locus-edit.scm"
      "link/link-edit.scm"
      "link/link-extern.scm"
      "link/link-navigate.scm"
      "link/link-extract.scm"
      "link/link-kbd.scm"
      "link/link-menu.scm"
      "link/ref-edit.scm"
      "link/ref-markup.scm"
      "link/ref-menu.scm"
      "math/math-stats.scm"
      "math/math-speech.scm"
      "math/math-adjust-en.scm"
      "math/math-adjust-fr.scm"
      "math/math-sem-edit.scm"
      "math/math-speech-en.scm"
      "math/math-speech-fr.scm"
      "notification/notification-base.scm"
      "part/part-shared.scm"
      "part/part-tmfs.scm"
      "part/part-menu.scm"
      "prog/prog-edit.scm"
      "prog/cpp-edit.scm"
      "prog/default-lang.scm"
      "prog/dot-edit.scm"
      "prog/fortran-edit.scm"
      "prog/glue-symbols.scm"
      "prog/java-edit.scm"
      "prog/prog-format.scm"
      "prog/scheme-autocomplete.scm"
      "utils/misc/tm-keywords.scm"
      "prog/scheme-edit.scm"
      "prog/python-edit.scm"
      "prog/scala-edit.scm"
      "prog/prog-kbd.scm"
      "prog/prog-menu.scm"
      "prog/scheme-menu.scm"
      "security/gpg/gpg-base.scm"
      "security/gpg/gpg-wallet.scm"
      "security/wallet/wallet-base.scm"
      "security/wallet/wallet-menu.scm"
      "security/gpg/gpg-widgets.scm"
      "security/gpg/gpg-edit.scm"
      "security/gpg/gpg-menu.scm"
      "security/keychain/macos-security.scm"
      "security/keychain/win-security.scm"
      "server/server-db-sync.scm"
      "server/server-db.scm"
      "server/server-live.scm"
      "server/server-sync.scm"
      "server/server-widgets.scm"
      "server/server-menu.scm"
      "source/shortcut-edit.scm"
      "source/shortcut-widgets.scm"
      "texmacs/keyboard/latex-kbd.scm"
      "texmacs/keyboard/prefix-kbd.scm"
      "text/chinese/chinese.scm"
      "text/cyrillic/cp1251-kbd.scm"
      "text/cyrillic/jcuken-kbd.scm"
      "text/cyrillic/koi8-kbd.scm"
      "text/cyrillic/translit-kbd.scm"
      "text/cyrillic/yawerty-kbd.scm"
      "text/text-speech.scm"
      "text/text-speech-en.scm"
      "text/text-speech-fr.scm"
      "text/vietnamese/vietnamese.scm"
      "version/version-drd.scm"
      "version/version-edit.scm"
      "tools/ai/ai-translate.scm"
      "tools/comment/comment-drd.scm"
      "tools/comment/comment-edit.scm"
      "tools/comment/comment-widgets.scm"
      "tools/comment/comment-menu.scm"
      "tools/poster/poster-drd.scm"
      "tools/poster/poster-edit.scm"
      "tools/poster/poster-menu.scm"
      "tools/spell/spell-edit.scm"
      "tools/spell/spell-kbd.scm"
      "tools/theme/theme-edit.scm"
      "tools/theme/theme-menu.scm"
      "utils/automate/auto-edit.scm"
      "utils/automate/auto-build.scm"
      "utils/automate/auto-tmfs.scm"
      "utils/automate/auto-kbd.scm"
      "utils/automate/auto-menu.scm"
      "utils/cas/cas-rewrite.scm"
      "utils/cas/cas-out.scm"
      "utils/email/email-tmfs.scm"
      "utils/literate/lp-edit.scm"
      "utils/literate/lp-build.scm"
      "utils/literate/lp-menu.scm"
      "utils/misc/doxygen.scm"
      "utils/misc/extern-demo.scm"
      "utils/misc/tiles.scm"
      "utils/misc/translation-list.scm"
      "utils/misc/updater.scm"
      "utils/relate/live-menu.scm"
      "utils/relate/relate-menu.scm"
      "utils/test/test-convert.scm"
      "utils/test/test-latex-export.scm"
      "version/version-compare.scm"
      "version/version-git.scm"
      "version/version-kbd.scm"
      "version/version-menu.scm"
      "version/version-svn.scm"
     ))


  ;;; -------------------------------------------------------------------------
  ;;; The tier table
  ;;; -------------------------------------------------------------------------
  ;;
  ;; One row per tier: (number short-name progress-title files).
  ;;
  ;;   short-name      used by the dependency-graph report
  ;;   progress-title  the banner compile-module-list prints before the tier
  ;;
  ;; Keeping the names next to the lists is what makes the driver's tier loop a
  ;; single for-each: adding a tier means adding a row, not editing three
  ;; places that have to agree.
  (define inventory-tiers
    (list (list 1 "kernel" "Compiling TeXmacs Kernel Modules" *kernel-files*)
          (list 2 "tier2" "Compiling Tier 2 (Application Core) Modules" *tier2-files*)
          (list 3 "tier3" "Compiling Tier 3 (Core Editing) Modules" *tier3-files*)
          (list 4 "tier4" "Compiling Tier 4 (Desktop GUI & Menus) Modules" *tier4-files*)
          (list 5 "tier5-convert" "Compiling Tier 5 (Document Converters) Modules"
                *tier5-convert-files*)
          (list 6 "tier6-dynamic" "Compiling Tier 6 (Dynamic & Interactive Sessions) Modules"
                *tier6-dynamic-files*)
          (list 7 "tier7-extended" "Compiling Tier 7 (Extended Subsystems & Plugins) Modules"
                *tier7-extended-files*)))

  (define (inventory-tier-number tier) (list-ref tier 0))
  (define (inventory-tier-name tier) (list-ref tier 1))
  (define (inventory-tier-title tier) (list-ref tier 2))
  (define (inventory-tier-files tier) (list-ref tier 3))

  ;; Every whitelisted module, in compile order. This is the list the scanner
  ;; walks and the graph is built from, so it has to stay the concatenation of
  ;; the tiers in the order above -- no sorting, no deduplication.
  (define *source-files*
    (apply append (map inventory-tier-files inventory-tiers)))
)
