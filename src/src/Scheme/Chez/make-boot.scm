;;; make-boot.scm -- Assemble ahead-of-time (AOT) Chez Scheme boot image (texmacs.boot)
;;;
;;; Usage (CLI):
;;;   scheme --script src/Scheme/Chez/make-boot.scm [boot-dir] [out-boot-file]
;;; Usage (Scheme):
;;;   (load "src/Scheme/Chez/make-boot.scm")
;;;   (assemble-aot-images!)

(import (chezscheme))

(define (make-boot:patch-cyclic-boot-order lst)
  (let ((doc "generic/document-menu.so")
        (gen "generic/generic-menu.so"))
    (let loop ((rest lst))
      (cond
        ((null? rest) lst)
        ((string=? (car rest) gen)
         lst)
        ((string=? (car rest) doc)
         (if (member gen (cdr rest))
             (begin
               (printf "[make-boot] Detected menu order inversion: ~a before ~a\n" doc gen)
               (printf "[make-boot] Reordering ~a before ~a to preserve focus toolbar overloads\n" gen doc)
               (let* ((without-gen (remp (lambda (x) (string=? x gen)) lst)))
                 (let insert ((l without-gen))
                   (cond
                     ((null? l) '())
                     ((string=? (car l) doc) (cons gen (cons doc (cdr l))))
                     (else (cons (car l) (insert (cdr l))))))))
             lst))
        (else (loop (cdr rest)))))))

;; Curated startup resident module list (178 .so modules = boot-chez.so + 177 app modules)
;; in topological import order: unforced startup closure + high-frequency top-level menus.
(define *startup-boot-modules*
  '("kernel/boot/boot-chez.so"
    "kernel/boot/chez-compat.so"
    "kernel/boot/compat.so"
    "kernel/boot/abbrevs.so"
    "kernel/boot/debug.so"
    "kernel/boot/srfi.so"
    "kernel/boot/ahash-table.so"
    "kernel/boot/prologue.so"
    "kernel/library/base.so"
    "kernel/library/list.so"
    "kernel/library/tree.so"
    "kernel/library/content.so"
    "kernel/library/patch.so"
    "kernel/regexp/regexp-match.so"
    "kernel/regexp/regexp-select.so"
    "kernel/logic/logic-bind.so"
    "kernel/logic/logic-unify.so"
    "kernel/logic/logic-rules.so"
    "kernel/logic/logic-query.so"
    "kernel/logic/logic-data.so"
    "kernel/texmacs/tm-define.so"
    "kernel/texmacs/tm-preferences.so"
    "kernel/texmacs/tm-modes.so"
    "kernel/texmacs/tm-dialogue.so"
    "kernel/texmacs/tm-plugins.so"
    "kernel/texmacs/tm-secure.so"
    "kernel/texmacs/tm-convert.so"
    "kernel/texmacs/tm-language.so"
    "kernel/texmacs/tm-file-system.so"
    "kernel/texmacs/tm-states.so"
    "kernel/gui/gui-markup.so"
    "kernel/gui/menu-define.so"
    "kernel/gui/kbd-define.so"
    "kernel/gui/menu-widget.so"
    "kernel/gui/speech-define.so"
    "kernel/gui/kbd-handlers.so"
    "kernel/gui/menu-test.so"
    "kernel/old-gui/old-gui-widget.so"
    "kernel/old-gui/old-gui-factory.so"
    "kernel/old-gui/old-gui-form.so"
    "kernel/old-gui/old-gui-test.so"
    "utils/library/cpp-wrap.so"
    "utils/library/tree.so"
    "utils/library/smart-table.so"
    "convert/rewrite/tmtm-brackets.so"
    "utils/plugins/plugin-convert.so"
    "utils/misc/markup-funcs.so"
    "utils/misc/artwork.so"
    "utils/misc/tooltip.so"
    "utils/handwriting/handwriting.so"
    "bibtex/bib-utils.so"
    "utils/base/environment.so"
    "texmacs/texmacs/tm-tools.so"
    "utils/library/length.so"
    "utils/library/cursor.so"
    "utils/edit/variants.so"
    "utils/library/ptrees.so"
    "bibtex/bib-complete.so"
    "source/macro-search.so"
    "generic/generic-edit.so"
    "generic/document-style.so"
    "generic/document-edit.so"
    "texmacs/texmacs/tm-server.so"
    "texmacs/texmacs/tm-view.so"
    "texmacs/texmacs/tm-files.so"
    "texmacs/texmacs/tm-print.so"
    "texmacs/keyboard/config-kbd.so"
    "texmacs/menus/main-menu.so"
    "tools/ai/ai-batch.so"
    "fonts/fonts-ec.so"
    "fonts/fonts-adobe.so"
    "fonts/fonts-x.so"
    "fonts/fonts-math.so"
    "fonts/fonts-foreign.so"
    "fonts/fonts-misc.so"
    "fonts/fonts-composite.so"
    "fonts/fonts-truetype.so"
    "database/title-transform.so"
    "database/title-markup.so"
    "utils/edit/selections.so"
    "texmacs/keyboard/prefix-kbd.so"
    "utils/edit/auto-close.so"
    "generic/format-edit.so"
    "text/text-drd.so"
    "text/text-edit.so"
    "texmacs/keyboard/latex-kbd.so"
    "generic/embedded-edit.so"
    "generic/format-drd.so"
    "generic/format-geometry-edit.so"
    "source/source-drd.so"
    "source/source-edit.so"
    "doc/help-funcs.so"
    "generic/generic-kbd.so"
    "generic/generic-speech.so"
    "generic/generic-speech-en.so"
    "generic/generic-speech-fr.so"
    "text/text-kbd.so"
    "math/math-drd.so"
    "convert/tools/tmconcat.so"
    "math/math-edit.so"
    "table/table-edit.so"
    "math/math-kbd.so"
    "math/math-stats.so"
    "math/math-speech.so"
    "text/text-speech.so"
    "math/math-speech-en.so"
    "text/text-speech-en.so"
    "math/math-speech-fr.so"
    "text/text-speech-fr.so"
    "source/source-kbd.so"
    "dynamic/dynamic-drd.so"
    "dynamic/fold-edit.so"
    "dynamic/fold-kbd.so"
    "utils/plugins/plugin-eval.so"
    "utils/plugins/plugin-cmd.so"
    "dynamic/scripts-drd.so"
    "dynamic/scripts-edit.so"
    "dynamic/scripts-kbd.so"
    "text/text-structure.so"
    "link/locus-edit.so"
    "version/version-drd.so"
    "version/version-edit.so"
    "dynamic/session-drd.so"
    "dynamic/session-edit.so"
    "dynamic/calc-drd.so"
    "dynamic/calc-edit.so"
    "dynamic/calc-table.so"
    "dynamic/calc-kbd.so"
    "utils/library/process.so"
    "tools/spell/spell-edit.so"
    "tools/spell/spell-lantool.so"
    "tools/spell/spell-kbd.so"
    "texmacs/menus/print-widgets.so"
    "texmacs/menus/file-menu.so"
    "generic/generic-menu.so"
    "generic/document-menu.so"
    "text/text-menu.so"
    "generic/insert-menu.so"
    "tools/theme/theme-edit.so"
    "tools/theme/theme-menu.so"
    "convert/rewrite/init-rewrite.so"
    "doc/apidoc-collect.so"
    "doc/apidoc-funcs.so"
    "doc/apidoc-widgets.so"
    "prog/scheme-tools.so"
    "prog/scheme-menu.so"
    "doc/apidoc-markup.so"
    "doc/apidoc.so"
    "language/natural.so"
    "utils/misc/gui-utils.so"
    "texmacs/menus/developer-menu.so"
    "texmacs/menus/view-widgets.so"
    "texmacs/menus/view-menu.so"
    "database/db-base.so"
    "database/db-format.so"
    "database/db-users.so"
    "database/db-version.so"
    "database/db-edit.so"
    "database/db-convert.so"
    "database/db-tmfs.so"
    "database/db-widgets.so"
    "database/db-menu.so"
    "graphics/graphics-drd.so"
    "graphics/graphics-utils.so"
    "graphics/graphics-env.so"
    "graphics/graphics-object.so"
    "graphics/graphics-main.so"
    "graphics/graphics-single.so"
    "dynamic/animate-edit.so"
    "graphics/graphics-group.so"
    "graphics/graphics-animate.so"
    "graphics/graphics-edit.so"
    "utils/misc/gui-keyboard.so"
    "texmacs/menus/edit-menu.so"
    "generic/embedded-menu.so"
    "generic/format-menu.so"
    "doc/help-menu.so"
    "math/math-menu.so"))

(define (assemble-aot-images! . args)
  (let* ((boot-dir
          (cond ((>= (length args) 1) (car args))
                ((getenv "TEXMACS_BOOT_PATH") (getenv "TEXMACS_BOOT_PATH"))
                ((file-exists? "build-texmacos/boot") "build-texmacos/boot/")
                ((file-exists? "boot") "boot/")
                (else "build-texmacos/boot/")))
         (boot-dir (if (and (> (string-length boot-dir) 0)
                            (not (char=? (string-ref boot-dir (- (string-length boot-dir) 1)) #\/)))
                       (string-append boot-dir "/")
                       boot-dir))
         (full-boot? (let ((v (getenv "TEXMACS_BOOT_FULL")))
                       (and v (or (string=? v "1") (string=? v "true") (string=? v "yes")))))
         (order-file (string-append boot-dir "boot-order-full.scm"))
         (out-boot
          (cond ((>= (length args) 2) (cadr args))
                ((getenv "TEXMACS_BOOT_FILE") (getenv "TEXMACS_BOOT_FILE"))
                (else (string-append boot-dir "texmacs.boot")))))

    (when (and full-boot? (not (file-exists? order-file)))
      (error 'make-boot "Boot order manifest not found" order-file))

    (let* ((raw-boot-order
            (if full-boot?
                (read (open-input-file order-file))
                *startup-boot-modules*))
           (boot-order
            (if full-boot?
                (make-boot:patch-cyclic-boot-order raw-boot-order)
                raw-boot-order))
           (raw-so-paths (map (lambda (rel) (string-append boot-dir rel)) boot-order))
           (legacy-so (string-append boot-dir "texmacs.so")))

      (for-each
        (lambda (f)
          (unless (file-exists? f)
            (error 'make-boot "Missing compiled module .so file" f)))
        raw-so-paths)

      (when (file-exists? legacy-so)
        (delete-file legacy-so))

      ;; Generate native Chez boot file: texmacs.boot (CSUG 12.1)
      ;; TEXMACS_BOOT_MODULE_COUNT allows packing only the first N modules into
      ;; texmacs.boot. During Sbuild_heap(), Chez evaluates registered boot files
      ;; before user exception handlers are established. If any module triggers a
      ;; low-level crash or memory corruption during startup, setting this variable
      ;; (e.g. TEXMACS_BOOT_MODULE_COUNT=77) enables fast binary-search bisection
      ;; to isolate the offending module. Defaults to all modules when unset.
      (let* ((count (or (and (getenv "TEXMACS_BOOT_MODULE_COUNT")
                             (string->number (getenv "TEXMACS_BOOT_MODULE_COUNT")))
                        (length raw-so-paths)))
             (boot-so-paths (list-head raw-so-paths (min count (length raw-so-paths)))))
        (printf "Generating ~a (native boot file with ~a/~a modules)...\n"
                out-boot (length boot-so-paths) (length raw-so-paths))
        (let ((use-vfasl? (let ((v (getenv "TEXMACS_BOOT_VFASL")))
                            (not (and v (or (string=? v "0") (string=? v "false") (string=? v "no")))))))
          (if use-vfasl?
              (let ((tmp-boot (string-append out-boot ".tmp")))
                (when (file-exists? tmp-boot) (delete-file tmp-boot))
                (apply make-boot-file tmp-boot '("petite" "scheme") boot-so-paths)
                (printf "Converting ~a to high-performance VFASL boot format...\n" out-boot)
                (when (file-exists? out-boot) (delete-file out-boot))
                (vfasl-convert-file tmp-boot out-boot '("petite" "scheme"))
                (delete-file tmp-boot))
              (begin
                (when (file-exists? out-boot) (delete-file out-boot))
                (apply make-boot-file out-boot '("petite" "scheme") boot-so-paths))))
        (let ((sz (file-length (open-file-input-port out-boot))))
          (printf "SUCCESS: Created AOT boot image ~a (~,2f MB, ~a bytes)\n"
                  out-boot (/ sz 1048576.0) sz))))))

;; If executed directly via scheme --script, run immediately
(let ((args (command-line-arguments)))
  (cond
    ((and (pair? args) (string=? (car args) "--print-startup-excludes"))
     (for-each
       (lambda (rel)
         (display "/")
         (display rel)
         (newline))
       *startup-boot-modules*))
    ((or (null? args)
         (and (pair? args)
              (not (string=? (car args) "-compile-modules"))))
     (apply assemble-aot-images! args))))
