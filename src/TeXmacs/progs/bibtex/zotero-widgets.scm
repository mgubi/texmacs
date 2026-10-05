
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : zotero-widgets.scm
;; DESCRIPTION : dialogs for the citations from Zotero
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: (bibtex zotero-db) loads the modules of the database: it is only
;; loaded when they are needed (the database tool, or the search window of
;; references), through the lazy definitions
(texmacs-module (bibtex zotero-widgets)
  (:use (bibtex zotero)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keeping the database in sync, entries changed on both sides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (field-text v)
  ;; The value @v of a field, as one line of text (in cork)
  (cond ((not v) "(none)")
        ((string? v) v)
        (else (with s (convert v "texmacs-stree" "verbatim-snippet")
                (string-replace (if (string? s) (utf8->cork s) "?")
                                "\n" " ")))))

(define conflict-choices '())

(tm-widget ((zotero-conflict-widget name rows) cmd)
  (padded
    (bold (text (zotero-tr "%1: changed in TeXmacs and in Zotero" name)))
    ===
    (text "Choose the value to keep for each field")
    ===
    (resize '("400px" "650px" "9999px") '("150px" "320px" "9999px")
      (scrollable
        ;; NOTE: no for inside aligned
        (vlist
          (for (row rows)
            (hlist (bold (text (car row))) >>)
            (hlist // (text "TeXmacs: ") (text (field-text (cadr row))) >>)
            (hlist // (text "Zotero: ") (text (field-text (caddr row))) >>)
            (hlist
              // (text "Keep: ") //
              (enum (set! conflict-choices
                          (assoc-set! conflict-choices (car row)
                                      (if (== answer "Zotero")
                                          'zotero 'texmacs)))
                    '("TeXmacs" "Zotero")
                    (if (== (list-ref row 4) 'zotero) "Zotero" "TeXmacs")
                    "8em")
              >>)
            ===))))
    ===
    (bottom-buttons >>
      ("Later" (cmd #f)) // //
      ("Save" (cmd #t)))))

(define (resolve-conflicts l)
  ;; One dialog per entry changed on both sides
  (when (nnull? l)
    (let* ((old (car (car l)))
           (new (cdr (car l)))
           (rows (zotero-conflict-fields old new)))
      (set! conflict-choices
            (map (lambda (row) (cons (car row) (list-ref row 4))) rows))
      (dialogue-window
       (zotero-conflict-widget (list-ref old 3) rows)
       (lambda (save?)
         (when save?
           (zotero-merge-entries old new conflict-choices))
         (resolve-conflicts (cdr l)))
       "Reference changed on both sides"))))

(tm-define (zotero-synchronize)
  (:synopsis "Update the references of the database which come from Zotero")
  (:interactive #t)
  (zotero-forget-state)
  (zotero-command synchronize-again synchronize))

(define (synchronize-again)
  (zotero-command synchronize-again synchronize))

(define (synchronize)
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (with r (zotero-sync-database #t)
        ;; NOTE: in a web browser, once all the answers have come
        (when (not (zotero-asking?))
          (set-message (or (zotero-sync-message r)
                           "The references from Zotero are up to date")
                       "Zotero")
          (resolve-conflicts (cadddr r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking the citations against Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keys-text l)
  (string-recompose l ", "))

(tm-define (zotero-check-lines r)
  ;; The lines of the report @r of zotero-check-document
  (let* ((get (lambda (cat) (or (assoc-ref r cat) '())))
         (count (lambda (cat what)
                  (zotero-tr what (number->string (length (get cat)))))))
    (append
     (list (count 'zotero "%1 citations found in Zotero")
           (count 'elsewhere "%1 citations found in the other sources"))
     (map (lambda (p) (zotero-tr "%1 is now %2 in Zotero" (car p) (cdr p)))
          (get 'renamed))
     (if (null? (get 'deleted)) '()
         (list (zotero-tr (string-append "No longer in Zotero (the exported "
                                         "copy is kept): %1")
                          (keys-text (get 'deleted)))))
     (if (null? (get 'missing)) '()
         (list (zotero-tr "Not found: %1" (keys-text (get 'missing)))))
     (map (lambda (k)
            (zotero-tr (string-append "%1 is a different work in Zotero and "
                                      "in the other source, which wins")
                       k))
          (get 'collisions))
     (if (null? (get 'copies)) '()
         (list (zotero-tr (string-append "Copies of Zotero items in the "
                                         "database, not kept in sync: %1")
                          (keys-text (get 'copies))))))))

(tm-widget ((zotero-check-widget r) cmd)
  (padded
    (resize '("400px" "600px" "9999px") '("150px" "250px" "9999px")
      (scrollable
        (vlist
          (for (l (zotero-check-lines r))
            (hlist (text l) >>)))))
    ===
    (bottom-buttons >>
      ("Close" (cmd #f))
      (assuming (nnull? (assoc-ref r 'renamed))
        // // ("Update the citations" (cmd 'rename)))
      (assuming (nnull? (assoc-ref r 'copies))
        // // ("Keep the copies in sync" (cmd 'adopt))))))

(tm-define (open-zotero-check)
  (:synopsis "Check the citation keys of the document against Zotero")
  (:interactive #t)
  (zotero-forget-state)
  (zotero-command check-again check))

(define (check-again)
  (zotero-command check-again check))

(define (check)
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (with r (zotero-check-document)
        ;; NOTE: in a web browser, the report once all the answers have come
        (when (not (zotero-asking?))
        (dialogue-window
         (zotero-check-widget r)
         (lambda (what)
           (cond ((== what 'rename) (zotero-update-citations))
                 ((== what 'adopt)
                  (with conflicts (zotero-adopt-entries (assoc-ref r 'copies))
                    (set-message
                     (zotero-tr (if (null? conflicts)
                                    "The copies are kept in sync with Zotero"
                                    (string-append
                                     "The copies are kept in sync with Zotero; "
                                     "choose the fields of those which differ")))
                     "Zotero")
                    (resolve-conflicts conflicts)))))
         "Check against Zotero")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define settings-status "")

(define (settings-test)
  (zotero-forget-state)
  (zotero-forget-keys)
  (settings-show-status))

(define (settings-show-status)
  ;; in a web browser the state may come later, and is shown again then
  ;; NOTE: without forgetting again, which would ask again forever
  (set! settings-status
        (zotero-with-retry settings-show-status
                           (lambda () (zotero-status-message (zotero-status)))))
  (refresh-now "zotero-settings-status"))

(define (settings-changed)
  ;; the search window of references, when it is open, shows its results
  ;; and its sources again (database/db-widgets.scm, when it is loaded)
  (catch #t (lambda () (db-search-refresh)) (lambda args #f)))

(define (set-zotero-preference which val)
  (set-preference which val)
  (zotero-forget-state)
  (zotero-forget-keys)
  (settings-changed))

(define source-names
  '(("auto" . "Automatic") ("local" . "The Zotero application")
    ("web" . "zotero.org")))

(define (source-name) (assoc-ref source-names (get-preference "zotero source")))

(define (set-source name)
  (with x (list-find source-names (lambda (p) (== (cdr p) name)))
    (when x (set-zotero-preference "zotero source" (car x)))))

(tm-widget ((zotero-settings-widget) cmd)
  (padded
    (aligned
      (item (text "Read the library from:")
        (enum (set-source answer)
              (map cdr (if (zotero-in-browser?)
                           ;; NOTE: a web page cannot reach the application
                           (list-filter source-names
                                        (lambda (p) (!= (car p) "local")))
                           source-names))
              (or (source-name) "Automatic") "20em"))
      (item (text "API key of zotero.org:")
        (input (when (and answer (!= answer (zotero-api-key-shown)))
                 (zotero-set-api-key (tm-string-trim-both answer))
                 (settings-changed))
               "string" (list (zotero-api-key-shown)) "20em"))
      ;; NOTE: a web page cannot reach the application
      (assuming (not (zotero-in-browser?))
        (item (text "Zotero server:")
          (input (when answer (set-zotero-preference "zotero server" answer))
                 "string" (list (get-preference "zotero server")) "20em")))
      (item (text "Libraries:")
        (enum (set-zotero-preference "zotero libraries"
                                     (if (== answer "My Library and groups")
                                         "all" "user"))
              '("My Library" "My Library and groups")
              (if (== (get-preference "zotero libraries") "all")
                  "My Library and groups" "My Library")
              "20em"))
      (item (text "Export format:")
        (enum (set-zotero-preference "zotero export format" answer)
              '("bibtex" "biblatex")
              (get-preference "zotero export format") "20em"))
      (item (text "Complete keys from Zotero:")
        (toggle (set-preference "zotero completion" (if answer "on" "off"))
                (== (get-preference "zotero completion") "on")))
      (item (text "Search Zotero in the search of references:")
        (toggle (begin
                  (set-preference "zotero in database search"
                                  (if answer "on" "off"))
                  (settings-changed))
                (!= (get-preference "zotero in database search") "off")))
      (item (text "Add the references of Zotero to the BibTeX file:")
        (toggle (set-preference "zotero add to bib file"
                                (if answer "on" "off"))
                (!= (get-preference "zotero add to bib file") "off"))))
    ===
    (hlist
      (text "A read-only key is made at https://www.zotero.org/settings/keys")
      >>)
    ===
    (refreshable "zotero-settings-status"
      (hlist (text settings-status) >>))
    ===
    (bottom-buttons >>
      ("Test the connection" (settings-test)) // //
      ("Close" (cmd #f)))))

(tm-define (open-zotero-settings)
  (:synopsis "The settings of the citations from Zotero")
  (:interactive #t)
  (set! settings-status
        (if (zotero-web?)
            (zotero-tr (if (zotero-api-key) "zotero.org, with an API key"
                           "zotero.org: give an API key"))
            (zotero-tr "Zotero (local API) at %1"
                       (get-preference "zotero server"))))
  (dialogue-window (zotero-settings-widget) noop "Zotero settings"))
