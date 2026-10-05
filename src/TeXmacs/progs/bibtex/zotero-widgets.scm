
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
    (bold (text (string-append name ": changed in TeXmacs and in Zotero")))
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
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (with r (zotero-sync-database #t)
        (set-message (or (zotero-sync-message r)
                         "The references from Zotero are up to date")
                     "Zotero")
        (resolve-conflicts (cadddr r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking the citations against Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keys-text l)
  (string-recompose l ", "))

(tm-define (zotero-check-lines r)
  ;; The lines of the report @r of zotero-check-document
  (let* ((get (lambda (cat) (or (assoc-ref r cat) '())))
         (count (lambda (n what)
                  (string-append (number->string n) " " what))))
    (append
     (list (count (length (get 'zotero)) "citations found in Zotero")
           (count (length (get 'elsewhere))
                  "citations found in the other sources"))
     (map (lambda (p) (string-append (car p) " is now " (cdr p)
                                     " in Zotero"))
          (get 'renamed))
     (if (null? (get 'deleted)) '()
         (list (string-append "No longer in Zotero (the exported copy is "
                              "kept): " (keys-text (get 'deleted)))))
     (if (null? (get 'missing)) '()
         (list (string-append "Not found: " (keys-text (get 'missing)))))
     (map (lambda (k) (string-append k " is a different work in Zotero "
                                     "and in the other source, which wins"))
          (get 'collisions))
     (if (null? (get 'copies)) '()
         (list (string-append "Copies of Zotero items in the database, "
                              "not kept in sync: "
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
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (with r (zotero-check-document)
        (dialogue-window
         (zotero-check-widget r)
         (lambda (what)
           (cond ((== what 'rename) (zotero-update-citations))
                 ((== what 'adopt)
                  (with conflicts (zotero-adopt-entries (assoc-ref r 'copies))
                    (set-message
                     (string-append "The copies are kept in sync with Zotero"
                                    (if (null? conflicts) ""
                                        "; choose the fields of those which differ"))
                     "Zotero")
                    (resolve-conflicts conflicts)))))
         "Check against Zotero"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define settings-status "")

(define (settings-test)
  (zotero-forget-state)
  (zotero-forget-keys)
  (set! settings-status (zotero-status-message (zotero-status)))
  (refresh-now "zotero-settings-status"))

(define (set-zotero-preference which val)
  (set-preference which val)
  (zotero-forget-state)
  (zotero-forget-keys))

(tm-widget ((zotero-settings-widget) cmd)
  (padded
    (aligned
      (item (text "Zotero server:")
        (input (when answer (set-zotero-preference "zotero server" answer))
               "string" (list (get-preference "zotero server")) "20em"))
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
        (toggle (set-preference "zotero in database search"
                                (if answer "on" "off"))
                (!= (get-preference "zotero in database search") "off"))))
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
        (string-append "Zotero (local API) at " (get-preference "zotero server")))
  (dialogue-window (zotero-settings-widget) noop "Zotero settings"))
