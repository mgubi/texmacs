
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-project.scm
;; DESCRIPTION : projects, descriptions of changes and snapshots for Git
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version git-project)
  (:use (version version-git)
        (version version-merge)))

(define-preferences
  ("git simple mode" "off" noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files used by a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The files on which a document depends are its included documents
;; (recursively), images, bibliography files and local style files.

(define (existing-file base name . suffixes)
  (with l (map (lambda (suf) (url-relative base (string-append name suf)))
               (cons "" suffixes))
    (list-find l (lambda (u) (and (url-exists? u) (not (url-directory? u)))))))

(define (tree-references base t)
  ;; References to files in the stree t of the document base
  (cond ((npair? t) '())
        ((and (tm-func? t 'include 1) (string? (cadr t)))
         (list (list 'include (cadr t))))
        ((and (tm-is? t 'image) (pair? (cdr t)) (string? (cadr t)))
         (list (list 'file (cadr t))))
        ((and (tm-is? t 'bibliography) (>= (length t) 4) (string? (cadddr t))
              (!= (cadddr t) ""))
         (list (list 'bib (cadddr t))))
        ((and (tm-is? t 'style) (pair? (cdr t)))
         (with st (cadr t)
           (map (lambda (s) (list 'style s))
                (list-filter (if (tm-is? st 'tuple) (cdr st) (list st))
                             string?))))
        (else (append-map (cut tree-references base <>) (cdr t)))))

(define (document-references u)
  (with t (tree->stree (if (buffer-exists? u) (buffer-get u)
                           (tree-import u "texmacs")))
    (tree-references u t)))

(tm-define (git-document-dependencies u)
  (:synopsis "The document @u and the files which it uses")
  ;; NOTE: the table associates urls to their system paths
  (with done (make-ahash-table)
    (let loop ((u u))
      (when (not (ahash-ref done (url->system u)))
        (ahash-set! done (url->system u) u)
        (when (git-texmacs-file? u)
          (for (r (document-references u))
            (with (kind name) r
              (and-with f (cond ((== kind 'include) (existing-file u name))
                                ((== kind 'bib) (existing-file u name ".bib"))
                                ((== kind 'style) (existing-file u name ".ts"))
                                (else (existing-file u name)))
                (if (in? kind '(include style))
                    (loop f)
                    (ahash-set! done (url->system f) f))))))))
    (map cdr (ahash-table->list done))))

(tm-define (git-project-document u)
  (:synopsis "The main document of the project containing @u")
  (if (and (== u (current-buffer)) (project-attached?)) (project-get) u))

(tm-define (git-project-files u)
  (:synopsis "The files of the project of @u in its working tree, as paths")
  (and-with root (git-root u)
    (with l (git-document-dependencies (git-project-document u))
      (map (cut git-relative root <>)
           (list-filter l (lambda (f) (with r (git-root f)
                                        (and r (== r root)))))))))

(tm-define (git-project-untracked u)
  (:synopsis "The files used by the project of @u which are not tracked")
  (and-with root (git-root u)
    (with l (or (git-project-files u) '())
      (list-filter l (lambda (p)
                       (== (git-file-state (git-absolute root p))
                           'untracked))))))

(tm-define (git-add-project-files u)
  (:synopsis "Add the files used by the project of @u to the repository")
  (and-with root (git-root u)
    (with l (or (git-project-untracked u) '())
      (when (nnull? l)
        (git-report (git-run-list root (append (list "add" "--") l))
                    (string-append "Added " (number->string (length l))
                                   " files"))
        (git-refresh root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Describing changes (for commit messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define section-tags
  '(part part* chapter chapter* section section* subsection subsection*
    subsubsection subsubsection* paragraph paragraph* appendix
    bibliography-text))

(define (plain-text t)
  (with s (tm-string-trim-both (cpp-texmacs->verbatim (tm->tree t) #f "utf-8"))
    (if (> (string-length s) 40) (string-append (substring s 0 37) "...") s)))

(define (section-title p)
  (and (pair? p) (in? (car p) section-tags) (pair? (cdr p))
       (plain-text (cAr p))))

(define (sections-of-changes old new)
  ;; Titles of the sections of the paragraph list new (or old, for removed
  ;; paragraphs) which contain changes; "" stands for the text before the
  ;; first section
  (define (collect l m)
    (let loop ((l l) (i 0) (title "") (acc '()))
      (if (null? l) (reverse acc)
          (let* ((t (or (section-title (car l)) title))
                 (changed? (not (vector-ref m i))))
            (loop (cdr l) (+ i 1) t
                  (if (and changed? (nin? t acc)) (cons t acc) acc))))))
  (let* ((l1 (collect new (version-match new old)))
         (l2 (collect old (version-match old new))))
    (append l1 (list-filter l2 (lambda (x) (nin? x l1))))))

(define (body-paragraphs s u)
  (and (!= s "")
       (and-with b (document-body (tree->stree (tree-import-loaded
                                                s u "texmacs")))
         (if (tm-is? b 'document) (cdr b) (list b)))))

(define (describe-entry root e)
  (let* ((path (git-entry-path e))
         (u (git-absolute root path)))
    (cond ((git-entry-untracked? e) (string-append "Add " path))
          ((== (git-entry-index e) #\A) (string-append "Add " path))
          ((or (== (git-entry-index e) #\D) (== (git-entry-worktree e) #\D))
           (string-append "Remove " path))
          ((git-entry-orig e)
           (string-append "Rename " (git-entry-orig e) " to " path))
          ((git-texmacs-file? u)
           (let* ((old (body-paragraphs (git-show-file root "HEAD" path) u))
                  (new (body-paragraphs (string-load u) u))
                  (l (if (and old new) (sections-of-changes old new) '()))
                  (l* (map (lambda (x) (if (== x "") "beginning" x)) l)))
             (if (null? l*)
                 (string-append "Update " path)
                 (string-append "Update " path ": "
                                (string-recompose l* ", ")))))
          (else (string-append "Update " path)))))

(tm-define (git-describe-changes root entries)
  (:synopsis "A proposed commit message (list of lines) for @entries")
  (with l (map (cut describe-entry root <>) entries)
    (cond ((null? l) (list ""))
          ((null? (cdr l)) l)
          (else (cons* (string-append "Update " (number->string (length l))
                                      " files")
                       ""
                       (map (lambda (x) (string-append "- " x)) l))))))

(define (git-short-rev rev)
  (if (> (string-length rev) 7) (string-take rev 7) rev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snapshots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For users who do not want to deal with staging: a snapshot records the
;; state of all files of the working tree, and restoring a snapshot puts
;; back the files as they were, as a new change (the history is kept).

(tm-define (git-simple-mode?)
  (== (get-preference "git simple mode") "on"))

(tm-define (git-toggle-simple-mode)
  (:synopsis "Toggle the simple mode, with snapshots instead of commits")
  (:check-mark "v" git-simple-mode?)
  (set-preference "git simple mode" (if (git-simple-mode?) "off" "on")))

(tm-define (git-save-snapshot root msg . opt-done)
  (:synopsis "Save the state of all files of @root, described by @msg")
  ;; The message @msg is in the utf8 encoding; the optional argument
  ;; is called with #t when the snapshot has been saved
  (with done (if (null? opt-done) ignore (car opt-done))
    (if (== (tm-string-trim-both msg) "")
        (set-message "Please describe the snapshot" "Snapshot")
        (git-when-saved root
          (lambda ()
            (with ret (git-run root "add" "--all")
              (if (not (git-ok? ret))
                  (git-report ret "Snapshot")
                  (done (git-commit-staged root msg)))))))))

(tm-define (git-snapshots root)
  (:synopsis "The most recent snapshots (commits) of @root")
  (git-log root 0 15))

(tm-define (git-restore-snapshot-now root rev)
  ;; NOTE: restoring discards the current state of the files, so this
  ;; state is first saved in an automatic snapshot, if it has changes
  (let* ((changes? (nnull? (git-status-entries root)))
         (saved? (or (not changes?)
                     (and (git-ok? (git-run root "add" "--all"))
                          (git-ok? (git-run-with-input
                                    root (string-append
                                          "Automatic snapshot before "
                                          "restoring " (git-short-rev rev))
                                    "commit" "--file=-"))))))
    (git-invalidate root)
    (if (not saved?)
        (set-message "Could not save the current state; nothing restored"
                     "Snapshot")
        (git-with-reload root
          (lambda ()
            (git-report (git-run root "restore" (string-append "--source=" rev)
                                 "--staged" "--worktree" "--" ".")
                        "Restored snapshot"))))))

(tm-define (git-restore-snapshot root rev)
  (:synopsis "Put back all files of @root as they were at @rev")
  (if (not (git-safe-name? rev))
      (set-message "Invalid snapshot" "Snapshot")
      (user-confirm (string-append "Put back all files as they were in the "
                                   "snapshot " (git-short-rev rev)
                                   "? The current state is first saved in "
                                   "an automatic snapshot.") #f
        (lambda (answ)
          (when answ
            (git-when-saved root
              (lambda () (git-restore-snapshot-now root rev))))))))

(tm-define (git-sync root)
  (:synopsis "Get the changes of the others, then send ours")
  (if (null? (git-remotes root))
      (set-message "This repository has no remote to synchronize with" "Git")
      (git-sync-now root)))

(define (git-sync-now root)
  (git-pull root
    (lambda (ret)
      (when (and (git-ok? ret) (not (git-merging? root))
                 (> (or (git-status-ref (git-status root) 'ahead) 0) 0))
        (git-push root)))))
