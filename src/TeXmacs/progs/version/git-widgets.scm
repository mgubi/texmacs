
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-widgets.scm
;; DESCRIPTION : dialogs for the Git tools
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version git-widgets)
  (:use (version version-git)
        (version git-project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The dialog lists all changed files; the selected ones are those which will
;; be committed.  Initially, these are the files with staged changes.
;; A selected file without staged changes is staged entirely; for a selected
;; file with staged changes, only the staged changes are committed; the
;; changes of files which are not selected are unstaged.
;; During a merge, all changes have to be committed together.

(define (commit-candidates root)
  (list-filter (git-status-entries root)
               (lambda (e) (not (git-entry-conflicted? e)))))

(define (commit-label e)
  (string-append (utf8->cork (git-entry-path e))
                 (cond ((git-entry-untracked? e) "  (new)")
                       ((and (git-entry-staged? e) (git-entry-unstaged? e))
                        "  (partially staged)")
                       ((or (== (git-entry-index e) #\D)
                            (== (git-entry-worktree e) #\D)) "  (deleted)")
                       (else ""))))

(define (commit-message u)
  (with t (buffer-get-body u)
    (tm-string-trim-both (cpp-texmacs->verbatim t #f "utf-8"))))

(define (initially-selected entries labels . opt-paths)
  ;; The files with staged changes and the files in opt-paths
  (with paths (if (null? opt-paths) '() (car opt-paths))
    (list-filter labels
                 (lambda (lab)
                   (with e (list-ref entries (list-find-index labels
                                                              (cut == <> lab)))
                     (or (git-entry-staged? e)
                         (in? (git-entry-path e) paths)))))))

(define (selected-entries entries labels selected)
  (list-filter entries
               (lambda (e)
                 (in? (list-ref labels (list-find-index entries
                                                        (cut == <> e)))
                      selected))))

(define (suggest-message root u entries labels selected)
  (with l (git-describe-changes root (selected-entries entries labels
                                                        selected))
    (buffer-set-body u `(document ,@(map utf8->cork l)))))

(define (commit-update-index root entries labels selected)
  ;; Stage the selected files without staged changes, unstage the others
  (list-and
   (map (lambda (e lab)
          (let* ((paths (git-entry-paths e))
                 (sel? (in? lab selected)))
            (cond ((and sel? (not (git-entry-staged? e)))
                   (git-ok? (git-run-list root (append (list "add" "--all" "--")
                                                       paths))))
                  ((and (not sel?) (git-entry-staged? e))
                   (git-ok? (git-unstage-paths root paths)))
                  (else #t))))
        entries labels)))

(define (commit-now root u entries labels selected amend?)
  ;; Returns #t if the dialog can be closed
  (let* ((msg (commit-message u))
         (merging? (git-merging? root))
         (initial (initially-selected entries labels)))
    (cond ((and (== msg "") (not amend?))
           (set-message "Please enter a commit message" "Git commit")
           #f)
          ((and merging? (list-find (git-status-entries root)
                                    git-entry-conflicted?))
           (set-message "Please resolve all conflicts first" "Git commit")
           #f)
          ((and merging? (not (== (length selected)
                                  (length initial))))
           (set-message "During a merge, all changes must be committed"
                        "Git commit")
           #f)
          ((and (null? selected) (not amend?) (not merging?))
           (set-message "Nothing selected for commit" "Git commit")
           #f)
          ((not (or merging? (commit-update-index root entries labels
                                                  selected)))
           (git-refresh root)
           (set-message "Could not prepare the files for commit" "Git commit")
           #f)
          ((and amend? (== msg ""))
           (with ok? (git-report (git-run-list root
                                               (append (list "commit" "--amend"
                                                             "--no-edit")
                                                       (git-commit-options)))
                                 "Amended commit")
             (git-refresh root)
             ok?))
          (amend? (git-commit-staged root msg :amend))
          (else (git-commit-staged root msg)))))

(define (commit-initial-message root entries labels selected)
  ;; The prepared message of a merge, or a suggested message
  (with l (and (git-merging? root) (git-merge-message root))
    (cond ((and l (nnull? l)) `(document ,@(map utf8->cork l)))
          ((and (get-boolean-preference "git suggest messages")
                (nnull? selected))
           `(document ,@(map utf8->cork
                             (git-describe-changes
                              root (selected-entries entries labels
                                                     selected)))))
          (else '(document "")))))

(define (long-summary? u)
  (with l (git-split (commit-message u) "\n")
    (and (nnull? l) (> (string-length (car l)) 72))))

(tm-widget ((git-commit-widget root u paths) quit)
  (let* ((entries (commit-candidates root))
         (labels (map commit-label entries))
         (selected (initially-selected entries labels paths))
         (amend? #f)
         (merging? (git-merging? root))
         (branch (or (git-current-branch root) "(detached)")))
    (padded
      (text (string-append (if merging? "Merge commit on branch "
                               "Commit on branch ")
                           (utf8->cork branch) " in " (url->system root)))
      (if merging?
          (text "A merge commit contains all changes: keep all files selected"))
      ===
      (bold (text "Commit message"))
      (text "A short summary on the first line, then the details if needed")
      ===
      (resize "500px" "120px"
        (texmacs-input (commit-initial-message root entries labels selected)
                       `(style (tuple "generic")) u))
      ===
      (hlist
        (bold (text "Files to commit"))
        >>
        ("All" (begin (set! selected labels)
                      (refresh-now "git-commit-files")))
        // //
        ("None" (begin (set! selected '())
                       (refresh-now "git-commit-files"))))
      ===
      (resize "500px" "180px"
        (refreshable "git-commit-files"
          (scrollable
            (choices (set! selected answer) labels selected))))
      ===
      (hlist
        (toggle (set! amend? answer) amend?) // (text "Amend last commit")
        >>
        (explicit-buttons
          ("Suggest message"
           (suggest-message root u entries labels selected))
          // //
          ("Cancel" (quit))
          // //
          ("Commit"
           (let* ((long? (long-summary? u)))
             (when (commit-now root u entries labels selected amend?)
               (when long?
                 (set-message (string-append "Tip: keep the first line of "
                                             "the message under 72 "
                                             "characters") "Git commit"))
               (quit)))))))))

(define commit-dialogs 0)

(tm-define (git-interactive-commit . opt)
  (:synopsis "Open a dialog for committing changes in the working tree")
  ;; Optional arguments: the root and the paths to be selected initially
  (:interactive #t)
  (and-with root (if (null? opt) (current-git-root) (car opt))
    ;; NOTE: each dialog needs its own buffer for the message
    (set! commit-dialogs (+ commit-dialogs 1))
    (let* ((u (string->url (string-append "tmfs://aux/git-commit-"
                                          (number->string commit-dialogs))))
           (b (current-buffer))
           (paths (if (or (null? opt) (null? (cdr opt))) '() (cadr opt))))
      ;; NOTE: Git commits what is on disk
      (git-when-saved root
        (lambda ()
          (git-invalidate root)
          (buffer-set-master u b)
          (dialogue-window (git-commit-widget root u paths)
                           (lambda x (noop))
                           "Git commit" u))))))

(tm-define (git-interactive-commit-project name)
  (:synopsis "Commit the changes to the files used by the document @name")
  (:interactive #t)
  (and-with root (git-root name)
    (git-interactive-commit root (or (git-project-files name) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Side tool with the status of the working tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tool-root win)
  (git-buffer-root (window->buffer win)))

(define (tool-sync-text root)
  (let* ((st (git-status root))
         (up (git-status-ref st 'upstream))
         (ahead (or (git-status-ref st 'ahead) 0))
         (behind (or (git-status-ref st 'behind) 0)))
    (cond ((not up) "")
          ((and (== ahead 0) (== behind 0)) "up to date")
          (else (string-append
                 (if (> ahead 0) (string-append (number->string ahead)
                                                " to send ") "")
                 (if (> behind 0) (string-append (number->string behind)
                                                 " to get") ""))))))

(define (tool-sections root simple?)
  ;; List of (title entries) for the changes tab
  (let* ((l (git-status-entries root))
         (conflicts (list-filter l git-entry-conflicted?))
         (staged (list-filter l git-entry-staged?))
         (changed (list-filter l git-entry-unstaged?))
         (untracked (list-filter l git-entry-untracked?)))
    (list-filter
     (if simple?
         (list (list "Conflicts" conflicts)
               (list "Changes" (list-filter l (lambda (e)
                                                (not (git-entry-conflicted?
                                                      e))))))
         (list (list "Conflicts" conflicts) (list "Staged" staged)
               (list "Changed" changed) (list "New files" untracked)))
     (lambda (x) (nnull? (cadr x))))))

(tm-widget (git-tool-entry root e section simple?)
  (let* ((u (git-absolute root (git-entry-path e)))
         (tm? (git-texmacs-file? u)))
    (hlist
      ((eval (utf8->cork (git-entry-path e)))
       (when (url-exists? u) (load-buffer u)))
      >>
      (if (== section "Conflicts")
          (if tm?
              ("Resolve" (begin (load-buffer u) (git-resolve-conflict u))))
          ("Resolved" (git-mark-resolved u)))
      (if (and (in? section '("Changed" "Changes")) tm? (url-exists? u)
               (not (git-entry-untracked? e)))
          ("Compare" (git-compare-with u "HEAD")))
      (if (and (not simple?) (in? section '("Changed" "New files")))
          ((balloon (icon "tm_add.xpm") "Stage") (git-stage u)))
      (if (and (not simple?) (== section "Staged"))
          ((balloon (icon "tm_close_tool.xpm") "Unstage") (git-unstage u))))))

(tm-widget (git-tool-changes root simple?)
  (with l (tool-sections root simple?)
    (if (null? l)
        (text "Nothing to commit: your work is saved"))
    (for (x l)
      (division "discrete"
        (bold (text (car x))))
      (division "plain"
        (for (e (cadr x))
          (dynamic (git-tool-entry root e (car x) simple?)))))))

;; The message of the commit box, one buffer per window

(define panel-buffers (make-ahash-table))

(define (panel-buffer win)
  (with key (url->string win)
    (or (ahash-ref panel-buffers key)
        (with u (string->url (string-append "tmfs://aux/git-panel-"
                                            (number->string
                                             (+ 1 (length (ahash-table->list
                                                           panel-buffers))))))
          (ahash-set! panel-buffers key u)
          u))))

(define (panel-commit root u simple?)
  ;; NOTE: the message is only cleared once it has been used
  (with msg (commit-message u)
    (cond ((== msg "")
           (set-message "Please describe the changes first" "Git"))
          (simple?
           (git-save-snapshot root msg
             (lambda (ok?) (when ok? (buffer-set-body u '(document ""))))))
          ((not (git-has-staged? root))
           (set-message "Stage some changes first, or use Commit..." "Git"))
          (else
            (git-when-saved root
              (lambda ()
                (when (git-commit-staged root msg)
                  (buffer-set-body u '(document "")))))))))

(define (panel-suggest win u)
  (and-with root (tool-root win)
    (buffer-set-body
     u `(document ,@(map utf8->cork
                         (git-describe-changes
                          root (if (git-simple-mode?)
                                   (git-status-entries root)
                                   (list-filter (git-status-entries root)
                                                git-entry-staged?))))))))

(tm-widget (git-tool-commit-box win)
  ;; NOTE: this part of the panel is not refreshed, so that the editor of
  ;; the message is not destroyed while typing
  (with u (panel-buffer win)
    (resize "250px" "60px"
      (texmacs-input (if (buffer-exists? u)
                         (tree->stree (buffer-get-body u))
                         '(document ""))
                     '(style (tuple "generic")) u))
    (hlist
      ("Suggest" (panel-suggest win u))
      >>
      ((eval (if (git-simple-mode?) "Save snapshot" "Commit"))
       (and-with root (tool-root win)
         (panel-commit root u (git-simple-mode?)))))))

(define (commit-path root c name)
  ;; The path of the document @name at the commit @c (renames!)
  (if (nnull? (git-commit-files c)) (car (git-commit-files c))
      (git-relative root name)))

(define (history-revision root c name)
  (string-append (git-commit-hash c) ":"
                 (url->tmfs-string (git-absolute root
                                                 (commit-path root c name)))))

(tm-widget (git-tool-history root name)
  (with l (if (and name (git-root name)) (or (git-file-log name) '()) '())
    (if (null? l) (text "No history for this document"))
    (division "plain"
      (for (c (sublist l 0 (min 20 (length l))))
        (hlist
          ((eval (string-append (git-commit-date c) " "
                                (utf8->cork (git-short-message
                                             (git-commit-subject c)))))
           (revert-buffer-revert (tmfs-url-commit root (git-commit-hash c))))
          >>
          (if (git-texmacs-file? name)
              ("Compare"
               (git-compare-with name (history-revision root c name))))
          ("Restore"
           (git-restore-revision name (git-commit-hash c)
                                 (commit-path root c name))))))
    ===
    (hlist ("Full history" (git-show-log root)) >>)))

(tm-widget (git-tool-branches root)
  (with l (git-branches root)
    (division "plain"
      (for (b l)
        (hlist
          (if (git-branch-current? b)
              (bold (text (utf8->cork (git-branch-name b)))))
          (if (not (git-branch-current? b))
              (text (utf8->cork (git-branch-name b))))
          >>
          (if (not (git-branch-current? b))
              ("Switch" (git-switch-branch root (git-branch-name b)))))))
    ===
    (hlist ("New branch..." (git-interactive-create-branch root)) // //
           ("All branches" (git-show-branches root)) >>)))

(define (tool-context win)
  ;; The working tree, the document and the mode for the panel of win
  (list (tool-root win)
        (with b (window->buffer win)
          (and b (not (url-rooted-tmfs? b)) b))
        (git-simple-mode?)))

(tm-widget (git-tool-sync-bar win)
  (let* ((root (tool-root win))
         (simple? (git-simple-mode?))
         (busy? (and root (git-busy? root)))
         (remote? (and root (nnull? (git-remotes root)) (not busy?)))
         (branch (if root (or (git-current-branch root) "(no branch)") ""))
         (sync (cond ((not root) "")
                     (busy? "working...")
                     (else (tool-sync-text root)))))
    (if (not root)
        (text "The current document is not in a Git working tree"))
    (if root
        (hlist
          (bold (text (utf8->cork branch))) // //
          (text sync)
          >>
          (if busy? ("Cancel" (git-cancel root)))
          (if (and remote? simple?)
              ("Synchronize" (git-sync root)))
          (if (and remote? (not simple?))
              ((balloon (icon "tm_cloud_download.xpm") "Get changes (pull)")
               (git-pull root))
              ((balloon (icon "tm_cloud_upload.xpm") "Send changes (push)")
               (git-push root)))))))

(tm-widget (git-tool-changes-of win)
  (with (root name simple?) (tool-context win)
    (if root (dynamic (git-tool-changes root simple?)))))

(tm-widget (git-tool-history-of win)
  (with (root name simple?) (tool-context win)
    (if root (dynamic (git-tool-history root name)))))

(tm-widget (git-tool-branches-of win)
  (with (root name simple?) (tool-context win)
    (if root (dynamic (git-tool-branches root)))))

(tm-widget (git-tool-contents win)
  ;; NOTE: only the parts which depend on the state of the working tree
  ;; are refreshed (all refreshables with the same identifier are)
  (refreshable "git-tool"
    (dynamic (git-tool-sync-bar win)))
  ===
  (tabs
    (tab (text "Changes")
      (vlist
        (refreshable "git-tool"
          (dynamic (git-tool-changes-of win)))
        ===
        (dynamic (git-tool-commit-box win))))
    (tab (text "History")
      (refreshable "git-tool"
        (dynamic (git-tool-history-of win))))
    (tab (text "Branches")
      (refreshable "git-tool"
        (dynamic (git-tool-branches-of win))))))

(tm-tool* (git-tool win)
  (:name "Git")
  (dynamic (git-tool-contents win)))

(tm-define (git-open-tool)
  (:synopsis "Show the status of the working tree in a side tool")
  (:interactive #t)
  (git-with-mode (lambda () (tool-select :right 'git-tool))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A dialog with some fields (label default-value) and some check boxes
;; (label initial-value).  The action receives the values of the fields
;; (in utf8) and of the check boxes; it returns #f on success, or a
;; message explaining why the values are not acceptable, in which case
;; the dialog stays open.

(define form-error "")

(tm-widget ((git-form-widget fields toggles action) quit)
  (let* ((flags (list->vector (map cadr toggles))))
    (padded
      (form "git-form"
        (for (f fields)
          (hlist
            (text (car f)) >>
            (form-input (car f) "string" (list (cadr f)) "25em")))
        (for (i (.. 0 (length toggles)))
          (hlist
            (toggle (vector-set! flags i answer) (vector-ref flags i))
            // (text (car (list-ref toggles i))) >>))
        ===
        (refreshable "git-form-error"
          (if (!= form-error "")
              (hlist (text form-error) >>)))
        (bottom-buttons
          >>
          ("Cancel" (quit))
          // //
          ("Ok"
           (let* ((vals (map (lambda (x) (if (string? x) (cork->utf8 x) ""))
                             (form-values)))
                  (err (action vals (vector->list flags))))
             (if err
                 (begin
                   (set! form-error err)
                   (refresh-now "git-form-error"))
                 (quit)))))))))

(tm-define (git-form-dialog title fields toggles action)
  (:synopsis "Show a dialog with @fields and @toggles, validated by @action")
  (set! form-error "")
  (dialogue-window (git-form-widget fields toggles action) noop title))

;; A dialog with a message (several lines) and some check boxes; the
;; action receives the message (in utf8) and the values of the check boxes.

(define message-dialogs 0)

(tm-widget ((git-message-widget prompt u toggles action) quit)
  (let* ((flags (list->vector (map cadr toggles))))
    (padded
      (bold (text prompt))
      ===
      (resize "450px" "100px"
        (texmacs-input '(document "") '(style (tuple "generic")) u))
      ===
      (for (i (.. 0 (length toggles)))
        (hlist
          (toggle (vector-set! flags i answer) (vector-ref flags i))
          // (text (car (list-ref toggles i))) >>))
      (refreshable "git-form-error"
        (if (!= form-error "")
            (hlist (text form-error) >>)))
      (bottom-buttons
        >>
        ("Cancel" (quit))
        // //
        ("Ok"
         (let* ((msg (commit-message u))
                (err (action msg (vector->list flags))))
           (if err
               (begin
                 (set! form-error err)
                 (refresh-now "git-form-error"))
               (quit))))))))

(tm-define (git-message-dialog title prompt toggles action)
  (:synopsis "Show a dialog for entering a message, handled by @action")
  (set! form-error "")
  (set! message-dialogs (+ message-dialogs 1))
  (let* ((u (string->url (string-append "tmfs://aux/git-message-"
                                        (number->string message-dialogs))))
         (b (current-buffer)))
    (buffer-set-master u b)
    (dialogue-window (git-message-widget prompt u toggles action)
                     noop title u)))

(define (empty? s) (== (tm-string-trim-both s) ""))

(tm-define (git-interactive-commit-file name)
  (:synopsis "Commit the changes of the file @name")
  (:interactive #t)
  (git-message-dialog
   "Commit this file" (string-append "Describe the changes to "
                                     (url->system (url-tail name)))
   (list (list "Sign the commit" (git-signing?)))
   (lambda (msg flags)
     (if (empty? msg) "Please describe the changes"
         (with r (git-commit-file* name msg (car flags))
           (set-message (utf8->cork (cdr r)) "Commit file")
           (and (not (car r)) (utf8->cork (cdr r))))))))

(tm-define (git-interactive-save-snapshot root)
  (:synopsis "Save a snapshot of all files of @root")
  (:interactive #t)
  (git-message-dialog
   "Save snapshot" "Describe this snapshot" '()
   (lambda (msg flags)
     (if (empty? msg) "Please describe the snapshot"
         (begin (git-save-snapshot root msg) #f)))))

(tm-define (git-interactive-create-branch root)
  (:interactive #t)
  (git-form-dialog
   "New branch" (list (list "Name:" "")) (list (list "Switch to it" #t))
   (lambda (vals flags)
     (with name (car vals)
       (if (not (git-valid-branch-name? root name))
           "This is not a valid branch name"
           (begin (git-create-branch root name (car flags)) #f))))))

(tm-define (git-interactive-tag root)
  (:interactive #t)
  (git-form-dialog
   "Tag this version" (list (list "Name:" "") (list "Message:" ""))
   (list (list "Sign the tag" (git-signing?)))
   (lambda (vals flags)
     (with (name msg) vals
       (cond ((not (git-valid-tag-name? root name))
              "This is not a valid tag name")
             ((in? name (map git-branch-name (git-tags root)))
              "This tag already exists")
             (else (git-create-tag root name msg (car flags)) #f))))))

(tm-define (git-interactive-add-remote root)
  (:interactive #t)
  (git-form-dialog
   "Add remote"
   (list (list "Name:" (if (in? "origin" (git-remotes root)) "" "origin"))
         (list "URL:" ""))
   '()
   (lambda (vals flags)
     (with (name url) vals
       (cond ((not (git-valid-branch-name? root name))
              "This is not a valid name")
             ((in? name (git-remotes root)) "This remote already exists")
             ((not (git-safe-name? url)) "Please give the URL of the remote")
             (else (git-add-remote root name url) #f))))))

(tm-define (git-interactive-compare-with name)
  (:interactive #t)
  (git-form-dialog
   "Compare with revision"
   (list (list "Revision:" "HEAD~1")) '()
   (lambda (vals flags)
     (with root (git-root name)
       (if (not (and root (git-rev-parse root (string-append (car vals)
                                                             "^{commit}"))))
           "Unknown revision (try a hash, a tag, a branch or HEAD~2)"
           (begin (git-compare-with-revision name (car vals)) #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pretty-value l val)
  ;; l is a list of pairs (value . pretty-name)
  (or (assoc-ref l val) val))

(define (value-of-pretty l pretty)
  (with x (list-find l (lambda (p) (== (cdr p) pretty)))
    (if x (car x) pretty)))

(define versioning-tool-names
  '(("auto" . "Automatic") ("on" . "Always") ("off" . "Never")))

(define mode-names
  '(("on" . "Simple (snapshots)") ("off" . "Full (staging, branches)")))

(define pull-names
  '(("fast-forward" . "Ask before merging") ("merge" . "Merge")
    ("rebase" . "Rebase")))

(tm-widget (git-preferences-widget)
  (padded
    (aligned
      (item (text "Versioning tool:")
        (enum (set-preference "versioning tool"
                              (value-of-pretty versioning-tool-names answer))
              (map cdr versioning-tool-names)
              (pretty-value versioning-tool-names
                            (get-preference "versioning tool"))
              "15em"))
      (item (text "Mode:")
        (enum (set-preference "git simple mode"
                              (value-of-pretty mode-names answer))
              (map cdr mode-names)
              (pretty-value mode-names (get-preference "git simple mode"))
              "15em"))
      (item (text "When both sides changed:")
        (enum (set-preference "git pull mode"
                              (value-of-pretty pull-names answer))
              (map cdr pull-names)
              (pretty-value pull-names (get-preference "git pull mode"))
              "15em"))
      (item (text "Git executable:")
        (enum (set-preference "git executable" answer)
              (list (get-preference "git executable") "git" "")
              (get-preference "git executable") "15em"))
      (item (text "Warn for files larger than (MB):")
        (enum (set-preference "git large file size" answer)
              '("5" "10" "50" "100" "")
              (get-preference "git large file size") "5em"))
      (item (text "Commits examined by blame:")
        (enum (set-preference "git blame depth" answer)
              '("10" "30" "100" "")
              (get-preference "git blame depth") "5em"))
      (item (text "Commits per page of the log:")
        (enum (set-preference "git log length" answer)
              '("50" "250" "1000" "")
              (get-preference "git log length") "5em")))
    ======
    (aligned
      (meti (hlist // (text "Sign commits and tags with GnuPG"))
        (toggle (set-boolean-preference "git sign" answer)
                (get-boolean-preference "git sign")))
      (meti (hlist // (text "Suggest commit messages"))
        (toggle (set-boolean-preference "git suggest messages" answer)
                (get-boolean-preference "git suggest messages"))))))

(tm-define (open-git-preferences)
  (:synopsis "Open the preferences for the Git tools")
  (:interactive #t)
  (top-window git-preferences-widget "Git preferences"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choice of the mode, the first time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((git-mode-widget cont) quit)
  (padded
    (bold (text "How do you want to work with Git?"))
    ======
    (explicit-buttons
      ("Simple: save snapshots and synchronize with coauthors"
       (begin (set-preference "git simple mode" "on") (quit) (cont))))
    ===
    (explicit-buttons
      ("Full: staging, branches and remotes (for Git users)"
       (begin (set-preference "git simple mode" "off") (quit) (cont))))
    ======
    (text "You may change this later in the Git preferences.")))

(tm-define (git-with-mode cont)
  (:synopsis "Execute @cont, after asking for the mode the first time")
  (if (or (== (get-preference "git mode chosen") "on")
          (headless?) (not (current-window)))
      (cont)
      (begin
        (set-preference "git mode chosen" "on")
        (dialogue-window (git-mode-widget cont) noop "Git"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explaining failures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each entry: (patterns explanation action-label action), where the action
;; receives the working tree

(define failure-explanations
  (list
   (list '("[rejected]" "non-fast-forward" "fetch first")
         "Others sent changes first. Get their changes, then send yours again."
         "Get changes" (lambda (root) (git-pull root)))
   (list '("Authentication failed" "Permission denied" "could not read Username"
           "terminal prompts disabled" "access denied")
         (string-append "The server refused access. Check your credentials "
                        "(credential helper or SSH key).")
         #f #f)
   (list '("Could not resolve host" "unable to access" "Connection refused"
           "Network is unreachable" "timed out")
         "The server could not be reached. Check your network connection."
         #f #f)
   (list '("would be overwritten")
         (string-append "Some of your changes would be overwritten. Save "
                        "them in a snapshot or a commit first.")
         "Save snapshot..." (lambda (root) (git-interactive-save-snapshot root)))
   (list '("CONFLICT" "Automatic merge failed" "unmerged files")
         "Some parts were changed on both sides. Resolve the conflicts first."
         "Show status" (lambda (root) (git-show-status root)))
   (list '("nothing to commit" "nothing added to commit")
         "There are no changes to save." #f #f)))

(define (explain-failure ret)
  (with msg (string-append (git-out ret) "\n" (git-err ret))
    (or (list-find failure-explanations
                   (lambda (x)
                     (list-or (map (cut string-contains? msg <>) (car x)))))
        (list '() "Git could not complete this operation." #f #f))))

(tm-widget ((git-failure-widget what explanation msg label action) quit)
  (padded
    (bold (text (string-append what " failed")))
    ===
    (text explanation)
    ===
    (hlist (text msg) >>)
    ======
    (bottom-buttons
      ("Details" (begin (quit) (git-show-output)))
      >>
      (if label
          ((eval label) (begin (quit) (action))))
      // //
      ("Close" (quit)))))

(tm-define (git-show-failure ret what)
  (:require (and (not (headless?)) (current-window)))
  (let* ((root (git-last-root))
         (x (explain-failure ret))
         (label (third x))
         (action (and (fourth x) root (lambda () ((fourth x) root)))))
    (dialogue-window (git-failure-widget what (second x)
                                         (utf8->cork (git-message ret))
                                         (and action label) action)
                     noop (string-append "Git: " what))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reviewing differences and conflicts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (review-differences u)
  (if (not (and u (buffer-exists? u))) '()
      (tree-search (buffer-get u)
                   (lambda (t) (tree-in? t '(version-old version-new
                                             version-both))))))

(define (review-index l)
  (and-with t (tree-innermost version-context?)
    (with i (list-find-index l (cut == <> t))
      (and i (+ i 1)))))

(define (review-do cmd)
  (cmd)
  (refresh-now "version-review"))

(define (review-conflict? u)
  (and (url? u) (git-root u) (== (git-file-state u) 'conflicted)))

(tm-widget (version-review-contents win)
  (let* ((u (window->buffer win))
         (conflict? (review-conflict? u))
         (l (review-differences u))
         (n (length l))
         (i (review-index l))
         (what (if conflict? "Conflict" "Difference")))
    (hlist
      (if (== n 0)
          (text (if conflict? "All conflicts resolved"
                    "No differences left")))
      (if (> n 0)
          (text (cond (i (string-append what " " (number->string i) " of "
                                        (number->string n)))
                      ((== n 1) (string-append "1 " (locase-first what)))
                      (else (string-append (number->string n) " "
                                           (locase-first what) "s"))))
          // //
          ("Previous" (review-do version-previous-difference))
          ("Next" (review-do version-next-difference))
          // // (text "Keep:")
          ((eval (if conflict? "Mine" "Old"))
           (review-do (lambda () (version-retain 0))))
          ((eval (if conflict? "Theirs" "New"))
           (review-do (lambda () (version-retain 1))))
          // // (text "Show:")
          ("Both" (review-do (lambda () (version-show 'version-both))))
          ((eval (if conflict? "Mine" "Old"))
           (review-do (lambda () (version-show 'version-old))))
          ((eval (if conflict? "Theirs" "New"))
           (review-do (lambda () (version-show 'version-new)))))
      >>
      (if conflict?
          ("Mark as resolved"
           (git-mark-resolved u)
           (refresh-now "version-review")))
      // //
      ("Close" (tool-close :transient-bottom 'version-review-tool #f win)))))

(tm-tool* (version-review-tool win)
  (:name "Review differences")
  (refreshable "version-review"
    (dynamic (version-review-contents win))))

(define (clone-default-directory)
  (with b (current-buffer)
    (url->system (if (and b (not (url-rooted-tmfs? b)) (url-exists? b))
                     (url-head b)
                     (system->url "$HOME")))))

(define (clone-destination repository dir)
  ;; Clone into dir, or into a subdirectory named after the repository
  ;; if dir already exists
  (let* ((u (system->url dir))
         (name (with l (string-tokenize-by-char
                        (string-replace repository "\\" "/") #\/)
                 (with last (if (null? l) "" (cAr l))
                   (if (string-ends? last ".git")
                       (string-drop-right last 4)
                       last)))))
    (url->system (if (and (url-directory? u) (!= name ""))
                     (url-append u name)
                     u))))

(tm-widget ((git-clone-widget) quit)
  (padded
    (form "git-clone"
      (aligned
        (item (text "Repository:")
          (form-input "repository" "string" (list "") "30em"))
        (item (text "Into directory:")
          (form-input "directory" "string"
                      (list (clone-default-directory)) "30em")))
      ===
      (bottom-buttons
        >>
        ("Cancel" (quit))
        // //
        ("Clone"
         (with l (form-values)
           (when (and (== (length l) 2) (string? (car l)) (string? (cadr l))
                      (!= (car l) ""))
             (let* ((repository (cork->utf8 (car l)))
                    (dir (cork->utf8 (cadr l))))
               (git-clone repository (clone-destination repository dir))
               (quit)))))))))

(tm-define (git-interactive-clone)
  (:synopsis "Clone a Git repository")
  (:interactive #t)
  (dialogue-window (git-clone-widget) noop "Clone Git repository"))

(tm-define (git-interactive-trust name)
  (:synopsis "Allow TeXmacs to run Git for the working tree of @name")
  (:interactive #t)
  (and-with root (git-root name)
    (user-confirm (string-append "Use Git in " (utf8->cork (url->system root))
                                 "? Only do this for folders whose origin "
                                 "you trust: the configuration of a "
                                 "repository can make Git run programs.") #f
      (lambda (answ)
        (when answ
          (git-trust root)
          (git-remember-repository root)
          (git-refresh root)
          (set-message "Git is now used in this folder" "Git"))))))

(tm-define (git-interactive-init name)
  (:synopsis "Create a Git repository for the document @name")
  (:interactive #t)
  (with dir (url-head name)
    (user-confirm (string-append "Create a Git repository in "
                                 (url->system dir) "?") #t
      (lambda (answ)
        (when answ
          (git-init dir)
          (when (git-root name)
            (git-show-status dir)))))))
