
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

(define (commit-initial-message root)
  (with l (and (git-merging? root) (git-merge-message root))
    (if (and l (nnull? l))
        `(document ,@(map utf8->cork l))
        '(document ""))))

(tm-widget ((git-commit-widget root u paths) quit)
  (let* ((entries (commit-candidates root))
         (labels (map commit-label entries))
         (selected (initially-selected entries labels paths))
         (amend? #f)
         (branch (or (git-current-branch root) "(detached)")))
    (padded
      (text (string-append (if (git-merging? root) "Merge commit on branch "
                               "Commit on branch ")
                           (utf8->cork branch) " in " (url->system root)))
      ===
      (bold (text "Commit message"))
      ===
      (resize "500px" "120px"
        (texmacs-input (commit-initial-message root)
                       `(style (tuple "generic")) u))
      ===
      (bold (text "Files to commit"))
      ===
      (resize "500px" "180px"
        (scrollable
          (choices (set! selected answer) labels selected)))
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
           (when (commit-now root u entries labels selected amend?)
             (quit))))))))

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
      (git-invalidate root)
      (buffer-set-master u b)
      (dialogue-window (git-commit-widget root u paths)
                       (lambda x (noop))
                       "Git commit" u))))

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

(define (tool-branch root)
  (let* ((st (git-status root))
         (head (or (git-status-ref st 'head) "?"))
         (up (git-status-ref st 'upstream))
         (ahead (or (git-status-ref st 'ahead) 0))
         (behind (or (git-status-ref st 'behind) 0)))
    (string-append "On " (utf8->cork head)
                   (if (not up) ""
                       (string-append " (" (number->string ahead) " ahead, "
                                      (number->string behind) " behind)")))))

(define (tool-code e)
  (cond ((git-entry-conflicted? e) "C")
        ((git-entry-untracked? e) "?")
        ((and (git-entry-staged? e) (git-entry-unstaged? e)) "+~")
        ((git-entry-staged? e) "+")
        (else "~")))

(tm-widget (git-tool-entry root e)
  (with u (git-absolute root (git-entry-path e))
    (hlist
      (text (tool-code e)) // //
      ((eval (utf8->cork (git-entry-path e)))
       (when (url-exists? u) (load-buffer u)))
      >>
      (if (or (git-entry-unstaged? e) (git-entry-untracked? e))
          ("Stage" (git-stage u)))
      (if (git-entry-conflicted? e)
          ("Resolved" (git-mark-resolved u)))
      (if (git-entry-staged? e)
          ("Unstage" (git-unstage u))))))

(tm-widget (git-tool-contents win)
  (let* ((root (tool-root win))
         (l (if root (git-status-entries root) '()))
         (busy? (and root (git-busy? root)))
         (remote? (and root (nnull? (git-remotes root)) (not busy?)))
         (branch (if root (tool-branch root) "")))
    (if (not root)
        (text "The current document is not in a Git working tree"))
    (if root
        (text branch)
        ===
        (hlist
          ("Commit..." (git-interactive-commit root)) // //
          (if remote?
              ("Pull" (git-pull root)) // //
              ("Push" (git-push root)) // //)
          (if busy?
              ("Cancel" (git-cancel root)) // //)
          ("Status" (git-show-status root)) // //
          ("Refresh" (git-refresh root))
          >>)
        ===
        (if (null? l) (text "Nothing to commit"))
        (division "plain"
          (for (e l)
            (dynamic (git-tool-entry root e)))))))

(tm-tool* (git-tool win)
  (:name "Git")
  (refreshable "git-tool"
    (dynamic (git-tool-contents win))))

(tm-define (git-open-tool)
  (:synopsis "Show the status of the working tree in a side tool")
  (:interactive #t)
  (tool-select :right 'git-tool))

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
        (aligned
          (for (f fields)
            (item (text (car f))
              (form-input (car f) "string" (list (cadr f)) "25em"))))
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
         (with old (get-preference "git sign")
           (set-preference "git sign" (if (car flags) "on" "off"))
           (set-message (utf8->cork (git-commit-file name msg)) "Commit file")
           (set-preference "git sign" old)
           #f)))))

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
      ("Details" (quit) (git-show-output))
      >>
      (if label
          ((eval label) (quit) (action)))
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
  (if (not (buffer-exists? u)) '()
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
