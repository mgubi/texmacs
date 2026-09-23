
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

(tm-define (git-interactive-save-snapshot root)
  (:synopsis "Save a snapshot of all files of @root")
  (:interactive #t)
  (interactive
   (lambda (description)
     (git-save-snapshot root (cork->utf8 description)))))

(tm-define (git-interactive-commit-file name)
  (:synopsis "Commit the changes of the file @name")
  (:interactive #t)
  (interactive
   (lambda (message)
     (set-message (utf8->cork (version-commit name message))
                  "Commit file"))))

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
;; Simple prompts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (git-interactive-create-branch root)
  (:interactive #t)
  (interactive
   (lambda (branch) (git-create-branch root (cork->utf8 branch)))))

(tm-define (git-interactive-tag root)
  (:interactive #t)
  (interactive
   (lambda (tag message)
     (git-create-tag root (cork->utf8 tag) (cork->utf8 message)))))

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

(tm-define (git-interactive-add-remote root)
  (:interactive #t)
  (interactive
   (lambda (name url)
     (git-add-remote root (cork->utf8 name) (cork->utf8 url)))))

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
