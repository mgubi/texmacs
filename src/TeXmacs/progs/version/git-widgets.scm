
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
  (:use (version version-git)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The dialog lists all changed files; the selected ones are those which will
;; be committed.  Initially, these are the files with staged changes.
;; On commit, the index is updated so as to contain exactly the selected files.

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

(define (commit-update-index root entries labels selected)
  ;; Stage the selected files and unstage the others
  (for-each
   (lambda (e lab)
     (let* ((path (git-entry-path e))
            (sel? (in? lab selected)))
       (cond ((and sel? (or (git-entry-unstaged? e) (git-entry-untracked? e)))
              (git-run root "add" "--all" "--" path))
             ((and (not sel?) (git-entry-staged? e))
              (git-run root "reset" "--quiet" "--" path)))))
   entries labels))

(define (commit-now root u entries labels selected amend?)
  (let* ((msg (commit-message u))
         (changed? (!= selected
                       (list-filter labels
                                    (lambda (lab)
                                      (with i (list-find-index labels
                                                               (cut == <> lab))
                                        (git-entry-staged?
                                         (list-ref entries i))))))))
    (cond ((and (== msg "") (not amend?))
           (set-message "Please enter a commit message" "Git commit")
           #f)
          ((and (null? selected) (not amend?))
           (set-message "Nothing selected for commit" "Git commit")
           #f)
          (else
            (when changed?
              (commit-update-index root entries labels selected))
            (cond ((and amend? (== msg ""))
                   (git-report (git-run root "commit" "--amend" "--no-edit")
                               "Amended commit")
                   (git-refresh root))
                  (amend? (git-commit-staged root msg :amend))
                  (else (git-commit-staged root msg)))
            #t))))

(tm-widget ((git-commit-widget root u) quit)
  (let* ((entries (commit-candidates root))
         (labels (map commit-label entries))
         (selected (list-filter labels
                                (lambda (lab)
                                  (with i (list-find-index labels
                                                           (cut == <> lab))
                                    (git-entry-staged? (list-ref entries i))))))
         (amend? #f)
         (branch (or (git-current-branch root) "(detached)")))
    (padded
      (text (string-append "Commit on branch " (utf8->cork branch) " in "
                           (url->system root)))
      ===
      (bold (text "Commit message"))
      ===
      (resize "500px" "120px"
        (texmacs-input `(document "") `(style (tuple "generic")) u))
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
          ("Cancel" (quit))
          // //
          ("Commit"
           (when (commit-now root u entries labels selected amend?)
             (quit))))))))

(tm-define (git-interactive-commit . opt-root)
  (:synopsis "Open a dialog for committing changes in the working tree")
  (:interactive #t)
  (and-with root (if (null? opt-root) (current-git-root) (car opt-root))
    (let* ((u (string->url "tmfs://aux/git-commit"))
           (b (current-buffer)))
      (git-invalidate root)
      (buffer-set-master u b)
      (dialogue-window (git-commit-widget root u)
                       (lambda x (noop))
                       "Git commit" u))))

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
      (if (or (git-entry-unstaged? e) (git-entry-untracked? e)
              (git-entry-conflicted? e))
          ("Stage" (git-stage u)))
      (if (git-entry-staged? e)
          ("Unstage" (git-unstage u))))))

(tm-widget (git-tool-contents win)
  (let* ((root (tool-root win))
         (l (if root (git-status-entries root) '()))
         (remote? (and root (nnull? (git-remotes root))
                       (not (git-busy? root))))
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
  (interactive (lambda (branch) (git-create-branch root branch))))

(tm-define (git-interactive-tag root)
  (:interactive #t)
  (interactive
   (lambda (tag message) (git-create-tag root tag (cork->utf8 message)))))

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
