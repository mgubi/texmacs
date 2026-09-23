
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-git.scm
;; DESCRIPTION : subroutines for the Git tools
;; COPYRIGHT   : (C) 2019  Darcy Shen, Joris van der Hoeven
;;               (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-git)
  (:use (version version-tmfs)
        (version version-compare)
        (version version-merge)
        (version git-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Supported features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-supports-svn-style? name)
  (:require (== (version-tool name) "git"))
  #f)

(tm-define (version-supports-git-style? name)
  (:require (== (version-tool name) "git"))
  (versioned? name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (short-hash rev)
  (if (>= (string-length rev) 40) (string-take rev 7) rev))

(tm-define (git-texmacs-file? u)
  (in? (url-suffix u) '("tm" "ts" "tp" "stm" "tmml")))

(define (git-quote s)
  ;; Scheme literal for the string s, for use inside 'action' scripts
  (object->string s))

(define (git-action text cmd . args)
  `(action ,text ,(string-append "(" cmd " "
                                 (string-recompose (map git-quote args) " ")
                                 ")")))

(define (root-string root) (url->system root))
(define (string-root s) (system->url s))

(tm-define (current-git-root)
  (:synopsis "Root of the working tree for the current buffer or Git page")
  (git-buffer-root (current-buffer)))

(tm-define (git-buffer-root u)
  (:synopsis "Root of the working tree for the buffer @u or Git page")
  (cond ((not u) #f)
        ((or (url-rooted-tmfs-protocol? u "git")
             (url-rooted-tmfs-protocol? u "commit"))
         (with (class name) (tmfs-decompose-name u)
           (tmfs-string->url (tmfs-cdr name))))
        ((version-revision? u) (git-root (version-head u)))
        (else (git-root u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers of a working tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (git-page? u root)
  (and (or (url-rooted-tmfs-protocol? u "git")
           (url-rooted-tmfs-protocol? u "commit"))
       (with (class name) (tmfs-decompose-name u)
         (== (tmfs-string->url (tmfs-cdr name)) root))))

(define (git-buffer? u root)
  (and (not (url-rooted-tmfs? u))
       (string-starts? (url->system u)
                       (string-append (url->system root) "/"))))

(tm-define (git-buffers root)
  (:synopsis "Open buffers for files in the working tree @root")
  (list-filter (buffer-list) (cut git-buffer? <> root)))

(tm-define (git-modified-buffers root)
  (list-filter (git-buffers root) buffer-modified?))

(define (git-reload-buffer u)
  (url-cache-invalidate u)
  (with t (tree-import u (url-format u))
    (when (!= t (tm->tree "error"))
      (buffer-set u t)
      (buffer-pretend-saved u))))

(tm-define (git-refresh root)
  (:synopsis "Refresh the Git pages about @root after a change")
  (git-invalidate root)
  (for (u (buffer-list))
    (when (git-page? u root)
      (git-reload-buffer u)))
  (refresh-now "git-tool"))

(define (file-contents u)
  (if (url-exists? u) (string-load u) ""))

(tm-define (git-watch root)
  (:synopsis "Contents on disk of the open documents in @root")
  ;; NOTE: modification times have a too coarse resolution
  (map (lambda (u) (cons u (file-contents u))) (git-buffers root)))

(tm-define (git-reload root watch)
  (:synopsis "Reload the documents in @watch which changed on disk")
  (for (x watch)
    (let ((u (car x)) (old (cdr x)))
      (when (and (url-exists? u) (!= (file-contents u) old))
        (if (buffer-modified? u)
            (set-message `(concat "Modified on disk: "
                                  (verbatim ,(url->system u)))
                         "Git")
            (git-reload-buffer u)))))
  (git-refresh root))

(tm-define (git-with-reload root thunk)
  (:synopsis "Execute @thunk and reload the documents it changed on disk")
  (let* ((watch (git-watch root))
         (ret (thunk)))
    (git-reload root watch)
    ret))

(tm-define (git-when-saved root cont)
  (:synopsis "Execute @cont after asking to save the modified documents")
  (with l (git-modified-buffers root)
    (if (null? l)
        (cont)
        (user-confirm "Save the modified documents in this repository first?"
                      #t
          (lambda (answ)
            (when answ
              (for-each (lambda (u) (buffer-save u) (buffer-pretend-saved u)) l)
              (cont)))))))

(tm-define (git-report ret what)
  (:synopsis "Show the outcome @ret of a Git command in the footer")
  (with msg (git-message ret)
    (if (git-ok? ret)
        (set-message (if (== msg "") what (utf8->cork msg)) "Git")
        (set-message `(concat "Git error: " (verbatim ,(utf8->cork msg)))
                     what))
    (git-ok? ret)))

(tm-define (version-notify-saved name)
  (:require (git-root name))
  (git-refresh (git-root name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-status name)
  (:require (== (version-tool name) "git"))
  (with st (git-file-state name)
    (cond ((not st) "unknown")
          ((== st 'untracked) "unknown")
          ((== st 'unmodified) "unmodified")
          (else "modified"))))

(tm-define (git-state-description st)
  (cond ((== st 'untracked) "not tracked")
        ((== st 'unmodified) "unmodified")
        ((== st 'modified) "modified")
        ((== st 'staged) "staged")
        ((== st 'partial) "partially staged")
        ((== st 'added) "added")
        ((== st 'deleted) "deleted")
        ((== st 'conflicted) "conflict")
        (else "unknown")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File history and revisions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A revision of a file in its history is encoded as <hash>:<file>, where
;; <file> is the tmfs string for the file at that commit (which may differ
;; from the current name, due to renames).  Otherwise, a revision is any
;; revision understood by Git ("HEAD", a branch name, ...), "INDEX"
;; for the version in the index, or "BASE", "OURS", "THEIRS" for the
;; versions of a file with a merge conflict.

(tm-define (version-history name)
  (:require (== (version-tool name) "git"))
  (and-with root (git-root name)
    (and-with l (git-file-log name)
      (map (lambda (c)
             (with path (if (null? (git-commit-files c))
                            (git-relative root name)
                            (car (git-commit-files c)))
               (list (string-append (git-commit-hash c) ":"
                                    (url->tmfs-string
                                     (git-absolute root path)))
                     (git-commit-author c)
                     (git-commit-date c)
                     (git-commit-subject c))))
           l))))

(define git-stages
  '(("INDEX" . "") ("BASE" . ":1") ("OURS" . ":2") ("THEIRS" . ":3")))

(tm-define (version-revision name rev)
  (:require (== (version-tool name) "git"))
  (with root (git-root name)
    (if (not root) ""
        (with path (git-relative root name)
          (git-show-file root (or (assoc-ref git-stages rev) rev) path)))))

(tm-define (version-beautify-revision name rev)
  (:require (== (version-tool name) "git"))
  (short-hash rev))

(tm-define (git-master name)
  (and-with root (git-root name)
    (git-rev-parse root "HEAD")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (not-in-git) "the file is not in a Git working tree")

(tm-define (version-register name)
  (:require (== (version-tool name) "git"))
  (with root (git-root name)
    (if (not root)
        (not-in-git)
        (with ret (git-run root "add" "--" (git-relative root name))
          (git-refresh root)
          (if (git-ok? ret) "Added file" (git-message ret))))))

(tm-define (version-unregister name)
  (:require (== (version-tool name) "git"))
  (with root (git-root name)
    (if (not root)
        (not-in-git)
        (with ret (git-run root "rm" "--cached" "--quiet"
                           "--" (git-relative root name))
          (git-refresh root)
          (if (git-ok? ret) "Stopped tracking file" (git-message ret))))))

(tm-define (version-commit name msg)
  (:require (== (version-tool name) "git"))
  (with root (git-root name)
    (cond ((not root) (not-in-git))
          ((== (tm-string-trim-both msg) "") "Empty commit message")
          ((git-merging? root)
           "A merge is in progress; commit the whole working tree")
          (else
            (with path (git-relative root name)
              (when (== (git-file-state name) 'untracked)
                (git-run root "add" "--" path))
              (with ret (apply git-run-with-input
                               (append (list root (cork->utf8 msg) "commit"
                                             "--file=-")
                                       (git-commit-options)
                                       (list "--" path)))
                (git-refresh root)
                (git-message ret)))))))

(tm-define (git-stage-now name)
  (and-with root (git-root name)
    (git-report (git-run root "add" "--" (git-relative root name))
                "Staged file")
    (git-refresh root)))

(tm-define (git-stage name)
  (:synopsis "Stage the changes of the file @name")
  (if (git-large-file? name)
      (user-confirm (string-append (url->system (url-tail name))
                                   " is large; versioning it makes the "
                                   "repository big and slow. Stage anyway?")
                    #f
        (lambda (answ) (when answ (git-stage-now name))))
      (git-stage-now name)))

(tm-define (git-entry-paths e)
  (:synopsis "The paths affected by the status entry @e")
  ;; NOTE: a rename also removes the original name
  (if (git-entry-orig e)
      (list (git-entry-path e) (git-entry-orig e))
      (list (git-entry-path e))))

(tm-define (git-unstage-paths root paths)
  (git-run-list root (append (list "reset" "--quiet" "--") paths)))

(tm-define (git-run-list root args)
  (apply git-run (cons root args)))

(tm-define (git-unstage name)
  (:synopsis "Unstage the changes of the file @name")
  (and-with root (git-root name)
    (let* ((e (git-file-entry root name))
           (paths (if e (git-entry-paths e) (list (git-relative root name)))))
      (git-report (git-unstage-paths root paths) "Unstaged file")
      (git-refresh root))))

(tm-define (git-discard-now name)
  (and-with root (git-root name)
    (with path (git-relative root name)
      (git-with-reload root
        (lambda ()
          (git-report (git-run root "checkout" "--" path)
                      "Discarded changes"))))))

(tm-define (git-discard name)
  (:synopsis "Discard the changes to @name since it was last staged")
  (:interactive #t)
  (user-confirm (if (buffer-modified? name)
                    "Discard all changes, including the unsaved ones?"
                    "Discard all changes since the file was last staged?")
                #f
    (lambda (answ)
      (when answ
        (when (buffer-modified? name) (buffer-pretend-saved name))
        (git-discard-now name)))))

(define (version-markup? t)
  (tree-in? t '(version-old version-new version-both)))

(define (revision-body name rev)
  ;; Body of the document @name at the revision @rev, or #f
  (and (!= (version-revision name rev) "")
       (document-body
        (tree->stree (tree-import (string->url (version-revision-url name rev))
                                  "texmacs")))))

(define (resolve-message n)
  (if (== n 0)
      "Merged automatically; check the result, then mark as resolved"
      (string-append (number->string n) " conflicting changes; old: ours, "
                     "new: theirs. Retain the right versions, "
                     "then mark as resolved")))

(tm-define (git-resolve-conflict name)
  (:synopsis "Merge our and their versions of the conflicting file @name")
  (:interactive #t)
  ;; The changes made on only one side are merged automatically; the
  ;; others are shown as differences, with our version as the old one
  (let* ((base (revision-body name "BASE"))
         (ours (revision-body name "OURS"))
         (theirs (revision-body name "THEIRS")))
    (cond ((and (buffer-exists? name) (buffer-modified? name))
           (set-message "Please save or revert the document first"
                        "Resolve conflict"))
          ((not (and ours theirs))
           (set-message (string-append "The document was removed on one "
                                       "side; edit it, then mark as resolved")
                        "Resolve conflict"))
          (else
            (when (!= (url->url name) (url->url (current-buffer)))
              (load-buffer name))
            (buffer-set name (tree-import (string->url
                                           (version-revision-url name "OURS"))
                                          "texmacs"))
            (if base
                (begin
                  (tree-set (buffer-tree)
                            (stree->tree (merge-versions base ours theirs)))
                  (version-first-difference))
                (compare-with-newer (string->url
                                     (version-revision-url name "THEIRS"))))
            (set-message (resolve-message
                          (length (tree-search (buffer-tree)
                                               version-markup?)))
                         "Resolve conflict")))))

(tm-define (git-mark-resolved-now name)
  (when (buffer-exists? name)
    (buffer-save name)
    (buffer-pretend-saved name))
  (git-stage name))

(tm-define (git-mark-resolved name)
  (:synopsis "Save @name and mark its merge conflict as resolved")
  (:interactive #t)
  (if (and (buffer-exists? name)
           (nnull? (tree-search (buffer-get name) version-markup?)))
      (user-confirm "Some differences have not been resolved. Continue?" #f
        (lambda (answ)
          (when answ (git-mark-resolved-now name))))
      (git-mark-resolved-now name)))

(tm-define (git-restore-revision-now name rev)
  (and-with root (git-root name)
    (git-with-reload root
      (lambda ()
        (git-report (git-run root "checkout" rev "--" (git-relative root name))
                    (string-append "Restored the version "
                                   (short-hash rev)))))))

(tm-define (git-restore-revision name rev)
  (:synopsis "Replace @name by its version at the revision @rev")
  (:interactive #t)
  ;; NOTE: the history is kept: the restored version is a new change,
  ;; which can be committed or discarded
  (cond ((not (git-safe-name? rev)) (bad-name "revision"))
        ((and (buffer-exists? name) (buffer-modified? name))
         (set-message "Please save or revert the document first" "Restore"))
        (else
          (user-confirm (string-append "Replace the current version by the "
                                       "version " (short-hash rev) "?") #f
            (lambda (answ)
              (when answ (git-restore-revision-now name rev)))))))

(tm-define (git-revision-of u)
  (:synopsis "The Git revision shown in the revision buffer @u, or #f")
  (and (version-revision? u)
       (with rev (version-get-revision u)
         (and (git-safe-name? rev) (nin? rev '("INDEX" "BASE" "OURS" "THEIRS"))
              rev))))

(tm-define (git-compare-with-revision name rev)
  (:synopsis "Compare @name with its version at the Git revision @rev")
  (with root (git-root name)
    (with hash (and root (git-rev-parse root (string-append rev "^{commit}")))
      (if hash
          (git-compare-with name hash)
          (set-message (string-append "Unknown revision " rev) "Compare")))))

(tm-define (git-compare-with name rev)
  (:synopsis "Compare the document @name with its revision @rev")
  (with u (string->url (version-revision-url name rev))
    (when (!= (url->url name) (url->url (current-buffer)))
      (load-buffer name))
    (compare-with-older u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on the whole working tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (git-stage-all root)
  (:synopsis "Stage all changes to tracked files in @root")
  (git-report (git-run root "add" "--update") "Staged all changes")
  (git-refresh root))

(tm-define (git-has-staged? root)
  (list-or (map git-entry-staged? (git-status-entries root))))

(tm-define (git-commit-staged root msg . opts)
  (:synopsis "Commit the staged changes in @root with message @msg")
  ;; The message @msg is in the utf8 encoding
  ;; Options: :amend, :all (stage modified files first)
  (let* ((args (append (if (in? :amend opts) (list "--amend") '())
                       (if (in? :all opts) (list "--all") '())
                       (git-commit-options)))
         (ret (apply git-run-with-input
                     (append (list root msg "commit" "--file=-")
                             args))))
    (git-refresh root)
    (git-report ret "Committed changes")))

(tm-define (git-last-commit-message root)
  (if (git-rev-parse root "HEAD")
      (utf8->cork (git-commit-message root "HEAD"))
      ""))

(define (bad-name what)
  (set-message (string-append "Invalid " what) "Git")
  #f)

(tm-define (git-switch-branch root branch)
  (:synopsis "Switch the working tree @root to @branch")
  (if (not (git-safe-name? branch))
      (bad-name "branch name")
      (git-when-saved root
        (lambda ()
          (git-with-reload root
            (lambda ()
              (git-report (git-run root "checkout" "--quiet" branch "--")
                          (string-append "Switched to " branch))))))))

(tm-define (git-create-branch root branch)
  (:synopsis "Create a new branch @branch at HEAD and switch to it")
  (if (not (git-safe-name? branch))
      (bad-name "branch name")
      (begin
        (git-report (git-run root "checkout" "--quiet" "-b" branch)
                    (string-append "Created branch " branch))
        (git-refresh root))))

(tm-define (git-delete-branch root branch)
  (if (not (git-safe-name? branch))
      (bad-name "branch name")
      (user-confirm (string-append "Delete branch " branch "?") #f
        (lambda (answ)
          (when answ
            (git-report (git-run root "branch" "--delete" branch)
                        (string-append "Deleted branch " branch))
            (git-refresh root))))))

(tm-define (git-merge-branch root branch)
  (:synopsis "Merge @branch into the current branch of @root")
  (if (not (git-safe-name? branch))
      (bad-name "branch name")
      (git-when-saved root
        (lambda ()
          (git-with-reload root
            (lambda ()
              (git-report (git-run root "merge" "--no-edit" branch)
                          (string-append "Merged " branch))))))))

(tm-define (git-create-tag root tag msg)
  ;; The message @msg is in the utf8 encoding
  (if (not (git-safe-name? tag))
      (bad-name "tag name")
      (begin
        (git-report (cond ((git-signing?)
                           (git-run-with-input root (if (== msg "") tag msg)
                                               "tag" "--sign" "--file=-" tag))
                          ((== msg "") (git-run root "tag" tag))
                          (else (git-run-with-input root msg "tag" "--annotate"
                                                    "--file=-" tag)))
                    (string-append "Created tag " tag))
        (git-refresh root))))

(tm-define (git-stash root)
  (git-when-saved root
    (lambda ()
      (git-with-reload root
        (lambda () (git-report (git-run root "stash" "push") "Stashed"))))))

(tm-define (git-stash-pop root . opt-name)
  ;; NOTE: unsaved documents would be overwritten by the stash
  (if (not (git-safe-names? opt-name))
      (bad-name "stash")
      (git-when-saved root
        (lambda ()
          (git-with-reload root
            (lambda ()
              (git-report (apply git-run (append (list root "stash" "pop")
                                                 opt-name))
                          "Restored stash")))))))

(tm-define (git-stash-drop root name)
  (if (not (git-safe-name? name))
      (bad-name "stash")
      (user-confirm (string-append "Drop " name "?") #f
        (lambda (answ)
          (when answ
            (git-report (git-run root "stash" "drop" name) "Dropped stash")
            (git-refresh root))))))

(define default-gitignore
  (string-append
   "# Files which are not worth versioning in TeXmacs projects\n"
   "*~\n"
   "*#\n"
   ".DS_Store\n"
   "# Uncomment if the documents are exported to PDF next to the sources\n"
   "# *.pdf\n"))

(tm-define (git-init dir)
  (:synopsis "Create a new Git repository in the directory @dir")
  (with ret (git-run dir "init" "--quiet")
    (version-tool-reset)
    (when (and (git-ok? ret) (not (url-exists? (url-append dir ".gitignore"))))
      (string-save default-gitignore (url-append dir ".gitignore")))
    (when (git-ok? ret) (git-remember-repository dir))
    (git-report ret "Created repository")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (git-remote root what args done)
  ;; Run a remote command asynchronously, since it may take a long time
  (if (git-busy? root)
      (set-message "Please wait for the running Git command" what)
      (with watch (git-watch root)
        (set-message (string-append what "...") "Git")
        (git-run-async root args #f
          (lambda (ret)
            (git-report ret what)
            (git-reload root watch)
            (when done (done ret))))
        (refresh-now "git-tool"))))

(tm-define (git-fetch root . opt-done)
  (git-remote root "Fetch" (list "fetch" "--all" "--prune")
              (and (nnull? opt-done) (car opt-done))))

(define (pull-arguments mode)
  (cond ((== mode "merge") (list "pull" "--no-rebase" "--no-edit"))
        ((== mode "rebase") (list "pull" "--rebase"))
        (else (list "pull" "--ff-only"))))

(define (diverged? ret)
  (with msg (git-err ret)
    (or (string-contains? msg "fast-forward")
        (string-contains? msg "divergent")
        (string-contains? msg "diverged"))))

(define (pull-done root done)
  (lambda (ret)
    (when (git-merging? root)
      (git-show-status root)
      (set-message "There are conflicts; resolve them, then commit" "Pull"))
    (when done (done ret))))

(tm-define (git-pull-merge root . opt-done)
  (:synopsis "Pull and merge the remote changes into the local ones")
  (git-when-saved root
    (lambda ()
      (git-remote root "Pull" (pull-arguments "merge")
                  (pull-done root (and (nnull? opt-done) (car opt-done)))))))

(tm-define (git-pull root . opt-done)
  ;; If the branches diverged, then propose to merge them
  (let* ((done (and (nnull? opt-done) (car opt-done)))
         (mode (get-preference "git pull mode"))
         (cont (lambda (ret)
                 (if (and (not (git-ok? ret)) (== mode "fast-forward")
                          (diverged? ret))
                     (user-confirm (string-append "Your changes and the remote "
                                                  "changes diverged. Merge "
                                                  "them?") #t
                       (lambda (answ)
                         (if answ
                             (git-pull-merge root done)
                             (when done (done ret)))))
                     ((pull-done root done) ret)))))
    (git-when-saved root
      (lambda ()
        (git-remote root "Pull" (pull-arguments mode) cont)))))

(tm-define (git-push root . opt-done)
  (with remote (git-push-remote root)
    (if (not remote)
        (set-message "This repository has no remote; add one first" "Push")
        (apply git-push-to (cons* root remote opt-done)))))

(tm-define (git-push-to root remote . opt-done)
  (:synopsis "Push the current branch of @root to @remote")
  ;; The first push of a branch sets its upstream
  (let* ((branch (git-current-branch root))
         (up (git-status-ref (git-status root) 'upstream))
         (args (cond ((or (not branch) (not (git-safe-name? remote)))
                      (list "push"))
                     ((and up (string-starts? up (string-append remote "/")))
                      (list "push" remote))
                     (else (list "push" "--set-upstream" remote branch)))))
    (git-remote root "Push" args (and (nnull? opt-done) (car opt-done)))))

(tm-define (git-add-remote root name url)
  (:synopsis "Add the remote repository @url under the name @name")
  (if (not (and (git-safe-name? name) (git-safe-name? url)))
      (bad-name "remote")
      (begin
        (git-report (git-run root "remote" "add" name url)
                    (string-append "Added remote " name))
        (git-refresh root))))

(tm-define (git-remove-remote root name)
  (if (not (git-safe-name? name))
      (bad-name "remote")
      (user-confirm (string-append "Remove the remote " name "?") #f
        (lambda (answ)
          (when answ
            (git-report (git-run root "remote" "remove" name)
                        (string-append "Removed remote " name))
            (git-refresh root))))))

(tm-define (git-clone repository dir . opt-done)
  (:synopsis "Clone @repository into the new directory @dir")
  (let* ((dest (system->url dir))
         (parent (url-head dest))
         (done (and (nnull? opt-done) (car opt-done))))
    (cond ((url-exists? dest)
           (set-message (string-append dir " already exists") "Clone"))
          ((not (url-directory? parent))
           (set-message (string-append (url->system parent)
                                       " is not a directory") "Clone"))
          (else
            (set-message (string-append "Cloning " repository "...") "Git")
            (git-run-async parent
                           (list "clone" "--" repository
                                 (url->system (url-tail dest)))
                           #f
              (lambda (ret)
                (when (git-report ret "Cloned repository")
                  (version-tool-reset)
                  (when (not done) (git-show-status dest)))
                (when done (done ret))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions for the Git pages (callable from 'action' tags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (page-file root-s path)
  (git-absolute (string-root root-s) path))

(define (page-context? root-s)
  ;; The actions below may be put in any document by anybody, so we only
  ;; execute them from within the Git pages for the working tree root-s
  (with ok? (and (string? root-s) (current-buffer)
                 (git-page? (current-buffer) (string-root root-s)))
    (when (not ok?)
      (set-message "Git actions only work from the Git pages" "Git"))
    ok?))

(tm-define (git-page-stage root-s path)
  (:secure #t)
  (when (page-context? root-s)
    (git-stage (page-file root-s path))))

(tm-define (git-page-unstage root-s path)
  (:secure #t)
  (when (page-context? root-s)
    (git-unstage (page-file root-s path))))

(tm-define (git-page-discard root-s path)
  (:secure #t)
  (when (page-context? root-s)
    (git-discard (page-file root-s path))))

(tm-define (git-page-resolve root-s path)
  (:secure #t)
  (when (page-context? root-s)
    (git-resolve-conflict (page-file root-s path))))

(tm-define (git-page-mark-resolved root-s path)
  (:secure #t)
  (when (page-context? root-s)
    (git-mark-resolved (page-file root-s path))))

(tm-define (git-page-compare root-s path rev)
  (:secure #t)
  (when (page-context? root-s)
    (git-compare-with (page-file root-s path) rev)))

(tm-define (git-page-restore root-s path rev)
  (:secure #t)
  (when (page-context? root-s)
    (git-restore-revision (page-file root-s path) rev)))

(tm-define (git-page-stage-all root-s)
  (:secure #t)
  (when (page-context? root-s)
    (git-stage-all (string-root root-s))))

(tm-define (git-page-commit root-s)
  (:secure #t)
  (when (page-context? root-s)
    (git-interactive-commit (string-root root-s))))

(tm-define (git-page-switch root-s branch)
  (:secure #t)
  (when (page-context? root-s)
    (git-switch-branch (string-root root-s) branch)))

(tm-define (git-page-merge root-s branch)
  (:secure #t)
  (when (page-context? root-s)
    (git-merge-branch (string-root root-s) branch)))

(tm-define (git-page-delete-branch root-s branch)
  (:secure #t)
  (when (page-context? root-s)
    (git-delete-branch (string-root root-s) branch)))

(tm-define (git-page-stash-pop root-s name)
  (:secure #t)
  (when (page-context? root-s)
    (git-stash-pop (string-root root-s) name)))

(tm-define (git-page-stash-drop root-s name)
  (:secure #t)
  (when (page-context? root-s)
    (git-stash-drop (string-root root-s) name)))

(tm-define (git-page-refresh root-s)
  (:secure #t)
  (when (page-context? root-s)
    (git-refresh (string-root root-s))))

(tm-define (git-page-show root-s which)
  (:secure #t)
  (when (page-context? root-s)
    (git-show-page (string-root root-s) which)))

(tm-define (git-page-remote root-s which)
  (:secure #t)
  (when (page-context? root-s)
    (with root (string-root root-s)
      (cond ((== which "fetch") (git-fetch root))
            ((== which "pull") (git-pull root))
            ((== which "push") (git-push root))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git pages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmfs-url-git root which)
  (string-append "tmfs://git/" which "/" (url->tmfs-string root)))

(tm-define (git-menu-label root)
  (:synopsis "Label for the Git menu, which summarizes the state of @root")
  (let* ((st (git-status root))
         (head (git-status-ref st 'head))
         (n (length (or (git-status-ref st 'entries) '())))
         (ahead (or (git-status-ref st 'ahead) 0))
         (behind (or (git-status-ref st 'behind) 0))
         (l (append (if head (list (utf8->cork head)) '())
                    (if (> n 0) (list (string-append (number->string n)
                                                     " changed")) '())
                    (if (> ahead 0) (list (string-append (number->string ahead)
                                                         " ahead")) '())
                    (if (> behind 0) (list (string-append
                                            (number->string behind)
                                            " behind")) '()))))
    (if (null? l) "Git"
        (string-append "Git (" (string-recompose l ", ") ")"))))

(tm-define (git-show-page root which)
  (:synopsis "Show the Git page @which (status, log, ...) for @root")
  (cursor-history-add (cursor-path))
  (git-invalidate root)
  (git-remember-repository root)
  (revert-buffer-revert (tmfs-url-git root which)))

(tm-define (git-show-status . opt-root)
  (and-with root (if (null? opt-root) (current-git-root) (car opt-root))
    (git-show-page root "status")))

(tm-define (git-show-log . opt-root)
  (and-with root (if (null? opt-root) (current-git-root) (car opt-root))
    (git-show-page root "log")))

(tm-define (git-show-branches . opt-root)
  (and-with root (if (null? opt-root) (current-git-root) (car opt-root))
    (git-show-page root "branches")))

(tm-define (git-show-output . opt-root)
  (and-with root (if (null? opt-root) (current-git-root) (car opt-root))
    (git-show-page root "output")))

(define (git-page-menu root)
  (with r (root-string root)
    `(concat (with "font-size" "0.84"
             (concat ,(git-action "Status" "git-page-show" r "status") " | "
             ,(git-action "Log" "git-page-show" r "log") " | "
             ,(git-action "Graph" "git-page-show" r "graph") " | "
             ,(git-action "Branches" "git-page-show" r "branches") " | "
             ,(git-action "Output" "git-page-show" r "output") " | "
             ,(git-action "Refresh" "git-page-refresh" r))))))

(define (git-page root title . body)
  ;; Items of body are paragraphs, or lists of paragraphs
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic"))
     (body (document (tmfs-title ,title)
                     ,(git-page-menu root)
                     ,@(append-map (lambda (x) (if (and (pair? x) (pair? (car x)))
                                                   x (list x)))
                                   body)))))

(define (describe-item key body)
  `(concat (item* ,key) ,body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (status-code c)
  (cond ((== c #\M) "modified")
        ((== c #\A) "new file")
        ((== c #\D) "deleted")
        ((== c #\R) "renamed")
        ((== c #\C) "copied")
        ((== c #\T) "type change")
        (else "changed")))

(define (status-file root e)
  (let* ((path (git-entry-path e))
         (u (git-absolute root path)))
    (if (url-exists? u)
        ($link (url->unix u) ($verbatim (utf8->cork path)))
        ($verbatim (utf8->cork path)))))

(define (status-line root e which)
  (let* ((r (root-string root))
         (path (git-entry-path e))
         (u (git-absolute root path))
         (code (if (== which 'staged) (git-entry-index e) (git-entry-worktree e)))
         (desc (cond ((== which 'untracked) "untracked")
                     ((== which 'conflict) "conflict")
                     (else (status-code code))))
         (cmp? (and (git-texmacs-file? u) (url-exists? u)
                    (nin? which '(untracked conflict)) (!= code #\A)))
         (acts (append
                (if cmp? (list (git-action "compare" "git-page-compare"
                                           r path "HEAD"))
                    '())
                (cond ((== which 'staged)
                       (list (git-action "unstage" "git-page-unstage" r path)))
                      ((== which 'conflict)
                       (append
                        (if (git-texmacs-file? u)
                            (list (git-action "resolve" "git-page-resolve"
                                              r path))
                            '())
                        (list (git-action "mark resolved"
                                          "git-page-mark-resolved" r path))))
                      ((== which 'untracked)
                       (list (git-action "add" "git-page-stage" r path)))
                      (else
                       (list (git-action "stage" "git-page-stage" r path)
                             (git-action "discard" "git-page-discard"
                                         r path)))))))
    `(concat (with "color" "dark grey" ,desc) (hspace "1em")
             ,(status-file root e)
             ,(if (git-entry-orig e)
                  `(concat " (from " ,($verbatim (utf8->cork (git-entry-orig e)))
                           ")")
                  "")
             (hspace "1em")
             (with "font-size" "0.84"
               (concat "[" ,@(list-intersperse acts " | ") "]")))))

(define (list-intersperse l sep)
  (cond ((or (null? l) (null? (cdr l))) l)
        (else (cons* (car l) sep (list-intersperse (cdr l) sep)))))

(define (status-section root title l which)
  (if (null? l) '()
      (cons `(subsection* ,title)
            (map (cut status-line root <> which) l))))

(define (status-branch root st)
  (let* ((head (git-status-ref st 'head))
         (up (git-status-ref st 'upstream))
         (ahead (or (git-status-ref st 'ahead) 0))
         (behind (or (git-status-ref st 'behind) 0))
         (oid (git-status-ref st 'oid)))
    `(concat "On branch " (strong ,(utf8->cork (or head "?")))
             ,(if (and oid (!= oid "(initial)"))
                  `(concat " at " ,($link (tmfs-url-commit root oid)
                                     (short-hash oid)))
                  " (no commits yet)")
             ,(if up
                  `(concat ", tracking " ,(utf8->cork up)
                           ,(if (and (== ahead 0) (== behind 0)) " (up to date)"
                                `(concat " (" ,(number->string ahead)
                                         " ahead, " ,(number->string behind)
                                         " behind)")))
                  ""))))

(define (git-status-content root)
  (let* ((st (git-status root))
         (r (root-string root))
         (l (or (git-status-ref st 'entries) '()))
         (conflicts (list-filter l git-entry-conflicted?))
         (staged (list-filter l git-entry-staged?))
         (unstaged (list-filter l git-entry-unstaged?))
         (untracked (list-filter l git-entry-untracked?)))
    (if (not st)
        (git-page root "Git status"
                  "This directory is not a Git working tree.")
        (apply git-page
               (append
                (list root "Git status"
                      (status-branch root st)
                      `(concat
                        ,(git-action "Commit..." "git-page-commit" r) " | "
                        ,(git-action "Stage all" "git-page-stage-all" r) " | "
                        ,(git-action "Fetch" "git-page-remote" r "fetch") " | "
                        ,(git-action "Pull" "git-page-remote" r "pull") " | "
                        ,(git-action "Push" "git-page-remote" r "push")))
                (if (null? l) (list "Nothing to commit, working tree clean.")
                    '())
                (status-section root "Conflicts" conflicts 'conflict)
                (status-section root "Changes to be committed" staged 'staged)
                (status-section root "Changes not staged for commit"
                                unstaged 'unstaged)
                (status-section root "Untracked files" untracked
                                'untracked))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (log-item root c)
  (describe-item
   `(concat "Commit " (hlink ,(short-hash (git-commit-hash c))
                             ,(tmfs-url-commit root (git-commit-hash c)))
            " by " ,(utf8->cork (git-commit-author c))
            " on " ,(git-commit-date c))
   (utf8->cork (git-commit-subject c))))

(define (git-log-content root skip)
  (let* ((n (git-log-length))
         (h (git-log root skip n))
         (r (root-string root)))
    (git-page root "Git log"
      (if (null? h)
          "No commits."
          `(description-long
            (document ,@(map (cut log-item root <>) h))))
      (if (< (length h) n) ""
          (git-action "More..." "git-page-show" r
                      (string-append "log." (number->string (+ skip n))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-line root x)
  (with (prefix c) x
    (if (not c)
        `(verbatim ,prefix)
        (let* ((hash (first c))
               (refs (if (>= (length c) 6) (sixth c) "")))
          `(concat (verbatim ,prefix)
                   (hlink ,(short-hash hash) ,(tmfs-url-commit root hash))
                   " "
                   ,(if (== refs "") ""
                        `(concat (strong ,(utf8->cork (string-append
                                                       "(" refs ")")))
                                 " "))
                   ,(utf8->cork (fifth c))
                   (with "color" "dark grey"
                     ,(string-append " - " (utf8->cork (third c)) ", "
                                     (fourth c))))))))

(define (git-graph-content root)
  (with l (git-graph root (git-log-length))
    (git-page root "Git graph"
      (if (null? l) "No commits."
          `(with "par-par-sep" "0fn" "par-ver-sep" "0fn"
             (document ,@(map (cut graph-line root <>) l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (branch-line root b local?)
  (let* ((r (root-string root))
         (name (git-branch-name b))
         (cur? (git-branch-current? b))
         (up (git-branch-upstream b))
         (track (git-branch-track b))
         (acts (cond (cur? '())
                     (local?
                      (list (git-action "switch" "git-page-switch" r name)
                            (git-action "merge into current" "git-page-merge"
                                        r name)
                            (git-action "delete" "git-page-delete-branch"
                                        r name)))
                     (else
                      (list (git-action "merge into current" "git-page-merge"
                                        r name))))))
    `(concat ,(if cur? `(strong ,(utf8->cork name)) (utf8->cork name))
             ,(if (!= up "") `(concat " " (with "color" "dark grey"
                                            ,(utf8->cork (string-append
                                                          "-> " up " " track))))
                  "")
             (hspace "1em")
             (with "color" "dark grey" ,(fifth b))
             ,(if (null? acts) ""
                  `(concat " " (with "font-size" "0.84"
                                 (concat "[" ,@(list-intersperse acts " | ")
                                         "]")))))))

(define (stash-line root s)
  (with r (root-string root)
    `(concat ,(car s) ": " ,(utf8->cork (cadr s)) (hspace "1em")
             (with "font-size" "0.84"
               (concat "[" ,(git-action "pop" "git-page-stash-pop" r (car s))
                       " | " ,(git-action "drop" "git-page-stash-drop"
                                          r (car s)) "]")))))

(define (git-branches-content root)
  (let* ((local (git-branches root))
         (remote (list-filter (git-remote-branches root)
                              (lambda (b)
                                (not (string-ends? (git-branch-name b)
                                                   "/HEAD")))))
         (tags (git-tags root))
         (stashes (git-stashes root)))
    (git-page root "Git branches"
      '(subsection* "Local branches")
      (map (cut branch-line root <> #t) local)
      '(subsection* "Remote branches")
      (if (null? remote) "None." (map (cut branch-line root <> #f) remote))
      '(subsection* "Remotes")
      (with l (git-remotes root)
        (if (null? l) "None."
            (map (lambda (name)
                   `(concat (strong ,(utf8->cork name)) (hspace "1em")
                            (verbatim ,(utf8->cork (or (git-remote-url root name)
                                                       "")))))
                 l)))
      '(subsection* "Tags")
      (if (null? tags) "None."
          (map (lambda (t) (utf8->cork (git-branch-name t))) tags))
      '(subsection* "Stashes")
      (if (null? stashes) "None." (map (cut stash-line root <>) stashes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (verbatim-lines s)
  (with l (git-split s "\n")
    (if (null? l) ""
        `(verbatim (document ,@(map utf8->cork l))))))

(define (output-item x)
  (with (time dir args ret) x
    (list `(concat (strong (verbatim ,(utf8->cork (string-recompose
                                                   (cons "git" args) " "))))
                   " (exit code " ,(number->string (car ret)) ")")
          (verbatim-lines (cadr ret))
          (if (== (caddr ret) "") ""
              `(with "color" "dark red" ,(verbatim-lines (caddr ret)))))))

(define (git-output-content root)
  (with l (list-filter (git-command-history)
                       (lambda (x) (== (cadr x) (root-string root))))
    (git-page root "Git output"
      "Most recent Git commands first."
      (append-map output-item l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-repeat str n)
  (do ((i 1 (1+ i))
       (ret "" (string-append ret str)))
      ((> i n) ret)))

(define (diff-bar added removed maxv)
  (define (len nr)
    (with ret (if (== maxv 0) 0 (quotient (* nr 40) (max maxv 40)))
      (if (and (> nr 0) (== ret 0)) 1 ret)))
  `(concat (with "color" "dark green" ,(string-repeat "+" (len added)))
           (with "color" "dark red" ,(string-repeat "-" (len removed)))))

(define (commit-file-row root rev parent x)
  (let* ((added (first x))
         (removed (second x))
         (path (third x))
         (u (git-absolute root path))
         (r (root-string root))
         (name ($verbatim (utf8->cork path)))
         (link ($link (version-revision-url
                       u (string-append rev ":" (url->tmfs-string u)))
                 name)))
    `(row (cell ,(if (and added removed) link name))
          (cell ,(if (and added removed)
                     (number->string (+ added removed))
                     "bin"))
          (cell ,(if (and added removed)
                     (diff-bar added removed 40)
                     ""))
          (cell ,(if (and parent (git-texmacs-file? u) (url-exists? u))
                     `(concat
                       ,(git-action "compare with current" "git-page-compare"
                                    r path rev)
                       " | "
                       ,(git-action "restore" "git-page-restore" r path rev))
                     "")))))

(define (git-commit-content root rev)
  (let* ((c (git-commit-info root rev))
         (parents (if c (git-commit-parents c) '()))
         (parent (and (== (length parents) 1) (car parents)))
         (d (cond ((null? parents) (git-numstat root rev))
                  (parent (git-numstat root rev parent))
                  (else '())))
         (ins (list-fold + 0 (map (lambda (x) (or (first x) 0)) d)))
         (del (list-fold + 0 (map (lambda (x) (or (second x) 0)) d))))
    (if (not c)
        (git-page root "Unknown commit" "")
        (git-page root (string-append "Commit " (short-hash rev))
          `(concat "Author: " ,(utf8->cork (git-commit-author c))
                   ", " ,(git-commit-date c)
                   ,(with sig (git-signature root rev)
                      (if sig (string-append ". Signature: " (utf8->cork sig))
                          "")))
          `(concat ,(if (<= (length parents) 1) "Parent: " "Parents: ")
                   ,@(if (null? parents) (list "none")
                         (list-intersperse
                          (map (lambda (p) ($link (tmfs-url-commit root p)
                                             (short-hash p)))
                               parents)
                          ", ")))
          (verbatim-lines (git-commit-message root rev))
          (if (not (or (null? parents) parent))
              "This is a merge commit."
              (list
               `(tabular
                 (tformat (cwith "1" "-1" "1" "-1" "cell-lsep" "0pt")
                          (cwith "1" "-1" "2" "2" "cell-halign" "r")
                          (table ,@(map (cut commit-file-row root rev parent <>)
                                        d))))
               `(concat ,(number->string (length d)) " files changed, "
                        ,(number->string ins) " insertions("
                        (with "color" "dark green" "+") "), "
                        ,(number->string del) " deletions("
                        (with "color" "dark red" "-") ")")))))))

(tm-define (tmfs-url-commit root rev)
  (string-append "tmfs://commit/" rev "/" (url->tmfs-string root)))

(tmfs-format-handler (commit name)
  "texmacs")

(tmfs-title-handler (commit name doc)
  (let* ((root (tmfs-string->url (tmfs-cdr name)))
         (rev (tmfs-car name)))
    (string-append "Commit " (short-hash rev) " - "
                   (url->system (url-tail root)))))

(tmfs-load-handler (commit name)
  (let* ((root (tmfs-string->url (tmfs-cdr name)))
         (rev (tmfs-car name)))
    (git-commit-content root rev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The tmfs handlers for Git pages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (page-skip which)
  (with n (and (string-starts? which "log.")
               (string->number (string-drop which 4)))
    (or n 0)))

(tmfs-title-handler (git name doc)
  (let* ((root (tmfs-string->url (tmfs-cdr name)))
         (short (url->system (url-tail root)))
         (which (tmfs-car name)))
    (cond ((== which "status") (string-append "Git Status - " short))
          ((string-starts? which "log") (string-append "Git Log - " short))
          ((== which "branches") (string-append "Git Branches - " short))
          ((== which "graph") (string-append "Git Graph - " short))
          ((== which "output") (string-append "Git Output - " short))
          (else (string-append "Git - " short)))))

(tmfs-load-handler (git name)
  (let* ((root (tmfs-string->url (tmfs-cdr name)))
         (which (tmfs-car name)))
    (cond ((== which "status") (git-status-content root))
          ((string-starts? which "log")
           (git-log-content root (page-skip which)))
          ((== which "branches") (git-branches-content root))
          ((== which "graph") (git-graph-content root))
          ((== which "output") (git-output-content root))
          (else '(document "")))))
