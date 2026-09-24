
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-menu.scm
;; DESCRIPTION : menus for versioning portions of text
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-menu)
  (:use (version version-compare)
        (version version-tmfs)
        (version git-widgets)
        (version git-project)
        (version git-blame)
        (version git-drivers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compare with other revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind version-compare-menu
  (when (version-revision? (current-buffer))
    ("Current user version"
     (compare-with-newer* (version-head (current-buffer))))
    ---)
  (with history (version-history* (current-buffer))
    (assuming (list? history)
      (for (line (if (< (length history) 25) history (sublist history 0 25)))
        (with (rev by date msg) line
          (let* ((cur (current-buffer))
                 (head (if (version-revision? cur) (version-head cur) cur))
                 (msg* (if (<= (string-length msg) 50) msg
                           (string-append (substring msg 0 50) "...")))
                 (name (string-append
                        "Version " (version-beautify-revision cur rev)
                        " by " by " on " date ": " msg*))
                 (dest (version-revision-url head rev)))
            (when (!= (url->url dest) (url->url cur))
              ((eval name)
               (if (version-newer? dest cur)
                   (compare-with-newer dest)
                   (compare-with-older dest)))))))
      ---))
  ("Older version"
   (choose-file compare-with-older "Compare with older version" ""))
  ("Newer version"
   (choose-file compare-with-newer "Compare with newer version" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (git-state? . l)
  (in? (git-file-state (current-buffer)) l))

(tm-define (git-can-init? u)
  (and (not (url-rooted-tmfs? u))
       (url-exists? u)
       (not (versioned? u))
       (git-available?)))

(define (current-root) (git-root (current-buffer)))

(menu-bind git-compare-menu
  ("Last commit" (git-compare-with (current-buffer) "HEAD"))
  (when (git-state? 'staged 'partial)
    ("Staged version" (git-compare-with (current-buffer) "INDEX")))
  (assuming (git-status-ref (git-status (current-root)) 'upstream)
    ("Remote version"
     (git-compare-with-revision (current-buffer) "@{upstream}")))
  (assuming (git-rev-parse (current-root) "ORIG_HEAD")
    ("Before the last pull or merge"
     (git-compare-with-revision (current-buffer) "ORIG_HEAD")))
  ("Other revision..." (git-interactive-compare-with (current-buffer)))
  (with l (git-tags (current-root))
    (assuming (nnull? l)
      ---
      (for (t (sublist l 0 (min 10 (length l))))
        ((eval (string-append "Tag " (utf8->cork (git-branch-name t))))
         (git-compare-with-revision (current-buffer) (git-branch-name t))))))
  (with l (list-filter (git-branches (git-root (current-buffer)))
                       (lambda (b) (not (git-branch-current? b))))
    (assuming (nnull? l)
      ---
      (for (b l)
        ((eval (string-append "Branch " (utf8->cork (git-branch-name b))))
         (git-compare-with (current-buffer) (git-branch-name b)))))))

(define (sublist* l i j)
  (sublist l i (min j (length l))))

(menu-bind git-restore-menu
  (with l (sublist* (or (version-history (current-buffer)) '()) 0 15)
    (for (x l)
      (with (rev by date msg) x
        ((eval (string-append date " " (utf8->cork by) ": "
                              (utf8->cork (git-short-message msg))))
         (git-restore-revision (current-buffer)
                               (car (string-tokenize-by-char rev #\:))))))))

(menu-bind git-file-menu
  (assuming (git-state? 'untracked)
    ("Add to repository" (git-stage (current-buffer))))
  (assuming (git-state? 'modified 'partial)
    ("Stage changes" (git-stage (current-buffer))))
  (assuming (git-state? 'staged 'partial 'added)
    ("Unstage changes" (git-unstage (current-buffer))))
  (assuming (not (git-state? 'conflicted))
    (when (or (not (git-state? 'unmodified))
              (buffer-modified? (current-buffer)))
      ("Commit this file..." (git-interactive-commit-file (current-buffer)))))
  (assuming (git-state? 'modified 'partial)
    ("Discard changes..." (git-discard (current-buffer)))))

(menu-bind git-project-menu
  ("Commit project..." (git-interactive-commit-project (current-buffer)))
  (with l (or (git-project-untracked (current-buffer)) '())
    (when (nnull? l)
      ((eval (string-append "Add " (number->string (length l))
                            " missing files"))
       (git-add-project-files (current-buffer))))))

(menu-bind git-simple-repository-menu
  ("Status" (git-show-status))
  ("Git panel" (git-open-tool))
  ("History" (git-show-log))
  ---
  ("Save snapshot..." (git-interactive-save-snapshot (current-git-root)))
  (-> "Restore snapshot"
      (for (c (git-snapshots (current-git-root)))
        ((eval (string-append (git-commit-date c) " "
                              (utf8->cork (git-short-message
                                           (git-commit-subject c)))))
         (git-restore-snapshot (current-git-root) (git-commit-hash c)))))
  (with remotes? (nnull? (git-remotes (current-git-root)))
    (assuming (git-busy? (current-git-root))
      ("Cancel running command" (git-cancel (current-git-root))))
    (when (and remotes? (not (git-busy? (current-git-root))))
      ("Synchronize" (git-sync (current-git-root)))))
  ---
  (-> "Preferences"
      ("Simple mode" (git-toggle-simple-mode)))
  ("Git output" (git-show-output)))

(menu-bind git-repository-menu
  (assuming (git-simple-mode?)
    (link git-simple-repository-menu))
  (assuming (not (git-simple-mode?))
    (link git-full-repository-menu)))

(menu-bind git-full-repository-menu
  ("Status" (git-show-status))
  ("Git panel" (git-open-tool))
  ("Log" (git-show-log))
  ("Graph" (git-show-page (current-git-root) "graph"))
  ("Branches and tags" (git-show-branches))
  ---
  ("Commit..." (git-interactive-commit))
  ("Stage all changes" (git-stage-all (current-git-root)))
  ---
  ("New branch..." (git-interactive-create-branch (current-git-root)))
  (with l (list-filter (git-branches (current-git-root))
                       (lambda (b) (not (git-branch-current? b))))
    (when (nnull? l)
      (-> "Switch to branch"
          (for (b l)
            ((eval (utf8->cork (git-branch-name b)))
             (git-switch-branch (current-git-root) (git-branch-name b)))))
      (-> "Merge branch"
          (for (b l)
            ((eval (utf8->cork (git-branch-name b)))
             (git-merge-branch (current-git-root) (git-branch-name b)))))))
  ("Tag this version..." (git-interactive-tag (current-git-root)))
  ---
  (with remotes? (nnull? (git-remotes (current-git-root)))
    (assuming (git-busy? (current-git-root))
      ("Cancel running command" (git-cancel (current-git-root))))
    (when (and remotes? (not (git-busy? (current-git-root))))
      ("Fetch" (git-fetch (current-git-root)))
      ("Get changes (pull)" (git-pull (current-git-root)))
      ("Send changes (push)" (git-push (current-git-root)))))
  (-> "Remotes"
      ("Add remote..." (git-interactive-add-remote (current-git-root)))
      (with l (git-remotes (current-git-root))
        (assuming (nnull? l)
          ---
          (-> "Push to"
              (for (r l)
                ((eval (utf8->cork r))
                 (git-push-to (current-git-root) r))))
          (-> "Remove"
              (for (r l)
                ((eval (utf8->cork r))
                 (git-remove-remote (current-git-root) r)))))))
  ---
  ("Stash changes" (git-stash (current-git-root)))
  (when (nnull? (git-stashes (current-git-root)))
    ("Restore last stash" (git-stash-pop (current-git-root))))
  ---
  (-> "Preferences"
      ("All preferences..." (open-git-preferences))
      ---
      ("Simple mode" (git-toggle-simple-mode))
      ("Sign commits and tags" (git-toggle-signing))
      (-> "Pull"
          ("Fast-forward only" (git-set-pull-mode "fast-forward"))
          ("Merge" (git-set-pull-mode "merge"))
          ("Rebase" (git-set-pull-mode "rebase"))))
  (when (not (git-merge-driver-installed? (current-git-root)))
    ("Merge documents structurally"
     (git-install-merge-driver (current-git-root))))
  ("Git output" (git-show-output))
  ("Refresh" (begin (version-tool-reset) (git-refresh (current-git-root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main version menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (git-document?)
  ;; Is the current buffer a document in a Git working tree?
  (and (versioned? (current-buffer))
       (version-supports-git-style? (current-buffer))))

(define (git-history-document?)
  (and (git-document?) (not (git-state? 'untracked 'added))))

(menu-bind version-differences-menu
  ("First difference" (version-first-difference))
  ("Previous difference" (version-previous-difference))
  ("Next difference" (version-next-difference))
  ("Last difference" (version-last-difference))
  ---
  (when (or (inside-version?) (selection-active-any?))
    (-> "Show"
	("Both versions" (version-show 'version-both))
	("Old version" (version-show 'version-old))
	("New version" (version-show 'version-new)))
    (-> "Retain"
	("Current version" (version-retain 'current))
	("Old version" (version-retain 0))
	("New version" (version-retain 1))))
  (-> "Grain"
      ("Detailed" (version-set-grain "detailed"))
      ("Block" (version-set-grain "block"))
      ("Rough" (version-set-grain "rough")))
  ---
  ("Review bar" (version-review-open)))

(menu-bind version-menu
  ;; Conflicts come first, since they have to be resolved
  (assuming (and (git-document?) (git-state? 'conflicted))
    (group "Conflict")
    (assuming (git-texmacs-file? (current-buffer))
      ("Resolve conflict..." (git-resolve-conflict (current-buffer))))
    ("Mark as resolved" (git-mark-resolved (current-buffer)))
    ---)
  ;; The most frequent actions
  (assuming (current-git-root)
    (assuming (git-simple-mode?)
      ("Save snapshot..." (git-interactive-save-snapshot (current-git-root))))
    (assuming (not (git-simple-mode?))
      ("Commit..." (git-interactive-commit)))
    (with remotes? (nnull? (git-remotes (current-git-root)))
      (assuming (git-busy? (current-git-root))
        ("Cancel running command" (git-cancel (current-git-root))))
      (assuming (and remotes? (not (git-busy? (current-git-root))))
        ("Synchronize" (git-sync (current-git-root)))))
    ("Git panel" (git-open-tool))
    ---)
  ;; The current document and its history
  (assuming (git-revision-of (current-buffer))
    ("Restore this version"
     (git-restore-revision (version-head (current-buffer))
                           (git-revision-of (current-buffer)))))
  (assuming (and (git-history-document?) (git-texmacs-file? (current-buffer))
                 (not (git-state? 'conflicted)))
    (-> "Compare with"
        (link git-compare-menu)
        ---
        (link version-compare-menu)))
  (assuming (not (and (git-history-document?)
                      (git-texmacs-file? (current-buffer))
                      (not (git-state? 'conflicted))))
    (assuming (or (versioned? (current-buffer))
                  (version-revision? (current-buffer)))
      (-> "Compare with" (link version-compare-menu)))
    (assuming (not (or (versioned? (current-buffer))
                       (version-revision? (current-buffer))))
      (-> "Compare"
          ("With older version"
           (choose-file compare-with-older "Compare with older version" ""))
          ("With newer version"
           (choose-file compare-with-newer "Compare with newer version" "")))))
  (assuming (git-history-document?)
    (-> "Restore version" (link git-restore-menu)))
  (assuming (versioned? (current-buffer))
    (assuming (version-supports-history? (current-buffer))
      (when (!= (version-status (current-buffer)) "unknown")
        ("History of this document"
         (version-show-history (current-buffer))))))
  (assuming (version-revision? (current-buffer))
    (assuming (version-supports-history? (version-head (current-buffer)))
      ("History of this document"
       (version-show-history (version-head (current-buffer))))))
  (assuming (and (git-history-document?) (git-texmacs-file? (current-buffer)))
    ("Who changed what" (git-show-blame (current-buffer))))
  ---
  ;; Subversion
  (assuming (versioned? (current-buffer))
    (assuming (version-supports-svn-style? (current-buffer))
      (when (!= (version-status (current-buffer)) "unknown")
        ("Update" (version-interactive-update (current-buffer))))
      (when (== (version-status (current-buffer)) "unknown")
        ("Register" (register-buffer (current-buffer))))
      (when (and (!= (version-status (current-buffer)) "unknown")
                 (or (== (version-status (current-buffer)) "modified")
                     (buffer-modified? (current-buffer))))
        ("Commit" (version-interactive-commit (current-buffer))))
      ---))
  ;; Submenus for Git
  (assuming (and (git-document?) (not (git-simple-mode?)))
    (-> "This file" (link git-file-menu)))
  (assuming (and (git-document?) (git-texmacs-file? (current-buffer)))
    (-> "Project" (link git-project-menu)))
  (assuming (current-git-root)
    (-> (eval (git-menu-label (current-git-root)))
        (link git-repository-menu)))
  (assuming (git-can-init? (current-buffer))
    ("Create Git repository..." (git-interactive-init (current-buffer))))
  (assuming (git-available?)
    ("Clone Git repository..." (git-interactive-clone))
    (with l (git-recent-repositories)
      (assuming (nnull? l)
        (-> "Recent Git repositories"
            (for (r l)
              ((eval (utf8->cork r)) (git-show-status (system->url r))))))))
  ---
  (-> "Differences" (link version-differences-menu))
  (assuming (git-available?)
    ("Git preferences..." (open-git-preferences))))
