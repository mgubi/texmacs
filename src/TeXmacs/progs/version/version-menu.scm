
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

(tm-define (git-interactive-compare-with name)
  (:interactive #t)
  (interactive
   (lambda (revision)
     (git-compare-with-revision name (cork->utf8 revision)))))

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

(menu-bind git-file-menu
  (group "Git")
  (assuming (git-state? 'untracked)
    ("Add to repository" (git-stage (current-buffer))))
  (assuming (git-state? 'modified 'partial)
    ("Stage changes" (git-stage (current-buffer))))
  (assuming (git-state? 'conflicted)
    (assuming (git-texmacs-file? (current-buffer))
      ("Resolve conflict..." (git-resolve-conflict (current-buffer))))
    ("Mark as resolved" (git-mark-resolved (current-buffer))))
  (assuming (git-state? 'staged 'partial 'added)
    ("Unstage changes" (git-unstage (current-buffer))))
  (assuming (not (git-state? 'conflicted))
    (when (or (not (git-state? 'unmodified))
              (buffer-modified? (current-buffer)))
      ("Commit this file..." (git-interactive-commit-file (current-buffer)))))
  (assuming (git-state? 'modified 'partial)
    ("Discard changes..." (git-discard (current-buffer))))
  (assuming (and (git-texmacs-file? (current-buffer))
                 (not (git-state? 'untracked 'added 'conflicted)))
    (-> "Compare with" (link git-compare-menu))))

(menu-bind git-repository-menu
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
      ("Pull" (git-pull (current-git-root)))
      ("Push" (git-push (current-git-root)))))
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

(menu-bind version-menu
  (assuming (versioned? (current-buffer))
    (assuming (version-supports-history? (current-buffer))
      (when (!= (version-status (current-buffer)) "unknown")
        ("History" (version-show-history (current-buffer))))
      ---))
  (assuming (version-revision? (current-buffer))
    (assuming (version-supports-history? (version-head (current-buffer)))
      ("History" (version-show-history (version-head (current-buffer))))
      ---))
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
  (assuming (versioned? (current-buffer))
    (assuming (version-supports-git-style? (current-buffer))
      (link git-file-menu)
      ---))
  (assuming (git-revision-of (current-buffer))
    ("Restore this version"
     (git-restore-revision (version-head (current-buffer))
                           (git-revision-of (current-buffer))))
    ---)
  (assuming (current-git-root)
    (-> (eval (git-menu-label (current-git-root)))
        (link git-repository-menu))
    ---)
  (assuming (git-can-init? (current-buffer))
    ("Create Git repository..." (git-interactive-init (current-buffer))))
  (assuming (git-available?)
    ("Clone Git repository..." (git-interactive-clone))
    (with l (git-recent-repositories)
      (assuming (nnull? l)
        (-> "Recent Git repositories"
            (for (r l)
              ((eval (utf8->cork r)) (git-show-status (system->url r)))))))
    ---)
  (assuming (or (versioned? (current-buffer))
                (version-revision? (current-buffer)))
    (-> "Compare with"
        ;;(when (versioned? (current-buffer))
        ;;  (when (buffer-tmfs? (current-buffer))
        ;;    ("With current version"
        ;;      (git-compare-with-current (current-buffer))))
        ;;  (when (buffer-tmfs? (current-buffer))
        ;;    ("With parent version"
        ;;      (git-compare-with-parent (current-buffer))))
        ;;  (when (and (not (buffer-tmfs? (current-buffer)))
        ;;             (buffer-has-diff? (current-buffer)))
        ;;    ("With the HEAD"
        ;;      (git-compare-with-master (current-buffer)))))
        (link version-compare-menu)))
  (assuming (not (or (versioned? (current-buffer))
                     (version-revision? (current-buffer))))
    (-> "Compare"
        ("With older version"
         (choose-file compare-with-older "Compare with older version" ""))
        ("With newer version"
         (choose-file compare-with-newer "Compare with newer version" ""))))
  (-> "Move::difference"
      ("First difference" (version-first-difference))
      ("Previous difference" (version-previous-difference))
      ("Next difference" (version-next-difference))
      ("Last difference" (version-last-difference)))
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
      ("Rough" (version-set-grain "rough"))))
