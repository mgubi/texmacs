
;; Tests of the Git support which need the event loop (asynchronous commands)
;; or open documents.  Run with doc/tests/run-git-tests.sh --gui, which uses
;; the offscreen Qt platform, so that no windows are shown.

(use-modules (version version-menu))

(define failures 0)
(set-preference "git mode chosen" "on")
(define (check name ok?)
  (display* (if ok? "ok   " "FAIL ") name "\n")
  (when (not ok?) (set! failures (+ failures 1))))

(define T (getenv "GIT_TEST_DIR"))
(define A (system->url (string-append T "/remote/clone a")))
(define B (system->url (string-append T "/remote/clone b")))
(define DA (url-append A "doc.tm"))
(define DB (url-append B "doc.tm"))
(define C (system->url (string-append T "/conflict/paper.tm")))
(define C2 (system->url (string-append T "/conflict2/paper.tm")))

(for (r (list "/remote/clone a" "/remote/clone b" "/conflict" "/conflict2"))
  (git-trust (system->url (string-append T r))))

(define (doc text)
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  "
                 text "\n</body>\n"))

(define finished? #f)

(define (finish)
  (set! finished? #t)
  (display* "FAILURES: " failures "\n")
  (quit-TeXmacs))

;; NOTE: the tests are one chain of callbacks: an error in a callback stops
;; it, which is then reported instead of waiting for the alarm of the runner
(delayed
  (:pause 240000)
  (when (not finished?)
    (check "the tests completed (an error stopped the chain)" #f)
    (finish)))

(define (menu-strings m)
  ;; The strings in the expansion of the menu @m
  (let loop ((x (menu-expand m)))
    (cond ((string? x) (list x))
          ((pair? x) (append-map loop x))
          (else '()))))

(define (menu-has? m . l)
  (with s (menu-strings m)
    (list-and (map (lambda (x) (in? x s)) l))))

;; Conflict resolution with the structured comparison
(define (test-conflict)
  (check "conflicted" (== (git-file-state C) 'conflicted))
  (load-buffer C)
  (git-resolve-conflict C)
  (check "one difference"
         (== (length (tree-search (buffer-get C)
                                  (lambda (t) (tree-is? t 'version-both))))
             1))
  (version-first-difference)
  (version-retain 1)
  (git-mark-resolved C)
  (check "resolved" (== (git-file-state C) 'staged))
  (check "their text" (string-contains? (string-load C) "as they wrote it"))
  (test-automatic-merge))

;; Changes to different words of a same line, merged automatically
(define (test-automatic-merge)
  (check "line conflict for git" (== (git-file-state C2) 'conflicted))
  (load-buffer C2)
  (git-resolve-conflict C2)
  (check "no remaining difference"
         (null? (tree-search (buffer-get C2)
                             (lambda (t) (tree-in? t '(version-old
                                                       version-new
                                                       version-both))))))
  (check "both changes merged"
         (== (tree->stree (buffer-get-body C2))
             '(document "The slow brown fox leaps." "Second.")))
  (git-mark-resolved C2)
  (check "merge resolved" (== (git-file-state C2) 'staged))
  (git-open-tool)
  (check "side tool" (in? '(git-tool) (window->tools (current-window) :right)))
  (git-refresh (git-root C2))
  (test-clone))

(define (test-save)
  ;; Saving invalidates the cached status at once
  (with u (system->url (string-append T "/remote/clone c/doc.tm"))
    (load-buffer u)
    (check "clean before save" (== (git-file-state u) 'unmodified))
    (buffer-set-body u '(document "Edited."))
    (buffer-pretend-modified u)
    (save-buffer u)
    (check "modified after save" (== (git-file-state u) 'modified))
    ;; page actions work from the Git pages of the same working tree
    (with root (git-root u)
      (git-show-status root)
      (git-page-stage (url->system root) "doc.tm")
      (check "page action from status page" (== (git-file-state u) 'staged)))
    (test-menus)))

(define (test-menus)
  ;; The menus and dialogs can be built without errors
  (with u (system->url (string-append T "/remote/clone c/doc.tm"))
    (switch-to-buffer u)
    (check "version menu" (menu-has? '(link version-menu)
                                     "Commit" "Git panel" "This file"))
    (check "git menu" (menu-has? '(link git-repository-menu)
                                 "Status" "History" "New branch"))
    (git-interactive-clone)
    (git-compare-with-revision u "HEAD~1")
    (check "compare with revision"
           (nnull? (tree-search (buffer-get u)
                                (lambda (t) (tree-in? t '(version-both))))))
    (check "compare menu" (menu-has? '(link git-compare-menu) "Last commit"))
    (git-interactive-commit (git-root u))
    (git-interactive-commit-project u)
    (set-preference "git simple mode" "on")
    (check "simple file menu"
           (and (menu-has? '(link version-menu) "Save snapshot")
                (not (menu-has? '(link version-menu) "This file"))
                (not (menu-has? '(link version-menu) "Commit"))))
    (check "simple git menu" (menu-has? '(link git-simple-repository-menu)
                                        "History"))
    (set-preference "git simple mode" "off")
    (check "restore menu" (nnull? (menu-strings '(link git-restore-menu))))
    (check "review bar opened"
           (in? '(version-review-tool)
                (window->tools (current-window) :transient-bottom)))
    (refresh-now "version-review")
    (revert-buffer-revert u)
    (test-ui)))

(define (test-ui)
  ;; Footer, automatic versioning tool, dialogs, explanations of failures
  (with u (system->url (string-append T "/remote/clone c/doc.tm"))
    (set-preference "versioning tool" "auto")
    (check "auto versioning tool in repository" (versioning-tool-active?))
    (git-status (git-root u))
    (check "footer indicator"
           (string-contains? (object->string
                              (tree->stree (footer-hook (tm->tree "x"))))
                             "Git main"))
    (git-interactive-create-branch (git-root u))
    (git-interactive-tag (git-root u))
    (git-interactive-add-remote (git-root u))
    (git-interactive-commit-file u)
    (git-interactive-save-snapshot (git-root u))
    (git-interactive-compare-with u)
    (check "valid branch name" (git-valid-branch-name? (git-root u) "topic"))
    (check "invalid branch name"
           (not (git-valid-branch-name? (git-root u) "a..b")))
    (git-show-failure (list 1 "" " ! [rejected] main -> main (fetch first)")
                      "Push")
    (open-git-preferences)
    (set-preference "git mode chosen" "off")
    (with called? #f
      (git-with-mode (lambda () (set! called? #t)))
      (check "mode asked before continuing" (not called?))
      (check "mode not chosen until answered"
             (== (get-preference "git mode chosen") "off")))
    (set-preference "git mode chosen" "on")
    (check "style package for pages"
           (url-exists? (url-resolve "$TEXMACS_PATH/packages/miscellaneous/git-pages.ts" "r")))
    (check "status page uses buttons"
           (string-contains? (tmfs-load (tmfs-url-git (git-root u) "status"))
                             "git-button"))
    (let* ((v (system->url (string-append T "/outside.tm"))))
      (string-save "<TeXmacs|2.1>\n\n<\\body>\n  x\n</body>\n" v)
      (load-buffer v)
      (check "no versioning tool outside repositories"
             (not (versioning-tool-active?)))
      (check "no footer indicator outside"
             (not (string-contains? (object->string
                                     (tree->stree (footer-hook (tm->tree "x"))))
                                    "Git")))
      (switch-to-buffer u))
    (test-commit-safety)))

(define (test-commit-safety)
  ;; Committing saves the document first; the panel keeps the message
  (with u (system->url (string-append T "/remote/clone c/doc.tm"))
    (switch-to-buffer u)
    (buffer-set-body u '(document "Unsaved, then committed."))
    (buffer-pretend-modified u)
    (with r (git-commit-file* u "commit unsaved" #f)
      (check "commit of unsaved document" (car r))
      (check "unsaved edits committed"
             (string-contains? (git-show-file (git-root u) "HEAD" "doc.tm")
                               "Unsaved, then committed.")))
    (git-open-tool)
    (with p (list-find (buffer-list)
                       (lambda (b) (string-starts? (url->unix b)
                                                   "tmfs://aux/git-panel")))
      (check "panel message buffer" p)
      (when p
        (buffer-set-body p '(document "a message being typed"))
        (refresh-now "git-tool")
        (git-refresh (git-root u))
        (check "panel keeps the message"
               (== (buffer-get-body p) (tm->tree '(document
                                                   "a message being typed"))))))
    (test-panel)))

(define (steps l)
  ;; Execute the thunks @l, leaving time for the interface in between
  (when (nnull? l)
    ((car l))
    (delayed (:pause 600) (steps (cdr l)))))

(define (git-commands-since t0)
  (map (lambda (x) (car (caddr x)))
       (list-filter (git-command-history) (lambda (x) (> (car x) t0)))))

(define (test-panel)
  ;; The panel follows the working tree and the document, and runs no
  ;; Git command while typing (audit 2, C1 and C2)
  (let* ((u (system->url (string-append T "/remote/clone c/doc.tm")))
         (root (git-root u))
         (f (url-append root "panel-new.txt"))
         (t0 0))
    (switch-to-buffer u)
    (string-save "new\n" f)
    (git-refresh root)
    ;; NOTE: the status is recomputed once after git-refresh (by the
    ;; footer and the menus, when idle); typing starts afterwards
    (steps
     (list
      (lambda () (noop))
      (lambda () (noop))
      (lambda () (noop))
      (lambda ()
        (check "panel lists a new file"
               (in? "panel-new.txt" (gui-test-buttons)))
        (check "panel offers to stage it" (in? "Stage" (gui-test-buttons)))
        (set! t0 (texmacs-time))
        (insert "a"))
      (lambda () (insert "b"))
      (lambda () (insert "c"))
      (lambda () (insert "d"))
      (lambda () (insert "e"))
      (lambda () (insert "f"))
      (lambda ()
        ;; NOTE: the footer may compute the status once, when idle
        (with l (git-commands-since t0)
          (check "no Git command while typing"
                 (and (<= (length l) 1) (list-and (map (cut == <> "status") l))))
          (when (nnull? l) (display* "     commands: " l "\n")))
        (git-stage f))
      (lambda ()
        (check "panel refreshed after staging"
               (in? "Unstage" (gui-test-buttons)))
        (switch-to-buffer DA))
      (lambda ()
        (check "panel follows the document"
               (not (in? "panel-new.txt" (gui-test-buttons))))
        (switch-to-buffer u)
        (git-unstage f)
        (system-remove f)
        (revert-buffer-revert u)
        (git-refresh root))
      (lambda () (test-diverged))))))

(define (test-diverged)
  ;; Pull when the local and remote branches diverged: merge them
  (let* ((a (system->url (string-append T "/remote/clone a")))
         (c (system->url (string-append T "/remote/clone c"))))
    (git-run c "reset" "--quiet" "--hard" "HEAD")
    (string-save "from a\n" (url-append a "a.txt"))
    (git-stage (url-append a "a.txt"))
    (git-commit-staged a "from a")
    (git-push a
      (lambda (r)
        (string-save "from c\n" (url-append c "c.txt"))
        (git-stage (url-append c "c.txt"))
        (git-commit-staged c "from c")
        (git-run c "config" "user.email" "c@example.com")
        (git-run c "config" "user.name" "User c")
        (check "ff-only pull fails"
               (not (git-ok? (git-run c "pull" "--ff-only"))))
        (git-pull-merge c
          (lambda (r)
            (check "diverged pull merged" (git-ok? r))
            (check "both changes" (and (url-exists? (url-append c "a.txt"))
                                       (url-exists? (url-append c "c.txt"))))
            (test-cancel)))))))

(define (test-cancel)
  ;; A fetch from a remote which hangs, then cancelled
  (let* ((root (system->url (string-append T "/remote/clone c")))
         (t0 (texmacs-time)))
    (git-run root "remote" "add" "slow" (string-append T "/remote/origin.git"))
    (git-run root "config" "remote.slow.uploadpack"
             "sleep 30; git-upload-pack")
    (git-run-async root (list "fetch" "slow") #f
      (lambda (r)
        (check "cancelled fetch failed" (not (git-ok? r)))
        (check "cancelled quickly" (< (- (texmacs-time) t0) 10000))
        (check "not busy after cancel" (not (git-busy? root)))
        (finish)))
    (check "busy fetching" (git-busy? root))
    (delayed (:pause 1000) (git-cancel root))))

(define (test-clone)
  (with dest (string-append T "/remote/clone c")
    (git-clone (string-append T "/remote/origin.git") dest
      (lambda (r)
        (check "clone" (git-ok? r))
        ;; NOTE: do not depend on the global identity of the user
        (git-run (system->url dest) "config" "user.email" "c@example.com")
        (git-run (system->url dest) "config" "user.name" "User c")
        (check "cloned document"
               (url-exists? (system->url (string-append dest "/doc.tm"))))
        (check "clone is versioned"
               (== (version-tool (system->url (string-append dest "/doc.tm")))
                   "git"))
        (test-save)))))

;; Asynchronous remote commands and reloading of open documents
(define (test-remote)
  (string-save (doc "Version one.") DA)
  (git-stage DA)
  (git-commit-staged A "one")
  (git-push A
    (lambda (r)
      (check "push with new upstream" (git-ok? r))
      (check "upstream" (== (git-status-ref (git-status A) 'upstream)
                            "origin/main"))
      (git-pull B
        (lambda (r)
          (check "pull" (git-ok? r))
          (load-buffer DB)
          (string-save (doc "Version two.") DA)
          (git-stage DA)
          (git-commit-staged A "two")
          (git-push A
            (lambda (r)
              (git-fetch B
                (lambda (r)
                  (check "behind after fetch"
                         (== (git-status-ref (git-status B) 'behind) 1))
                  (git-pull B
                    (lambda (r)
                      (check "open document reloaded"
                             (== (tree->stree (buffer-get-body DB))
                                 '(document "Version two.")))
                      (check "not busy" (not (git-busy? B)))
                      (test-conflict)))))))))))
  (check "busy while pushing" (git-busy? A)))

(test-remote)
