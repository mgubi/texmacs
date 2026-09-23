
;; Headless test of the Git support.  Run with doc/tests/run-git-tests.sh,
;; which creates the repository $GIT_TEST_DIR/repo test and a worktree.

(use-modules (version version-menu))

(define failures 0)
(define (check name ok?)
  (display* (if ok? "ok   " "FAIL ") name "\n")
  (when (not ok?) (set! failures (+ failures 1))))

(define D (string-append (getenv "GIT_TEST_DIR") "/repo test"))
(define R (system->url D))
(define F (system->url (string-append D "/sub dir/a b.tm")))
(define (file name) (system->url (string-append D "/" name)))

;; Detection
(check "root with spaces" (== (url->system (git-root F)) D))
(check "tool" (== (version-tool F) "git"))
(check "no root outside" (not (git-root (system->url "/"))))
(check "worktree root"
       (git-root (system->url (string-append (getenv "GIT_TEST_DIR")
                                             "/wt test/base.txt"))))

;; File states and staging
(check "untracked" (== (git-file-state F) 'untracked))
(check "version-status unknown" (== (version-status F) "unknown"))
(git-stage F)
(check "added" (== (git-file-state F) 'added))
(git-unstage F)
(check "unstaged" (== (git-file-state F) 'untracked))
(git-stage F)

;; Commit with a message that needs quoting in a shell
(define msg "Quote \" dollar $HOME tick ` back \\ done")
(git-commit-staged R msg)
(check "committed" (== (git-file-state F) 'unmodified))
(check "message" (== (tm-string-trim-both (git-commit-message R "HEAD")) msg))

;; Modification, history and revisions
(string-save "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  Changed.\n</body>\n" F)
(git-invalidate R)
(check "modified" (== (git-file-state F) 'modified))
(check "history" (== (length (version-history F)) 1))
(check "revision HEAD" (string-contains? (version-revision F "HEAD") "Hello"))
(with rev (car (car (version-history F)))
  (check "revision url"
         (string-contains? (tmfs-load (version-revision-url F rev)) "Hello")))
(git-stage F)
(string-save "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  Again.\n</body>\n" F)
(git-invalidate R)
(check "partial" (== (git-file-state F) 'partial))
(check "revision INDEX" (string-contains? (version-revision F "INDEX") "Changed"))
(git-discard-now F)
(check "discarded" (== (git-file-state F) 'staged))
(git-commit-staged R "second")

;; Pages
(define (page which) (tmfs-load (tmfs-url-git R which)))
(check "status page" (string-contains? (page "status") "On branch"))
(check "log page" (string-contains? (page "log") "second"))
(check "branches page" (string-contains? (page "branches") "Local branches"))
(check "output page" (string-contains? (page "output") "exit code"))
(check "commit page"
       (string-contains? (tmfs-load (tmfs-url-commit R (git-rev-parse R "HEAD")))
                         "files changed"))
(check "root commit page"
       (string-contains? (tmfs-load (tmfs-url-commit R (git-rev-parse R "HEAD~2")))
                         "none"))

;; Branches, tags, stashes
(git-create-branch R "feature x")
(check "invalid branch refused" (== (git-current-branch R) "main"))
(git-create-branch R "feature")
(check "new branch" (== (git-current-branch R) "feature"))
(git-switch-branch R "main")
(check "switched back" (== (git-current-branch R) "main"))
(git-create-tag R "v1" "First version")
(check "tag" (== (map git-branch-name (git-tags R)) '("v1")))
(string-save "stash me\n" (file "base.txt"))
(git-stash R)
(check "stashed" (== (length (git-stashes R)) 1))
(git-stash-pop R)
(check "stash popped" (== (git-file-state (file "base.txt")) 'modified))

;; Renames and conflicts
(git-run R "checkout" "--quiet" "--" "base.txt")
(git-run R "mv" "base.txt" "renamed.txt")
(git-invalidate R)
(check "rename parsed"
       (list-find (git-status-entries R)
                  (lambda (e) (and (== (git-entry-kind e) 'renamed)
                                   (== (git-entry-orig e) "base.txt")))))
(git-commit-staged R "rename")
(string-save "one\n" (file "c.txt"))
(git-run R "checkout" "--quiet" "-b" "c1")
(git-stage (file "c.txt"))
(git-commit-staged R "c1")
(git-run R "checkout" "--quiet" "main")
(string-save "two\n" (file "c.txt"))
(git-stage (file "c.txt"))
(git-commit-staged R "c2")
(git-merge-branch R "c1")
(check "conflict" (== (git-file-state (file "c.txt")) 'conflicted))
(git-run R "merge" "--abort")

;; Secure actions
(check "secure page action" (secure? '(git-page-stage "a" "b")))

(display* "FAILURES: " failures "\n")
