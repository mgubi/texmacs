;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-test.scm
;; DESCRIPTION : tests of the Git support
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite checks the Git support of TeXmacs/progs/version:
;;
;;   - git-base.scm: running Git (only in trusted repositories, never with
;;     options smuggled in as names), the status of the working tree and of
;;     files, the history, branches, remotes, signatures, the footer;
;;   - version-git.scm: the operations on files and working trees, and the
;;     Git pages (tmfs://git/..., tmfs://commit/...);
;;   - version-merge.scm and git-drivers.scm: the structured three way merge
;;     of documents, and the merge driver which lets Git use it;
;;   - git-blame.scm and git-project.scm: blame by paragraph, descriptions
;;     of changes, the files of a project, snapshots.
;;
;; The repositories are made in git-test in the temporary directory, with
;; their identity in their own configuration; the global and system
;; configurations of Git are ignored (GIT_CONFIG_GLOBAL, GIT_CONFIG_NOSYSTEM,
;; with Git 2.32 or newer), so that the settings of the user
;; (commit.gpgsign, hooks...) do not matter. The preferences which the
;; suite changes are put back at the end.
;; The asynchronous commands (fetch, pull, push, clone) need the event loop,
;; and are checked by doc/tests/git-gui-test.scm instead.

(texmacs-module (check git-test)
  (:use (check check-lib)
        (version version-tmfs)
        (version version-git)
        (version version-merge)
        (version git-drivers)
        (version git-blame)
        (version git-project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define T (string-append (url->system (url-temp-dir)) "/git-test"))

(define (path . l) (apply string-append (cons* T "/" l)))
(define (dir . l) (system->url (apply path l)))

(define (shell . l)
  (eval-system (apply string-append l)))

(define (sh d . l)
  ;; a shell command in the directory @d (a system path)
  (apply shell (cons* "cd '" d "' && " l)))

(define (make-repo d)
  ;; a new repository with the branch main, whose identity is its own
  (system-mkdir (system->url d))
  (sh d "git init -q && git symbolic-ref HEAD refs/heads/main"
      " && git config user.email test@example.com"
      " && git config user.name 'Test User'"))

(define (tm . paragraphs)
  ;; a .tm file whose body has the strings @paragraphs
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                 (apply string-append
                        (map (lambda (p) (string-append "  " p "\n\n"))
                             paragraphs))
                 "</body>\n"))

(define (run-group thunk)
  ;; an error in a group counts as one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(define saved-preferences '())

(define test-preferences
  '("git trusted repositories" "git recent repositories"
    "git large file size" "git sign" "git log length" "git executable"))

(define (save-preferences)
  ;; #f for a preference which has no value of its own (the default)
  (set! saved-preferences
        (map (lambda (p) (cons p (and (cpp-has-preference? p)
                                      (get-preference p))))
             test-preferences)))

(define (restore-preferences)
  (for (x saved-preferences)
    (if (cdr x)
        (set-preference (car x) (cdr x))
        (reset-preference (car x)))))


;; The main repository, with a space in its name and in a subdirectory
(define R #f)
(define F #f)
(define (file name) (url-append R (unix->url name)))

(define (setup)
  (shell "rm -rf '" T "'")
  (system-mkdir (system->url T))
  (check-isolate-git)
  (make-repo (path "repo test"))
  (system-mkdir (dir "repo test/sub dir"))
  (string-save (tm "Hello world.") (dir "repo test/sub dir/a b.tm"))
  (string-save "base\n" (dir "repo test/base.txt"))
  (sh (path "repo test") "git add base.txt && git commit -q -m base"
      " && git worktree add -q '" (path "wt test") "' -b wt")
  (set! R (dir "repo test"))
  (set! F (dir "repo test/sub dir/a b.tm"))
  (version-tool-reset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines which do not run Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-subroutines)
  (check-group "subroutines")
  (check= (git-split "a,b,c" ",") '("a" "b" "c"))
  (check= (git-split "a,b," ",") '("a" "b"))
  (check= (git-split "a,,b" ",") '("a" "" "b"))
  (check= (git-split "" ",") '())
  (check= (git-split "a<>b<>" "<>") '("a" "b"))
  (check= (git-shell-quote "plain") "'plain'")
  (check= (git-shell-quote "it's") "'it'\\''s'")
  (check-true (git-safe-name? "main"))
  (check-true (git-safe-name? "topic/x"))
  (check-false (git-safe-name? ""))
  (check-false (git-safe-name? "-f"))
  (check-false (git-safe-name? "--output=x"))
  (check-false (git-safe-name? "a\nb"))
  (check-false (git-safe-name? 'main))
  (check-true (git-safe-names? '("a" "b")))
  (check-false (git-safe-names? '("a" "-b")))
  (with args (git-arguments (system->url "/some where") '("status"))
    (check= (cAr args) "status")
    (check-true (in? "--literal-pathspecs" args))
    (check-true (in? "core.fsmonitor=false" args))
    (check-true (in? "core.quotepath=off" args))
    (check-true (in? "/some where" args)))
  (let ((root (system->url "/a b/repo"))
        (u (system->url "/a b/repo/sub dir/x y.tm")))
    (check= (git-relative root u) "sub dir/x y.tm")
    (check= (url->system (git-absolute root "sub dir/x y.tm"))
            "/a b/repo/sub dir/x y.tm"))
  ;; the informative line of the output of a command
  (check= (git-message (list 0 "first\nlast\n\n" "")) "last")
  (check= (git-message (list 0 "" "")) "")
  (check= (git-message (list 1 "" (string-append "From /some/remote\n"
                                                 "hint: Diverging branches\n"
                                                 "fatal: Not possible to "
                                                 "fast-forward, aborting.")))
          "fatal: Not possible to fast-forward, aborting.")
  (check= (git-message (list 1 (string-append "Auto-merging paper.tm\n"
                                              "CONFLICT (content): Merge "
                                              "conflict in paper.tm\n")
                             ""))
          "CONFLICT (content): Merge conflict in paper.tm")
  (check= (git-message (list 1 "" "  \nsomething went wrong\n"))
          "something went wrong")
  (check-true (git-ok? (list 0 "" "")))
  (check-false (git-ok? (list 128 "" "")))
  (check= (git-short-message "short") "short")
  (with s (git-short-message (make-string 60 #\x))
    (check= (string-length s) 50)
    (check-true (string-ends? s "...")))
  (check-true (git-texmacs-file? (system->url "/x/a.tm")))
  (check-true (git-texmacs-file? (system->url "/x/a.ts")))
  (check-false (git-texmacs-file? (system->url "/x/a.txt")))
  (check= (git-state-description 'partial) "partially staged")
  (check= (git-state-description 'conflicted) "conflict")
  (check= (git-state-description #f) "unknown")
  (check= (git-commit-options #t) '("--gpg-sign"))
  (check= (git-commit-options #f) '())
  (set-preference "git log length" "nonsense")
  (check= (git-log-length) 250)
  (set-preference "git log length" "20")
  (check= (git-log-length) 20)
  (set-preference "git log length" "250")
  (check= (git-entry-paths '(renamed "R." "new.tm" "old.tm"))
          '("new.tm" "old.tm"))
  (check= (git-entry-paths '(ordinary ".M" "a.tm" #f)) '("a.tm"))
  (with e '(ordinary "MM" "a.tm" #f)
    (check-true (git-entry-staged? e))
    (check-true (git-entry-unstaged? e))
    (check-false (git-entry-untracked? e)))
  (check-false (git-entry-staged? '(untracked "??" "a.tm" #f)))
  (check-true (git-entry-conflicted? '(unmerged "UU" "a.tm" #f)))
  (check= (git-history-path F "abc") '("abc" #f))
  (check= (git-history-path F (string-append "abc:" (url->tmfs-string F)))
          '("abc" "sub dir/a b.tm"))
  (check-true (secure? '(git-page-stage "a" "b")))
  (check-false (secure? '(git-stage-now "a"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trusted repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The configuration of a repository could make Git run programs (here
;; through core.fsmonitor), so that Git is only run in trusted ones.
(define (test-trust)
  (check-group "trust")
  (make-repo (path "evil"))
  (string-save "x\n" (dir "evil/a.tm"))
  (sh (path "evil") "git config core.fsmonitor \"touch '"
      (path "evil-pwned") "'; false\"")
  (with E (dir "evil")
    ;; the program does run when Git is called without precautions
    (shell "cd '" (path "evil") "' && git status > /dev/null 2>&1")
    (check-true (url-exists? (dir "evil-pwned")))
    (system-remove (dir "evil-pwned"))
    (check-false (git-trusted? E))
    (check-false (git-status E))
    (check-true (git-status-known? E))
    (check= (git-status-entries E) '())
    (check-false (git-ok? (git-run E "status")))
    (check-false (url-exists? (dir "evil-pwned")))
    (git-trust E)
    (check-true (git-trusted? E))
    (check-true (git-trusted? (dir "evil/a.tm")))
    (check-true (git-status E))
    ;; the file system monitor is never run, even when trusted
    (check-false (url-exists? (dir "evil-pwned"))))
  ;; outside working trees, for git init
  (check-true (git-trusted? (system->url "/no-such-dir-git-test/x.tm")))
  (check-false (git-trusted? R))
  (git-trust R)
  (git-trust (dir "wt test"))
  (check-true (git-trusted? R)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detection of working trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-detection)
  (check-group "detection")
  (check= (url->system (git-root F)) (path "repo test"))
  (check= (url->system (git-root R)) (path "repo test"))
  (check= (url->system (git-root (dir "repo test/sub dir")))
          (path "repo test"))
  (check= (version-tool F) "git")
  (check-true (git-active? F))
  (check-false (git-root (system->url "/")))
  (check-false (git-root (string->url "https://www.texmacs.org/doc.tm")))
  (check-false (git-root (string->url "tmfs://git/status/x")))
  ;; a linked worktree has a .git file instead of a directory
  (check= (url->system (git-root (dir "wt test/base.txt"))) (path "wt test"))
  (check= (git-current-branch (dir "wt test")) "wt")
  (check-true (git-available?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; States of files, staging and commits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Git must use the repository whose trust was checked, which may not be
;; the one it would find by itself (audit 2, A1)
(define (marker-program name)
  ;; a program which leaves the file @name in the scratch directory
  (with f (path name ".sh")
    (string-save (string-append "#!/bin/sh\ntouch '" (path name) "'\nexit 1\n")
                 (system->url f))
    (shell "chmod +x '" f "'")
    f))

(define (test-pinning)
  (check-group "pinning")
  ;; a bare repository with a signed commit, whose configuration names a
  ;; program for checking signatures
  (string-save fake-gpg (dir "fake-gpg"))
  (shell "chmod +x '" (path "fake-gpg") "'")
  (make-repo (path "signed"))
  (sh (path "signed") "echo a > a.txt && git add a.txt"
      " && git -c gpg.program='" (path "fake-gpg") "'"
      " -c user.signingkey=test commit -q -S -m signed"
      " && cd .. && git clone -q --bare signed bare.git"
      " && git -C bare.git config gpg.program '" (marker-program "bare-ran")
      "'")
  (let* ((B (dir "bare.git"))
         (brev (string-drop-right (sh (path "signed") "git rev-parse HEAD") 1))
         (doc (dir "includes.tm")))
    (check-false (git-root B))
    (check= (git-run B "log") (list -1 "" "not in a Git working tree"))
    (check-false (git-signature B brev))
    (check-false (url-exists? (dir "bare-ran")))
    (tmfs-load (tmfs-url-commit B brev))
    (check-false (url-exists? (dir "bare-ran")))
    ;; a document which includes the page of the commit
    (string-save (tm (string-append "<include|" (tmfs-url-commit B brev) ">"))
                 doc)
    (load-buffer doc)
    (update-forced)
    (buffer-pretend-saved doc)
    (buffer-close doc)
    (check-false (url-exists? (dir "bare-ran")))
    ;; the same signature check runs in a trusted repository
    (git-trust (dir "signed"))
    (git-run (dir "signed") "config" "gpg.program" (marker-program "signed-ran"))
    (check-false (git-signature (dir "signed") brev))
    (check-true (url-exists? (dir "signed-ran")))
    ;; a symbolic link from a trusted working tree into a directory of an
    ;; untrusted one (a link to its root has a .git entry, and is not
    ;; trusted)
    (shell "git clone -q '" (path "signed") "' '" (path "untrusted2") "'")
    (sh (path "untrusted2") "mkdir sub && git config gpg.program '"
        (marker-program "link-ran") "'")
    (shell "ln -s '" (path "untrusted2/sub") "' '" (path "repo test/lnk") "'")
    (with L (dir "repo test/lnk")
      (check= (git-root L) R)
      (check-false (git-signature L brev))
      ;; Git uses the trusted repository, never the one of the link
      (check= (git-rev-parse L "HEAD") (git-rev-parse R "HEAD"))
      (check-false (url-exists? (dir "link-ran"))))
    (shell "rm '" (path "repo test/lnk") "'")
    (git-invalidate R)
    ;; init and clone still run outside working trees
    (check-true (git-ok? (git-run (dir "") "clone" "--quiet" "--"
                                  (path "bare.git") (path "cloned"))))
    (check-true (url-exists? (dir "cloned/a.txt")))))

(define (test-states)
  (check-group "states")
  (check= (git-file-state F) 'untracked)
  (check= (version-status F) "unknown")
  (check= (git-current-branch R) "main")
  (with st (git-status R)
    (check= (git-status-ref st 'head) "main")
    (check= (string-length (git-status-ref st 'oid)) 40)
    (check-false (git-status-ref st 'upstream))
    ;; the files of an untracked directory are not listed one by one
    (check= (git-status-entries R) '((untracked "??" "sub dir/" #f))))
  (check= (git-status-cached R) (git-status R))
  (git-stage F)
  (check= (git-file-state F) 'added)
  (check-true (git-has-staged? R))
  (git-unstage F)
  (check= (git-file-state F) 'untracked)
  (check-false (git-has-staged? R))
  (git-stage F)
  ;; a message which needs quoting in a shell
  (with msg "Quote \" dollar $HOME tick ` back \\ done"
    (git-commit-staged R msg)
    (check= (git-file-state F) 'unmodified)
    (check= (version-status F) "unmodified")
    (check= (tm-string-trim-both (git-commit-message R "HEAD")) msg)
    (check= (git-last-commit-message R)
            (utf8->cork (string-append msg "\n\n"))))
  ;; modified, staged, partially staged, deleted
  (string-save (tm "Changed.") F)
  (git-invalidate R)
  (check= (git-file-state F) 'modified)
  (check= (version-status F) "modified")
  (check= (length (version-history F)) 1)
  (check-true (string-contains? (version-revision F "HEAD") "Hello"))
  (with rev (car (car (version-history F)))
    (check-true (string-contains? (tmfs-load (version-revision-url F rev))
                                  "Hello")))
  (git-stage F)
  (check= (git-file-state F) 'staged)
  (string-save (tm "Again.") F)
  (git-invalidate R)
  (check= (git-file-state F) 'partial)
  (check-true (string-contains? (version-revision F "INDEX") "Changed"))
  (check-true (string-contains? (version-revision F "HEAD") "Hello"))
  (git-discard-now F)
  (check= (git-file-state F) 'staged)
  (check-true (string-contains? (string-load F) "Changed"))
  (git-commit-staged R "second")
  (system-remove (file "base.txt"))
  (git-invalidate R)
  (check= (git-file-state (file "base.txt")) 'deleted)
  (git-run R "checkout" "--" "base.txt")
  (git-invalidate R)
  (check= (git-file-state (file "base.txt")) 'unmodified)
  ;; files of an untracked directory are untracked
  (system-mkdir (dir "repo test/new dir"))
  (string-save "x\n" (file "new dir/x.txt"))
  (git-invalidate R)
  (check= (git-file-state (file "new dir/x.txt")) 'untracked)
  (shell "rm -rf '" (path "repo test/new dir") "'")
  (git-invalidate R)
  ;; the commands are remembered
  (with h (car (git-command-history))
    (check= (cadr h) (path "repo test"))
    (check= (length (cadddr h)) 3))
  (check= (git-last-root) R))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-history)
  (check-group "history")
  (with l (git-log R 0 10)
    (check= (map git-commit-subject (cdr l))
            '("Quote \" dollar $HOME tick ` back \\ done" "base"))
    (check= (git-commit-subject (car l)) "second")
    (check= (git-commit-author (car l)) "Test User")
    (check= (git-commit-parents (car l)) (list (git-commit-hash (cadr l))))
    (check= (git-commit-parents (cAr l)) '())
    (check= (string-length (git-commit-date (car l))) 16))
  (check= (length (git-log R 1 10)) 2)
  (check= (length (git-log R 0 1)) 1)
  (check= (git-commit-subject (git-commit-info R "HEAD~1"))
          (git-commit-subject (cadr (git-log R 0 3))))
  (check-false (git-commit-info R "no-such-revision"))
  (check-false (git-rev-parse R "no-such-revision"))
  (check= (git-rev-parse R "HEAD") (git-master F))
  (check= (git-numstat R "HEAD") '((1 1 "sub dir/a b.tm")))
  (check= (git-numstat R "HEAD" "HEAD~2") '((8 0 "sub dir/a b.tm")))
  ;; the history of a file follows its renames
  (git-run R "mv" "sub dir/a b.tm" "sub dir/c d.tm")
  (git-commit-staged R "rename a b")
  (with G (file "sub dir/c d.tm")
    (with l (git-file-log G)
      (check= (map git-commit-subject l)
              '("rename a b" "second"
                "Quote \" dollar $HOME tick ` back \\ done"))
      (check= (map git-commit-files l)
              '(("sub dir/c d.tm") ("sub dir/a b.tm") ("sub dir/a b.tm"))))
    (with h (version-history G)
      (check= (length h) 3)
      (check= (cadr (git-history-path G (car (cAr h)))) "sub dir/a b.tm")
      (with rev (car (git-history-path G (caar h)))
        (check-true (string-contains? (version-revision G rev) "Changed")))
      (check-true (string-contains? (tmfs-load (version-revision-url
                                                G (car (cAr h))))
                                    "Hello"))))
  (check= (git-numstat R "HEAD") '((0 0 "sub dir/c d.tm")))
  (git-run R "mv" "sub dir/c d.tm" "sub dir/a b.tm")
  (git-commit-staged R "rename back")
  ;; binary files have no line counts
  (string-save (list->string (map integer->char '(0 1 2 0 255 0)))
               (file "bin.dat"))
  (git-stage (file "bin.dat"))
  (git-commit-staged R "binary")
  (check= (git-numstat R "HEAD") '((#f #f "bin.dat")))
  (check-false (git-signature R "HEAD"))
  (check= (git-show-file R "HEAD~1" "base.txt") "base\n")
  (check= (git-show-file R "HEAD" "no-such-file") ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git pages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (page which) (tmfs-load (tmfs-url-git R which)))

(define (test-pages)
  (check-group "pages")
  (check= (tmfs-url-git R "log")
          (string-append "tmfs://git/log/" (url->tmfs-string R)))
  (check-true (string-contains? (page "status") "On branch"))
  (check-true (string-contains? (page "log") "binary"))
  (check-true (string-contains? (page "branches") "Local branches"))
  (check-true (string-contains? (page "output") "exit code"))
  (check-true (string-contains? (page "graph") "Git graph"))
  (check-true (string-contains? (tmfs-load (tmfs-url-commit
                                            R (git-rev-parse R "HEAD")))
                                "files changed"))
  (with root-commit (git-commit-hash (cAr (git-log R 0 100)))
    (check-true (string-contains? (tmfs-load (tmfs-url-commit R root-commit))
                                  "none")))
  (string-save "untracked\n" (file "u.txt"))
  (git-invalidate R)
  (with s (page "status")
    (check-true (string-contains? s "Untracked files"))
    (check-true (string-contains? s "u.txt")))
  (system-remove (file "u.txt"))
  (git-invalidate R)
  ;; the actions of the pages only work from the pages
  (string-save "page\n" (file "base.txt"))
  (git-invalidate R)
  (git-page-stage (url->system R) "base.txt")
  (check= (git-file-state (file "base.txt")) 'modified)
  (git-run R "checkout" "--" "base.txt")
  (git-invalidate R)
  (with (line commit) (car (git-graph R 20))
    (check-true (string-contains? line "*"))
    (check= (fifth commit) "binary"))
  (with l (git-graph R 20)
    (check= (length (list-filter l cadr)) (length (git-log R 0 100)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches, tags and stashes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-branches)
  (check-group "branches")
  (check-true (git-valid-branch-name? R "feature"))
  (check-false (git-valid-branch-name? R "feature x"))
  (check-false (git-valid-branch-name? R "a..b"))
  (check-false (git-valid-branch-name? R "-x"))
  (check-true (git-valid-tag-name? R "v1.0"))
  (check-false (git-valid-tag-name? R "v 1"))
  (git-create-branch R "feature x")
  (check= (git-current-branch R) "main")
  (check-false (in? "feature x" (map git-branch-name (git-branches R))))
  (git-create-branch R "feature")
  (check= (git-current-branch R) "feature")
  (git-switch-branch R "main")
  (check= (git-current-branch R) "main")
  (git-create-branch R "later" #f)
  (check= (git-current-branch R) "main")
  (with l (git-branches R)
    (check= (sort (map git-branch-name l) string<?)
            '("feature" "later" "main" "wt"))
    (check= (map git-branch-name (list-filter l git-branch-current?))
            '("main")))
  (check-false (git-switch-branch R "-f"))
  (check= (git-current-branch R) "main")
  (git-create-tag R "v1" "First version")
  (check= (map git-branch-name (git-tags R)) '("v1"))
  (check= (git-output R "cat-file" "-t" "v1") "tag\n")
  (git-create-tag R "light" "")
  (check= (git-output R "cat-file" "-t" "light") "commit\n")
  (git-run R "tag" "--delete" "light")
  (string-save "stash me\n" (file "base.txt"))
  (git-invalidate R)
  (git-stash R)
  (check= (git-file-state (file "base.txt")) 'unmodified)
  (with l (git-stashes R)
    (check= (length l) 1)
    (check= (car (car l)) "stash@{0}"))
  (git-stash-pop R)
  (check= (git-file-state (file "base.txt")) 'modified)
  (check= (git-stashes R) '())
  (git-run R "checkout" "--quiet" "--" "base.txt")
  (git-invalidate R))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renames and conflicts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-conflicts)
  (check-group "conflicts")
  (git-run R "mv" "base.txt" "renamed.txt")
  (git-invalidate R)
  (with e (list-find (git-status-entries R)
                     (lambda (e) (== (git-entry-kind e) 'renamed)))
    (check= (git-entry-path e) "renamed.txt")
    (check= (git-entry-orig e) "base.txt")
    (check-true (git-entry-staged? e)))
  (git-commit-staged R "rename")
  (string-save "one\n" (file "c.txt"))
  (git-run R "checkout" "--quiet" "-b" "c1")
  (git-stage (file "c.txt"))
  (git-commit-staged R "c1")
  (git-run R "checkout" "--quiet" "main")
  (git-invalidate R)
  (string-save "two\n" (file "c.txt"))
  (git-stage (file "c.txt"))
  (git-commit-staged R "c2")
  (check-false (git-merging? R))
  (check-false (git-merge-message R))
  (git-merge-branch R "c1")
  (check= (git-file-state (file "c.txt")) 'conflicted)
  (check-true (git-merging? R))
  (check-true (string-starts? (car (git-merge-message R)) "Merge branch"))
  (check-true (string-contains? (version-revision (file "c.txt") "OURS") "two"))
  (check-true (string-contains? (version-revision (file "c.txt") "THEIRS")
                                "one"))
  (check= (version-revision (file "c.txt") "BASE") "")
  (check= (git-commit-file (file "c.txt") "only this file")
          "A merge is in progress; commit the whole working tree")
  (check-true (string-contains? (git-menu-label R) "Git (main, 1 changed"))
  (check= (git-footer-text R) "Git main <#B7> 1 conflict")
  (git-run R "merge" "--abort")
  (git-invalidate R)
  (check-false (git-merging? R))
  (check= (git-status-entries R) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upstream branches, remotes and the footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-upstream)
  (check-group "upstream")
  ;; a bare repository with two clones: the clone a is one commit ahead of
  ;; its upstream and one behind it
  (system-mkdir (dir "remote"))
  (sh (path "remote") "git init -q --bare origin.git"
      " && git -C origin.git symbolic-ref HEAD refs/heads/main"
      " && git clone -q origin.git a 2> /dev/null"
      " && git clone -q origin.git b 2> /dev/null")
  (for (c '("a" "b"))
    (sh (path "remote/" c) "git config user.email " c "@example.com"
        " && git config user.name 'User " c "'"
        " && git checkout -q -b main 2> /dev/null; true"))
  (sh (path "remote/a") "echo a > a.txt && git add a.txt"
      " && git commit -q -m a && git push -q -u origin main 2> /dev/null")
  (sh (path "remote/b") "git pull -q origin main 2> /dev/null"
      " && echo b > b.txt && git add b.txt && git commit -q -m b"
      " && git push -q origin main 2> /dev/null")
  (sh (path "remote/a") "echo c > c.txt && git add c.txt"
      " && git commit -q -m c && git fetch -q 2> /dev/null")
  (with A (dir "remote/a")
    (git-trust A)
    (with st (git-status A)
      (check= (git-status-ref st 'head) "main")
      (check= (git-status-ref st 'upstream) "origin/main")
      (check= (git-status-ref st 'ahead) 1)
      (check= (git-status-ref st 'behind) 1))
    (check= (git-menu-label A) "Git (main, 1 ahead, 1 behind)")
    (check= (git-footer-text A)
            "Git main <#B7> saved <#B7> <#2191>1 <#B7> <#2193>1")
    (check= (git-remotes A) '("origin"))
    (check= (git-push-remote A) "origin")
    (check= (git-remote-url A "origin") (path "remote/origin.git"))
    (with b (car (git-branches A))
      (check= (git-branch-name b) "main")
      (check-true (git-branch-current? b))
      (check= (git-branch-upstream b) "origin/main")
      (check= (git-branch-track b) "[ahead 1, behind 1]"))
    (check= (sort (map git-branch-name (git-remote-branches A)) string<?)
            '("origin" "origin/main")))
  ;; the footer of a repository whose status is not known yet
  (with B (dir "remote/b")
    (git-trust B)
    (git-invalidate B)
    (check-false (git-footer-text B))
    (git-status B)
    (check= (git-footer-text B) "Git main <#B7> saved"))
  ;; remotes of the main repository
  (check= (git-remotes R) '())
  (check-false (git-push-remote R))
  (check= (git-menu-label R) "Git (main)")
  (git-add-remote R "upstream" "/nowhere/repo.git")
  (check= (git-remotes R) '("upstream"))
  (check= (git-remote-url R "upstream") "/nowhere/repo.git")
  (check= (git-push-remote R) "upstream")
  (git-add-remote R "-x" "y")
  (git-add-remote R "x" "--upload-pack=evil")
  (check= (git-remotes R) '("upstream"))
  (git-add-remote R "origin" "/nowhere/b.git")
  (check= (git-push-remote R) "origin")
  (git-run R "config" "branch.main.remote" "upstream")
  (check= (git-push-remote R) "upstream")
  (git-run R "config" "--unset" "branch.main.remote")
  (git-run R "remote" "remove" "upstream")
  (git-run R "remote" "remove" "origin")
  (git-invalidate R)
  (check= (git-remotes R) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured three way merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (check-merge o a b expected conflicts)
  `(with m (merge-versions ,o ,a ,b)
     (when ,expected
       (check-equal ,(object->string (list 'merge-versions o a b))
                    (lambda () m) ,expected))
     (check-equal ,(object->string (list 'merge-conflicts o a b))
                  (lambda () (merge-conflicts m)) ,conflicts)))

(define (test-merge)
  (check-group "merge")
  (check= (vector->list (version-match '(a b c d) '(b x d)))
          '(#f 0 #f 2))
  (check= (vector->list (version-match '() '(a))) '())
  (check= (vector->list (version-match '(a b) '())) '(#f #f))
  (check= (merge-versions-list 'document
                               '("A" "B") '("A" "B" "C") '("X" "A" "B"))
          '("X" "A" "B" "C"))
  (check-merge '(document "A.") '(document "A.") '(document "A!")
               '(document "A!") 0)
  (check-merge '(document "A.") '(document "A!") '(document "A!")
               '(document "A!") 0)
  (check-merge '(document "A." "B." "C.") '(document "A!" "B." "C.")
               '(document "A." "B." "C!") '(document "A!" "B." "C!") 0)
  (check-merge '(document "The quick brown fox jumps.")
               '(document "The slow brown fox jumps.")
               '(document "The quick brown fox leaps.")
               '(document "The slow brown fox leaps.") 0)
  (check-merge '(document "The quick fox.") '(document "The slow fox.")
               '(document "The lazy fox.") #f 1)
  (check-merge '(document "A." "B.") '(document "A." "X." "B.")
               '(document "A." "B." "Y.") '(document "A." "X." "B." "Y.") 0)
  (check-merge '(document (section "Intro") "Text.")
               '(document (section "Introduction") "Text.")
               '(document (section "Intro") "Text, more.")
               '(document (section "Introduction") "Text, more.") 0)
  (check-merge '(document (concat "Let " (math "x+y") " be."))
               '(document (concat "Let " (math "x+z") " be."))
               '(document (concat "Let " (math "x+y") " be given."))
               '(document (concat "Let " (math "x+z") " be given.")) 0)
  (check-merge '(document "A." "B." "C.") '(document "A." "C.")
               '(document "A." "B!" "C.") #f 1)
  (check-merge '(document "A." "B.") '(document "B.") '(document "B.")
               '(document "B.") 0)
  (check= (merge-conflicts '(document (version-both "a" "b") "c"
                                      (concat (version-old "d"))))
          2)
  (check= (merge-conflicts "plain") 0)
  (with doc '(document (TeXmacs "2.1") (style "generic") (body (document "A.")))
    (check= (document-body doc) '(document "A."))
    (check= (document-body (document-set-body doc '(document "B.")))
            '(document "B."))
    (check-false (document-body '(document "A.")))
    (check-false (document-body "A."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Merge driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Git runs this TeXmacs (headless) for merging documents
(define (test-driver)
  (check-group "driver")
  (make-repo (path "drv"))
  (string-save (tm "The quick brown fox jumps.") (dir "drv/paper.tm"))
  (sh (path "drv") "git add paper.tm && git commit -q -m base"
      " && git checkout -q -b theirs")
  (string-save (tm "The quick brown fox leaps.") (dir "drv/paper.tm"))
  (sh (path "drv") "git commit -q -a -m theirs && git checkout -q -b conflict")
  (string-save (tm "The fast brown fox leaps.") (dir "drv/paper.tm"))
  (sh (path "drv") "git commit -q -a -m conflict && git checkout -q main")
  (string-save (tm "The slow brown fox jumps.") (dir "drv/paper.tm"))
  (sh (path "drv") "git commit -q -a -m ours")
  (let* ((DR (dir "drv"))
         (DP (url-append DR "paper.tm")))
    (git-trust DR)
    (check-false (git-merge-driver-installed? DR))
    (check-true (string-contains? (git-merge-driver-command)
                                  "(git-merge-driver"))
    (git-install-merge-driver DR)
    (check-true (git-merge-driver-installed? DR))
    ;; installing twice does not repeat the attribute
    (git-install-merge-driver DR)
    (check= (string-load (url-append DR ".gitattributes"))
            "*.tm merge=texmacs\n")
    (git-stage (url-append DR ".gitattributes"))
    (git-commit-staged DR "attributes")
    (git-merge-branch DR "theirs")
    (check= (git-file-state DP) 'unmodified)
    (check-true (string-contains? (string-load DP) "The slow brown fox leaps."))
    (git-merge-branch DR "conflict")
    (check= (git-file-state DP) 'conflicted)
    (check-true (string-contains? (string-load DP) "<version-both|"))
    (git-run DR "merge" "--abort"))
  ;; the merge of files, as the driver does it
  (let ((o (dir "m-base.tm")) (a (dir "m-ours.tm")) (b (dir "m-theirs.tm")))
    (string-save (tm "A." "B.") o)
    (string-save (tm "A!" "B.") a)
    (string-save (tm "A." "B!") b)
    (check= (git-merge-files (url->system o) (url->system a) (url->system b))
            0)
    (check-true (string-contains? (string-load a) "A!"))
    (check-true (string-contains? (string-load a) "B!")))
  ;; the textual merge, when the structured one is not possible
  (let ((o (dir "m-base.txt")) (a (dir "m-ours.txt")) (b (dir "m-theirs.txt")))
    (string-save "line\n" o)
    (string-save "ours\n" a)
    (string-save "theirs\n" b)
    (check= (git-textual-merge (url->system o) (url->system a)
                               (url->system b)) 1)
    (check-true (string-contains? (string-load a) "<<<<<<<"))
    (string-save "line\nmore\n" a)
    (string-save "line\n" b)
    (check= (git-textual-merge (url->system o) (url->system a)
                               (url->system b)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Safety: names which could be taken for options, quoting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-unsaved)
  ;; a document which cannot be saved is never marked as saved (audit 2, A2)
  (check-group "unsaved")
  (with f (file "ro.tm")
    (string-save (tm "Original.") f)
    (git-stage f)
    (git-commit-staged R "ro")
    (load-buffer f)
    (buffer-set-body f (stree->tree '(document "Precious edits.")))
    (buffer-pretend-modified f)
    (check-true (buffer-modified? f))
    (shell "chmod 444 '" (url->system f) "'")
    (check-false (git-save-buffer f))
    (check-true (buffer-modified? f))
    (check= (git-commit-file* f "edits") '(#f . "The document could not be saved"))
    (check-true (buffer-modified? f))
    (git-mark-resolved-now f)
    (check-true (buffer-modified? f))
    (check= (git-file-state f) 'unmodified)
    (check-true (string-contains? (string-load f) "Original."))
    (shell "chmod 644 '" (url->system f) "'")
    (check-true (git-save-buffer f))
    (check-false (buffer-modified? f))
    (check-true (string-contains? (string-load f) "Precious edits."))
    (check= (car (git-commit-file* f "edits")) #t)
    (check= (git-file-state f) 'unmodified)
    (buffer-close f)))

(define (test-safety)
  (check-group "safety")
  (check= (version-commit F "") "Empty commit message")
  (check= (version-commit F "  ") "Empty commit message")
  (check-false (git-ok? (git-run-with-input R "" "commit" "--file=-")))
  (check= (git-run-with-input #f "" "status")
          (list -1 "" "not in a Git working tree"))
  (with owned (path "git-test-owned")
    (check= (git-log R 0 1 (string-append "--output=" owned)) '())
    (check= (git-show-file R (string-append "--output=" owned) "base.txt") "")
    (check= (git-numstat R (string-append "--output=" owned)) '())
    (check= (git-commit-message R (string-append "--output=" owned)) "")
    (check-false (git-signature R (string-append "--output=" owned)))
    (check-false (git-rev-parse R (string-append "--output=" owned)))
    (check-false (url-exists? (system->url owned))))
  ;; file names are never patterns
  (string-save "one\n" (file "n1.tm"))
  (string-save "bracket\n" (file "n[1].tm"))
  (git-stage (file "n1.tm"))
  (git-stage (file "n[1].tm"))
  (git-commit-staged R "brackets")
  (string-save "one changed\n" (file "n1.tm"))
  (string-save "bracket changed\n" (file "n[1].tm"))
  (git-discard-now (file "n[1].tm"))
  (check= (string-load (file "n1.tm")) "one changed\n")
  (check= (string-load (file "n[1].tm")) "bracket\n")
  (git-run R "checkout" "--" "n1.tm")
  ;; unstaging a rename unstages both names
  (git-run R "mv" "n1.tm" "n2.tm")
  (git-invalidate R)
  (git-unstage (file "n2.tm"))
  (check-false (git-has-staged? R))
  (git-run R "add" "--all")
  (git-commit-staged R "rename n1")
  ;; names with quotes
  (string-save "x\n" (file "q\"uote.tm"))
  (git-stage (file "q\"uote.tm"))
  (git-commit-staged R "quote")
  (check= (length (version-history (file "q\"uote.tm"))) 1)
  (string-save "x\n" (file "it's.tm"))
  (git-stage (file "it's.tm"))
  (check= (git-file-state (file "it's.tm")) 'added)
  (git-commit-staged R "apostrophe")
  (check= (git-file-state (file "it's.tm")) 'unmodified))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restoring, large files, new repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-restore)
  (check-group "restore")
  (string-save "restore me\n" (file "r.txt"))
  (git-stage (file "r.txt"))
  (git-commit-staged R "r one")
  (with first (git-rev-parse R "HEAD")
    (string-save "restore me, changed\n" (file "r.txt"))
    (git-stage (file "r.txt"))
    (git-commit-staged R "r two")
    (git-restore-revision-now (file "r.txt") first)
    (check= (string-load (file "r.txt")) "restore me\n")
    (git-invalidate R)
    (check= (git-file-state (file "r.txt")) 'modified)
    (git-run R "checkout" "HEAD" "--" "r.txt")
    ;; a revision where the file does not exist changes nothing
    (git-restore-revision-now (file "r.txt") (git-rev-parse R "HEAD~5"))
    (check= (string-load (file "r.txt")) "restore me, changed\n"))
  ;; the staged version is not touched, and renames are followed
  (with f (file "k.txt")
    (string-save "one\n" f)
    (git-stage f)
    (git-commit-staged R "k one")
    (with first (git-rev-parse R "HEAD")
      (string-save "two\n" f)
      (git-stage f)
      (git-restore-revision-now f first)
      (check= (string-load f) "one\n")
      (check= (git-show-file R "" "k.txt") "two\n")
      (git-run R "reset" "--quiet" "--hard" "HEAD")
      (git-run R "mv" "k.txt" "k2.txt")
      (git-commit-staged R "rename k")
      (git-restore-revision-now (file "k2.txt") first "k.txt")
      (check= (string-load (file "k2.txt")) "one\n")
      (check-false (url-exists? f))
      (git-run R "checkout" "--" "k2.txt")))
  (git-invalidate R)
  (set-preference "git large file size" "0")
  (check-true (git-large-file? (file "renamed.txt")))
  (set-preference "git large file size" "10")
  (check-false (git-large-file? (file "renamed.txt")))
  (check-false (git-large-file? R))
  (check-false (git-large-file? (file "no-such-file")))
  (with d (dir "new repo")
    (system-mkdir d)
    (check-false (git-root (url-append d "x.tm")))
    (git-init d)
    (check= (git-root (url-append d "x.tm")) d)
    (check-true (git-trusted? d))
    (check-true (string-contains? (string-load (url-append d ".gitignore"))
                                  "*~"))
    (check= (car (git-recent-repositories)) (url->system d))
    (check= (version-tool (url-append d "x.tm")) "git"))
  ;; an existing .gitignore is kept
  (with d (dir "new repo 2")
    (system-mkdir d)
    (string-save "mine\n" (url-append d ".gitignore"))
    (git-init d)
    (check= (string-load (url-append d ".gitignore")) "mine\n")
    (check= (sublist (git-recent-repositories) 0 2)
            (list (url->system d) (path "new repo"))))
  ;; untrusted repositories are not remembered
  (make-repo (path "untrusted"))
  (git-remember-repository (dir "untrusted"))
  (check= (car (git-recent-repositories)) (path "new repo 2"))
  (git-remember-repository (dir "new repo"))
  (check= (sublist (git-recent-repositories) 0 2)
          (list (path "new repo") (path "new repo 2"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signed commits and tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A fake GnuPG, which signs anything
(define fake-gpg
  (string-append
   "#!/bin/sh\n"
   "cat > /dev/null\n"
   "printf '\\n[GNUPG:] SIG_CREATED D 1 8 00 1234567890 ABCDEF\\n' >&2\n"
   "printf -- '-----BEGIN PGP SIGNATURE-----\\n\\nfake\\n"
   "-----END PGP SIGNATURE-----\\n'\n"))

(define (test-signing)
  (check-group "signing")
  (string-save fake-gpg (dir "fake-gpg"))
  (shell "chmod +x '" (path "fake-gpg") "'")
  (git-run R "config" "gpg.program" (path "fake-gpg"))
  (git-run R "config" "user.signingkey" "test")
  (set-preference "git sign" "off")
  (check-false (git-signing?))
  (git-toggle-signing)
  (check-true (git-signing?))
  (check= (git-commit-options) '("--gpg-sign"))
  (string-save "signed\n" (file "s.txt"))
  (git-stage (file "s.txt"))
  (git-commit-staged R "signed commit")
  (check= (git-commit-subject (git-commit-info R "HEAD")) "signed commit")
  (check-true (string-contains? (git-output R "cat-file" "commit" "HEAD")
                                "gpgsig"))
  (git-create-tag R "v-signed" "Signed tag")
  (check-true (string-contains? (or (git-output R "cat-file" "tag" "v-signed")
                                    "")
                                "BEGIN PGP SIGNATURE"))
  (git-toggle-signing)
  (check-false (git-signing?))
  (git-commit-staged R "not signed" :amend)
  (check= (git-commit-subject (git-commit-info R "HEAD")) "not signed")
  (check-false (string-contains? (git-output R "cat-file" "commit" "HEAD")
                                 "gpgsig"))
  (git-run R "config" "--unset" "gpg.program")
  (git-run R "config" "--unset" "user.signingkey"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blame, descriptions of changes, projects, snapshots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (commit-as d name msg)
  (sh d "git -c user.name='" name "' commit -q -a -m " msg))

(define (setup-project)
  (with d (path "proj")
    (make-repo d)
    (string-save (tm "<section|Intro>" "One." "Two." "Three.")
                 (dir "proj/paper.tm"))
    (sh d "git add paper.tm && git commit -q -m c1")
    (string-save (tm "<section|Intro>" "One." "Two, revised." "Three.")
                 (dir "proj/paper.tm"))
    (commit-as d "Second Author" "c2")
    (string-save (tm "<section|Intro>" "One." "Two, revised." "Three."
                     "<section|Results>" "Four.")
                 (dir "proj/paper.tm"))
    (commit-as d "Third Author" "c3")
    (string-save (tm "<section|Intro>" "One, not committed." "Two, revised."
                     "Three." "<section|Results>" "Four.")
                 (dir "proj/paper.tm"))
    (string-save (tm "<include|part.tm>" "<image|fig.png|1par|||>"
                     (string-append "<bibliography|bib|tm-plain|refs|"
                                    "<\\bib-list|0>\n  </bib-list>>"))
                 (dir "proj/main.tm"))
    (string-save (tm "A part.") (dir "proj/part.tm"))
    (string-save "png\n" (dir "proj/fig.png"))
    (string-save "@article{a, title={A}}\n" (dir "proj/refs.bib"))
    (sh d "git add main.tm && git commit -q -m main")
    (git-trust (dir "proj"))))

(define (test-project)
  (check-group "project")
  (setup-project)
  (let* ((P (dir "proj"))
         (PP (url-append P "paper.tm"))
         (PM (url-append P "main.tm")))
    (receive (attr oldest)
        (git-blame PP (document-body (tree->stree (tree-import PP "texmacs"))))
      (check= (map (lambda (c) (and c (git-commit-subject c))) attr)
              '("c1" #f "c2" "c1" "c3" "c3"))
      (check= (map (lambda (c) (and c (git-commit-author c))) attr)
              '("Test User" #f "Second Author" "Test User"
                "Third Author" "Third Author"))
      (check-false oldest))
    (check-true (string-contains? (tmfs-load (string-append
                                              "tmfs://blame/"
                                              (url->tmfs-string PP)))
                                  "Second Author"))
    (check= (git-describe-changes P (list (git-file-entry P PP)))
            '("Update paper.tm: Intro"))
    (check= (git-describe-changes P '()) '(""))
    (string-save "x\n" (url-append P "notes.txt"))
    (git-invalidate P)
    (with l (git-describe-changes P (git-status-entries P))
      ;; paper.tm, notes.txt and the untracked files of main.tm
      (check= (car l) "Update 5 files")
      (check= (cadr l) "")
      (check= (length l) 7)
      (check-true (in? "- Update paper.tm: Intro" l)))
    (system-remove (url-append P "notes.txt"))
    (git-invalidate P)
    (check= (sort (map (cut git-relative P <>) (git-document-dependencies PM))
                  string<?)
            '("fig.png" "main.tm" "part.tm" "refs.bib"))
    (check= (sort (git-project-files PM) string<?)
            '("fig.png" "main.tm" "part.tm" "refs.bib"))
    (check= (sort (git-project-untracked PM) string<?)
            '("fig.png" "part.tm" "refs.bib"))
    (git-add-project-files PM)
    (check= (git-project-untracked PM) '())
    (check= (git-file-state (url-append P "part.tm")) 'added)
    (git-run P "reset" "--quiet")
    (git-invalidate P)
    ;; snapshots
    (git-save-snapshot P "  ")
    (check-true (nnull? (git-status-entries P)))
    (git-save-snapshot P "snapshot one")
    (check= (git-status-entries P) '())
    (check= (git-commit-subject (car (git-snapshots P))) "snapshot one")
    (with rev (git-rev-parse P "HEAD")
      (string-save "<TeXmacs|2.1>\n\n<\\body>\n  Lost.\n</body>\n" PP)
      (git-save-snapshot P "snapshot two")
      (git-restore-snapshot-now P rev)
      (check-true (string-contains? (string-load PP) "One, not committed."))
      ;; restoring a snapshot is a new change: the history is kept
      (check= (git-commit-subject (car (git-snapshots P))) "snapshot two"))
    ;; restoring never loses work: it is saved in an automatic snapshot
    (string-save "unsaved work\n" (url-append P "work.txt"))
    (git-run P "add" "work.txt")
    (string-save "<TeXmacs|2.1>\n\n<\\body>\n  Edited.\n</body>\n" PP)
    (git-invalidate P)
    (git-restore-snapshot-now P "HEAD~1")
    (check-true (string-starts? (git-commit-subject (git-commit-info P "HEAD"))
                                "Automatic snapshot"))
    (check= (git-show-file P "HEAD" "work.txt") "unsaved work\n")
    (check-true (string-contains? (git-show-file P "HEAD" "paper.tm")
                                  "Edited."))
    (check-false (url-exists? (url-append P "work.txt")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Robustness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-robustness)
  (check-group "robustness")
  (git-run R "branch" "topic/with-slash")
  (check-true (git-rev-parse R "topic/with-slash^{commit}"))
  ;; the bars of the changes of a commit are scaled
  (with big (file "big.txt")
    (string-save (apply string-append (map (lambda (i) "line\n") (.. 0 500)))
                 big)
    (git-stage big)
    (git-commit-staged R "big change")
    (check= (git-numstat R "HEAD") '((500 0 "big.txt")))
    (check-false (string-contains? (tmfs-load (tmfs-url-commit
                                               R (git-rev-parse R "HEAD")))
                                   (make-string 41 #\+))))
  ;; the configuration of the untracked files is respected
  (git-run R "config" "status.showUntrackedFiles" "no")
  (string-save "x\n" (file "not-listed.txt"))
  (git-invalidate R)
  (check-false (list-find (git-status-entries R)
                          (lambda (e)
                            (== (git-entry-path e) "not-listed.txt"))))
  (check= (git-file-state (file "not-listed.txt")) 'untracked)
  (git-run R "config" "--unset" "status.showUntrackedFiles")
  (system-remove (file "not-listed.txt"))
  (git-invalidate R)
  ;; a detached head
  (git-run R "checkout" "--quiet" "--detach" "HEAD~1")
  (git-invalidate R)
  (check-false (git-current-branch R))
  (check= (git-status-ref (git-status R) 'head) "(detached)")
  (git-run R "checkout" "--quiet" "main")
  (git-invalidate R)
  (check= (git-current-branch R) "main")
  ;; another Git executable
  (set-preference "git executable" "no-such-git-executable")
  (check-false (git-available?))
  (check-false (git-ok? (git-run R "status")))
  (set-preference "git executable" "git")
  (check-true (git-available?))
  (check-true (git-ok? (git-run R "status"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (git-test-failures)
  (check-suite "git")
  (if (not (string-starts? (shell "git --version 2>/dev/null") "git version"))
      (display* "  git is not available, the suite is skipped\n")
      (begin
        (save-preferences)
        (setup)
        (run-group test-subroutines)
        (run-group test-trust)
        (run-group test-detection)
        (run-group test-pinning)
        (run-group test-states)
        (run-group test-history)
        (run-group test-pages)
        (run-group test-branches)
        (run-group test-conflicts)
        (run-group test-upstream)
        (run-group test-merge)
        (run-group test-driver)
        (run-group test-unsaved)
        (run-group test-safety)
        (run-group test-restore)
        (run-group test-signing)
        (run-group test-project)
        (run-group test-robustness)
        (restore-preferences)
        (version-tool-reset)
        (shell "rm -rf '" T "'")))
  (check-end))
