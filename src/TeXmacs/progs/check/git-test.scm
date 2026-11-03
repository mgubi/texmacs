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
;; suite changes are put back at the end, and the dialogue which explains a
;; failure is built but not opened, since a window saves its geometry in the
;; preferences.
;; The asynchronous commands (fetch, pull, push, clone) need the event loop,
;; and are checked by src/docs/tests/git-gui-test.scm instead.

(texmacs-module (check git-test)
  (:use (check check-lib)
        (version version-tmfs)
        (version version-git)
        (version git-widgets)
        (version version-merge)
        (version git-drivers)
        (version git-blame)
        (version git-project)
        (version version-menu)
        (version version-kbd)))

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

(define (tm-doc style init . paragraphs)
  ;; a .tm file with the style @style (a string, as in <style|...>), the
  ;; initial environment @init (a list of (key value)) and the paragraphs
  (string-append "<TeXmacs|2.1>\n\n<style|" style ">\n\n<\\body>\n"
                 (apply string-append
                        (map (lambda (p) (string-append "  " p "\n\n"))
                             paragraphs))
                 "</body>\n"
                 (if (null? init) ""
                     (string-append
                      "\n<\\initial>\n  <\\collection>\n"
                      (apply string-append
                             (map (lambda (x)
                                    (string-append "    <associate|" (car x)
                                                   "|" (cadr x) ">\n"))
                                  init))
                      "  </collection>\n</initial>\n"))))

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

;; The dialogues of the suite are kept here instead of being opened: for each
;; one its name and the text of its widget, which is really built
(define keep-dialogues? #f)
(define dialogues '())

(tm-define (dialogue-window menu-promise cmd name . opts)
  (:require keep-dialogues?)
  (set! dialogues (rcons dialogues (cons name (object->string
                                               (menu-promise noop))))))

(define (check-failure-dialogue explanation)
  ;; the failure of the last command was explained in one dialogue, where
  ;; there is a window (see git-show-failure in git-widgets.scm)
  (when (and (not (headless?)) (current-window))
    (check= (map car dialogues) '("Git"))
    (check-true (and (pair? dialogues)
                     (string-contains? (cdar dialogues) explanation))))
  (set! dialogues '()))

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
  (set! dialogues '())
  ;; a name which could be an option never reaches Git
  (check-false (git-switch-branch R "-f"))
  (check= dialogues '())
  (check-false (git-switch-branch R "no-such-branch"))
  (check-failure-dialogue "Git could not complete this operation.")
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
  (set! dialogues '())
  (git-merge-branch R "c1")
  (check-failure-dialogue "Some parts were changed on both sides.")
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
  ;; different insertions at the same place conflict
  (check-merge '(document "f is continuous on X.")
               '(document "f is uniformly continuous on X.")
               '(document "f is bounded continuous on X.") #f 1)
  (check= (merge-conflicts '(document (version-both "a" "b") "c"
                                      (concat (version-old "d"))))
          2)
  (check= (merge-conflicts "plain") 0)
  ;; the preamble: style packages as a set, the initial environment key by
  ;; key (#676)
  (let* ((doc (lambda (style init . body)
                `(document (TeXmacs "2.1") (style ,style)
                           (body (document ,@body))
                           ,@(if (null? init) '()
                                 `((initial (collection ,@init)))))))
         (fbs (lambda (v) `(associate "font-base-size" ,v)))
         (pm (lambda (v) `(associate "page-medium" ,v))))
    (check= (merge-documents (doc "generic" '() "A." "B.")
                             (doc "generic" '() "A!" "B.")
                             (doc '(tuple "generic" "french")
                                  (list (fbs "11")) "A." "B!"))
            (list (doc '(tuple "generic" "french") (list (fbs "11"))
                       "A!" "B!") 0 '()))
    (check= (merge-documents (doc '(tuple "article" "number-europe")
                                  (list (fbs "10") (pm "paper")) "A.")
                             (doc '(tuple "article" "number-europe" "french")
                                  (list (fbs "10")) "A.")
                             (doc '(tuple "book" "number-europe")
                                  (list (fbs "12") (pm "paper")) "A."))
            (list (doc '(tuple "book" "number-europe" "french")
                       (list (fbs "12")) "A.") 0 '()))
    (check= (merge-documents (doc '(tuple "article" "french") '() "A.")
                             (doc "article" '() "A.")
                             (doc '(tuple "article" "french" "dutch") '()
                                  "A."))
            (list (doc '(tuple "article" "dutch") '() "A.") 0 '()))
    ;; a change of the same key, or of the main style, on both sides keeps
    ;; ours and is a conflict
    (check= (merge-documents (doc "generic" (list (fbs "10")) "A.")
                             (doc "generic" (list (fbs "11")) "A.")
                             (doc "generic" (list (fbs "12")) "A."))
            (list (doc "generic" (list (fbs "11")) "A.") 1
                  '("font-base-size")))
    (check= (merge-documents (doc "generic" '() "A.")
                             (doc "article" '() "A.")
                             (doc "book" '() "A."))
            (list (doc "article" '() "A.") 1 '("style")))
    (check= (merge-documents (doc "generic" (list (fbs "10")) "A." "B.")
                             (doc "generic" (list (fbs "11")) "A!" "B.")
                             (doc "generic" (list (fbs "12")) "A." "B!"))
            (list (doc "generic" (list (fbs "11")) "A!" "B!") 1
                  '("font-base-size")))
    ;; a document without a style part keeps the style of the base; the
    ;; main style never comes from the packages
    (check= (merge-documents (doc "article" '() "A.")
                             '(document (TeXmacs "2.1") (body (document "A.")))
                             (doc '(tuple "book" "french") '() "A."))
            (list '(document (TeXmacs "2.1")
                             (style (tuple "book" "french"))
                             (body (document "A.")))
                  0 '()))
    ;; packages which are not names are kept
    (check= (merge-documents (doc '(tuple "g" (macro "x")) '() "A.")
                             (doc '(tuple "g" (macro "x")) '() "A.")
                             (doc '(tuple "g" (macro "x") "p") '() "A."))
            (list (doc '(tuple "g" (macro "x") "p") '() "A.") 0 '()))
    (check= (merge-documents (doc '(tuple "g" (macro "x")) '() "A.")
                             (doc '(tuple "g" (macro "x") "q") '() "A.")
                             (doc "g" '() "A."))
            (list (doc '(tuple "g" "q") '() "A.") 0 '()))
    ;; the message of Resolve conflict names the settings where ours was
    ;; kept
    (check= (git-resolve-message 0 '() 0)
            "Merged automatically; check the result, then mark as resolved")
    (check= (git-resolve-message 1 '("font-base-size") 0)
            '(concat "Merged automatically" "; "
                     (replace "settings changed on both sides, where your version was kept: %1"
                              (verbatim "font-base-size"))
                     "; " "check the result, then mark as resolved"))
    ;; during a rebase, Git's ours (kept) is the upstream
    (check= (git-resolve-message 3 '("style" "font-base-size") 1)
            '(concat (replace "Conflicts to resolve: %1; keep my version or their version of each"
                              "1")
                     "; "
                     (replace "settings changed on both sides, where the upstream version was kept: %1"
                              (verbatim "style, font-base-size"))
                     "; " "then mark as resolved"))
    (check= (git-resolve-message 2 '() 0)
            '(concat (replace "Conflicts to resolve: %1; keep my version or their version of each"
                              "2")
                     "; " "then mark as resolved")))
  (with doc '(document (TeXmacs "2.1") (style "generic") (body (document "A.")))
    (check= (document-body doc) '(document "A."))
    (check= (document-body (document-set-body doc '(document "B.")))
            '(document "B."))
    (check-false (document-body '(document "A.")))
    (check-false (document-body "A.")))
  ;; tables are merged cell by cell (audit 2, B7)
  (let* ((tab (lambda (c1 c2 . opt)
                `(document (tabular (tformat ,@opt (table (row (cell ,c1)
                                                               (cell ,c2))))))))
         (bold '(cwith "1" "1" "1" "1" "cell-background" "pastel red")))
    (check-merge (tab "1" "2") (tab "1x" "2") (tab "1" "2y") (tab "1x" "2y") 0)
    (check-merge (tab "1" "2") (tab "1x" "2") (tab "1y" "2") #f 1)
    (check-merge (tab "1" "2") (tab "1" "2" bold) (tab "1" "2y")
                 (tab "1" "2y" bold) 0)
    (check-merge (tab "1" "2" bold) (tab "1" "2")
                 (tab "1" "2" '(cwith "1" "1" "1" "1" "cell-background" "blue"))
                 #f 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The rules of the structured merge (#676, git-features.md, section 5a)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (numbered-paragraphs n suffix)
  (let loop ((k (- n 1)) (acc '()))
    (if (< k 0) acc
        (loop (- k 1)
              (cons (string-append "Paragraph number" (number->string k)
                                   " with word" (number->string k) suffix)
                    acc)))))

(define (test-merge-rules)
  (check-group "merge rules")
  ;; the two examples of the issue
  (check-merge '(document "A." "B." "C." "D.")
               '(document "A." "C." "D.")
               '(document "A." "B." "C2." "D.")
               '(document "A." "C2." "D.") 0)
  (check-merge '(document "A." "B." "C." "D.")
               '(document "A." "B." "C2." "D.")
               '(document "A." "C." "D.")
               '(document "A." "C2." "D.") 0)
  (check-merge '(document "A." (theorem (document "T.")) "C.")
               '(document "A." (theorem (document "T.")) "C2.")
               '(document "A." (theorem (document (concat (label "l") "T.")))
                          (todo "x") "C.")
               '(document "A." (theorem (document (concat (label "l") "T.")))
                          (todo "x") "C2.") 0)
  ;; touching changes inside a paragraph conflict
  (check-merge '(document "The quick fox jumps.")
               '(document "The slow fox jumps.")
               '(document "The quick red fox jumps.")
               '(document (concat "The " (version-both "slow " "quick red ")
                                  "fox jumps.")) 1)
  (check-merge '(document "The quick fox jumps.")
               '(document "The quick red fox jumps.")
               '(document "The slow fox jumps.")
               '(document (concat "The " (version-both "quick red " "slow ")
                                  "fox jumps.")) 1)
  (check-merge '(document "f is continuous on X.")
               '(document "f is uniformly continuous on X.")
               '(document "f is bounded on X.")
               '(document (concat "f is " (version-both "uniformly continuous "
                                                        "bounded ")
                                  "on X.")) 1)
  (check-merge '(document "We take n = 3 here.")
               '(document "We take n = 3 or 4 here.")
               '(document "We take n = 5 here.") #f 1)
  (check-merge '(document "The quick brown fox.")
               '(document "The slow brown fox.")
               '(document "The quick red fox.") #f 1)
  (check-merge '(document "The quick brown fox.")
               '(document "The brown fox.")
               '(document "The quick red fox.") #f 1)
  ;; but not with a word in between, nor next to a formula
  (check-merge '(document "The quick brown fox jumps.")
               '(document "The slow brown fox jumps.")
               '(document "The quick brown cat jumps.")
               '(document "The slow brown cat jumps.") 0)
  ;; unrelated paragraphs are never paired: stop words do not count
  (check-merge '(document "The map is continuous.")
               '(document "The map is uniformly continuous.")
               '(document "The set is open.") #f 2)
  (check-merge '(document "Intro." "The map is continuous." "Proof omitted." "End.")
               '(document "Intro." "The map is uniformly continuous." "Proof omitted here." "End.")
               '(document "Intro." "The set is open." "Proof omitted." "End.")
               #f 2)
  (check-merge '(document "The map f is continuous and bounded.")
               '(document "The map f is uniformly continuous and bounded.")
               '(document "We now prove that the set is open and closed.")
               ;; as before the rules: the differences of the two versions
               (compare-versions
                '(document "The map f is uniformly continuous and bounded.")
                '(document "We now prove that the set is open and closed."))
               1)
  (check-merge '(document "A." "The map f is continuous." "B.")
               '(document "A." "We now prove that the set is open." "B.")
               '(document "A." "The map f is continuous. See [1]." "B.")
               '(document "A." (version-both
                                (document "We now prove that the set is open.")
                                (document "The map f is continuous. See [1]."))
                          "B.") 1)
  ;; a paragraph edited on both sides is merged word by word
  (check-merge '(document "The first theorem holds for every space.")
               '(document "The second theorem holds for every space.")
               '(document "The first theorem holds for every compact space.")
               '(document "The second theorem holds for every compact space.") 0)
  (check-merge '(document (section "Short introduction"))
               '(document (section "Short overview introduction"))
               '(document (section "Short introduction here")) #f 0)
  ;; an edit next to an insertion of the other side, in all positions
  (let* ((p "The function is continuous on the interval.")
         (e "The function is uniformly continuous on the interval.")
         (x "Another remark about something else.")
         (q "Last words."))
    (check-merge `(document ,p ,q) `(document ,e ,q) `(document ,p ,x ,q)
                 `(document ,e ,x ,q) 0)
    (check-merge `(document ,p ,q) `(document ,p ,x ,q) `(document ,e ,q)
                 `(document ,e ,x ,q) 0)
    (check-merge `(document ,q ,p) `(document ,q ,e) `(document ,q ,x ,p)
                 `(document ,q ,x ,e) 0)
    (check-merge `(document ,q ,p) `(document ,q ,x ,p) `(document ,q ,e)
                 `(document ,q ,x ,e) 0)
    ;; not next to a rewrite, a removal or two new paragraphs
    (with r "We now consider another topic entirely."
      (check-merge `(document ,q ,p) `(document ,q ,x ,p) `(document ,q ,r)
                   `(document ,q (version-both (document ,x ,p) (document ,r)))
                   1))
    (check-merge `(document ,q ,p) `(document ,q ,x ,p) `(document ,q)
                 #f 1)
    (check-merge `(document ,q ,p) `(document ,q ,x ,p)
                 `(document ,q ,x "Other words here." ,e)
                 #f 2))
  ;; without real words, an edit is not certain
  (check-merge '(document "A." "B." "C.") '(document "A." "X." "B." "C.")
               '(document "A." "B2." "C.")
               '(document "A." (version-both (document "X." "B.")
                                             (document "B2."))
                          "C.") 1)
  (check-merge '(document "A." "B." "C.") '(document "A." "B." "B2." "C.")
               '(document "A." "D." "C.")
               '(document "A." (version-both (document "B." "B2.")
                                             (document "D."))
                          "C.") 1)
  ;; a paragraph removed and another one inserted are not a change of it
  (check-merge '(document "S." "Third para here." "Fourth para there." "E.")
               '(document "S." "Third para here." "Fourth para there, edited." "E.")
               '(document "S." "Fourth para there." "Why new." "E.")
               '(document "S." "Fourth para there, edited." "Why new." "E.") 0)
  (check-merge '(document "S." "The third para here." "Fourth para there." "E.")
               '(document "S." "The third para here, edited." "Fourth para there." "E.")
               '(document "S." "Fourth para there." "The new para here." "E.")
               #f 1)
  (check-merge '(document "Z." "B." "C." "Y.") '(document "Z." "C2." "Y.")
               '(document "Z." "B." "Y.") #f 1)
  (check-merge '(document "Z." "B." "C." "Y.") '(document "Z." "C2." "Y.")
               '(document "Z." "C." "Y.") #f 1)
  ;; repeated paragraphs: which one has changed is not certain
  (check-merge '(document "Z." "P." "P." "Y.") '(document "Z." "P." "Y.")
               '(document "Z." "P2." "P." "Y.") #f 1)
  (check-merge '(document "Z." "P." "Y.") '(document "Z." "P." "P." "Y.")
               '(document "Z." "P!" "Y.") #f 1)
  (check-merge '(document "A." "E." "E.") '(document "A." "S." "E.")
               '(document "S." "E." "E.") #f 2)
  ;; near-identical insertions come out once, as a conflict
  (let* ((i1 "We thank the referee for helpful coments.")
         (i2 "We thank the referee for helpful comments.")
         (c1 "Conclusion.")
         (c2 "Conclusion, revised."))
    (check-merge `(document "Intro." ,c1) `(document "Intro." ,i1 ,c1)
                 `(document "Intro." ,i2 ,c2)
                 `(document "Intro." (concat "We thank the referee for helpful "
                                             (version-both "coments." "comments."))
                            (version-both (document ,c1) (document ,c2))) 2)
    (check-merge `(document "Intro." ,c1) `(document "Intro." ,i2 ,c2)
                 `(document "Intro." ,i1 ,c1) #f 2)
    (check-merge `(document "Intro." ,c1) `(document "Intro, revised." ,i2 ,c1)
                 `(document "Intro." ,i1 ,c1) #f 2)
    ;; also in places which do not touch
    (check-merge '(document "The one paragraph about groups." "The other paragraph about rings.")
                 `(document ,i1 "The one paragraph about groups." "The other paragraph about rings!")
                 `(document "The one paragraph about groups, changed." ,i2 "The other paragraph about rings.")
                 #f 3))
  (check-merge '(document "A.") '(document "A." "Same text added.")
               '(document "A." "Same text added.")
               '(document "A." "Same text added.") 0)
  ;; replacements of several elements: a conflict stays local
  (check-merge '(document "Intro." "Alpha beta gamma." "Delta epsilon zeta."
                          "Eta theta iota." "End.")
               '(document "Intro." "Alpha beta gamma, A." "Delta epsilon zeta, A."
                          "Eta theta iota, A." "End.")
               '(document "Intro." "Totally new text." "Delta epsilon zeta."
                          "Eta theta iota." "End.")
               '(document "Intro." (version-both (document "Alpha beta gamma, A.")
                                                 (document "Totally new text."))
                          "Delta epsilon zeta, A." "Eta theta iota, A." "End.") 1)
  (let* ((n 200)
         (o (numbered-paragraphs n ""))
         (a (numbered-paragraphs n "a"))
         (b (cons "Something new." (cdr o)))
         (m (merge-versions (cons 'document o) (cons 'document a) (cons 'document b))))
    (check= (merge-conflicts m) 1)
    (check= (cddr m) (cdr a)))
  (let* ((n 200)
         (o (numbered-paragraphs n ""))
         (a (numbered-paragraphs n "a"))
         (b (numbered-paragraphs n " b"))
         (m (merge-versions (cons 'document o) (cons 'document a) (cons 'document b))))
    (check= (merge-conflicts m) n))
  (let* ((n 200)
         (o (numbered-paragraphs n ""))
         (a (map (lambda (k) (string-append "New " k)) o))
         (b (map (lambda (k) (string-append k " end")) o))
         (m (merge-versions (cons 'document o) (cons 'document a) (cons 'document b))))
    (check= (merge-conflicts m) 0)
    (check= (cdr m) (map (lambda (k) (string-append "New " k " end")) o)))
  ;; a swapped paragraph is not merged with its neighbour
  (check-merge '(document "Z." "Alpha beta gamma delta." "Epsilon zeta theta iota." "Y.")
               '(document "Z." "Epsilon zeta theta iota, A." "Alpha beta gamma delta, A." "Y.")
               '(document "Z." "Alpha beta gamma delta, B." "Epsilon zeta theta iota, B." "Y.")
               #f 3)
  ;; elements of another kind
  (check-merge '(document (section "The main results"))
               '(document (section "The main results here"))
               '(document "The main results") #f 1)
  ;; each side removes a different one of two touching paragraphs: nothing
  ;; of them would remain (audit of #715)
  (check-merge '(document "First paragraph alpha." "Second paragraph beta.")
               '(document "Second paragraph beta.")
               '(document "First paragraph alpha.") #f 2)
  (check-merge '(document "First paragraph alpha." "Second paragraph beta.")
               '(document "First paragraph alpha.")
               '(document "Second paragraph beta.") #f 2)
  (check-merge '(document "Aaaa one." "Bbbb two." "Cccc three." "Dddd four."
                          "Eeee five." "Ffff six.")
               '(document "Aaaa one." "Cccc three." "Eeee five.")
               '(document "Bbbb two." "Dddd four." "Ffff six.") #f 1)
  (check-merge '(document "I." (theorem (document "Aaaa one." "Bbbb two.")) "E.")
               '(document "I." (theorem (document "Aaaa one.")) "E.")
               '(document "I." (theorem (document "Bbbb two.")) "E.") #f 1)
  (let* ((p "The map f is continuous on the whole space X.")
         (q "The map f is continuous on all of the space X.")
         (x "We now discuss an unrelated topic instead of this."))
    ;; variants of each other
    (check-merge `(document "I." ,p ,q "E.") `(document "I." ,q "E.")
                 `(document "I." ,p "E.") #f 2)
    ;; a removal which relies on a paragraph that the other side rewrites
    (check-merge `(document "I." ,p ,q "E.") `(document "I." ,q "E.")
                 `(document "I." ,p ,x "E.") #f 3)
    (check-merge `(document "I." ,p ,q "E.") `(document "I." ,p ,x "E.")
                 `(document "I." ,q "E.") #f 3)
    ;; a new text closer to the removed paragraph than to its own
    (check-merge '(document "The compact operator acts on Hilbert spaces."
                            "The bounded functional vanishes on Banach subspaces.")
                 '(document "The compact operator acts on Hilbert spaces.")
                 '(document "The compact operator acts, the bounded functional vanishes on Banach subspaces."
                            "The bounded functional vanishes on Banach subspaces.")
                 #f 3))
  ;; decided: a removal next to an edit of the next paragraph, a removed
  ;; theorem with an edited proof and a proof added next to a rewritten
  ;; theorem merge; the markup does not link a proof to its theorem
  (check-merge '(document "Let f be a continuous function on X."
                          "Then f is bounded on X and attains its maximum.")
               '(document "Then f is bounded on X and attains its maximum.")
               '(document "Let f be a continuous function on X."
                          "Then f is bounded on X and attains its minimum.")
               '(document "Then f is bounded on X and attains its minimum.") 0)
  (check-merge '(document (theorem (document "Every continuous function is bounded."))
                          (proof (document "Use the covering argument.")))
               '(document (proof (document "Use the covering argument.")))
               '(document (theorem (document "Every continuous function is bounded."))
                          (proof (document "Use the finite covering argument.")))
               '(document (proof (document "Use the finite covering argument."))) 0)
  (check-merge '(document (theorem (document "Every continuous function is bounded on compact sets.")) "End.")
               '(document (theorem (document "Every continuous function is bounded on compact sets."))
                          (proof (document "By compactness.")) "End.")
               '(document (theorem (document "Not every continuous function is bounded on noncompact sets.")) "End.")
               '(document (theorem (document "Not every continuous function is bounded on noncompact sets."))
                          (proof (document "By compactness.")) "End.") 0)
  ;; the same text added by both sides, through hunks of several kinds
  (let* ((s "Note that the constant depends on the dimension.")
         (k "Alpha beta gamma words.")
         (l "Delta epsilon zeta words.")
         (ls (string-append s " " l)))
    (check-merge `(document ,k ,l) `(document "Alpha beta gamma words, edited." ,s ,l)
                 `(document ,k ,ls) #f 3)
    (check-merge `(document ,k ,l) `(document ,k ,ls)
                 `(document "Alpha beta gamma words, edited." ,s ,l) #f 3)
    (check-merge `(document ,k ,l) `(document ,k ,s ,l) `(document ,k ,ls) #f 2)
    (check-merge `(document ,k ,l)
                 `(document ,k "Delta epsilon zeta words, edited." ,s)
                 `(document ,(string-append k " " s) ,l) #f 3)
    (check-merge `(document ,k ,l) `(document ,s "Fresh words of ours here." ,l)
                 `(document ,k ,ls) #f 3))
  ;; neighbours rewritten into the same text by each side
  (check-merge '(document "I." "The main theorem holds for smooth functions."
                          "The main lemma holds for smooth functions." "E.")
               '(document "I." "The main statement holds for smooth functions."
                          "The main lemma holds for smooth functions." "E.")
               '(document "I." "The main theorem holds for smooth functions."
                          "The main statement holds for smooth functions." "E.")
               #f 2)
  ;; pairing by text only, and by more than one real word
  (check-merge '(document (theorem (document (concat (label "thm-main-result") "Short old."))) "End.")
               '(document (theorem (document (concat (label "thm-main-result") "Short old.")))
                          (proof (document "Obvious.")) "End.")
               '(document (theorem (document (concat (label "thm-main-result") "Unrelated claim."))) "End.")
               #f 2)
  (check-merge '(document (with "font-series" "bold" "Old note.") "End.")
               '(document (with "font-series" "bold" "Old note.") "Fresh remark." "End.")
               '(document (with "font-series" "bold" "Unrelated idea.") "End.")
               #f 2)
  (check-merge '(document (image "figure-one.png" "0.5par" "" "" "") "End.")
               '(document (image "figure-one.png" "0.5par" "" "" "") "Fresh remark." "End.")
               '(document (image "figure-one.png" "0.8par" "" "" "") "End.")
               #f 2)
  (check-merge '(document "See the theorem." "End words.")
               '(document "See the theorem." "It follows from compactness." "End words.")
               '(document "A different theorem about measure spaces has nothing to do with this."
                          "End words.")
               #f 1)
  ;; another old element is closer
  (let* ((o1 "The compact operator acts on Hilbert spaces.")
         (o2 "The bounded functional vanishes on Banach subspaces."))
    (check-merge `(document ,o1 ,o2) `(document ,o1 "Fresh remark here." ,o2)
                 `(document "The compact operator acts, the bounded functional vanishes on Banach subspaces."
                            ,o2) #f 3))
  ;; a word replaced by markup, and several spaces, still touch
  (check-merge '(document "The quick fox jumps.") '(document "The slow fox jumps.")
               '(document (concat "The quick " (em "cat") " jumps.")) #f 1)
  (check-merge '(document "The quick  fox jumps.") '(document "The slow  fox jumps.")
               '(document "The quick  red jumps.") #f 1)
  ;; the pairing also holds for texts below a document
  (check-merge '(theorem "The map f is continuous on all spaces")
               '(theorem "The map f is continuous on all sets")
               '(theorem "This set g was open on all spaces") #f 2)
  (check-merge '(theorem "The bounded map f is continuous on all spaces")
               '(theorem "The bounded map f is continuous on all sets")
               '(theorem "The nice bounded map f is continuous on all spaces")
               '(theorem "The nice bounded map f is continuous on all sets") 0)
  ;; inside a block with a body, the rules apply to the paragraphs
  (check-merge '(document (theorem (document "Para one alpha beta gamma." "Para two delta epsilon zeta."
                                             "Para three theta iota kappa." "Para four lambda sigma omega.")))
               '(document (theorem (document "Rewritten first totally unrelated." "Rewritten second totally unrelated."
                                             "Rewritten third totally different." "Para four lambda sigma omega.")))
               '(document (theorem (document "Para one alpha beta gamma." "Para two delta epsilon zeta."
                                             "Para three theta iota kappa." "Para four lambda sigma changed.")))
               '(document (theorem (document "Rewritten first totally unrelated." "Rewritten second totally unrelated."
                                             "Rewritten third totally different." "Para four lambda sigma changed."))) 0)
  ;; a paragraph moved next to a change of the other side conflicts, as
  ;; with Git
  (check-merge '(document "Alpha beta gamma." "Delta epsilon zeta.")
               '(document "Delta epsilon zeta." "Alpha beta gamma.")
               '(document "Alpha beta gamma." "Delta epsilon zeta, edited.")
               #f 2)
  ;; further away, moves are not recognized, as with Git
  (check-merge '(document "Keep this." "Alpha beta gamma." "Delta epsilon zeta.")
               '(document "Delta epsilon zeta." "Keep this." "Alpha beta gamma.")
               '(document "Keep this." "Alpha beta gamma.")
               '(document "Delta epsilon zeta." "Keep this." "Alpha beta gamma.") 0))

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
  (string-save (tm-doc "<tuple|generic|french>" '() "The fast brown fox leaps.")
               (dir "drv/paper.tm"))
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
    ;; their packages are kept by the driver (#676)
    ;; NOTE: git-resolve-conflict is not called here, since it opens the
    ;; review tool, which saves the geometry of the window in the preferences
    (check-true (string-contains? (string-load DP) "<style|<tuple|generic|french>>"))
    ;; while resolving, the menus speak of my and their versions (#676)
    (with-document DP
      (lambda ()
        (with l (menu-strings '(link version-retain-menu))
          (check-true (in? "Keep my version" l))
          (check-true (in? "Keep their version" l))
          (check-false (in? "Old version" l)))))
    (check= (git-mine-version DR) 0)
    (git-run DR "merge" "--abort")
    (git-invalidate DR)
    ;; during a rebase, the user's version is the new one (theirs for Git)
    (sh (path "drv") "git checkout -q conflict && git rebase main"
        " > /dev/null 2>&1")
    (git-invalidate DR)
    (check-true (git-rebasing? DR))
    (check= (git-mine-version DR) 1)
    (sh (path "drv") "git rebase --abort && git checkout -q main")
    (git-invalidate DR)
    (check-false (git-rebasing? DR))
    (check= (git-mine-version DR) 0)
    (with-document DP
      (lambda ()
        (with l (menu-strings '(link version-retain-menu))
          (check-true (in? "Old version" l))
          (check-false (in? "Keep my version" l)))))
    (buffer-close DP))
  ;; the merge of files, as the driver does it
  (let ((o (dir "m-base.tm")) (a (dir "m-ours.tm")) (b (dir "m-theirs.tm")))
    (string-save (tm "A." "B.") o)
    (string-save (tm "A!" "B.") a)
    (string-save (tm "A." "B!") b)
    (check= (git-merge-files (url->system o) (url->system a) (url->system b))
            0)
    (check-true (string-contains? (string-load a) "A!"))
    (check-true (string-contains? (string-load a) "B!"))
    ;; the preamble of their version is merged too (#676)
    (string-save (tm-doc "generic" '() "A." "B.") o)
    (string-save (tm-doc "generic" '(("page-medium" "paper")) "A!" "B.") a)
    (string-save (tm-doc "<tuple|generic|french>" '(("font-base-size" "11"))
                         "A." "B!") b)
    (check= (git-merge-files (url->system o) (url->system a) (url->system b))
            0)
    (with t (tree->stree (tree-import a "texmacs"))
      (check= (document-body t) '(document "A!" "B!"))
      (check= (list-find (cdr t) (cut tm-is? <> 'style))
              '(style (tuple "generic" "french")))
      (check= (list-find (cdr t) (cut tm-is? <> 'initial))
              '(initial (collection (associate "page-medium" "paper")
                                    (associate "font-base-size" "11")))))
    ;; a conflicting change of the preamble keeps ours and is reported
    (string-save (tm-doc "generic" '(("font-base-size" "10")) "A.") o)
    (string-save (tm-doc "generic" '(("font-base-size" "11")) "A.") a)
    (string-save (tm-doc "generic" '(("font-base-size" "12")) "A.") b)
    (check= (git-merge-files (url->system o) (url->system a) (url->system b))
            1)
    (check-true (string-contains? (string-load a) "<associate|font-base-size|11>")))
  ;; outside a trusted folder, nothing is installed, and this is said (#676)
  (make-repo (path "drv-untrusted"))
  (with DU (dir "drv-untrusted")
    (check-false (git-trusted? DU))
    (with r (git-install-merge-driver-now DU)
      (check-false (car r))
      (check-true (string-contains? (cdr r) "not trusted")))
    (check-false (url-exists? (url-append DU ".gitattributes")))
    (check= (sh (path "drv-untrusted")
                "git config --get merge.texmacs.driver || echo none")
            "none\n"))
  (with r (git-install-merge-driver-now (dir "drv"))
    (check-true (car r))
    (check= (cdr r) "TeXmacs documents will be merged structurally"))
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

;; The strings in the expansion of the menu @m
(define (menu-strings m)
  (let loop ((x (menu-expand m)))
    (cond ((string? x) (list x))
          ((pair? x) (append-map loop x))
          (else '()))))

(define (with-document u thunk)
  ;; Run @thunk with the document @u as the current buffer
  (let ((old (current-buffer)))
    (load-buffer u)
    (switch-to-buffer* u)
    (check-run thunk)
    (when (and old (buffer-exists? old)) (switch-to-buffer* old))))

(define (kbd-call name)
  ;; Call the function @name of version-kbd.scm (a shortcut)
  (eval (list name) (resolve-module '(version version-kbd))))

(define (test-names)
  ;; A repository and a document with accented names (audit 2, B10, B11)
  (check-group "names")
  (let* ((dname (cork->utf8 "d<#E9>p<#F4>t"))
         (fname (cork->utf8 "r<#E9>sum<#E9>.tm"))
         (d (dir dname))
         (f (url-append d fname)))
    (make-repo (path dname))
    (string-save (tm "Accents.") f)
    (git-trust d)
    (check= (git-root f) d)
    (check= (git-file-state f) 'untracked)
    (git-stage f)
    (git-commit-staged d (cork->utf8 "Caf<#E9>"))
    (check= (git-file-state f) 'unmodified)
    (check= (git-commit-subject (git-commit-info d "HEAD"))
            (cork->utf8 "Caf<#E9>"))
    (check= (map cadddr (version-history f)) (list (cork->utf8 "Caf<#E9>")))
    (check-true (string-contains? (version-revision f "HEAD") "Accents."))
    (string-save "x\n" (url-append d "new.txt"))
    (git-invalidate d)
    (check-true (string-contains? (tmfs-load (tmfs-url-git d "log"))
                                  (utf8->cork (cork->utf8 "Caf<#E9>"))))
    ;; links are followed after a conversion from cork
    (with t (tmfs-url-commit d (git-rev-parse d "HEAD"))
      (check= (cork->utf8 (git-link-target t)) t)
      (check-true (string-contains? (tmfs-load (tmfs-url-git d "log"))
                                    (git-link-target t))))
    ;; NOTE: the history of git is in utf8, the texts shown in cork
    (check= (version-history-text f (cork->utf8 "Caf<#E9>"))
            (utf8->cork (cork->utf8 "Caf<#E9>")))
    (check= (version-history-text f (cork->utf8 "<#E9><#E9><#E9>") 5)
            (string-append (utf8->cork (cork->utf8 "<#E9>")) "..."))
    (check= (git-utf8-prefix (cork->utf8 "a<#E9>") 2) "a")
    (check= (git-utf8-prefix (cork->utf8 "a<#E9>") 3) (cork->utf8 "a<#E9>"))
    (check= (git-utf8-shorten "abcdef" 5) "ab...")
    (check= (git-utf8-shorten (cork->utf8 "<#E9><#E9><#E9>") 6)
            (cork->utf8 "<#E9><#E9><#E9>"))
    (check= (git-utf8-shorten (cork->utf8 "<#E9><#E9><#E9>") 5)
            (string-append (cork->utf8 "<#E9>") "..."))
    (check= (git-short-message "short") "short")))

(define (test-shortcuts)
  ;; The shortcuts make the checks of the menus (audit 2, B9)
  (check-group "shortcuts")
  (make-repo (path "kbd"))
  (with f (dir "kbd/k.tm")
    (string-save (tm "Kept.") f)
    (sh (path "kbd") "git add k.tm && git commit -q -m k")
    (with-document f
      (lambda ()
        (check-false (kbd-call 'shortcut-root))
        (kbd-call 'shortcut-compare)
        (check= (tree->stree (buffer-get-body f)) '(document "Kept."))
        (git-trust (dir "kbd"))
        (check= (kbd-call 'shortcut-root) (dir "kbd"))))
    (buffer-close f))
  (with-document (file "renamed.txt")
    (lambda ()
      (check-false (kbd-call 'shortcut-compare))))
  (buffer-close (file "renamed.txt")))

(define (test-new-repositories)
  ;; A repository created outside TeXmacs is noticed (audit 2, B12)
  (check-group "new repositories")
  (system-mkdir (dir "later"))
  (with f (dir "later/x.tm")
    (string-save (tm "x") f)
    (check-false (versioning-directory f))
    (make-repo (path "later"))
    (eval '(set! versioning-directory-delay 0)
          (resolve-module '(kernel texmacs tm-modes)))
    (check= (versioning-directory f) (dir "later"))
    (eval '(set! versioning-directory-delay 10000)
          (resolve-module '(kernel texmacs tm-modes)))))

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
  ;; the menus do not offer Git, but explain how to find it (audit 2, B8)
  (with-document F
    (lambda ()
      (with l (menu-strings '(link version-menu))
        (check-true (in? "Git was not found: locate it in the preferences" l))
        (check-false (in? "Commit" l))
        (check-false (in? "Git panel" l)))))
  (set-preference "git executable" "git")
  (check-true (git-available?))
  (check-true (git-ok? (git-run R "status"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-groups)
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
  (run-group test-merge-rules)
  (run-group test-driver)
  (run-group test-unsaved)
  (run-group test-safety)
  (run-group test-restore)
  (run-group test-signing)
  (run-group test-project)
  (run-group test-names)
  (run-group test-shortcuts)
  (run-group test-new-repositories)
  (run-group test-robustness))

(tm-define (git-test-failures)
  (check-suite "git")
  (cond
    ((or (os-mingw?) (os-win32?))
     ;; the repositories of the suite are made by command lines with the
     ;; syntax of /bin/sh, while eval-system runs no shell on Windows (the
     ;; Git support itself on Windows: the group git of the suite version)
     (display* "  the suite needs a POSIX shell, it is skipped on Windows\n"))
    ((not (string-starts? (shell "git --version 2>/dev/null") "git version"))
     (display* "  git is not available, the suite is skipped\n"))
    (else
      (save-preferences)
      (set! keep-dialogues? #t)
      ;; an error outside the groups also comes back here
      (run-group run-groups)
      (set! keep-dialogues? #f)
      (restore-preferences)
      (version-tool-reset)
      (shell "rm -rf '" T "'")))
  (check-end))
