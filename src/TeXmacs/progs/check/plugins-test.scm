
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : plugins-test.scm
;; DESCRIPTION : tests of plugins, sessions and the communication with them
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite checks the configuration of plugins (tm-plugins.scm), the
;; serialization of what is sent to a plugin (plugin-cmd.scm), the parsing
;; of what a plugin answers (texmacs_input in Data/Convert/Generic/input.cpp)
;; and live sessions with the shell and python plugins.
;;
;; How a plugin is read without a window. Sessions in a document are fed
;; asynchronously: the event loop reads the pipes and calls back
;; connection-notify and connection-notify-status (plugin-eval.scm), and the
;; scheme session runs its commands with delayed. None of this happens
;; without the event loop. Three entry points work synchronously:
;;
;;   - connection-eval (and connection-cmd) writes to the plugin and reads
;;     its pipes itself until the outermost DATA_BEGIN/DATA_END block of the
;;     answer is closed; it returns the "output" channel of the answer. The
;;     first call on a session starts the plugin and reads its banner.
;;   - connection-interrupt and connection-stop read what is pending on the
;;     pipes and pass it, channel by channel, to connection-notify and to the
;;     handlers of the plugin. A plugin which ignores SIGINT can thus be
;;     read without writing to it: interrupting it is a poll.
;;   - the serializers, format-command and the evaluator of scheme sessions
;;     (scheme-eval) are plain functions.
;;
;; connection-eval has no timeout: it waits until the answer is complete
;; (see the FIXME in test-connection-status). The suite only sends commands
;; whose answers are complete and quick, and the parser is tested with a
;; plugin which echoes its input (cat), so that the answer is exactly the
;; stream the test writes. Every other wait is a poll with a time limit, and
;; every process started is stopped at the end of its group.

(texmacs-module (check plugins-test)
  (:use (check check-lib)
        (utils plugins plugin-eval)
        (utils plugins plugin-cmd)
        (dynamic session-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The control characters of the protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ch n) (string (integer->char n)))
(define ABORT (ch 1))
(define BEGIN (ch 2))
(define END (ch 5))
(define COMMAND (ch 16))
(define ESCAPE (ch 27))

(define protocol
  ;; all the protocol characters
  (string-append "a" ABORT "b" BEGIN "c" END "d" COMMAND "e" ESCAPE "f"))

(define (blk head . l)
  ;; the block DATA_BEGIN head l... DATA_END, head being "format:" or
  ;; "channel#"
  (string-append BEGIN head (apply string-append l) END))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The plugins are initialized first, so that the plugin cache, which is
;; saved once they all are, does not list the test plugins below.
(lazy-plugin-force)

;; With a valid plugin cache, plugin-configure does not evaluate :require
;; and takes the availability of a plugin from the cache, where the test
;; plugins are not: they are configured as if the cache were being rebuilt.
(define saved-reconfigure-flag reconfigure-flag?)
(set! reconfigure-flag? #t)

(define (raw-serialize lan t)
  ;; the test plugins receive exactly the string which is evaluated
  (if (string? t) t (tree->string t)))

;; echoes its input: the answer is the stream which the test writes
(plugin-configure tmtestecho
  (:require #t)
  (:launch "cat")
  (:serializer ,raw-serialize)
  (:commander ,(lambda (s) (blk "verbatim:" "cmd=" s)))
  (:tab-completion #t)
  (:test-input-done #t)
  (:session "Echo test")
  (:scripts "Echo test"))

;; echoes its input and ignores SIGINT: connection-interrupt reads it
(plugin-configure tmtestpoll
  (:launch "sh -c \"trap '' INT; printf '\\002verbatim:ready\\005'; exec cat\"")
  (:serializer ,raw-serialize)
  (:handler "mychan" plugins-test-handler))

;; echoes its input on its standard output and its standard error
(plugin-configure tmtesterr
  (:launch "sh -c \"trap '' INT; printf '\\002verbatim:ready\\005'; exec tee /dev/stderr\"")
  (:serializer ,raw-serialize))

;; two variants, the session name chooses the variant
(plugin-configure tmtestvariants
  (:launch "v1" "cat")
  (:launch "v2" "perl -pe 'BEGIN{$|=1} s/A/B/g'")
  (:serializer ,raw-serialize)
  (:session "Variants test"))

;; a program which does not exist
(plugin-configure tmtestnone
  (:launch "tm-plugins-test-no-such-program")
  (:session "None test"))

;; a plugin whose requirement fails: the options after :require are ignored
(plugin-configure tmtestabsent
  (:require (url-exists-in-path? "tm-plugins-test-no-such-program"))
  (:launch "cat")
  (:session "Absent test"))

;; a plugin evaluating each command with a command line
(plugin-configure tmtestcmdline
  (:cmdline ,(lambda (name chat cmd) (string-append "echo " cmd))
            ,(lambda (name chat res) (tm->tree (string-append "res=" res))))
  (:session "Cmdline test"))

(set! reconfigure-flag? saved-reconfigure-flag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the plugins notify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; connection-notify is called by the reads of connection-interrupt and
;; connection-stop; for the test plugins and the sessions of the suite, what
;; is notified is recorded, newest first, instead of going to a document.
(define notified '())

(define (test-session? lan ses)
  (or (string-starts? lan "tmtest")
      (string-starts? ses "plugins-test")))

(tm-define (connection-notify lan ses ch t)
  (:require (test-session? lan ses))
  (set! notified (cons (list ch (tree->stree t)) notified)))

(tm-define (plugins-test-handler t)
  (set! notified (cons (list "handler" (tree->stree t)) notified)))

(define plugins-test-notes '())

(tm-define (plugins-test-note! x)
  ;; called by the scheme commands which the test plugins send
  (set! plugins-test-notes (cons x plugins-test-notes)))

(define (notified-on ch)
  ;; what was notified on the channel @ch, oldest first
  (reverse (map cadr (list-filter notified (lambda (x) (== (car x) ch))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (skip what why)
  ;; a group which cannot run is not counted
  (display* "  SKIP [" what "] " why "\n")
  (force-output))

(define (pause secs)
  (system (string-append "sleep " secs)))

(define (wait-until ok? msecs)
  ;; poll @ok? every 0.1 s during at most @msecs milliseconds
  (let ((start (texmacs-time)))
    (let loop ()
      (cond ((ok?) #t)
            ((> (- (texmacs-time) start) msecs) #f)
            (else (pause "0.1") (loop))))))

(define (stree-text t)
  ;; the text of a tree in scheme format, to look for a word in it
  (object->string t))

(define (contains? t what)
  (string-contains? (stree-text t) what))

(define (pid-alive? pid)
  (== (system (string-append "kill -0 " pid " 2>/dev/null")) 0))

(define started '())

(define (eval* lan ses in)
  ;; evaluate @in in the session, as a tree in scheme format
  (when (nin? (list lan ses) started)
    (set! started (cons (list lan ses) started)))
  (tree->stree (connection-eval lan ses in)))

(define (start* lan ses)
  (when (nin? (list lan ses) started)
    (set! started (cons (list lan ses) started)))
  (connection-start lan ses))

(define (stop-all)
  ;; stop every session which the suite started
  (for (x started)
    (when (!= (connection-status (car x) (cadr x)) 0)
      (connection-stop (car x) (cadr x))))
  (set! started '()))

(define (run-group thunk)
  ;; run a group; an error is a failure, and the processes are stopped
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))
    (stop-all)))

(define (shell-available?)
  (and (url-exists-in-path? "sh")
       (url-exists-in-path? "tm_shell")
       (connection-defined? "shell")))

(define (python-available?)
  (and (connection-defined? "python")
       (!= (python-command) "")
       (url-exists-in-path? (python-command))
       (url-exists? "$TEXMACS_PATH/plugins/tmpy/session/tm_python.py")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installed plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The plugins of $TEXMACS_PATH/plugins are listed, initialized and
;; declared; the shell plugin is available whenever sh is, and its
;; connection is a pipe to tm_shell with one variant.
(define (test-installed)
  (check-group "installed plugins")
  ;; the plugins are listed as symbols
  (check-true (list? (plugin-list)))
  (check-true (in? 'shell (plugin-list)))
  (check-true (in? 'python (plugin-list)))
  (check-true (in? 'tmpy (plugin-list)))
  (check-false (in? 'tmtestecho (plugin-list)))
  (check-true (in? "shell" (declared-plugins)))
  (check-true (in? "python" (declared-plugins)))
  (check-false (in? "tm-plugins-test-nothing" (declared-plugins)))
  (check= (url-exists-in-path? "sh") #t)
  (check= (url-exists-in-path? "tm-plugins-test-no-such-program") #f)
  (check= (supports-shell?) (url-exists-in-path? "sh"))
  (check-true (connection-defined? "shell"))
  (check-false (connection-defined? "tm-plugins-test-nothing"))
  (check-true (in? "shell" (connection-list)))
  (check= (connection-variants "shell") '("default"))
  (check= (connection-info "shell" "default") '(tuple "pipe" "tm_shell"))
  ;; the part of a session name after a colon is not part of the variant
  (check= (connection-info "shell" "default:other") '(tuple "pipe" "tm_shell"))
  ;; a session which is not a variant uses the first variant
  (check= (connection-info "shell" "my-session") '(tuple "pipe" "tm_shell"))
  (check= (connection-info "tm-plugins-test-nothing" "default") #f)
  (check-false (connection-cmdline? "shell"))
  (check-false (connection-request? "shell"))
  (check-true (in? "shell" (session-list)))
  (check-true (session-defined? "shell"))
  (check= (session-name "shell") "Shell")
  (check= (plugin->name "shell") "Shell")
  (check= (plugin->name 'shell) "Shell")
  (check= (name->plugin "Shell") "shell")
  ;; an unknown name is only decapitalized
  (check= (name->plugin "Nothing") "nothing")
  (check= (session-name "tm-plugins-test-nothing") "Tm-plugins-test-nothing")
  (check-false (session-defined? "tm-plugins-test-nothing"))
  (check= (connection-get-handlers "shell") '(tuple))
  (check= (remote-connection-defined? "shell") #f)
  (if (python-available?)
      (begin
        (check-true (supports-python?))
        (check-true (in? "python" (connection-list)))
        (check-true (in? "python" (scripts-list)))
        (check= (scripts-name "python") "Python")
        (check= (session-name "python") "Python")
        (check-true (plugin-supports-completions? "python"))
        (check= (car (connection-info "python" "default")) 'tuple)
        (check-true (contains? (connection-info "python" "default")
                               "tm_python.py")))
      (skip "installed plugins" "python3 or tmpy is missing")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugin-configure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The options of plugin-configure fill the tables which the connections and
;; the menus read: the launchers and their variants, the session and script
;; names, the serializer, the commander, the completion and input-done
;; flags, the handlers and the command line evaluators; a failed :require
;; makes the plugin unsupported and skips the options which follow it.
(define (test-configure)
  (check-group "plugin-configure")
  (check-true (supports-tmtestecho?))
  (check-true (in? "tmtestecho" (declared-plugins)))
  (check-true (connection-defined? "tmtestecho"))
  (check= (connection-info "tmtestecho" "default") '(tuple "pipe" "cat"))
  (check= (connection-variants "tmtestecho") '("default"))
  (check-true (in? "tmtestecho" (session-list)))
  (check= (session-name "tmtestecho") "Echo test")
  (check-true (in? "tmtestecho" (scripts-list)))
  (check= (scripts-name "tmtestecho") "Echo test")
  (check= (plugin->name "tmtestecho") "Echo test")
  (check= (name->plugin "Echo test") "tmtestecho")
  (check-true (plugin-supports-completions? "tmtestecho"))
  (check-true (plugin-supports-input-done? "tmtestecho"))
  (check-false (plugin-supports-completions? "tmtestpoll"))
  (check-false (plugin-supports-input-done? "tmtestpoll"))
  (check-false (plugin-has-preferences? "tmtestecho"))
  (check-false (in? "tmtestecho" (plugins-with-preferences)))
  ;; a plugin without :session or :scripts is not listed as such
  (check-false (in? "tmtestpoll" (session-list)))
  (check-false (in? "tmtestpoll" (scripts-list)))
  (check-false (in? "tmtestvariants" (scripts-list)))
  ;; the launch command is kept as written; it is split into words later
  (check= (connection-info "tmtestvariants" "v2")
          '(tuple "pipe" "perl -pe 'BEGIN{$|=1} s/A/B/g'"))
  (check= (connection-get-handlers "tmtestpoll")
          '(tuple (tuple "mychan" "plugins-test-handler")))
  (check= (connection-get-handlers "tmtestecho") '(tuple))

  (check-group "variants")
  (check= (connection-variants "tmtestvariants") '("v1" "v2"))
  (check= (connection-info "tmtestvariants" "v1") '(tuple "pipe" "cat"))
  (check= (connection-info "tmtestvariants" "v2")
          '(tuple "pipe" "perl -pe 'BEGIN{$|=1} s/A/B/g'"))
  (check= (connection-info "tmtestvariants" "v2:my-session")
          '(tuple "pipe" "perl -pe 'BEGIN{$|=1} s/A/B/g'"))
  (check= (connection-info "tmtestvariants" "default") '(tuple "pipe" "cat"))
  (check= (connection-info "tmtestvariants" "other") '(tuple "pipe" "cat"))

  (check-group ":require")
  (check= (supports-tmtestabsent?) #f)
  (check-false (connection-defined? "tmtestabsent"))
  (check-false (session-defined? "tmtestabsent"))
  (check-true (in? "tmtestabsent" (declared-plugins)))
  (check= (supports-tmtestnone?) #t)
  (check-true (connection-defined? "tmtestnone"))

  (check-group ":cmdline")
  ;; the command line plugins build one command per input, and turn its
  ;; output into a tree; neither needs a process
  (check-true (connection-cmdline? "tmtestcmdline"))
  (check-false (connection-request? "tmtestcmdline"))
  (check= (car (connection-info "tmtestcmdline" "default")) 'tuple)
  (check= (cadr (connection-info "tmtestcmdline" "default")) "cmdline")
  (check= (connection-cmdline "tmtestcmdline" "default" "x y") "echo x y")
  (check= (tree->stree (connection-result "tmtestcmdline" "default" "out"))
          "res=out")
  ;; the other plugins have no command line
  (check= (connection-cmdline "tmtestecho" "default" "x") "")
  (check= (tree->stree (connection-result "tmtestecho" "default" "x")) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What is sent to a plugin: by default one line of verbatim text, in which
;; newlines and tabs become spaces; the generic serializer sends a verbatim
;; block. Both convert the tree to UTF-8 with texmacs->code first, so that
;; the characters below 32, which are accents in the Cork encoding of
;; TeXmacs strings, become UTF-8 accents, and the protocol characters
;; never appear in what is sent. A plugin can have its own serializer, and
;; its own format for commands.
(define (test-serialize)
  (check-group "escapes")
  (check= (escape-verbatim "a\nb\tc") "a b c")
  (check= (escape-verbatim (string-append "a" BEGIN "b" END "c" ESCAPE "d"))
          "abcd")
  (check= (escape-verbatim "") "")
  (check= (escape-generic (string-append "a" BEGIN "b" END "c" ESCAPE "d"))
          (string-append "a" ESCAPE BEGIN "b" ESCAPE END "c" ESCAPE ESCAPE "d"))
  (check= (escape-generic "a\nb") "a\nb")

  (check-group "serializers")
  (check= (verbatim-serialize "tmtestnone" "1+1") "1+1\n")
  (check= (verbatim-serialize "tmtestnone" (stree->tree "1+1")) "1+1\n")
  ;; a document of one line is that line, a longer one is joined
  (check= (verbatim-serialize "tmtestnone" '(document "x")) "x\n")
  (check= (verbatim-serialize "tmtestnone" '(document "a" "b")) "a b\n")
  (check= (verbatim-serialize "tmtestnone" "<less>a<#E9>")
          (string-append "<a" (ch 195) (ch 169) "\n"))
  (check= (list-filter (char-codes (verbatim-serialize "tmtestnone" protocol))
                       (lambda (c) (< c 32)))
          '(10))
  (check= (generic-serialize "tmtestnone" "x")
          (blk "verbatim:" "x"))
  (check= (generic-serialize "tmtestnone" "<less>a<#E9>")
          (blk "verbatim:" "<a" (ch 195) (ch 169)))
  (check= (list-filter (char-codes (generic-serialize "tmtestnone" protocol))
                       (lambda (c) (< c 32)))
          '(2 5))
  (check= (generic-serialize "tmtestnone" '(document "a" "b"))
          (blk "verbatim:" "a\nb"))
  (check= (pre-serialize "tmtestnone" '(document "x")) "x")
  (check= (pre-serialize "tmtestnone" '(document "a" "b")) '(document "a" "b"))
  ;; the default serializer, and the one of a plugin
  (check= (plugin-serialize "tmtestnone" "ls -l") "ls -l\n")
  (check= (plugin-serialize "shell" "ls -l") "ls -l\n")
  (check= (plugin-serialize "tmtestecho" "ls -l") "ls -l")
  (if (python-available?)
      (begin
        (check= (plugin-serialize "python" "1+1") "1+1\n<EOF>\n")
        (check= (plugin-serialize "python" '(document "x = 1" "x"))
                "x = 1\nx\n<EOF>\n"))
      (skip "serializers" "python3 or tmpy is missing"))

  (check-group "commands")
  (check= (format-command "tmtestnone" "(complete \"pr\" 2)")
          (string-append COMMAND "(complete \"pr\" 2)\n"))
  (check= (format-command "tmtestecho" "x") (blk "verbatim:" "cmd=x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing of the answers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (echo s)
  ;; the output channel of the answer @s, as parsed by TeXmacs
  (eval* "tmtestecho" "plugins-test-parse" s))

(define (char-codes s)
  (map char->integer (string->list s)))

;; The answer of a plugin is a stream of blocks DATA_BEGIN format: ...
;; DATA_END, possibly nested, or DATA_BEGIN channel# ... DATA_END, which
;; send their content to another channel (prompt, input, error...). The
;; echo plugin returns the stream which is written to it, so that the
;; parser is checked on exact input. connection-eval returns the output
;; channel, as a document, once the outermost block is closed.
(define (test-parse)
  (check-group "parse: start")
  (check= (connection-status "tmtestecho" "plugins-test-parse") 0)
  (check= (start* "tmtestecho" "plugins-test-parse") "ok")
  ;; nothing has been read yet: the plugin is expected to send a banner
  (check= (connection-status "tmtestecho" "plugins-test-parse") 3)
  (check= (echo (blk "verbatim:" "hello")) '(document "hello"))
  (check= (connection-status "tmtestecho" "plugins-test-parse") 2)

  (check-group "parse: formats")
  (check= (echo (blk "verbatim:" "a\nb")) '(document "a" "b"))
  ;; a final newline ends the line: an empty line follows
  (check= (echo (blk "verbatim:" "hi\n")) '(document "hi" ""))
  (check= (echo (blk "verbatim:" "")) "")
  ;; verbatim text is converted as a file of unknown encoding: a text
  ;; which looks like a TeXmacs string (the symbol <x> here) is kept, any
  ;; other is encoded (< and > become <less> and <gtr>)
  (check= (echo (blk "verbatim:" "<x> {y} $z$ \\w")) '(document "<x> {y} $z$ \\w"))
  (check= (echo (blk "verbatim:" "a > b")) '(document "a <gtr> b"))
  (check= (echo (blk "verbatim:" "a < b")) '(document "a <less> b"))
  ;; a tab is expanded, a backspace erases
  (check= (echo (blk "verbatim:" "a" (ch 9) "b")) '(document "a       b"))
  (check= (echo (blk "verbatim:" "ab" (ch 8) "c")) '(document "ac"))
  ;; carriage returns are newlines
  (check= (echo (blk "verbatim:" "a" (ch 13) "\nb")) '(document "a" "b"))
  ;; utf8 is converted to the internal (Cork) encoding
  (check= (char-codes (cadr (echo (blk "utf8:" "caf" (ch 195) (ch 169)))))
          '(99 97 102 233))
  (check= (echo (blk "scheme:" "(frac \"1\" \"2\")")) '(document (frac "1" "2")))
  (check= (echo (blk "scheme:" "(document \"a\" \"b\")")) '(document "a" "b"))
  (check= (echo (blk "latex:" "$x^2$"))
          '(document (math (concat "x" (rsup "2")))))
  (check= (echo (blk "latex:" "\\textbf{b}"))
          '(document (with "font-series" "bold" "b")))
  (check= (echo (blk "html:" "<b>x</b>"))
          '(document (html-text (with "font-series" "bold" "x"))))
  (check= (echo (blk "math:" "x")) '(document (with "mode" "math" "x")))
  ;; a format which TeXmacs converts from
  (check= (echo (blk "texmacs:" "<strong|x>")) '(document (strong "x")))
  ;; an unknown format is verbatim
  (check= (echo (blk "tm-no-such-format:" "bar")) '(document "bar"))
  (check= (echo (blk "ps:" "%!PS"))
          '(document (image (tuple (raw-data "%!PS") "ps") "0.7par" "" "" "")))
  (check= (echo (blk "file:" "/tm-plugins-test-nothing.png"))
          '(document "[/tm-plugins-test-nothing.png] does not exist"))
  (check= (echo (blk "file:" "/tm-plugins-test-nothing.png?width=3cm"))
          '(document "cm is not allowed, please pt, px or par!"))
  (check= (echo (blk "file:" "/tm-plugins-test-nothing.png?height=3par"))
          '(document "par is not allowed, please pt, px or pag!"))

  (check-group "parse: nesting and escapes")
  (check= (echo (blk "verbatim:" "a" (blk "scheme:" "(strong \"b\")") "c"))
          '(document (concat "a" (strong "b") "c")))
  (check= (echo (blk "verbatim:" "a" (blk "verbatim:" "b" (blk "latex:" "$c$"))
                     "d"))
          '(document (concat "a" "b" (math "c") "d")))
  ;; text before the first block is verbatim output
  (check= (echo (string-append "plain" (blk "verbatim:" "x")))
          '(document (concat "plain" "x")))
  ;; DATA_ESCAPE quotes the next character, the protocol ones included
  (check= (echo (blk "verbatim:" "a" ESCAPE BEGIN "b" ESCAPE END "c"
                     ESCAPE ESCAPE "d"))
          (list 'document (string-append "a" BEGIN "b" END "c" ESCAPE "d")))
  (check= (echo (blk "verbatim:" ESCAPE "x")) '(document "x"))
  ;; a superfluous DATA_END is ignored
  (check= (echo (string-append (blk "verbatim:" "x") END)) '(document "x"))
  ;; DATA_ABORT at the start of verbatim text drops the verbatim output
  ;; until the end of the outermost block
  (check= (echo (blk "verbatim:" ABORT "ignored")) "")
  (check= (echo (blk "verbatim:" "a" (blk "verbatim:" ABORT "ign") "b"))
          '(document "a"))
  ;; elsewhere it is an ordinary character
  (check= (echo (blk "verbatim:" "a" ABORT "b"))
          (list 'document (string-append "a" ABORT "b")))
  (check= (echo (blk "scheme:" "(strong \"" ABORT "\")"))
          (list 'document (list 'strong ABORT)))

  (check-group "parse: channels")
  ;; the prompt, the input and the errors are not output
  (check= (echo (blk "verbatim:" "out" (blk "prompt#" "p> "))) '(document "out"))
  (check= (echo (blk "verbatim:" "out" (blk "input#" "1+1"))) '(document "out"))
  (check= (echo (blk "verbatim:" "ok" (blk "error#" "bad"))) '(document "ok"))
  (check= (echo (blk "verbatim:" (blk "prompt#" "p" (blk "output#" "x"))))
          '(document "x"))
  (check= (echo (blk "verbatim:" (blk "mychan#" "x") "y")) '(document "y"))
  ;; a channel block can change format inside
  (check= (echo (blk "output#" (blk "scheme:" "(em \"e\")")))
          '(document (em "e")))

  (check-group "parse: commands")
  ;; a command block is evaluated as scheme, and is not output
  (set! plugins-test-notes '())
  (check= (echo (blk "command:" "(plugins-test-note! 42)")) "")
  (check= plugins-test-notes '(42))
  (check= (echo (blk "verbatim:" "a" (blk "command:" "(plugins-test-note! 'b)")
                     "c"))
          '(document (concat "a" "c")))
  (check= plugins-test-notes '(b 42))

  (check-group "parse: partial input")
  ;; an answer may come in several pieces: the parser keeps its state
  (connection-write-string "tmtestecho" "plugins-test-parse"
                           (string-append BEGIN "verbatim:hel"))
  (check= (echo (string-append "lo" END)) '(document "hello"))
  (connection-write "tmtestecho" "plugins-test-parse"
                    (string-append BEGIN "verbatim:a" BEGIN "scheme:(em"))
  (check= (echo (string-append " \"b\")" END END)) '(document (concat "a" (em "b"))))
  ;; a block cut after DATA_BEGIN, and after DATA_ESCAPE
  (connection-write-string "tmtestecho" "plugins-test-parse" BEGIN)
  (check= (echo (string-append "verbatim:x" END)) '(document "x"))
  (connection-write-string "tmtestecho" "plugins-test-parse"
                           (string-append BEGIN "verbatim:x" ESCAPE))
  (check= (echo (string-append END "y" END)) (list 'document (string-append "x" END "y")))
  ;; long answers, more than one read of the pipe
  (with s (make-string 20000 #\a)
    (check= (echo (blk "verbatim:" s)) (list 'document s)))
  (with l (map number->string (iota 300))
    (check= (echo (blk "verbatim:" (string-join l "\n")))
            (cons 'document l)))

  (check-group "parse: connection-cmd and plugin-eval")
  ;; connection-cmd sends the command formatted by the commander of the
  ;; plugin and returns the answer without its document
  (check= (tree->stree (connection-cmd "tmtestecho" "plugins-test-parse" "zz"))
          "cmd=zz")
  ;; plugin-eval simplifies the answer
  (check= (plugin-eval "tmtestecho" "plugins-test-parse" (blk "verbatim:" "x"))
          "x")
  (check= (plugin-eval "tmtestecho" "plugins-test-parse"
                       (blk "verbatim:" "a\nb"))
          '(document "a" "b"))
  (check= (plugin-eval "tmtestecho" "plugins-test-parse"
                       (blk "verbatim:" (blk "math:" "y")))
          '(math "y"))
  (check= (plugin-eval "tmtestecho" "plugins-test-parse"
                       (blk "verbatim:" "\nx\n"))
          "x"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (poll lan ses ok?)
  ;; read the plugin until @ok? holds, during at most 5 s
  (wait-until (lambda () (connection-interrupt lan ses) (ok?)) 5000))

;; What a document session receives: connection-interrupt reads the pipes
;; as the event loop does, and notifies each channel. The test plugins
;; ignore SIGINT; they send a banner once they do, which the first
;; connection-eval reads. The prompt, the input, the output and the
;; channels which have a handler are notified separately; the errors are
;; those of the standard error.
(define (test-notify)
  (check-group "notify: channels")
  (set! notified '())
  (check= (eval* "tmtestpoll" "plugins-test-poll" (blk "verbatim:" "sync"))
          '(document "sync"))
  (connection-write-string "tmtestpoll" "plugins-test-poll"
                           (blk "verbatim:" "o"
                                (blk "prompt#" "p> ")
                                (blk "input#" "in")
                                (blk "mychan#" "zz")
                                (blk "error#" "e")))
  (check-true (poll "tmtestpoll" "plugins-test-poll"
                    (lambda () (nnull? (notified-on "output")))))
  (check= (notified-on "output") '((document "o")))
  ;; verbatim text is converted as in a file of unknown encoding
  (check= (notified-on "prompt") '((document "p<gtr> ")))
  (check= (notified-on "input") '((document "in")))
  (check= (notified-on "handler") '((document "zz")))
  ;; an error channel on the standard output is dropped
  (check= (notified-on "error") '())
  ;; the answer is complete: the plugin waits for input
  (check= (connection-status "tmtestpoll" "plugins-test-poll") 2)
  ;; a new prompt replaces the previous one
  (set! notified '())
  (connection-write-string "tmtestpoll" "plugins-test-poll"
                           (blk "verbatim:" (blk "prompt#" "a")
                                (blk "prompt#" "b")))
  (check-true (poll "tmtestpoll" "plugins-test-poll"
                    (lambda () (nnull? (notified-on "prompt")))))
  (check= (notified-on "prompt") '((document "b")))
  (check= (notified-on "output") '())
  ;; an answer which is not complete leaves the plugin busy, and what has
  ;; come is notified line by line
  (set! notified '())
  (connection-write-string "tmtestpoll" "plugins-test-poll"
                           (string-append BEGIN "verbatim:part\n"))
  (check-true (poll "tmtestpoll" "plugins-test-poll"
                    (lambda () (nnull? (notified-on "output")))))
  (check= (notified-on "output") '((document "part" "")))
  (check= (connection-status "tmtestpoll" "plugins-test-poll") 3)
  (set! notified '())
  (connection-write-string "tmtestpoll" "plugins-test-poll"
                           (string-append "rest" END))
  (check-true (poll "tmtestpoll" "plugins-test-poll"
                    (lambda () (nnull? (notified-on "output")))))
  (check= (notified-on "output") '((document "rest")))
  (check= (connection-status "tmtestpoll" "plugins-test-poll") 2)
  (connection-stop "tmtestpoll" "plugins-test-poll")
  (check= (connection-status "tmtestpoll" "plugins-test-poll") 0)

  (check-group "notify: standard error")
  ;; the plugin writes what it reads on both its outputs
  (check= (eval* "tmtesterr" "plugins-test-err" (blk "verbatim:" "sync"))
          '(document "sync"))
  (set! notified '())
  (connection-write-string "tmtesterr" "plugins-test-err"
                           (blk "verbatim:" "e1"))
  ;; the standard error is read independently from the output, and may
  ;; still hold the end of the previous answer
  (check-true (poll "tmtesterr" "plugins-test-err"
                    (lambda () (and (contains? (notified-on "error") "e1")
                                    (nnull? (notified-on "output"))))))
  (check= (notified-on "output") '((document "e1")))
  (check-false (contains? (notified-on "output") "sync"))
  (set! notified '())
  ;; on the standard error, the blocks of the other channels are dropped
  (connection-write-string "tmtesterr" "plugins-test-err"
                           (blk "verbatim:" "e2" (blk "prompt#" "p")))
  (check-true (poll "tmtesterr" "plugins-test-err"
                    (lambda () (and (contains? (notified-on "error") "e2")
                                    (nnull? (notified-on "prompt"))))))
  (check-false (contains? (notified-on "error") "p"))
  (check= (notified-on "prompt") '((document "p")))
  (connection-stop "tmtesterr" "plugins-test-err")
  (check= (connection-status "tmtesterr" "plugins-test-err") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The status of a connection is 0 (dead), 2 (waiting for input) or 3
;; (waiting for output); a connection can be stopped and started again in
;; the same session, the sessions of a plugin are separate processes, and
;; the name of a session chooses the variant of the plugin.
(define (test-connection-status)
  (check-group "connections")
  (check= (connection-start "tm-plugins-test-nothing" "default")
          "Error: connection tm-plugins-test-nothing has not been declared")
  (check= (tree->stree (connection-eval "tm-plugins-test-nothing" "default" "x"))
          "")
  (check= (connection-status "tm-plugins-test-nothing" "default") 0)
  ;; FIXME: after this failed start, connection-eval in the same session
  ;; never returns (see the FIXME below); it is not checked
  (check= (start* "tmtestnone" "plugins-test-none")
          "Error: cannot start application")
  (check= (connection-status "tmtestnone" "plugins-test-none") 0)

  (check= (start* "tmtestecho" "plugins-test-a") "ok")
  (check= (echo* "plugins-test-a" "a1") '(document "a1"))
  ;; starting a running session continues it
  (check= (connection-start "tmtestecho" "plugins-test-a")
          "Continuation of 'tmtestecho' session")
  (check= (connection-status "tmtestecho" "plugins-test-a") 2)
  (check= (echo* "plugins-test-a" "a2") '(document "a2"))
  ;; two sessions at once
  (check= (start* "tmtestecho" "plugins-test-b") "ok")
  (check= (echo* "plugins-test-b" "b1") '(document "b1"))
  (check= (echo* "plugins-test-a" "a3") '(document "a3"))
  (connection-stop "tmtestecho" "plugins-test-a")
  (check= (connection-status "tmtestecho" "plugins-test-a") 0)
  (check= (connection-status "tmtestecho" "plugins-test-b") 2)
  (check= (echo* "plugins-test-b" "b2") '(document "b2"))
  ;; FIXME: connection-eval on a stopped connection, or on a plugin which
  ;; exits before its answer is complete, never returns: connection_retrieve
  ;; (src/System/Link/connection.cpp) loops until the status is
  ;; WAITING_FOR_INPUT, which a dead link never reaches, and connection_get
  ;; starts only a connection which was never made. It is not checked; the
  ;; stopped session is started again before it is evaluated in.
  (check= (connection-start "tmtestecho" "plugins-test-a") "ok")
  (check= (echo* "plugins-test-a" "a4") '(document "a4"))
  (check= (connection-status "tmtestecho" "plugins-test-a") 2)
  ;; FIXME: with the Qt pipes, the status of a plugin whose process has
  ;; exited by itself stays 2 (or 3) instead of 0: qt_pipe_link.cpp sets
  ;; alive to false only in stop. It is 0 only after connection-stop, and
  ;; the check that it is 0 before is left out.
  (connection-stop "tmtestecho" "plugins-test-a")
  (connection-stop "tmtestecho" "plugins-test-b")
  (check= (connection-status "tmtestecho" "plugins-test-a") 0)
  (check= (connection-status "tmtestecho" "plugins-test-b") 0)
  ;; stopping a dead connection does nothing
  (connection-stop "tmtestecho" "plugins-test-a")
  (check= (connection-status "tmtestecho" "plugins-test-a") 0)

  (check-group "connections: variants")
  ;; the session v2 runs the second launcher, which replaces A by B
  (check= (start* "tmtestvariants" "v1") "ok")
  (check= (start* "tmtestvariants" "v2") "ok")
  (check= (eval* "tmtestvariants" "v1" (string-append (blk "verbatim:" "A") "\n"))
          '(document "A" ""))
  (check= (eval* "tmtestvariants" "v2" (string-append (blk "verbatim:" "A") "\n"))
          '(document "B" "")))

(define (echo* ses s)
  (eval* "tmtestecho" ses (blk "verbatim:" s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The shell plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sh ses cmd)
  (eval* "shell" ses cmd))

(define (shell-pids ses)
  ;; the pids of tm_shell and of the shell it runs
  (with r (sh ses "echo $PPID $$")
    (and (func? r 'document 2)
         (string-tokenize-by-char (cadr r) #\space))))

;; tm_shell runs sh -i in a pseudo terminal and sends each answer as one
;; verbatim block; the first evaluation in a session starts tm_shell and
;; reads its banner. The checks evaluate commands, read their output and
;; their errors, keep the state of the shell between commands, run two
;; shells at once, interrupt a command, stop a shell (its processes end)
;; and start it again.
(define (test-shell)
  (check-group "shell: evaluation")
  (check= (sh "plugins-test-sh1" "echo hello") '(document "hello" ""))
  (check= (connection-status "shell" "plugins-test-sh1") 2)
  (check= (sh "plugins-test-sh1" "printf 'a\\nb\\nc\\n'")
          '(document "a" "b" "c" ""))
  (check= (sh "plugins-test-sh1" "true") "")
  (check= (sh "plugins-test-sh1" "printf x") '(document "x"))
  (check= (sh "plugins-test-sh1" "echo '<x>' '{y}' '$z'")
          '(document "<x> {y} $z" ""))
  ;; a failing command: its error message is output, as is its status (1
  ;; for the ls of BSD and macOS, 2 for the ls of GNU)
  (with r (sh "plugins-test-sh1" "ls /tm-plugins-test-nothing; echo status=$?")
    (check-true (contains? r "tm-plugins-test-nothing"))
    (check-true (contains? r "status="))
    (check-false (contains? r "status=0")))
  (check= (sh "plugins-test-sh1" "false; echo $?") '(document "1" ""))
  (check= (sh "plugins-test-sh1" "tm-plugins-test-nothing >/dev/null 2>&1; echo $?")
          '(document "127" ""))
  ;; the state of the shell is kept between commands
  (check= (sh "plugins-test-sh1" "X=41") "")
  (check= (sh "plugins-test-sh1" "echo $((X+1))") '(document "42" ""))
  (check= (sh "plugins-test-sh1" "cd /; pwd") '(document "/" ""))
  (check= (sh "plugins-test-sh1" "pwd") '(document "/" ""))
  (check= (sh "plugins-test-sh1" "echo $TEXMACS_MODE") '(document "shell" ""))

  (check-group "shell: two sessions")
  (check= (sh "plugins-test-sh2" "X=2; echo $X") '(document "2" ""))
  (check= (sh "plugins-test-sh1" "echo $X") '(document "41" ""))
  (with p1 (shell-pids "plugins-test-sh1")
    (with p2 (shell-pids "plugins-test-sh2")
      (check-true (and (list? p1) (list? p2) (!= p1 p2)))))

  (check-group "shell: stop")
  (with pids (shell-pids "plugins-test-sh2")
    (check-true (and (list? pids) (= (length pids) 2)))
    (when (and (list? pids) (= (length pids) 2))
      (check-true (pid-alive? (car pids)))
      (connection-stop "shell" "plugins-test-sh2")
      (check= (connection-status "shell" "plugins-test-sh2") 0)
      ;; tm_shell and its shell end
      (check-true (wait-until (lambda () (not (pid-alive? (car pids)))) 5000))
      (check-true (wait-until (lambda () (not (pid-alive? (cadr pids)))) 5000))))
  ;; the other session goes on
  (check= (sh "plugins-test-sh1" "echo $X") '(document "41" ""))

  (check-group "shell: restart")
  ;; started again, the session is a new shell, which sends its banner
  (check= (start* "shell" "plugins-test-sh2") "ok")
  (check= (connection-status "shell" "plugins-test-sh2") 3)
  (check-true (contains? (sh "plugins-test-sh2" "echo again")
                         "Shell session inside TeXmacs"))
  ;; FIXME: the answer to "echo again" may still be pending: there is no
  ;; synchronous way to read a plugin without writing to it, so the
  ;; session is not used further
  (connection-stop "shell" "plugins-test-sh2")
  (check= (connection-status "shell" "plugins-test-sh2") 0)

  (check-group "shell: interrupt")
  (set! notified '())
  (with pids (shell-pids "plugins-test-sh3")
    (check-true (and (list? pids) (= (length pids) 2)))
    (when (and (list? pids) (= (length pids) 2))
      (connection-write-string "shell" "plugins-test-sh3" "sleep 20\n")
      (check= (connection-status "shell" "plugins-test-sh3") 3)
      ;; tm_shell catches SIGINT: it says so, kills the shell and exits
      (connection-interrupt "shell" "plugins-test-sh3")
      (check-true (wait-until (lambda () (not (pid-alive? (car pids)))) 5000))
      (check-true (wait-until (lambda () (not (pid-alive? (cadr pids)))) 5000))
      ;; reading what is left: the process is gone, interrupting only reads
      (check-true (poll "shell" "plugins-test-sh3"
                        (lambda () (contains? notified "Interrupted"))))
      ;; the pending output of the command, empty, then the message
      (check= (notified-on "output")
              '((document "" (with "color" "red" "Interrupted TeXmacs shell"))))
      (connection-stop "shell" "plugins-test-sh3")
      (check= (connection-status "shell" "plugins-test-sh3") 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The python plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (py ses in)
  (eval* "python" ses in))

(define (py-start ses)
  ;; tm_python sends its banner and then its prompt, as two blocks: the
  ;; first evaluation reads the banner and may stop after the prompt, its
  ;; answer is then still pending, and an empty line (which tm_python
  ;; ignores) reads it
  (with r (py ses "6*7")
    (or (== r '(document "42"))
        (and (== r "")
             (with old (ahash-ref plugin-serializer-table "python")
               (plugin-serializer-set! "python" (lambda (lan t) "\n"))
               (with r2 (py ses "")
                 (plugin-serializer-set! "python" old)
                 (== r2 '(document "42"))))))))

(define plugin-serializer-table
  ;; the serializers of plugin-cmd.scm
  (module-ref (resolve-module '(utils plugins plugin-cmd)) 'plugin-serializer))

;; tm_python reads the code until a line <EOF> (see python-serialize) and
;; answers with one utf8 block; it evaluates expressions, executes
;; statements, reports exceptions, keeps its state, answers commands for
;; the completion with a scheme block, and ends when it is stopped.
(define (test-python)
  (check-group "python: evaluation")
  (check-true (py-start "plugins-test-py1"))
  (check= (connection-status "python" "plugins-test-py1") 2)
  (check= (py "plugins-test-py1" "6*8") '(document "48"))
  (check= (py "plugins-test-py1" "'a' + 'b'") '(document "ab"))
  (check= (py "plugins-test-py1" "'a\\nb'") '(document "a" "b"))
  (check= (py "plugins-test-py1" "None") "")
  ;; a statement has no value
  (check= (py "plugins-test-py1" "y = 5") "")
  (check= (py "plugins-test-py1" "y * 2") '(document "10"))
  ;; several lines: the value of the last expression
  (check= (py "plugins-test-py1" '(document "z = 1" "z + 41")) '(document "42"))
  (check= (py "plugins-test-py1"
              '(document "def f(n):" "    return n * n" "" "f(7)"))
          '(document "49"))
  ;; non ascii output, in the internal encoding
  (check= (char-codes (cadr (py "plugins-test-py1" "'caf\\u00e9'")))
          '(99 97 102 233))
  ;; printed output is the output of the statement
  (check= (py "plugins-test-py1" "print('p1')") '(document "p1" ""))

  (check-group "python: errors")
  (with r (py "plugins-test-py1" "1/0")
    (check-true (contains? r "ZeroDivisionError")))
  (with r (py "plugins-test-py1" "undefined_name_xyz")
    (check-true (contains? r "NameError")))
  (with r (py "plugins-test-py1" "1 +")
    (check-true (contains? r "SyntaxError")))
  ;; the session goes on after an error
  (check= (py "plugins-test-py1" "y + 1") '(document "6"))

  (check-group "python: completion")
  (check-true (plugin-supports-completions? "python"))
  (check= (tree->stree (connection-cmd "python" "plugins-test-py1"
                                       "(complete \"pri\" 3)"))
          '(tuple "pri" "nt"))
  (check= (tree->stree (connection-cmd "python" "plugins-test-py1"
                                       "(complete \"y\" 1)"))
          '(tuple "y" "" "ield"))

  (check-group "python: two sessions and stop")
  (check-true (py-start "plugins-test-py2"))
  (check= (py "plugins-test-py2" "y = 7") "")
  (check= (py "plugins-test-py1" "y") '(document "5"))
  (check= (py "plugins-test-py2" "y") '(document "7"))
  ;; a python session and a shell session at once
  (when (shell-available?)
    (check= (sh "plugins-test-sh4" "echo shell") '(document "shell" ""))
    (check= (py "plugins-test-py2" "y") '(document "7")))
  (with r (py "plugins-test-py2" '(document "import os" "os.getpid()"))
    (check-true (func? r 'document 1))
    (when (func? r 'document 1)
      (with pid (cadr r)
        (check-true (pid-alive? pid))
        (connection-stop "python" "plugins-test-py2")
        (check= (connection-status "python" "plugins-test-py2") 0)
        (check-true (wait-until (lambda () (not (pid-alive? pid))) 5000)))))
  (check= (py "plugins-test-py1" "y") '(document "5")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The scheme sessions run in TeXmacs itself: plugin-write evaluates the
;; input with scheme-eval in a delayed command, which needs the event loop;
;; scheme-eval itself is checked.
(define (test-scheme-session)
  (check-group "scheme session")
  (check= (scheme-eval "(+ 1 2)" :silent) "3")
  ;; a silent evaluation returns the trees and strings as trees
  (with r (scheme-eval "\"abc\"" :silent)
    (check-true (tree? r))
    (check= (and (tree? r) (tree->stree r)) "abc"))
  (check= (scheme-eval "(if #f #f)" :silent) "")
  (check= (scheme-eval "'(a b)" :silent) "(a b)")
  (check= (scheme-eval (stree->tree "(* 6 7)") :silent) "42")
  ;; an error is a tree errput
  (with r (scheme-eval "(car '())" :silent)
    (check-true (and (tree? r) (tree-is? r 'errput))))
  (with r (scheme-eval "(tm-plugins-test-undefined)" :silent)
    (check-true (and (tree? r) (tree-is? r 'errput))))
  (check= (connection-status "scheme" "default") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-forms u)
  ;; the forms of the Scheme file @u
  (with-input-from-file (url-concretize u)
    (lambda ()
      (let loop ((acc '()))
        (with x (read)
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

(define (python-requirements)
  ;; the :require clauses of the installed plugins which use python-command
  (append-map
   (lambda (dir)
     (with u (url-append dir (string-append "progs/init-"
                                            (url->string (url-tail dir))
                                            ".scm"))
       (if (not (url-exists? u)) '()
           (append-map
            (lambda (form)
              (if (not (func? form 'plugin-configure)) '()
                  (map cadr
                       (list-filter (cddr form)
                                    (lambda (x)
                                      (and (func? x :require 1)
                                           (contains? x "python-command")))))))
            (file-forms u)))))
   (url->list (url-expand (url-complete "$TEXMACS_PATH/plugins/*" "d")))))

(define (without-python thunk)
  ;; the value of @thunk when no python interpreter is found
  (let ((saved python-command))
    (set! python-command (lambda () ""))
    (with r (check-run thunk)
      (set! python-command saved)
      r)))

;; The availability of plugins rests on looking for programs in the path,
;; and the plugins which are not sessions run external commands.
(define (test-external)
  (check-group "external commands")
  (check= (eval-system "echo hello") "hello\n")
  (check= (eval-system "printf 'a\\nb'") "a\nb")
  (check= (eval-system "true") "")
  (check= (var-eval-system "echo hello") "hello")
  (check-true (url-exists-in-path? "sh"))
  (check-true (url-exists-in-path? "tm_shell"))
  (check-false (url-exists-in-path? "tm-plugins-test-no-such-program"))
  (check= (first-in-path "tm-plugins-test-no-such-program" "sh") "sh")
  ;; none found: the empty string, which is a true value
  (check= (first-in-path "tm-plugins-test-no-such-program") "")
  ;; so the plugins which need python check that python-command is not
  ;; empty: without python, none of their requirements holds (#12)
  (check-true (>= (length (python-requirements)) 10))
  (check= (without-python
           (lambda ()
             (list-filter (python-requirements)
                          (lambda (r) (eval r (current-module))))))
          '())
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (plugins-test-failures)
  (check-suite "plugins")
  (run-group test-installed)
  (run-group test-configure)
  (run-group test-serialize)
  (run-group test-parse)
  (run-group test-notify)
  (run-group test-connection-status)
  (if (shell-available?)
      (run-group test-shell)
      (skip "shell" "sh or tm_shell is not in the path"))
  (if (python-available?)
      (run-group test-python)
      (skip "python" "python3 or tmpy is missing"))
  (stop-all)
  (run-group test-scheme-session)
  (run-group test-external)
  (check-end))
