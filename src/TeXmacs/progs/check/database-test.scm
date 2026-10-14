;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : database-test.scm
;; DESCRIPTION : tests of the TeXmacs databases
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The TeXmacs databases (.tmdb files) on two levels:
;;
;;   - the C++ layer (src/Plugins/Database), reached through the glue
;;     functions tmdb-set-field, tmdb-get-field, tmdb-query, ...: a
;;     database is a list of lines (id, attribute, value, created, expires),
;;     appended to the file when the databases are synchronized;
;;   - the Scheme layer (database/db-*.scm): db-base (current database,
;;     time, limit), db-format (encoding of values), db-users (access
;;     rights), db-version (versions and import), db-convert (conversion
;;     from and to db-entry markup) and db-edit (db-entry markup).
;;
;; Every database lives in a directory of its own under (url-temp-dir),
;; which is removed at the end; the global database and the user database
;; under the home directory are never opened.  Each file is created empty
;; before it is opened (see test-db).
;;
;; A database is written to its file only when sync_databases runs (from
;; the idle loop, which does not run during the suite, or when
;; tmdb-keep-history changes the flag of some database): db-sync toggles
;; the flag of a database of its own, which writes all the others.
;;
;; Times passed to the C++ functions are arbitrary numbers of seconds; the
;; time 0 stands for "always", where all the values ever stored count.

(texmacs-module (check database-test)
  (:use (check check-lib)
        (database db-version)
        (database db-convert)))

(define test-dir #f)
(define sync-db #f)

(define (test-db name)
  ;; FIXME: a database whose file does not exist yet is created by
  ;; database_rep::initialize without setting its time stamp, so that the
  ;; first synchronization reloads it from the file and replays the
  ;; unsaved lines with replay (clone, start, true), which sets the
  ;; expiration of a removed line to its creation time
  ;; (src/Plugins/Database/db_disk.cpp:165, l->created instead of
  ;; l->expires): all the removals done before the first synchronization
  ;; lose their date.  The files are therefore created before being opened.
  (with u (url-append test-dir name)
    (string-save "" u)
    u))

(define (db-sync)
  (tmdb-keep-history sync-db #f)
  (tmdb-keep-history sync-db #t))

(define (copy-db u name)
  ;; a copy of the file is a new database, read from the disk
  (with v (url-append test-dir name)
    (system-copy u v)
    v))

(define (sorted l) (sort l string<=?))

(define (sorted-entry l)
  (sort (map (lambda (f) (cons (car f) (sorted (cdr f)))) l)
        (lambda (x y) (string<=? (car x) (car y)))))

(define (cork-e-acute) (string (integer->char 233)))
(define (cork-c-cedilla) (string (integer->char 231)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite works in a fresh directory under the temporary directory, and
;; nothing points to a database by default: the global database is under
;; the home directory and is not used.
(define (test-setup)
  (check-group "setup")
  (set! test-dir (url-append (url-temp-dir) "database-test"))
  (when (url-exists? test-dir) (system-rmdir-recursive test-dir))
  (system-mkdir test-dir)
  (set! sync-db (test-db "sync.tmdb"))
  (check-true (url-directory? test-dir))
  (check-true (string-starts? (url->system test-dir)
                              (url->system (url-temp-dir))))
  (check-true (string-starts? (url->system (global-database))
                              (url->system
                                (url-expand
                                  (string->url "$TEXMACS_HOME_PATH")))))
  (check-true (url-none? current-database))
  (check= db-encoding :default)
  (check= db-time :now)
  (check= db-limit #f)
  (check= db-time-stamp? #f)
  (check= db-current-user #t)
  (check= db-extra-fields '())
  (check-error (db-get-db) #t)
  (check-error (db-get-field "x" "y") #t)
  ;; FIXME: with-global (kernel/boot/abbrevs.scm:118) does not restore the
  ;; variable when its body raises an error, so that the failed
  ;; db-get-field above leaves db-encoding at #f (it runs its former
  ;; definition inside with-encoding #f); db-reset puts it back
  (db-reset)
  (check= db-encoding :default)
  (check-true (url-none? current-database))
  (check-false (url-exists? (url-append test-dir "absent.tmdb"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A field has a list of values; setting it replaces all of them, and an
;; empty list removes it.  Unknown entries and attributes give empty lists.
(define (test-fields)
  (check-group "fields")
  (with db (test-db "fields.tmdb")
    (tmdb-set-field db "e1" "name" '("Alpha") 100.0)
    (check= (tmdb-get-field db "e1" "name" 150.0) '("Alpha"))
    (tmdb-set-field db "e1" "name" '("Beta" "Gamma") 200.0)
    (check= (tmdb-get-field db "e1" "name" 250.0) '("Beta" "Gamma"))
    (tmdb-set-field db "e1" "tag" '("x" "y" "x") 200.0)
    (check= (tmdb-get-field db "e1" "tag" 250.0) '("x" "y" "x"))
    (check= (tmdb-get-attributes db "e1" 250.0) '("name" "tag"))
    (tmdb-set-field db "e1" "tag" '() 300.0)
    (check= (tmdb-get-field db "e1" "tag" 350.0) '())
    (check= (tmdb-get-attributes db "e1" 350.0) '("name"))
    (tmdb-remove-field db "e1" "name" 400.0)
    (check= (tmdb-get-field db "e1" "name" 450.0) '())
    (check= (tmdb-get-attributes db "e1" 450.0) '())
    ;; removing again or removing an absent field does nothing
    (tmdb-remove-field db "e1" "name" 500.0)
    (tmdb-remove-field db "e1" "absent" 500.0)
    (check= (tmdb-get-field db "e1" "name" 350.0) '("Beta" "Gamma"))
    (check= (tmdb-get-field db "none" "name" 0.0) '())
    (check= (tmdb-get-field db "e1" "none" 0.0) '())
    (check= (tmdb-get-attributes db "none" 0.0) '())
    (check= (tmdb-get-entry db "none" 0.0) '())
    ;; ids, attributes and values are arbitrary strings
    (tmdb-set-field db "" "" '("") 100.0)
    (check= (tmdb-get-field db "" "" 150.0) '(""))
    (tmdb-set-field db "id with space" "attr:x" '("v") 100.0)
    (check= (tmdb-get-field db "id with space" "attr:x" 150.0) '("v"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An entry is an association list of fields with one or more values;
;; setting it replaces the entry, repeated attributes are merged, and
;; malformed fields are skipped.
(define (test-entries)
  (check-group "entries")
  (with db (test-db "entries.tmdb")
    (tmdb-set-entry db "e2" '(("name" "x y") ("type" "article" "book")) 100.0)
    (check= (tmdb-get-entry db "e2" 150.0)
            '(("name" "x y") ("type" "article" "book")))
    (check= (tmdb-get-attributes db "e2" 150.0) '("name" "type"))
    (check= (tmdb-get-field db "e2" "type" 150.0) '("article" "book"))
    (tmdb-set-entry db "e2" '(("name" "z")) 200.0)
    (check= (tmdb-get-entry db "e2" 250.0) '(("name" "z")))
    (check= (tmdb-get-field db "e2" "type" 250.0) '())
    (tmdb-set-entry db "m" '(("k" "1") ("k" "2") ("j" "3") ("k" "4")) 100.0)
    (check= (tmdb-get-entry db "m" 150.0) '(("k" "1" "2" "4") ("j" "3")))
    (check= (tmdb-get-field db "m" "k" 150.0) '("1" "2" "4"))
    (tmdb-set-entry db "bad" '(("lonely") "notalist" ("ok" "v")) 100.0)
    (check= (tmdb-get-entry db "bad" 150.0) '(("ok" "v")))
    (tmdb-set-entry db "empty" '() 100.0)
    (check= (tmdb-get-entry db "empty" 150.0) '())
    (tmdb-set-field db "e2" "extra" '("1") 300.0)
    (check= (tmdb-get-entry db "e2" 350.0) '(("name" "z") ("extra" "1")))
    (tmdb-remove-entry db "e2" 400.0)
    (check= (tmdb-get-entry db "e2" 450.0) '())
    (check= (tmdb-get-attributes db "e2" 450.0) '())
    (check= (tmdb-get-entry db "e2" 350.0) '(("name" "z") ("extra" "1")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Values go through the glue as Scheme trees (quoted strings): quotes,
;; backslashes, newlines, SQL wildcards, Cork bytes and long strings come
;; back unchanged, and a query matches values exactly (no wildcard).
(define (test-values)
  (check-group "values")
  (let* ((db (test-db "values.tmdb"))
         (long (make-string 20000 #\z))
         (cork (string-append (cork-e-acute) "cole Fran"
                              (cork-c-cedilla) "aise"))
         (vals (list "" "a\"b" "it's" "50%_x" "back\\slash"
                     "new\nline" "(a . b)" "#t" cork)))
    (tmdb-set-field db "f" "v" vals 100.0)
    (check= (tmdb-get-field db "f" "v" 150.0) vals)
    (tmdb-set-entry db "g" (list (cons "v" vals)) 100.0)
    (check= (tmdb-get-entry db "g" 150.0) (list (cons "v" vals)))
    (tmdb-set-field db "l" "v" (list long) 100.0)
    (check= (tmdb-get-field db "l" "v" 150.0) (list long))
    (check= (tmdb-query db '(("v" "a\"b")) 150.0 0 0) '("f" "g"))
    (check= (tmdb-query db '(("v" "50%_x")) 150.0 0 0) '("f" "g"))
    (check= (tmdb-query db '(("v" "50%")) 150.0 0 0) '())
    (check= (tmdb-query db '(("v" "%")) 150.0 0 0) '())
    (check= (tmdb-query db '(("v" "_")) 150.0 0 0) '())
    (check= (tmdb-query db '(("v" "it's")) 150.0 0 0) '("f" "g"))
    (check= (tmdb-query db '(("v" "' OR 1=1 --")) 150.0 0 0) '())
    (check= (tmdb-query db '(("v" "")) 150.0 0 0) '("f" "g"))
    (check= (tmdb-query db (list (list "v" cork)) 150.0 0 0) '("f" "g"))
    (check= (tmdb-query db (list (list "v" long)) 150.0 0 0) '("l"))
    ;; the values "any", "order" are strings and not query keywords
    (tmdb-set-field db "kw" "order" '("any") 100.0)
    (check= (tmdb-query db '(("order" "any")) 150.0 0 0) '("kw"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A value lives from its creation (included) to its expiration
;; (excluded); a query at a time sees the values of that time, and the
;; time 0 sees every value ever stored.
(define (test-history)
  (check-group "history")
  (with db (test-db "history.tmdb")
    (tmdb-set-field db "e" "name" '("Alpha") 100.0)
    (tmdb-set-field db "e" "name" '("Beta" "Gamma") 200.0)
    (tmdb-remove-field db "e" "name" 300.0)
    (check= (tmdb-get-field db "e" "name" 50.0) '())
    (check= (tmdb-get-field db "e" "name" 100.0) '("Alpha"))
    (check= (tmdb-get-field db "e" "name" 199.0) '("Alpha"))
    (check= (tmdb-get-field db "e" "name" 200.0) '("Beta" "Gamma"))
    (check= (tmdb-get-field db "e" "name" 300.0) '())
    (check= (tmdb-get-field db "e" "name" 0.0) '("Alpha" "Beta" "Gamma"))
    (check= (tmdb-get-attributes db "e" 150.0) '("name"))
    (check= (tmdb-get-attributes db "e" 350.0) '())
    (check= (tmdb-get-attributes db "e" 0.0) '("name"))
    (check= (tmdb-query db '(("name" "Alpha")) 150.0 0 0) '("e"))
    (check= (tmdb-query db '(("name" "Alpha")) 250.0 0 0) '())
    (check= (tmdb-query db '(("name" "Alpha")) 0.0 0 0) '("e"))
    (tmdb-set-entry db "r" '(("name" "R") ("type" "t")) 100.0)
    (tmdb-remove-entry db "r" 200.0)
    (check= (tmdb-get-entry db "r" 150.0) '(("name" "R") ("type" "t")))
    (check= (tmdb-get-entry db "r" 250.0) '())
    (check= (tmdb-get-entry db "r" 0.0) '(("name" "R") ("type" "t")))
    (check= (tmdb-query db '(("type" "t")) 150.0 0 0) '("r"))
    (check= (tmdb-query db '(("type" "t")) 250.0 0 0) '())
    ;; a removed entry may be set again
    (tmdb-set-entry db "r" '(("name" "S")) 300.0)
    (check= (tmdb-get-entry db "r" 350.0) '(("name" "S")))
    (check= (tmdb-get-entry db "r" 0.0) '(("name" "R" "S") ("type" "t")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A query is a list of constraints which must all hold; a constraint
;; ("attr" v1 v2 ...) asks for one of the values, (any v ...) for a value
;; in any field, (order "attr" asc?) sorts, (modified "t1" "t2") keeps the
;; entries changed in [t1, t2).  Without order, the results come in the
;; order of the values of the most selective constraint, and for each value
;; in the order of creation.
(define (test-queries)
  (check-group "queries")
  (with db (test-db "queries.tmdb")
    (tmdb-set-entry db "a" '(("name" "A1") ("type" "t1") ("color" "blue")) 100.0)
    (tmdb-set-entry db "b" '(("name" "B1") ("type" "t1") ("color" "red")) 100.0)
    (tmdb-set-entry db "c" '(("name" "C1") ("type" "t1") ("color" "red")) 100.0)
    (tmdb-set-entry db "d" '(("name" "D1") ("type" "t2") ("color" "red")) 100.0)
    (check= (tmdb-query db '() 150.0 0 0) '("a" "b" "c" "d"))
    (check= (tmdb-query db '(("name" "A1")) 150.0 0 0) '("a"))
    (check= (tmdb-query db '(("name" "A1" "C1")) 150.0 0 0) '("a" "c"))
    (check= (tmdb-query db '(("type" "t1") ("color" "red")) 150.0 0 0)
            '("b" "c"))
    (check= (tmdb-query db '(("color" "red") ("type" "t1")) 150.0 0 0)
            '("b" "c"))
    (check= (tmdb-query db '((any "red")) 150.0 0 0) '("b" "c" "d"))
    ;; the candidates come value after value
    (check= (tmdb-query db '((any "red" "A1")) 150.0 0 0) '("b" "c" "d" "a"))
    ;; unknown attributes or values give nothing, unknown values among
    ;; known ones are ignored, and a malformed constraint gives nothing
    (check= (tmdb-query db '(("nope" "red")) 150.0 0 0) '())
    (check= (tmdb-query db '(("color" "green")) 150.0 0 0) '())
    (check= (tmdb-query db '(("color" "green" "blue")) 150.0 0 0) '("a"))
    (check= (tmdb-query db '(("color")) 150.0 0 0) '())
    (check= (tmdb-query db '((unknown "red")) 150.0 0 0) '())
    (check= (tmdb-query db '(("type" "t1") ("nope" "x")) 150.0 0 0) '())
    ;; ordering
    (check= (tmdb-query db '(("color" "red") (order "name" #t)) 150.0 0 0)
            '("b" "c" "d"))
    (check= (tmdb-query db '(("color" "red") (order "name" #f)) 150.0 0 0)
            '("d" "c" "b"))
    (check= (tmdb-query db '((order "color" #t) ("type" "t1")) 150.0 0 0)
            '("a" "b" "c"))
    (check= (tmdb-query db '((order "color" #f) (order "name" #t)) 150.0 0 0)
            '("d" "c" "b" "a"))
    ;; limits and offsets on a single constraint
    (check= (tmdb-query db '(("type" "t1")) 150.0 2 0) '("a" "b"))
    (check= (tmdb-query db '(("type" "t1")) 150.0 0 1) '("b" "c"))
    (check= (tmdb-query db '(("type" "t1")) 150.0 1 2) '("c"))
    (check= (tmdb-query db '(("type" "t1")) 150.0 0 5) '())
    (check= (tmdb-query db '(("type" "t1") ("color" "red")) 150.0 1 0)
            '("b"))
    ;; FIXME: the offset skips the candidates of the first constraint and
    ;; not the results (db_query.cpp:98, filter starts at qargs.offset in
    ;; the ansatz ids): with offset 1, (("type" "t1") ("color" "red"))
    ;; gives ("b" "c") instead of ("c"), since "a" is skipped.
    ;; FIXME: an order clause raises the limit to at least 1000 and the
    ;; result is not cut back (db_query.cpp:209): with limit 1,
    ;; (("color" "red") (order "name" #t)) gives ("b" "c" "d") instead of
    ;; ("b"), so that the limit of db-tmfs.scm (get-db-fields) is ignored.
    ;; modification dates
    (tmdb-remove-entry db "a" 200.0)
    (check= (tmdb-query db '((modified "150" "300")) 0.0 0 0) '("a"))
    (check= (tmdb-query db '((modified "50" "150")) 0.0 0 0)
            '("a" "b" "c" "d"))
    (check= (tmdb-query db '((modified "300" "400")) 0.0 0 0) '())
    (check= (tmdb-query db '(("color" "red") (modified "150" "300")) 0.0 0 0)
            '())
    (check= (tmdb-query db '(("type" "t1")) 250.0 0 0) '("b" "c"))
    (check= (tmdb-query db '(("type" "t1")) 0.0 0 0) '("a" "b" "c"))
    ;; FIXME: a query without field constraints (the empty query, a query
    ;; with only (order ...) or (contains "")) returns every id ever used,
    ;; also the removed entries: after removing "a" at 200,
    ;; (tmdb-query db '() 250.0 0 0) gives ("a" "b" "c" "d") instead of
    ;; ("b" "c" "d") (db_query.cpp:145 and 150 return ids_list, which
    ;; filter keeps as there is no constraint), and db-load loads them.
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords and completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Values (except contributors) are indexed by their lower case,
;; transliterated words: (contains "w ...") asks for all the words,
;; (completes "w ... p") also for a word starting with p.  Names are
;; indexed as a whole for the name completions.
(define (test-keywords)
  (check-group "keywords")
  (let* ((db (test-db "keywords.tmdb"))
         (cork (string-append (cork-e-acute) "cole Fran"
                              (cork-c-cedilla) "aise")))
    (tmdb-set-field db "w" "title" '("Introduction to Stochastic Analysis") 100.0)
    (tmdb-set-field db "x" "title" '("Stochastic calculus, 2nd ed.") 100.0)
    (tmdb-set-field db "u" "name" (list cork) 100.0)
    (tmdb-set-field db "n" "name" '("Bourbaki") 100.0)
    (tmdb-set-field db "k" "contributor" '("Hidden Person") 100.0)
    (check= (tmdb-query db '((contains "stochastic")) 150.0 0 0) '("w" "x"))
    (check= (tmdb-query db '((contains "STOCHASTIC analysis")) 150.0 0 0)
            '("w"))
    (check= (tmdb-query db '((contains "calculus 2nd")) 150.0 0 0) '("x"))
    (check= (tmdb-query db '((contains "Intro")) 150.0 0 0) '())
    (check= (tmdb-query db '((contains "nothing")) 150.0 0 0) '())
    (check= (tmdb-query db '((completes "Intro")) 150.0 0 0) '("w"))
    (check= (tmdb-query db '((completes "stochastic an")) 150.0 0 0) '("w"))
    (check= (tmdb-query db '((completes "Stoch")) 150.0 0 0) '("w" "x"))
    (check= (tmdb-query db '((completes "zzz")) 150.0 0 0) '())
    (check= (tmdb-query db '((contains "ecole")) 150.0 0 0) '("u"))
    (check= (tmdb-query db (list (list 'contains (string-append
                                                  (cork-e-acute) "cole")))
                        150.0 0 0)
            '("u"))
    (check= (tmdb-query db '((contains "person")) 150.0 0 0) '())
    (check= (tmdb-query db '((contains "stochastic") ("title" "Stochastic calculus, 2nd ed.")) 150.0 0 0)
            '("x"))
    (check= (tmdb-get-completions db "stoch") '("stochastic"))
    (check= (tmdb-get-completions db "stochastic") '("stochastic"))
    (check= (tmdb-get-completions db "stochasticx") '())
    (check= (tmdb-get-completions db "fra") '("francaise"))
    (check= (sorted (tmdb-get-completions db "a")) '("analysis"))
    (check= (tmdb-get-completions db "pers") '())
    (check= (tmdb-get-completions db "") '())
    (check= (tmdb-get-name-completions db "Bour") '("Bourbaki"))
    (check= (tmdb-get-name-completions db "bour") '())
    (check= (tmdb-get-name-completions db (cork-e-acute)) (list cork))
    (check= (tmdb-get-name-completions db "Bourbakix") '())
    (check= (tmdb-get-name-completions db "Intro") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Changes stay in memory until the databases are synchronized; then a copy
;; of the file, opened as a new database, has the same fields, values,
;; history and index.  Without history, the synchronization compresses the
;; file to the current values.  A file changed on disk is read again.
(define (test-persistence)
  (check-group "persistence")
  (let* ((db (test-db "persist.tmdb"))
         (long (make-string 5000 #\q))
         (odd (list "" "a\"b" "it's" "new\nline"
                    (string-append (cork-e-acute) "t" (cork-e-acute)))))
    (tmdb-set-entry db "p" '(("name" "Persistent") ("type" "a" "b")) 100.0)
    (tmdb-set-field db "c" "color" '("red") 100.0)
    (tmdb-remove-field db "c" "color" 300.0)
    (tmdb-set-field db "o" "v" odd 100.0)
    (tmdb-set-field db "l" "v" (list long) 100.0)
    (tmdb-set-field db "t" "title" '("Persistent keywords") 100.0)
    (check= (string-load db) "")
    (db-sync)
    (check-true (> (string-length (string-load db)) 5000))
    (check= (tmdb-get-field db "c" "color" 200.0) '("red"))
    (with db2 (copy-db db "persist-copy.tmdb")
      (check= (tmdb-get-entry db2 "p" 150.0)
              '(("name" "Persistent") ("type" "a" "b")))
      (check= (tmdb-get-field db2 "c" "color" 200.0) '("red"))
      (check= (tmdb-get-field db2 "c" "color" 350.0) '())
      (check= (tmdb-get-field db2 "o" "v" 150.0) odd)
      (check= (tmdb-get-field db2 "l" "v" 150.0) (list long))
      (check= (tmdb-query db2 '(("type" "b")) 150.0 0 0) '("p"))
      (check= (tmdb-query db2 '((contains "keywords")) 150.0 0 0) '("t"))
      (check= (tmdb-get-completions db2 "persis") '("persistent"))
      (check= (tmdb-get-name-completions db2 "Pers") '("Persistent")))
    ;; later changes are appended to the file
    (tmdb-set-field db "p" "name" '("Renamed") 400.0)
    (db-sync)
    (with db3 (copy-db db "persist-copy2.tmdb")
      (check= (tmdb-get-field db3 "p" "name" 450.0) '("Renamed"))
      (check= (tmdb-get-field db3 "p" "name" 150.0) '("Persistent"))
      (check= (tmdb-get-field db3 "c" "color" 200.0) '("red"))))
  ;; FIXME: for a database whose file did not exist before (see test-db),
  ;; (tmdb-set-field db "c" "color" '("red") 100.0),
  ;; (tmdb-remove-field db "c" "color" 300.0) and a synchronization make
  ;; (tmdb-get-field db "c" "color" 200.0) give () instead of ("red").
  ;; compression without history
  (with db (test-db "compress.tmdb")
    (for (i (iota 10))
      (tmdb-set-field db "x" "v" (list (number->string i)) (+ 100.0 i)))
    (check= (tmdb-get-field db "x" "v" 0.0)
            '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (tmdb-keep-history db #f)
    (check= (tmdb-get-field db "x" "v" 0.0) '("9"))
    (check= (tmdb-get-field db "x" "v" 200.0) '("9"))
    (with db2 (copy-db db "compress-copy.tmdb")
      (check= (tmdb-get-field db2 "x" "v" 0.0) '("9")))
    (tmdb-keep-history db #t))
  ;; a file replaced on disk is read again at the next access after a
  ;; synchronization (its date is put in the future, since the dates of
  ;; files are in seconds)
  (let* ((db (test-db "external.tmdb"))
         (other (test-db "external-other.tmdb")))
    (tmdb-set-field db "k" "v" '("mine") 100.0)
    (tmdb-set-field other "k" "v" '("theirs") 100.0)
    (tmdb-set-field other "k2" "v" '("new") 100.0)
    (db-sync)
    (system-copy other db)
    (eval-system (string-append "touch -t 203001010000 '"
                                (url->system db) "'"))
    (db-sync)
    (check= (tmdb-get-field db "k" "v" 150.0) '("theirs"))
    (check= (tmdb-get-field db "k2" "v" 150.0) '("new")))
  ;; FIXME: when the file changed on disk, the reload (check_for_updates,
  ;; db_disk.cpp:303-310) only replays the unsaved lines created since the
  ;; last write: the unsaved removal of a value which was already written
  ;; is lost.  Repro: set ("one") at 100, synchronize, set ("two") at 200,
  ;; touch the file into the future, synchronize; then
  ;; (tmdb-get-field db "k" "v" 250.0) gives ("one" "two") instead of
  ;; ("two").
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The basic Scheme interface (db-base.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The db- functions work on the current database at the current time
;; (or the time given by with-time); db-search rewrites the keyword
;; constraints (:order, :prefix, ...) into the C++ query language.
(define (test-base)
  (check-group "db-base")
  (with db (test-db "base.tmdb")
    (with-database db
      (check= (db-get-db) db)
      (check= (with-time 5 (db-get-time)) 5.0)
      (check= (with-time "7" (db-get-time)) 7.0)
      (check= (with-time :always (db-get-time)) 0.0)
      (check-true (inexact? (db-get-time)))
      (check-true (>= (db-get-time) (- (current-time) 5)))
      (with-time 100
        (db-set-field "f" "name" '("Foo"))
        (db-set-field "f" "tag" '("a" "b"))
        (db-set-entry "g" '(("name" "Gee") ("owner" "u1") ("title" "Bar baz"))))
      (with-time 200
        (db-remove-field "f" "tag")
        (db-set-field "f" "name" '("Foo2")))
      (check= (db-get-field "f" "name") '("Foo2"))
      (check= (with-time 150 (db-get-field "f" "name")) '("Foo"))
      (check= (with-time :always (db-get-field "f" "name")) '("Foo" "Foo2"))
      (check= (with-time 150 (db-get-field "f" "tag")) '("a" "b"))
      (check= (db-get-field "f" "tag") '())
      (check= (db-get-field-first "f" "name" 'none) "Foo2")
      (check= (db-get-field-first "f" "tag" 'none) 'none)
      (check= (db-get-attributes "f") '("name"))
      (check= (with-time 150 (db-get-attributes "f")) '("name" "tag"))
      (check-true (db-entry-exists? "f"))
      (check-false (db-entry-exists? "nothing"))
      (check= (db-get-entry "g")
              '(("name" "Gee") ("owner" "u1") ("title" "Bar baz")))
      ;; searches
      (check= (db-search '(("name" "Gee"))) '("g"))
      (check= (db-search-name "Gee") '("g"))
      (check= (db-search-name "Foo") '())
      (check= (db-search-owner "u1") '("g"))
      (check= (db-search '((:prefix "ba"))) '("g"))
      (check= (db-search '((:completes "bar b"))) '("g"))
      (check= (db-search '((:match "baz"))) '("g"))
      (check= (db-search '((:contains "BAR"))) '("g"))
      (check= (db-search (prefix->queries "Bar")) '("g"))
      (check= (db-search '((:order "name" #t))) '("f" "g"))
      (check= (db-search '((:order "name" #f))) '("g" "f"))
      (check= (db-search '((:order "name" "#f"))) '("g" "f"))
      (check= (with-time :always (db-search '((:modified "150" "250"))))
              '("f"))
      (check= (with-limit 1 (db-search '(("name" "Gee" "Foo2")))) '("g"))
      (check= (db-search-paginate '(("name" "Gee" "Foo2")) 1 1) '("f"))
      (check= (db-search-paginate '(("name" "Gee" "Foo2")) 5 0) '("g" "f"))
      (check= (index-get-completions "ba") '("bar" "baz"))
      (check= (index-get-name-completions "Ge") '("Gee"))
      (check= (prefix->queries "x") '((:completes "x")))
      ;; entries, extra fields and time stamps
      (with-extra-fields '(("contributor" "me"))
        (db-set-entry "e" '(("name" "E") ("contributor" "you")))
        (check= (db-get-entry "e") '(("contributor" "me") ("name" "E"))))
      (with-time-stamp #t
        (db-set-entry "ts" '(("name" "T")))
        (db-set-entry "ts2" '(("name" "T2") ("date" "12"))))
      (check= (map car (db-get-entry "ts")) '("name" "date"))
      (check-true (string->number (db-get-field-first "ts" "date" "")))
      (check= (db-get-field "ts2" "date") '("12"))
      (db-remove-entry "ts")
      (check= (db-get-entry "ts") '())
      (check-false (db-entry-exists? "ts"))
      ;; creation of entries with new identifiers
      (let* ((id1 (db-create-entry '(("name" "N1"))))
             (id2 (db-create-entry '(("name" "N1")))))
        (check-true (string? id1))
        (check-true (!= id1 id2))
        (check= (db-get-entry id1) '(("name" "N1")))
        (check= (sorted (db-search-name "N1")) (sorted (list id1 id2))))
      (check-true (string? (db-create-id)))
      (check-true (null? (db-get-attributes (db-create-id))))
      (check= (assoc-add '(("a" "1")) '(("a" "2") ("b" "3")))
              '(("a" "1") ("b" "3")))
      (check= (assoc-add '() '(("b" "3"))) '(("b" "3"))))
    (check-true (url-none? current-database))
    (check-error (with-time 'bad (db-get-time)) #t)
    (db-reset)
    (check= db-time :now)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding of values (db-format.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With the default encoding, values are stored as serialized TeXmacs
;; snippets and decoded when read; with-encoding #f gives the raw values.
(define (test-encoding)
  (check-group "encoding")
  (with db (test-db "encoding.tmdb")
    (with-database db
      (db-set-entry "s" '(("type" "x") ("title" "a<b" "x" "<with|a|b>")))
      (check= (with-encoding #f (db-get-field "s" "title"))
              '("a\\<b" "x" "\\<with\\|a\\|b\\>"))
      (check= (db-get-field "s" "title") '("a<b" "x" "<with|a|b>"))
      (check= (db-get-entry "s")
              '(("type" "x") ("title" "a<b" "x" "<with|a|b>")))
      (check= (db-search '(("title" "a<b"))) '("s"))
      (check= (with-encoding #f (db-search '(("title" "a\\<b")))) '("s"))
      (db-set-field "s" "note" '("p<q"))
      (check= (with-encoding #f (db-get-field "s" "note")) '("p\\<q"))
      (with-encoding #f (db-set-field "s" "raw" '("r<s")))
      (check= (with-encoding #f (db-get-field "s" "raw")) '("r<s"))
      (check= (db-encode-entry '(("type" "t") ("v" "a<b")))
              '(("type" "t") ("v" "a\\<b")))
      (check= (db-decode-entry '(("type" "t") ("v" "a\\<b")))
              '(("type" "t") ("v" "a<b")))
      (check= (db-encode-field "t" '("v" "x" "")) '("v" "x" ""))
      (check= (format->attributes '(and "a" (or "b" (optional "c")) 5))
              '("a" "b" "c"))
      (check= (format->attributes "a") '("a"))
      (check= (format->attributes 5) '())
      (check-true (in? "date" (db-meta-attributes)))
      (check-true (in? "type" (db-reserved-attributes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users and access rights (db-users.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A user may read an entry when one of its groups (closed under the
;; delegate- fields of groups) or "all" is among the readers or owners;
;; with-user restricts searches and adds the owner to created entries.
(define (test-users)
  (check-group "users")
  (with db (test-db "users.tmdb")
    (with-database db
      (db-set-entry "u1" '(("type" "user") ("pseudo" "alice")))
      (db-set-entry "u2" '(("type" "user") ("pseudo" "bob")))
      (db-set-entry "g1" '(("type" "group") ("delegate-readable" "u2")))
      (db-set-entry "r1" '(("type" "file") ("name" "doc")
                           ("owner" "u1") ("readable" "g1")))
      (db-set-entry "r2" '(("type" "file") ("name" "pub")
                           ("owner" "u1") ("readable" "all")))
      (check= (db-expand-user "u2" "readable") '("g1" "u2" "all"))
      (check= (db-expand-user "u1" "readable") '("u1" "all"))
      (check= (db-expand-user '("u1" "u2") "readable") '("g1" "u1" "u2" "all"))
      (check= (db-expand-user 3 "readable") '("all"))
      (check-true (db-allow? "r1" "u1" "owner"))
      (check-true (db-allow? "r1" "u1" "readable"))
      (check-true (db-allow? "r1" "u2" "readable"))
      (check-false (db-allow? "r1" "u2" "owner"))
      (check-false (db-allow? "r1" "zz" "readable"))
      (check-true (db-allow? "r2" "zz" "readable"))
      (check-true (db-allow? "r1" #t "owner"))
      (check= (with-user "u2" (db-search '(("type" "file")))) '("r1" "r2"))
      (check= (with-user "zz" (db-search '(("type" "file")))) '("r2"))
      (check= (with-user #t (db-search '(("type" "file")))) '("r1" "r2"))
      (with id (with-user "u2" (db-create-entry '(("type" "file")
                                                   ("name" "mine"))))
        (check-true (string? id))
        (check= (db-get-field id "owner") '("u2"))
        (check= (with-user "u2" (db-search '(("name" "mine")))) (list id))
        (check= (with-user "zz" (db-search '(("name" "mine")))) '()))
      (with id (with-user '("u1" "u2") (db-create-entry '(("name" "both"))))
        (check= (sorted (db-get-field id "owner")) '("u1" "u2")))
      (check-false (with-user '() (db-create-entry '(("name" "x")))))
      (check= db-current-user #t)
      ;; FIXME: db-get-field, db-get-entry, db-set-field, db-set-entry and
      ;; db-remove-entry recurse without end when the current user is not
      ;; #t: they call db-allow?, whose (db-get-field id attr)
      ;; (db-users.scm:288) runs again with the same user.  Repro:
      ;; (with-user "u2" (db-get-field "r1" "name")) raises stack-overflow
      ;; instead of giving ("doc").
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Versions and import (db-version.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Entries are the same when they agree up to meta fields and order; an
;; update creates a new entry which records the old one as "newer"
;; history; an import keeps or replaces existing entries according to
;; identifiers, exact matches, histories, modes and dates.
(define (test-versions)
  (check-group "versions")
  (with db (test-db "versions.tmdb")
    (with-database db
      (check-true (db-same-entries? '(("name" "a") ("date" "1"))
                                    '(("date" "2") ("name" "a"))))
      (check-false (db-same-entries? '(("name" "a")) '(("name" "b"))))
      (check-true (db-same-entries? '(("a" "1") ("b" "2"))
                                    '(("b" "2") ("a" "1"))))
      (check-true (db-same-entries? '() '(("newer" "x"))))
      (db-set-entry "v1" '(("type" "article") ("name" "art") ("title" "T1")))
      (check= (db-update-entry "v1" '(("type" "article") ("name" "art")
                                      ("title" "T1")))
              "v1")
      (with nid (db-update-entry "v1" '(("type" "article") ("name" "art")
                                        ("title" "T2")))
        (check-true (!= nid "v1"))
        (check= (sorted-entry (db-get-entry nid))
                '(("name" "art") ("newer" "v1") ("title" "T2")
                  ("type" "article")))
        (check= (db-get-entry "v1") '()))
      (check= (db-update-entry "absent" '(("name" "zz")) "chosen") "chosen")
      (check= (sorted-entry (db-get-entry "chosen"))
              '(("name" "zz") ("newer" "absent")))
      (with-global db-duplicate-warning? #f
        ;; a new identifier is stored
        (db-import-entry "i1" '(("type" "article") ("name" "imp")
                                ("title" "X")))
        (check= (db-get-entry "i1")
                '(("type" "article") ("name" "imp") ("title" "X")))
        ;; an existing identifier is not changed
        (db-import-entry "i1" '(("type" "article") ("name" "imp")
                                ("title" "Y")))
        (check= (db-get-field "i1" "title") '("X"))
        ;; an exact copy is not stored, the original supersedes it
        (db-import-entry "i2" '(("type" "article") ("name" "imp")
                                ("title" "X")))
        (check= (db-get-entry "i2") '())
        (check= (db-get-field "i1" "newer") '("i2"))
        ;; a newer version replaces the entries of its history
        (db-import-entry "i3" '(("type" "article") ("name" "imp")
                                ("title" "Z") ("newer" "i1")))
        (check= (db-get-field "i3" "title") '("Z"))
        (check= (db-get-entry "i1") '())
        (check= (db-search '(("newer" "i1"))) '("i3"))
        ;; an entry which is already superseded is not stored
        (db-import-entry "i1" '(("type" "article") ("name" "imp")
                                ("title" "Old")))
        (check= (db-get-entry "i1") '())
        ;; same name and contributor: the most recent date wins
        (db-import-entry "k1" '(("type" "a") ("name" "kk") ("contributor" "c")
                                ("date" "100") ("t" "1")))
        (db-import-entry "k2" '(("type" "a") ("name" "kk") ("contributor" "c")
                                ("date" "50") ("t" "2")))
        (check= (db-get-entry "k2") '())
        (check= (db-get-field "k1" "newer") '("k2"))
        (db-import-entry "k3" '(("type" "a") ("name" "kk") ("contributor" "c")
                                ("date" "200") ("t" "3")))
        (check= (db-get-field "k3" "t") '("3"))
        (check= (db-get-field "k3" "newer") '("k1"))
        (check= (db-get-entry "k1") '())
        ;; a manual entry is kept against an automatic one
        (db-import-entry "m1" '(("type" "a") ("name" "mm") ("contributor" "c")
                                ("modus" "manual") ("t" "1")))
        (db-import-entry "m2" '(("type" "a") ("name" "mm") ("contributor" "c")
                                ("date" "999") ("t" "2")))
        (check= (db-get-entry "m2") '())
        (check= (db-get-field "m1" "newer") '("m2"))
        ;; and replaces an automatic one
        (db-import-entry "m3" '(("type" "a") ("name" "mm") ("contributor" "c")
                                ("modus" "manual") ("t" "3")))
        (check= (db-get-field "m3" "newer") '("m1"))
        (check= (db-get-entry "m1") '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion to and from markup (db-convert.scm, db-edit.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An entry is loaded as (db-entry id type name (document meta) (document
;; fields)) and saved back to an association list; the fields id, type
;; and name are renamed with a star in the markup.
(define (test-convert)
  (check-group "convert")
  (check= (assoc-list->entry "id" '(("type" "book") ("name" "N")
                                    ("title" "A" "B") ("date" "5") ("bad")))
          '(db-entry "id" "book" "N" (document (db-field "date" "5"))
                     (document (db-field "title" "A") (db-field "title" "B"))))
  (check= (assoc-list->entry "id" '(("title" "A")))
          '(db-entry "id" "?" "?" (document) (document (db-field "title" "A"))))
  (check= (assoc-list->entry "i" '(("type" "t") ("name" "n") ("id*" "x")))
          '(db-entry "i" "t" "n" (document) (document (db-field "id" "x"))))
  (check= (entry->assoc-list
            '(db-entry "id" "book" "N" (document (db-field "date" "5"))
                       (document (db-field "title" "A") (db-field "title" "B")
                                 (db-field "id" "x"))))
          '(("type" "book") ("name" "N") ("date" "5") ("title" "A")
            ("title" "B") ("id*" "x")))
  (check= (db-save-pre '(db-entry "i" "t" "n" (document)
                                  (document (db-field "name" "x"))))
          '(db-entry "i" "t" "n" (document) (document (db-field "name*" "x"))))
  (check= (db-load-post '(db-entry "i" "t" "n" (document)
                                   (document (db-field "type*" "x"))))
          '(db-entry "i" "t" "n" (document) (document (db-field "type" "x"))))
  (check-true (db-url? (string->url "a.tmdb")))
  (check-true (db-url? (string->url "tmfs://db/x")))
  (check-false (db-url? (string->url "a.tm")))
  (with db (test-db "convert.tmdb")
    (with-database db
      (db-save '(document
                  (db-entry "s1" "book" "Saved" (document)
                            (document (db-field "title" "S")))
                  (db-entry "s2" "misc" "Other" (document) (document))))
      (check= (db-get-entry "s1")
              '(("type" "book") ("name" "Saved") ("title" "S")))
      (check= (db-get-entry "s2") '(("type" "misc") ("name" "Other")))
      ;; an entry whose identifier exists is not saved again
      (db-save '(document (db-entry "s2" "misc" "Changed" (document)
                                    (document))))
      (check= (db-get-field "s2" "name") '("Other"))
      (db-save-types '(document
                        (db-entry "s3" "book" "B3" (document) (document))
                        (db-entry "s4" "misc" "M4" (document) (document)))
                     '("book"))
      (check= (db-get-entry "s3") '(("type" "book") ("name" "B3")))
      (check= (db-get-entry "s4") '())
      (check= (db-load-entry "s1")
              '(db-entry "s1" "book" "Saved" (document)
                         (document (db-field "title" "S"))))
      (check= (db-load-types '("misc"))
              '(document (db-entry "s2" "misc" "Other" (document)
                                   (document))))
      (check= (db-load)
              '(document
                 (db-entry "s1" "book" "Saved" (document)
                           (document (db-field "title" "S")))
                 (db-entry "s2" "misc" "Other" (document) (document))
                 (db-entry "s3" "book" "B3" (document) (document))))
      (with l (db-change-list #t "general" 0)
        (check= (map car l) '("s1" "s2" "s3"))
        (check= (map cadr l) '("Saved" "Other" "B3"))
        (check= (sorted-entry (caddr (car l)))
                '(("name" "Saved") ("title" "S") ("type" "book"))))
      (check= (db-change-list #t "general" (+ (current-time) 1000)) '())
      (check= (db-change-list "nobody" "general" 0) '())))
  (with e '(db-entry "id" "book" "N" (document)
                     (document (db-field "title" "A") (db-field "year" "2000")))
    (check-true (db-entry? e))
    (check-true (db-entry-any? e))
    (check-false (db-entry? '(db-entry "x")))
    (check-false (db-entry-any? "text"))
    (check= (db-entry-ref e "title") "A")
    (check= (db-entry-ref e "id") "id")
    (check= (db-entry-ref e "type") "book")
    (check= (db-entry-ref e "name") "N")
    (check= (db-entry-ref e "nope") #f)
    (check= (db-entry-set e "year" "2001")
            '(db-entry "id" "book" "N" (document)
                       (document (db-field "title" "A")
                                 (db-field "year" "2001"))))
    (check= (db-entry-set e "pages" "3")
            '(db-entry "id" "book" "N" (document)
                       (document (db-field "title" "A")
                                 (db-field "year" "2000")
                                 (db-field "pages" "3"))))
    (check= (db-entry-set e "name" "M")
            '(db-entry "id" "book" "M" (document)
                       (document (db-field "title" "A")
                                 (db-field "year" "2000"))))
    (check= (db-entry-remove e "year")
            '(db-entry "id" "book" "N" (document)
                       (document (db-field "title" "A"))))
    (check= (db-entry-remove e "type") e)
    (check= (db-entry-rename e '(("title" . "titre")))
            '(db-entry "id" "book" "N" (document)
                       (document (db-field "titre" "A")
                                 (db-field "year" "2000"))))
    (check= (db-entry-rename "text" '()) "text")
    (check= (db-field-attr '(db-field "a" "b")) "a")
    (check= (db-field-attr '(other "a" "b")) #f)
    (check-true (db-field? '(db-field "a" "b")))
    (check-true (db-field-any? '(db-field-optional "a" "b")))
    (check-false (db-field? '(db-field "a")))
    (check= (db-field-find '((db-field "a" "1") (db-field "b" "2")) "b") "2")
    (check= (db-field-find '() "b") #f)
    (check= (db-field-set '((db-field "a" "1")) "a" "9")
            '((db-field "a" "9")))
    (check= (db-field-remove '((db-field "a" "1") (db-field "b" "2")) "a")
            '((db-field "b" "2")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sql-quote makes an SQL string literal by doubling the quotes; sql-exec
;; passes its command unchanged (sql_escape is the identity), so that a
;; command built from strings must quote them.  When SQLite is not
;; available, sql-exec gives an empty table.
(define (test-sql)
  (check-group "sql")
  (check= (sql-quote "") "''")
  (check= (sql-quote "abc") "'abc'")
  (check= (sql-quote "it's") "'it''s'")
  (check= (sql-quote "''") "''''''")
  (check= (sql-quote "50%_") "'50%_'")
  (check= (sql-quote "a\"b") "'a\"b'")
  (check= (sql-quote "x'); DROP TABLE t; --") "'x''); DROP TABLE t; --'")
  (with u (url-append test-dir "test.sqlite")
    (if (not (supports-sql?))
        (check= (sql-exec u "SELECT 1") '())
        (let* ((evil "x'); DROP TABLE t; --")
               (ins (lambda (s)
                      (sql-exec u (string-append "INSERT INTO t VALUES ("
                                                 (sql-quote s) ")")))))
          (sql-exec u "CREATE TABLE t (v TEXT)")
          (ins evil)
          (ins "it's")
          (check= (sql-exec u "SELECT v FROM t ORDER BY v")
                  '(("v") ("it's") ("x'); DROP TABLE t; --")))
          (check= (sql-exec u (string-append "SELECT count(*) FROM t WHERE v="
                                             (sql-quote evil)))
                  '(("count(*)") ("1")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Everything is written before the directory is removed, so that no later
;; synchronization writes into it.
(define (test-cleanup)
  (check-group "cleanup")
  (db-sync)
  (db-reset)
  (system-rmdir-recursive test-dir)
  (check-false (url-exists? test-dir))
  (check-true (url-none? current-database)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (database-test-failures)
  (check-suite "database")
  (test-setup)
  (test-fields)
  (test-entries)
  (test-values)
  (test-history)
  (test-queries)
  (test-keywords)
  (test-persistence)
  (test-base)
  (test-encoding)
  (test-users)
  (test-versions)
  (test-convert)
  (test-sql)
  (test-cleanup)
  (check-end))
