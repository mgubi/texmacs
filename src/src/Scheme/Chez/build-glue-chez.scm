;;; -*- mode: scheme -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : build-glue-chez.scm
;; DESCRIPTION : Generate the native Chez Scheme FFI glue layer
;; COPYRIGHT   : (C) 2026
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the
;; file LICENSE in the root directory or
;; <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The legacy glue path installs the ~1,170 C++ routines that TeXmacs exposes
;; to Scheme one at a time at run time (initialize_glue() ->
;; tmscm_install_procedure() -> Scall3 (tm-install-procedure! ...)).  That
;; path cannot run before Sbuild_heap(), so nothing evaluated during heap
;; construction -- notably a secondary boot file such as texmacs.boot -- can
;; see any glue at all.
;;
;; This generator replaces it with a table in C++.  It reads the same
;; declarative specifications that build-glue.scm consumes and writes
;;
;;   src/Scheme/Chez/glue_chez.cpp
;;       forward declarations for every tmg_* entry point, exception-safe
;;       tmgc_* shims around them, and the table of the routines: the name,
;;       the number of arguments and the shim of each.  initialize_glue_chez()
;;       registers one foreign symbol, tm_glue_table, from custom_init()
;;       inside Sbuild_heap(), before any boot file is evaluated.
;;
;; As with Guile and s7, nothing is written for the Scheme side: boot-chez.scm
;; asks the table for the routines while it is loaded, and defines each of
;; them with the procedure of its number of arguments (tm-glue-procedure),
;; one foreign-procedure per number of arguments for all the routines.  Chez
;; cannot make a Scheme procedure of a C function from C, as Guile and s7 do,
;; and one foreign-procedure and one procedure per routine made 5 MB of
;; compiled code (#944).  Strings cross the boundary as they are, byte strings
;; on both sides (string_to_tmscm and tmscm_to_string in chez_tm.cpp), so no
;; argument or result is converted (#947).
;;
;; Usage:
;;   scheme --script src/Scheme/Chez/build-glue-chez.scm [repo-root]
;;
;; repo-root defaults to the current directory, then its ancestors.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (chezscheme))

;;; ---------------------------------------------------------------------------
;;; Configuration
;;;
;;; Top-level constants carry earmuffs, as elsewhere in the kernel
;;; (*boot-chez-loaded*, *traced-so-files*).
;;; ---------------------------------------------------------------------------

;; The declarative specifications, in the order initialize_glue() installs
;; them, each with the macro of the optional feature it belongs to (glue.cpp
;; compiles glue_treesit.cpp and glue_lsp.cpp only with it), or #f.
(define *specs*
  '(("src/Scheme/Glue/build-glue-basic.scm" . #f)
    ("src/Scheme/Glue/build-glue-editor.scm" . #f)
    ("src/Scheme/Glue/build-glue-server.scm" . #f)
    ("src/Scheme/Glue/build-glue-treesit.scm" . "USE_TREE_SITTER")
    ("src/Scheme/Glue/build-glue-lsp.scm" . "USE_LSP")))

(define *spec-relative-paths* (map car *specs*))

(define *out-cpp-relative-path* "src/Scheme/Chez/glue_chez.cpp")

;; Routines the C++ side does not provide for Chez Scheme, because their
;; definitions and/or registrations in src/Scheme/Glue/glue_basic.cpp sit
;; inside
;;     #ifndef TEXMACS_ENGINE_CHEZ
;; blocks.  A table entry for any of them would either fail to link or
;; shadow the definition the kernel already has:
;;
;;   string-load                           defined by boot-chez.scm
;;   string-search-{forwards,backwards}    defined by chez-compat.scm
;;
;; The five os-*? predicates used to be on this list.  Their guard has been
;; removed from glue_basic.cpp, so they are now ordinary glue like everything
;; else: generated here and installed by initialize_glue() on the legacy path.
;; tm-output and tm-errput are generated: Scheme output goes through the
;; C++ streams, which cout-buffer captures (tm-with-output-to-string); the
;; definitions of boot-chez.scm only serve until this glue is loaded.
(define *chez-unsupported-names*
  '(string-load string-search-forwards string-search-backwards))

;; The seven core predicates are not part of the declarative specifications:
;; initialize_glue() installs them directly from src/Scheme/Scheme/glue.cpp.
;; Each entry is (scheme-name . c-function).
(define *core-predicates*
  '(("tree?"         . "treeP")
    ("tm?"           . "contentP")
    ("observer?"     . "observerP")
    ("url?"          . "urlP")
    ("modification?" . "modificationP")
    ("patch?"        . "patchP")
    ("blackbox?"     . "blackboxP")))

;;; ---------------------------------------------------------------------------
;;; Locating the source tree
;;; ---------------------------------------------------------------------------

(define (root-candidate? dir)
  (file-exists? (string-append dir "/" (car *spec-relative-paths*))))

(define (locate-root)
  (let* ((args (if (top-level-bound? 'command-line-arguments)
                   (command-line-arguments)
                   '()))
         (explicit (if (and (pair? args) (string? (car args)))
                       (list (car args))
                       '()))
         (candidates (append explicit
                             '("." ".." "../.." "../../.." "../../../.."))))
    (let loop ((c candidates))
      (cond ((null? c)
             (error 'build-glue-chez
                    "cannot locate the TeXmacs source tree; "
                    "pass its root as the first argument"))
            ((root-candidate? (car c)) (car c))
            (else (loop (cdr c)))))))

(define *root* (locate-root))

(define (under rel) (string-append *root* "/" rel))

;;; ---------------------------------------------------------------------------
;;; Name mangling
;;;
;;; Identical to translate-name in src/Scheme/Glue/build-glue.scm, so the
;;; tmg_* names emitted here are exactly the ones build-glue.scm already put
;;; into glue_basic.cpp / glue_editor.cpp / glue_server.cpp.
;;; ---------------------------------------------------------------------------

;; Map Scheme identifier punctuation to valid C identifier characters:
;;   ? -> P (predicate), ! -> S (setter), < -> F (from), > -> 2 (to),
;;   - -> _, = -> Q (equal).
(define (translate-name-char c)
  (case c
    ((#\?) #\P)
    ((#\!) #\S)
    ((#\<) #\F)
    ((#\>) #\2)
    ((#\-) #\_)
    ((#\=) #\Q)
    (else c)))

;; Convert a Scheme glue name (e.g. "cork->utf8" or "foo*") into its C shim
;; symbol ("tmg_cork_2utf8" or "tmg_foo_dot").  Trailing '*' maps to "_dot"
;; to match build-glue.scm.
(define (mangle name)
  (let* ((name (if (and (> (string-length name) 0)
                        (char=? (string-ref name (- (string-length name) 1)) #\*))
                   (string-append (substring name 0 (- (string-length name) 1))
                                  "_dot")
                   name)))
    (string-append "tmg_"
                   (list->string
                     (map translate-name-char (string->list name))))))

;;; ---------------------------------------------------------------------------
;;; Reading the specifications
;;; ---------------------------------------------------------------------------

(define (read-file-forms fname)
  (call-with-input-file fname
    (lambda (ip)
      (let loop ((acc '()))
        (let ((x (read ip)))
          (if (eof-object? x)
              (reverse acc)
              (loop (cons x acc))))))))

;; A specification is
;;     (build "<c++ prefix>" "<init function>" (name routine (ret . args)) ...)
;; Only the entries are of interest: the C++ prefix is already baked into the
;; generated tmg_* names, and the init function is not used by this path.
(define (spec-entries rel-path)
  (let loop ((forms (read-file-forms (under rel-path))) (acc '()))
    (cond
      ((null? forms) (reverse acc))
      ((and (pair? (car forms)) (eq? (car (car forms)) 'build))
       (let* ((form (car forms))
              (entries (cdddr form))
              (parsed (map (lambda (e)
                             (let ((name (car e))
                                   (types (caddr e)))
                               (list (symbol->string name)
                                     (car types)
                                     (cdr types))))
                           entries)))
         (loop (cdr forms) (append (reverse parsed) acc))))
      (else (loop (cdr forms) acc)))))

;;; ---------------------------------------------------------------------------
;;; Entries
;;;
;;; A resolved entry is
;;;     (scheme-name c-function foreign-symbol return-type arg-types)
;;; e.g.
;;;     ("version-before?" "tmg_version_beforeP" "tmg_version_beforeP"
;;;      bool (string string))
;;;
;;; c-function and foreign-symbol differ only for the core predicates, whose C
;;; functions (treeP, urlP, ...) predate the tmg_ convention; the foreign
;;; symbol names the tmgc_ shim of the routine.
;;; ---------------------------------------------------------------------------

(define (e-name e) (car e))
(define (e-cfn  e) (cadr e))
(define (e-sym  e) (caddr e))
(define (e-ret  e) (cadddr e))
(define (e-args e) (car (cddddr e)))
(define (e-arity e) (length (e-args e)))
;; the macro of the optional feature of the entry, or #f
(define (e-feature e) (and (> (length e) 5) (list-ref e 5)))

;; text wrapped in #ifdef FEATURE ... #endif when the entry has a feature
(define (cpp-if-feature e text)
  (if (e-feature e)
      (string-append "#ifdef " (e-feature e) "\n" text "#endif\n")
      text))

;;; ---------------------------------------------------------------------------
;;; Output helpers
;;; ---------------------------------------------------------------------------

(define (write-text path text)
  (call-with-output-file path
    (lambda (op) (display text op))
    'replace))

(define (join sep n make-item)
  (let loop ((i 1) (acc ""))
    (if (> i n)
        acc
        (loop (+ i 1)
              (string-append acc (if (= i 1) "" sep) (make-item i))))))

;; C++ parameter lists separate with commas; Scheme ones with spaces.
(define (cpp-formals n) (join ", " n (lambda (i) "tmscm")))
(define (cpp-named-formals n)
  (join ", " n (lambda (i) (string-append "tmscm a" (number->string i)))))
(define (cpp-actuals n) (join ", " n (lambda (i) (string-append "a" (number->string i)))))

;;; ---------------------------------------------------------------------------
;;; C++ artefact
;;; ---------------------------------------------------------------------------

(define (cpp-banner entries)
  (string-append "
/******************************************************************************
* MODULE     : glue_chez.cpp
* DESCRIPTION: Native Chez Scheme FFI registration of the C++ glue layer
* COPYRIGHT  : (C) 2026
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/******************************************************************************
* This file is generated by src/Scheme/Chez/build-glue-chez.scm from
*   src/Scheme/Glue/build-glue-basic.scm
*   src/Scheme/Glue/build-glue-editor.scm
*   src/Scheme/Glue/build-glue-server.scm
* Do not edit it by hand; re-run the generator instead.
*
* WHAT IT REPLACES
* ----------------
* initialize_glue() (src/Scheme/Scheme/glue.cpp) installs every C++ routine
* into Scheme at run time: one tmscm_install_procedure() per routine, each of
* which is a Scall3 into (tm-install-procedure! name id arity), which interns
* a symbol and allocates a closure.  Two consequences matter here:
*
*   - it costs ~" (number->string entries) " round trips into the interpreter during
*     every start-up, plus the resulting nursery churn;
*   - it needs a live Scheme heap, so it cannot run before Sbuild_heap()
*     returns.  Any boot file evaluated during heap construction -- a
*     secondary boot file such as texmacs.boot -- therefore sees no glue.
*
* This file replaces that with a table: the Scheme name, the number of
* arguments and the shim of each routine.  initialize_glue_chez() registers
* one foreign symbol, tm_glue_table, from custom_init() inside Sbuild_heap(),
* before a single boot file is evaluated; Sforeign_symbol() only writes to a
* C table.  boot-chez.scm calls it while it is loaded, and defines every
* routine with the foreign procedure of its number of arguments, called at
* the address of the shim (tm-glue-procedure): one compiled foreign call per
* number of arguments, not one per routine (#944).
*
* WHY THE tmgc_ SHIMS
* -------------------
* The generated tmg_* routines are ordinary C++ and may throw (tm_throw,
* FAILED, and the kernel's string exceptions).  Letting a C++ exception unwind
* through Chez's foreign-call frame would abort the process, so each entry
* point is wrapped in a tmgc_ shim that contains the throw and returns the
* (cons #t (cons msg '())) error pair the kernel already handles -- exactly
* what tm_fast_call_N did on the legacy path.  The shim also records the
* routine name for the diagnostics in chez_tm.hpp.
*
* " (number->string entries) " routines.
******************************************************************************/

#include \"chez_tm.hpp\"
#include \"basic.hpp\"
#include \"string.hpp\"
#include \"analyze.hpp\"

#include <string>

/* ---------------------------------------------------------------------------
 * Error containment
 *
 * A failing routine returns (tm-glue-error message), which the Scheme
 * side of the glue (tm-glue-check, boot-chez.scm) raises as a Guile error:
 * a C++ exception cannot cross the frames of Chez.
 * ------------------------------------------------------------------------- */

static tmscm
tmg_error_literal (const char* msg, iptr n) {
  return Scons (Sstring_to_symbol (\"tm-glue-error\"),
                Scons (chez_bytes_to_string (msg, n), (tm_null != NULL) ? tm_null : Snil));
}

/* TeXmacs throws its own string class (basic.cpp: throw string (msg)), which
   is unrelated to std::string, so both have to be caught explicitly. */
static tmscm
tmg_error_texmacs (const string& e) {
  c_string _e (e);
  return tmg_error_literal (_e, (iptr) N (e));
}

/* A wrong argument (TMSCM_ASSERT, tmscm_to_int): an ordinary Scheme error,
   not reported on stderr, as with s7 */
static bool
tmg_argument_error (const string& e) {
  return starts (e, \"Wrong type argument\") || starts (e, \"Value out of range\");
}

static tmscm
tmg_error_std (const std::string& e) {
  return tmg_error_literal (e.c_str (), (iptr) e.size ());
}

/* ---------------------------------------------------------------------------
 * Exception-safe shims
 *
 * One macro per arity, because a foreign-procedure call has a fixed arity and
 * the shim has to forward it positionally.  Arity 10 is the largest the
 * specifications use.
 * ------------------------------------------------------------------------- */

#define TMG_GUARD_BODY(NAME, FN, CALL)                                   \\
  current_glue_proc= NAME;                                               \\
  try { return FN CALL; }                                                \\
  catch (const string& e) {                                              \\
    if (!tmg_argument_error (e))                                         \\
      fprintf (stderr, \"[GLUE EXCEPTION in %s]: %s\\n\", NAME, (char*) c_string (e)); \\
    return tmg_error_texmacs (e);                                        \\
  }                                                                      \\
  catch (const std::string& e) {                                         \\
    fprintf (stderr, \"[GLUE EXCEPTION in %s]: %s\\n\", NAME, e.c_str ()); \\
    return tmg_error_std (e);                                            \\
  }                                                                      \\
  catch (...) {                                                          \\
    fprintf (stderr, \"[GLUE UNKNOWN EXCEPTION in %s]\\n\", NAME);         \\
    return tmg_error_literal (\"<exception>\", 11);                      \\
  }
"))

(define (cpp-guard-macro n)
  (string-append
    "\n#define TMG_GUARDED_" (number->string n) "(NAME, FN) \\\n"
    "  static tmscm tmgc_##FN (" (cpp-named-formals n) ") { \\\n"
    "    TMG_GUARD_BODY (NAME, FN, (" (cpp-actuals n) ")) \\\n"
    "  }\n"))

(define (cpp-externs all)
  (string-append
    "\n/* ---------------------------------------------------------------------------\n"
    " * Entry points generated into src/Scheme/Glue/glue_{basic,editor,server}.cpp,\n"
    " * plus the core predicates that initialize_glue() installs from\n"
    " * src/Scheme/Scheme/glue.cpp.\n"
    " * ------------------------------------------------------------------------- */\n"
    (apply string-append
      (map (lambda (e)
             (cpp-if-feature e
               (string-append "extern tmscm " (e-cfn e) " ("
                              (cpp-formals (e-arity e)) ");\n")))
           all))))

(define (cpp-shims all)
  (string-append
    "\n/* ---------------------------------------------------------------------------\n"
    " * Shims\n"
    " * ------------------------------------------------------------------------- */\n"
    (apply string-append
      (map (lambda (e)
             (cpp-if-feature e
               (string-append "TMG_GUARDED_" (number->string (e-arity e))
                              " (\"" (e-name e) "\", " (e-cfn e) ")\n")))
           all))))

(define (cpp-table all)
  (string-append
    "\n/* ---------------------------------------------------------------------------\n"
    " * The table of the routines\n"
    " *\n"
    " * The Scheme name, the number of arguments, and the shim, not the raw tmg_*\n"
    " * entry point.  The routines of an optional feature are in the table only\n"
    " * when it is built, so that (defined? 'name) tells whether the build has\n"
    " * them.\n"
    " * ------------------------------------------------------------------------- */\n"
    "\nstruct tmg_entry {\n"
    "  const char* name;\n"
    "  int         arity;\n"
    "  void*       fn;\n"
    "};\n"
    "\nstatic const tmg_entry tmg_entries[]= {\n"
    (apply string-append
      (map (lambda (e)
             (cpp-if-feature e
               (string-append "  { \"" (e-name e) "\", "
                              (number->string (e-arity e))
                              ", (void*) tmgc_" (e-cfn e) " },\n")))
           all))
    "};\n"
    "\nstatic const int tmg_entry_count=\n"
    "  (int) (sizeof (tmg_entries) / sizeof (tmg_entries[0]));\n"))

(define (cpp-registration entries)
  (string-append
    "\n/* ---------------------------------------------------------------------------\n"
    " * Registration\n"
    " * ------------------------------------------------------------------------- */\n"
    "\n/* The routines for boot-chez.scm, which defines them while it is loaded:\n"
    "   a list of (name arity . address), the address of the shim, which\n"
    "   foreign-procedure takes in place of the name of a foreign symbol. */\n"
    "static tmscm\n"
    "tm_glue_table () {\n"
    "  tmscm l= Snil;\n"
    "  for (int i= tmg_entry_count - 1; i >= 0; i--)\n"
    "    l= Scons (Scons (Sstring_to_symbol (tmg_entries[i].name),\n"
    "                     Scons (Sfixnum (tmg_entries[i].arity),\n"
    "                            Sunsigned ((uptr) tmg_entries[i].fn))),\n"
    "              l);\n"
    "  return l;\n"
    "}\n"
    "\nstatic bool tmg_registered= false;\n"
    "\n/* Called from custom_init(), i.e. inside Sbuild_heap() before the boot files\n"
    "   are evaluated, so that boot-chez.scm -- and any secondary boot file loaded\n"
    "   during heap construction -- has the glue. */\n"
    "void\n"
    "initialize_glue_chez () {\n"
    "  if (tmg_registered) return;\n"
    "  tmg_registered= true;\n"
    "  Sforeign_symbol (\"tm_glue_table\", (void*) tm_glue_table);\n"
    "}\n"
    "\nint\n"
    "chez_glue_procedure_count () {\n"
    "  return tmg_entry_count;\n"
    "}\n"))

;;; ---------------------------------------------------------------------------
;;; Main
;;; ---------------------------------------------------------------------------

;; Stop on the first repeat in `keys`.  A repeat means two spec entries claim
;; one binding: the generated C++ would define a symbol twice, or the table
;; would define one Scheme name twice -- which only surface at link or load
;; time, far from the cause.  The C-function check is
;; the one that can actually fire, because mangle is lossy (? -> P, - -> _,
;; > -> 2, < -> F): "a-b" and "a_b" both become tmg_a_b.
(define (assert-unique! what keys)
  (let loop ((l keys) (seen '()))
    (cond ((null? l) #t)
          ((member (car l) seen)
           (error 'build-glue-chez
                  (string-append "duplicate " what ": " (car l))))
          (else (loop (cdr l) (cons (car l) seen))))))

(define (build!)
  (let* ((all (apply append
                     (map (lambda (spec)
                            ;; (name return-type arg-types feature)
                            (map (lambda (e) (append e (list (cdr spec))))
                                 (spec-entries (car spec))))
                          *specs*)))
         (kept (filter (lambda (e)
                         (not (memq (string->symbol (car e))
                                    *chez-unsupported-names*)))
                       all))
         ;; (scheme-name c-function foreign-symbol return-type arg-types).
         ;; spec-entries still yields the raw (name return-type arg-types)
         ;; shape at this point.
         (entries (map (lambda (e)
                         (let ((m (mangle (car e))))
                           (list (car e) m m (cadr e) (caddr e)
                                 (cadddr e))))
                       kept))
         ;; The core predicates keep their historical C function names but are
         ;; in the table like everything else.
         (core (map (lambda (cp)
                      (let ((name (car cp)) (cfn (cdr cp)))
                        (list name cfn (mangle name) 'bool (list 'object))))
                    *core-predicates*))
         (all-entries (append entries core))
         (total (length all-entries)))

    ;; Two name spaces, one check each.  This runs before anything is
    ;; written, so a collision leaves the artefact untouched.
    (assert-unique! "Scheme name" (map e-name all-entries))
    (assert-unique! "C function name" (map e-cfn all-entries))

    (let* ((cpp-text
            (string-append
              (cpp-banner total)
              (apply string-append (map cpp-guard-macro '(0 1 2 3 4 5 6 7 8 9 10)))
              (cpp-externs all-entries)
              (cpp-shims all-entries)
              (cpp-table all-entries)
              (cpp-registration all-entries))))

      (write-text (under *out-cpp-relative-path*) cpp-text)

      (display "build-glue-chez: ")
      (display (number->string total))
      (display " routines (")
      (display (number->string (length core)))
      (display " core predicates), ")
      (display (number->string (- (length all) (length kept))))
      (display " skipped for Chez\n")
      (display "  -> ") (display (under *out-cpp-relative-path*)) (newline))))

(build!)
