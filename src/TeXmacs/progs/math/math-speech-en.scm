
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-speech-en.scm
;; DESCRIPTION : mathematical editing using English speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-speech-en)
  (:use (math math-speech)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sanitize input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-sanitize lan mode s)
  (:require (and (== lan 'english) (== mode 'math)))
  (set! s (locase-all s))
  (set! s (list->tmstring (clean-letter-digit (tmstring->list s))))
  (set! s (string-replace s "+" " plus "))
  (set! s (string-replace-trailing s "-" " minus "))
  (set! s (string-replace s "<times>" " times "))
  (set! s (string-replace s "/" " slash "))
  (set! s (string-replace s "," " comma "))
  (set! s (string-replace-trailing s "." " period "))
  (set! s (string-replace s ":" " colon "))
  (set! s (string-replace s ";" " semicolon "))
  (set! s (string-replace s "^" " hat "))
  (set! s (string-replace s "~" " tilda "))
  (set! s (string-replace s "<ldots>" " dots "))
  (set! s (string-replace s "<cdots>" " dots "))
  (set! s (string-replace s "  " " "))
  (set! s (string-replace s "  " " "))
  (set! s (tm-string-trim-both s))
  s)

(speech-collection dont-break english
  "ad" "ag" "ah" "al" "an" "ar" "as" "el" "em" "en" "ex" "if" "in" "is" "it"
  "of" "oh" "ok" "ol" "or" "be" "de" "he" "pe" "se" "ve" "we"
  "ma" "bi" "hi" "ji" "pi" "si" "xi"
  "do" "fo" "ho" "jo" "ko" "lo" "no" "po" "so" "to" "vo" "wo"
  "mu" "nu" "by" "hy" "ky" "my" "sy")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statistical resolution of ambiguities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (english-and)
  (with n (best-letter-variant "n")
    (if (stats-better? "<wedge>" n)
        (insert"<wedge>")
        (speech-insert-letter "n"))))

(define (english-in)
  (with n (best-letter-variant "n")
    (if (stats-better*? n "<in>")
        (speech-insert-letter "n")
        (insert "<in>"))))

(define (english-m)
  (let* ((m (best-letter-variant "m"))
         (n (best-letter-variant "n")))
    (if (stats-better*? n m)
        (speech-insert-letter "n")
        (speech-insert-letter "m"))))

(define (english-the)
  (speech-insert-letter "d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering mathematical symbols via English speech
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-symbols english
  ("zero" "0")
  ("one" "1")
  ("two" "2")
  ("three" "3")
  ("four" "4")
  ("five" "5")
  ("six" "6")
  ("seven" "7")
  ("eight" "8")
  ("nine" "9")
  ("ten" "10")
  ("hundred" "100")
  ("thousand" "1000")
  ("million" "1000000")
  ("billion" "1000000000")

  ("a" "a")
  ("b" "b")
  ("c" "c")
  ("d" "d")
  ("e" "e")
  ("f" "f")
  ("g" "g")
  ("h" "h")
  ("i" "i")
  ("j" "j")
  ("k" "k")
  ("l" "l")
  ("m" "m")
  ("n" "n")
  ("o" "o")
  ("p" "p")
  ("q" "q")
  ("r" "r")
  ("s" "s")
  ("t" "t")
  ("u" "u")
  ("v" "v")
  ("w" "w")
  ("x" "x")
  ("y" "y")
  ("z" "z")

  ("alpha" "<alpha>")
  ("beta" "<beta>")
  ("gamma" "<gamma>")
  ("delta" "<delta>")
  ("epsilon" "<varepsilon>")
  ("zeta" "<zeta>")
  ("eta" "<eta>")
  ("theta" "<theta>")
  ("iota" "<iota>")
  ("kappa" "<kappa>")
  ("lambda" "<lambda>")
  ("mu" "<mu>")
  ("nu" "<nu>")
  ("xi" "<xi>")
  ("omicron" "<omicron>")
  ("pi" "<pi>")
  ("rho" "<rho>")
  ("sigma" "<sigma>")
  ("tau" "<tau>")
  ("upsilon" "<upsilon>")
  ("phi" "<varphi>")
  ("psi" "<psi>")
  ("chi" "<chi>")
  ("omega" "<omega>")

  ("constant e" "<mathe>")
  ("constant i" "<mathi>")
  ("constant pi" "<mathpi>")
  ("constant gamma" "<mathgamma>")
  ("euler constant" "<mathgamma>")

  ("infinity" "<infty>")
  ("complex numbers" "<bbb-C>")
  ("positive integers" "<bbb-N>")
  ("rationals" "<bbb-Q>")
  ("reals" "<bbb-R>")
  ("integers" "<bbb-Z>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More complex mathematical speech commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-map english math
  ("uppercase" (speech-alter-letter :big))
  ("lowercase" (speech-alter-letter :small))
  ("bold" (speech-alter-letter :bold))
  ("medium" (speech-alter-letter :medium))
  ("upright" (speech-alter-letter :up))
  ("italic" (speech-alter-letter :it))
  ("calligraphic" (speech-alter-letter :cal))
  ("fraktur" (speech-alter-letter :frak))
  ("gothic" (speech-alter-letter :frak))
  ("blackboard bold" (speech-alter-letter :bbb))
  ("normal" (speech-alter-letter :normal))
  ("sans serif" (speech-alter-letter :ss))
  ("typewriter" (speech-alter-letter :tt))
  ("operator" (speech-operator))

  ("comma" (insert ","))
  ("such that" (insert "<suchthat>"))
  ("period" (insert "."))

  ("plus" (insert "+"))
  ("minus" (insert "-"))
  ("times" (insert "*"))
  ("factor" (speech-factor))
  ("cross" (insert "<times>"))
  ("slash" (insert "/"))
  ("inverse" (speech-insert-superscript "-1"))
  ("square" (speech-insert-superscript "2"))
  ("cube" (speech-insert-superscript "3"))
  ("sub" (speech-subscript))
  ("power" (speech-superscript))
  ("e power" (insert "<mathe>") (speech-superscript))
  ("subscript" (make 'rsub))
  ("superscript" (make 'rsup))
  ("e superscript" (insert "<mathe>") (make 'rsup))

  ("prime" (make-rprime "'"))
  ("double prime" (make-rprime "'") (make-rprime "'"))
  ("triple prime" (make-rprime "'") (make-rprime "'") (make-rprime "'"))
  ("dagger" (make-rprime "<dag>"))

  ("hat" (speech-accent "^"))
  ("tilda" (speech-accent "~"))
  ("bar" (speech-accent "<bar>"))
  ("wide hat" (speech-wide "^"))
  ("wide tilda" (speech-wide "~"))
  ("wide bar" (speech-wide "<bar>"))
  ("hat under" (speech-accent-under "^"))
  ("tilda under" (speech-accent-under "~"))
  ("bar under" (speech-accent-under "<bar>"))
  ("wide hat under" (speech-wide-under "^"))
  ("wide tilda under" (speech-wide-under "~"))
  ("wide bar under" (speech-wide-under "<bar>"))

  ("of" (speech-of))
  ("brackets" (math-bracket-open "(" ")" 'default))
  ("square brackets" (math-bracket-open "[" "]" 'default))
  ("curly brackets" (math-bracket-open "{" "}" 'default))

  ("equal" (insert "="))
  ("not equal" (insert "<neq>"))
  ("assign" (insert "<assign>"))

  ("less" (insert "<less>"))
  ("less equal" (insert "<leqslant>"))
  ("greater" (insert "<gtr>"))
  ("greater equal" (insert "<geqslant>"))

  ("in" (english-in))
  ("is in" (insert "<in>"))
  ("contains the element" (insert "<ni>"))
  ("subset" (insert "<subset>"))
  ("superset" (insert "<supset>"))
  ("right arrow" (insert "<rightarrow>"))
  ("long right arrow" (insert "<rightarrow>"))
  ("maps to" (insert "<mapsto>"))
  ("long maps to" (insert "<longmapsto>"))

  ("for all" (insert "<forall>"))
  ("exists" (insert "<exists>"))
  ("or" (insert "<vee>"))
  ("logical and" (insert "<wedge>"))
  ("and" (english-and))
  ("implies" (insert "<Rightarrow>"))
  ("equivalent" (insert "<Leftrightarrow>"))

  ("exponential" (speech-insert-operator "exp"))
  ("exponential of" (speech-insert-function "exp"))
  ("logarithm" (speech-insert-operator "log"))
  ("logarithm of" (speech-insert-function "log"))
  ("sine" (speech-insert-operator "sin"))
  ("sine of" (speech-insert-function "sin"))
  ("cosine" (speech-insert-operator "cos"))
  ("cosine of" (speech-insert-function "cos"))
  ("tangent" (speech-insert-operator "tan"))
  ("tangent of" (speech-insert-function "tan"))
  ("factorial" (insert "!"))
  
  ("plus dots plus" (insert "+<cdots>+"))
  ("minus dots minus" (insert "-<cdots>-"))
  ("times dots times" (insert "*<cdots>*"))
  ("comma dots comma" (insert ",<ldots>,"))
  ("colon dots colon" (insert ":<ldots>:"))
  ("semicolon dots semicolon" (insert ";<ldots>;"))
  ("and dots and" (insert "<wedge><cdots><wedge>"))
  ("or dots or" (insert "<vee><cdots><vee>"))
  ("equal dots equal" (insert "=<cdots>="))
  ("similar dots similar" (insert "<sim><cdots><sim>"))
  ("less dots less" (insert "<less><cdots><less>"))
  ("less equal dots less equal" (insert "<leqslant><cdots><leqslant>"))
  ("greater dots greater" (insert "<gtr><cdots><gtr>"))
  ("greater equal dots greater equal" (insert "<geqslant><cdots><geqslant>"))
  ("tensor dots tensor" (insert "<otimes><cdots><otimes>"))

  ("sum" (math-big-operator "sum"))
  ("product" (math-big-operator "prod"))
  ("integral" (math-big-operator "int"))
  ("contour integral" (math-big-operator "oint"))
  ("double integral" (math-big-operator "iint"))
  ("triple integral" (math-big-operator "iiint"))
  ("for" (speech-for))
  ("from" (speech-for))
  ("until" (speech-until))
  ("to" (speech-to))
  ("d u" (speech-insert-d "u"))
  ("d v" (speech-insert-d "v"))
  ("d w" (speech-insert-d "w"))
  ("d x" (speech-insert-d "x"))
  ("d y" (speech-insert-d "y"))
  ("d z" (speech-insert-d "z"))

  ("square root" (make 'sqrt))
  ("square root of" (speech-sqrt))
  ("fraction" (make 'frac))
  ("over" (speech-over))
  ("numerator" (go-to-fraction :numerator))
  ("denominator" (go-to-fraction :denominator))

  ("similar" (insert "<sim>"))
  ("asymptotic" (insert "<asymp>"))
  ("approx" (insert "<approx>"))
  ("isomorphic" (insert "<cong>"))
  ("negligible" (insert "<prec>"))
  ("dominated" (insert "<preccurlyeq>"))
  ("dominates" (insert "<succcurlyeq>"))
  ("strictly dominates" (insert "<succ>"))

  ("tensor" (insert "<otimes>"))

  ("matrix" (make 'matrix))
  ("determinant" (make 'det))
  ("choice" (make 'choice))
  ("horizontal dots" (insert "<cdots>"))
  ("vertical dots" (insert "<vdots>"))
  ("diagonal dots" (insert "<ddots>"))
  ("upward dots" (insert "<udots>"))

  ("m/n" (english-m))
  ("the" (english-the))

  ;;("more" "var")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speech reductions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-reduce english math
  ("letter a" "a")
  ("letter b" "b")
  ("letter c" "c")
  ("letter d" "d")
  ("letter e" "e")
  ("letter f" "f")
  ("letter g" "g")
  ("letter h" "h")
  ("letter i" "i")
  ("letter j" "j")
  ("letter k" "k")
  ("letter l" "l")
  ("letter m" "m")
  ("letter n" "n")
  ("letter o" "o")
  ("letter p" "p")
  ("letter q" "q")
  ("letter r" "r")
  ("letter s" "s")
  ("letter t" "t")
  ("letter u" "u")
  ("letter v" "v")
  ("letter w" "w")
  ("letter x" "x")
  ("letter y" "y")
  ("letter z" "z")

  ("big" "uppercase")
  ("capital" "uppercase")
  ("small" "lowercase")

  ("the complex" "complex")
  ("the positive integers" "positive integers")
  ("the rationals" "rationals")
  ("the reals" "reals")
  ("the integers" "integers")
  ("rational" "rationals")
  ("rational numbers" "rationals")
  ("real" "reals")
  ("real numbers" "reals")
  
  ("set" "curly brackets")
  ("set of" "curly brackets")
  ("the set" "curly brackets")
  ("the set of" "curly brackets")

  ("equals" "equal")
  ("equal to" "equal")
  ("is equal to" "equal")

  ("not equal to" "not equal")
  ("is not equal" "not equal")
  ("is not equal to" "not equal")
  ("unequal" "not equal")
  ("unequal to" "not equal")
  ("is unequal to" "not equal")
  ("different" "not equal")
  ("different from" "not equal")
  ("is different from" "not equal")

  ("inferior" "less")
  ("inferior to" "less")
  ("inferior" "less")
  ("is less" "less")
  ("less than" "less")
  ("less or equal" "less equal")

  ("superior" "greater")
  ("superior to" "greater")
  ("is greater" "greater")
  ("greater than" "greater")
  ("greater or equal to" "greater equal")

  ("is a subset of" "subset")
  ("contains" "superset")
  ("into" "right arrow")
  
  ("there exists a" "exists")
  ("there exists an" "exists")
  ("there exists" "exists")
  ("if and only if" "equivalent")

  ("exp" "exponential")
  ("log" "logarithm")
  ("the exponential of" "exponential of")
  ("the logarithm of" "logarithm of")
  ("the sine of" "sine of")
  ("the cosine of" "cosine of")
  ("the tangent of" "tangent of")
  ("the square root of" "square root of")

  ("etcetera" "dots")
  ("dot dot dot" "dots")
  ("plus plus" "plus dots plus")
  ("times times" "times dots times")
  ("comma comma" "comma dots comma")
  ("colon colon" "colon dots colon")
  ("semicolon semicolon" "semicolon dots semicolon")
  ("tensor tensor" "tensor dots tensor")

  ("similar to" "similar")
  ("is similar to" "similar")
  ("equivalent" "similar")
  ("equivalent to" "equivalent")
  ("is equivalent to" "equivalent")
  ("asymptotic to" "asymptotic")
  ("is asymptotic to" "asymptotic")
  ("approximately" "approx")
  ("approximately equal" "approx")
  ("approximately equal to" "approx")
  ("is approximately" "approximately")
  ("isomorphic to" "isomorphic")
  ("is isomorphic to" "isomorphic")
  
  ("negligible with respect to" "negligible")
  ("is negligible with respect to" "negiglible")
  ("is strictly dominated by" "negligible")
  ("dominated by" "dominated")
  ("is dominated by" "dominated")
  ("dominates" "dominates")

  ("tilde" "tilda")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust wrongly recognized words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust english math
  ("hey" "a")
  ("be" "b")
  ("bee" "b")
  ("see" "c")
  ("siri" "c")
  ("de" "d")
  ("he" "e")
  ("eat" "e")
  ("each" "e")
  ("if" "f")
  ("yes" "f")
  ("ji" "g")
  ("age" "h")
  ("edge" "h")
  ("hi" "i")
  ("eye" "i")
  ("eyes" "i")
  ("either" "i")
  ("jay" "j")
  ("ok" "k")
  ("kate" "k")
  ("katie" "k")
  ("que" "k")
  ("al" "l")
  ("el" "l")
  ("i'll" "l")
  ("em" "m/n")
  ("an" "n")
  ("oh" "o")
  ("all" "o")
  ("old" "o")
  ("piece" "p")
  ("pee" "p")
  ("pete" "p")
  ("queue" "q")
  ("cute" "q")
  ("are" "r")
  ("our" "r")
  ("cute" "q")
  ;;("yes" "s")
  ("ass" "s")
  ("tea" "t")
  ("you" "u")
  ("ve" "v")
  ("ask" "x")
  ("ex" "x")
  ("why" "y")
  
  ("l5" "alpha")
  ("beata" "beta")
  ("peta" "beta")
  ("vita" "beta")
  ("bita" "beta")
  ("beat a" "beta")
  ("gonna" "gamma")
  ("got my" "gamma")
  ("add simon" "epsilon")
  ("add cylon" "epsilon")
  ("abseiling" "epsilon")
  ("simon" "epsilon")
  ("atta" "eta")
  ("eat a" "eta")
  ("cheetah" "theta")
  ("tita" "theta")
  ("cita" "theta")
  ("theater" "theta")
  ("tata" "theta")
  ("santa" "theta")
  ("yota" "iota")
  ("utah" "iota")
  ("capela" "kappa")
  ("copper" "kappa")
  ("lanta" "lambda")
  ("lampa" "lambda")
  ("lamp that" "lambda")
  ("llamada" "lambda")
  ("mantha" "lambda")
  ("manta" "lambda")
  ("mamta" "lambda")
  ("mute" "mu")
  ("moo" "mu")
  ("mood" "mu")
  ("no" "nu")
  ("new" "nu")
  ;;("sigh" "xi")
  ("all my chrome" "omicron")
  ("oh my chrome" "omicron")
  ("omi cron" "omicron")
  ("bye" "pi")
  ("pie" "pi")
  ("row" "rho")
  ("sick my" "sigma")
  ("suck my" "sigma")
  ("sync my" "sigma")
  ("ciao" "tau")
  ("towel" "tau")
  ("tall" "tau")
  ("tao" "tau")
  ("toe" "tau")
  ("town" "tau")
  ("absolem" "upsilon")
  ("absalom" "upsilon")
  ("upside down" "upsilon")
  ("fight" "phi")
  ("fine" "phi")
  ("sigh" "psi")
  ("size" "psi")
  ("kai" "chi")
  ("kite" "chi")
  ("kind" "chi")
  ("sky" "chi")
  ("oh my god" "omega")

  ("ar" "a r")
  ("pique" "p k")
  ("the u" "d u")
  ("the v" "d v")
  ("the w" "d w")
  ("the x" "d x")
  ("the y" "d y")
  ("the z" "d z")
  ("buy ice" "pi i")
  ("bye bye" "pi i")
  ("bye-bye" "pi i")
  ("bye i" "pi i")
  ("to fight i" "two pi i")
  ("to buy i" "two pi i")
  
  ("biko" "big o")
  
  ("bolt" "bold")
  ("both" "bold")
  ("baltar" "bold r")
  ("double stroke" "blackboard bold")
  ("blackboard bolt" "blackboard bold")
  ("blackboard both" "blackboard bold")
  ("fractura" "fraktur")
  ("fracture" "fraktur")
  ("kelly graphic" "calligraphic")
  ("sensory" "sans serif")
  ("sounds arif" "sans serif")
  ("some sarife" "sans serif")
  ("salsarita" "sans serif")
  ("salsaritas" "sans serif")
  ("san sarith" "sans serif")
  ("sans sarith" "sans serif")
  ("sounds sarith" "sans serif")
  ("sounds to reefs" "sans serif")
  ("sounds to reach" "sans serif")
  
  ("does" "plus")
  ("play" "plus")
  ("mine is" "minus")
  ("time" "times")
  ("and times" "n times")
  ("endtimes" "n times")
  
  ("asap" "a sub")
  ("sab" "sub")
  ("sup" "sub")
  ("sub and" "sub n")
  ("sub end" "sub n")
  ("sub in" "sub n")
  ("geez up" "g sub")
  ("pizza" "p sub")
  ("power and" "power n")
  ("power in" "power n")
  ("empower" "m power")
  ("in power" "n power")
  ("is power" "e power")
  ("is superscript" "e superscript")
  ("subscripts" "subscript")
  ("subscribe" "subscript")
  ("subscribed" "subscript")
  ("exponent" "superscript")
  ("squared" "square")
  ("exquire" "x square")
  ("by square" "pi square")
  ("cubed" "cube")

  ("cosign" "cosine")

  ("dick" "big")
  ("pick" "big")

  ("call ma" "comma")
  ("call matt" "comma")
  ("call mark" "comma")
  ("call mom" "comma")
  ("docs" "dots")
  ("dutch" "dots")
  ("ducks" "dots")

  ("off" "of")
  ("fof" "f of")
  ("fof" "f of")
  ("find of" "phi of")
  ("rackets" "brackets")

  ("sam" "sum")
  ("some" "sum")
  ("sum 4" "sum for")
  ("sum four" "sum for")
  ("enter girl" "integral")

  ("offer" "over")
  ("ayo for" "a over")
  ("do for" "d over")
  ("fo for" "f over")
  ("geo for" "g over")
  ("ho for" "h over")
  ("iovu" "i over")
  ("jo for" "j over")
  ("ko for" "k over")
  ("lo for" "l over")
  ("i'm over" "m over")
  ("eno for" "n over")
  ("po for" "p over")
  ("so for" "s over")
  ("to for" "t over")
  ("vo for" "v over")
  ("exo for" "x over")
  ("roll over" "rho over")
  ("find over" "phi over")
  ("fine over" "phi over")
  ("fios for" "phi over")
  ("overby" "over b")
  ("oversee" "over c")

  ("smaller" "less")
  ("bigger" "greater")
  ("larger" "greater")
  ("less then" "less than")
  ("greater then" "greater than")
  ("difference" "different")
  ("and less" "n less")
  ("and greater" "n greater")

  ("reels" "reals")
  ("it's" "is")

  ("lock" "log")
  ("luck" "log")
  ("look" "log")
  ("clock" "log")
  ("log and" "log n")
  ("log in" "log n")
  ("unlock" "n log")
  ("timeslot" "times log")

  ("10 sir" "tensor")
  ("dancer" "tensor")

  ("white" "wide")
  ("head" "hat")
  ("had" "hat")
  ("hit" "hat")
  ("hunt" "hat")
  ("hurt" "hat")
  ("light head" "wide hat")
  ("why hat" "wide hat")
  ("why tilda" "wide tilda")
  ("why tilde" "wide tilda")
  ("why bar" "wide bar")
  ("my hat" "wide hat")
  ("my tilda" "wide tilda")
  ("my tilde" "wide tilda")
  ("my bar" "wide bar")
  ("the hat" "d hat")
  ("he had" "e hat")
  ("jihad" "g hat")
  ("ipad" "i hat")
  ("in hat" "n hat")
  ("and hat" "n hat")
  ("asshat" "s hat")
  ("you had" "u hat")
  ("we hat" "v hat")
  ("we had" "v hat")
  ("cupperhead" "kappa hat")
  ("rohat" "rho hat")
  ("find hat" "phi hat")
  ("sign hat" "psi hat")
  ("side hat" "psi hat")
  ("in tilda" "n tilda")
  ("and tilda" "n tilda")
  ("find tilda" "phi tilda")
  ("sign tilda" "psi tilda")
  ("side tilda" "psi tilda")
  ("bieber" "b bar")
  ("the bar" "d bar")
  ("ebar" "e bar")
  ("aybar" "i bar")
  ("al-bahr" "l bar")
  ("embar" "m bar")
  ("end bar" "n bar")
  ("in bar" "n bar")
  ("and bar" "n bar")
  ("p b r" "p bar")
  ("q b r" "q bar")
  ("cuba" "q bar")
  ("eubar" "u bar")
  ("rebar" "v bar")
  ("laptop bar" "lambda bar")
  ("rober" "rho bar")
  ("robert" "rho bar")
  ("robar" "rho bar")
  ("rovar" "rho bar")
  ("tovar" "tau bar")
  ("find bar" "phi bar")
  ("sign bar" "psi bar")
  ("side bar" "psi bar")
  
  ("write" "right")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further, more dangerous adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust english math
  ("8 plus" "a plus")
  ("8 minus" "a minus")
  ("8 times" "a times")
  ("plus 8" "plus a")
  ("minus 8" "minus a")
  ("times 8" "times a")
  ("plus eight" "plus a")
  ("minus eight" "minus a")
  ("times eight" "times a")
  ("eight plus" "a plus")
  ("eight minus" "a minus")
  ("eight times" "a times")
  ("plus eight" "plus eight")
  ("minus eight" "minus eight")
  ("times eight" "times eight")
  ("times 3" "times d")
  ("times three" "times d")
  ("28" "2 a")
  ("38" "3 a")
  ("48" "4 a")
  ("58" "5 a")
  ("81" "a 1")
  ("82" "a 2")
  ("83" "a 3")
  ("84" "a 4")
  ("85" "a 5")
  ("40" "for all")
  ("408" "for all a")
  ("search that" "such that")
  ("power a power" "power e power")
  ("a power e" "e power e")
  ("a.m." "a n")
  ("p.m." "p n")
  ("8 hat" "a hat")
  ("3 hat" "t hat")
  ("5 hat" "phi hat")
  ("8 tilda" "a tilda")
  ("3 tilda" "t tilda")
  ("5 tilda" "phi tilda")
  ("8 bar" "a bar")
  ("3 bar" "t bar")
  ("5 bar" "phi bar")
  ("sign" "sine")
  ("and log" "n log")
  ("end" "and")
  )
