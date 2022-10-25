
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-speech-fr.scm
;; DESCRIPTION : mathematical editing using French speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-speech-fr)
  (:use (math math-speech)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sanitize input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table french-numbers
  ("0" "zéro") ("1" "un") ("2" "deux") ("3" "trois") ("4" "quatre")
  ("5" "cinq") ("6" "six") ("7" "sept") ("8" "huit") ("9" "neuf"))

(define-table french-ambiguate
  ("deux" "deux/de"))

(define (string-table-replace s t)
  (with repl (lambda (x) (with y (ahash-ref t x) (if y (car y) x)))
    (with l (string-decompose s " ")
      (string-recompose (map repl l) " "))))

(tm-define (speech-pre-sanitize lan s)
  (:require (== lan 'french))
  (clean-quotes s))

(tm-define (speech-sanitize lan mode s)
  (:require (and (== lan 'french) (== mode 'math)))
  (set! s (locase-all s))
  (set! s (list->tmstring (clean-letter-digit (tmstring->list s))))
  (set! s (clean-quotes s))
  (set! s (string-replace s "+" " plus "))
  (set! s (string-replace-trailing s "-" " moins "))
  (set! s (string-replace s "<times>" " fois "))
  (set! s (string-replace-trailing s "." " point "))
  (set! s (string-replace s "," " virgule "))
  (set! s (string-replace s ":" " double points "))
  (set! s (string-replace s ";" " point virgule "))
  (set! s (string-replace s "^" " chapeau "))
  (set! s (string-replace s "~" " tilde "))
  (set! s (string-replace s "<ldots>" " points "))
  (set! s (string-replace s "<cdots>" " points "))
  (set! s (string-table-replace s french-numbers))
  (set! s (string-table-replace s french-ambiguate))
  (set! s (string-replace s "  " " "))
  (set! s (string-replace s "  " " "))
  (set! s (tm-string-trim-both s))
  (set! s (french-normalize 'math s))
  s)

(speech-collection dont-break french
  "ah" "an" "ar" "au" "el" "en" "es" "et" "ex" "ét"
  "il" "oh" "os" "ou" "un"
  "ma" "ta" "za" "de" "he" "le" "se" "te"
  "ai" "bi" "hi" "ji" "pi" "si" "ti" "xi"
  "ho" "no" "to" "du" "mu" "nu" "tu" "ou" "sy")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normalization of French text (singular/plural, masculin/feminin, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (french-resuffix* mode s suf1 suf2)
  (and (string-ends? s suf1)
       (let* ((l1 (string-length s))
              (l2 (string-length suf1))
              (r  (string-append (substring s 0 (- l1 l2)) suf2)))
         (and (>= l1 (+ l2 3))
              (raw-speech-accepts? 'french mode r)
              r))))

(define (french-resuffix mode s suf1 suf2)
  (or (french-resuffix* mode s suf1 suf2)
      (french-resuffix* mode s suf2 suf1)))

(define (french-normalize-one mode s)
  (or (and (raw-speech-accepts? 'french mode s) s)
      (french-resuffix mode s "e" "")
      (french-resuffix mode s "s" "")
      (french-resuffix mode s "es" "")
      (french-resuffix mode s "x" "")
      (french-resuffix mode s "aux" "al")
      (french-resuffix mode s "ère" "er")
      (french-resuffix mode s "ères" "ers")
      (french-resuffix mode s "ères" "er")
      (french-resuffix mode s "ve" "f")
      (french-resuffix mode s "ves" "f")
      (french-resuffix mode s "ves" "fs")
      (french-resuffix mode s "er" "")
      (french-resuffix mode s "er" "e")
      (french-resuffix mode s "er" "es")
      (french-resuffix mode s "er" "ent")
      (french-resuffix mode s "er" "é")
      (french-resuffix mode s "er" "ée")
      (french-resuffix mode s "er" "és")
      (french-resuffix mode s "er" "ées")
      (french-resuffix mode s "ir" "")
      (french-resuffix mode s "ir" "e")
      (french-resuffix mode s "ir" "es")
      (french-resuffix mode s "ir" "ent")
      (french-resuffix mode s "tir" "s")
      (french-resuffix mode s "re" "")
      (french-resuffix mode s "re" "s")
      (french-resuffix mode s "re" "ent")
      (french-resuffix mode s "qu'" "que")
      s))

(define (french-normalize-compute mode s)
  (let* ((l (string-decompose s " "))
         (r (map (cut french-normalize-one mode <>) l)))
    (string-recompose r " ")))

(define french-normal-table (make-ahash-table))

(tm-define (french-normalize mode s)
  (with key (list mode s)
    (when (not (ahash-ref french-normal-table key))
      (ahash-set! french-normal-table key (french-normalize-compute mode s)))
    (ahash-ref french-normal-table key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering mathematical symbols via French speech
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-symbols french
  ("zéro" "0")
  ("un" "1")
  ("deux" "2")
  ("trois" "3")
  ("quatre" "4")
  ("cinq" "5")
  ("six" "6")
  ("sept" "7")
  ("huit" "8")
  ("neuf" "9")
  ("dix" "10")
  ("cent" "100")
  ("mille" "1000")
  ("million" "1000000")
  ("milliard" "1000000000")

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
  ("epsilon" "<epsilon>")
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
  ("phi" "<phi>")
  ("psi" "<psi>")
  ("chi" "<chi>")
  ("omega" "<omega>")

  ("constante e" "<mathe>")
  ("constante i" "<mathi>")
  ("constante pi" "<mathpi>")
  ("constante gamma" "<mathgamma>")
  ("constante d'euler" "<mathgamma>")

  ("infini" "<infty>")
  ("complexes" "<bbb-C>")
  ("entiers positifs" "<bbb-N>")
  ("rationnels" "<bbb-Q>")
  ("réels" "<bbb-R>")
  ("entiers" "<bbb-Z>")

  ("plus" "+")
  ("moins" "-")
  ("fois" "*")
  ("croix" "<times>")
  ("appliquer" " ")
  ("espace" " ")
  ("rond" "<circ>")
  ("tenseur" "<otimes>")
  ("factoriel" "!")

  ("égal" "=")
  ("non égal" "<neq>")
  ("inférieur" "<less>")
  ("inférieur égal" "<leqslant>")
  ("supérieur" "<gtr>")
  ("supérieur égal" "<geqslant>")
  ("dans" "<in>")
  ("contient l'élément" "<ni>")
  ("sous ensemble" "<subset>")
  ("sur ensemble" "<supset>")

  ("similaire" "<sim>")
  ("asymptotique" "<asymp>")
  ;;("approx" "<approx>")
  ("isomorphe" "<cong>")
  ("négligeable" "<prec>")
  ("dominé" "<preccurlyeq>")
  ("domine" "<succcurlyeq>")
  ("domine strictement" "<succ>")

  ("flèche" "<rightarrow>")
  ("longue flèche" "<rightarrow>")

  ("pour tout" "<forall>")
  ("existe" "<exists>")
  ("ou" "<vee>")
  ("et" "<wedge>")
  ("implique" "<Rightarrow>")
  ("équivaut" "<Leftrightarrow>")

  ("point" ".")
  ("virgule" ",")
  ("double point" ":")
  ("point virgule" ";")
  ("point d'exclamation" "!")
  ("point d'interrogation" "?")
  ("." ".")
  ("," ",")
  (":" ":")
  (";" ";")
  ("!" "!")
  ("?" "?")
  ("tel que" "<suchthat>")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More complex mathematical speech commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-map french math
  ("grand" (speech-alter-letter :big))
  ("petit" (speech-alter-letter :small))
  ("majuscule" (speech-alter-letter* :big))
  ("minuscule" (speech-alter-letter* :small))
  ("gras" (speech-alter-letter* :bold))
  ("medium" (speech-alter-letter* :medium))
  ("droit" (speech-alter-letter* :up))
  ("italique" (speech-alter-letter* :it))
  ("calligraphique" (speech-alter-letter* :cal))
  ("fraktur" (speech-alter-letter* :frak))
  ("tableau noir gras" (speech-alter-letter* :bbb))
  ("normal" (speech-alter-letter* :normal))
  ("sans serif" (speech-alter-letter* :ss))
  ("machine à écrire" (speech-alter-letter* :tt))
  ("opérateur" (speech-operator))

  ("facteur" (speech-factor))
  ("inverse" (speech-insert-superscript "-1"))
  ("carré" (speech-insert-superscript "2"))
  ("cube" (speech-insert-superscript "3"))
  ("indice" (speech-subscript))
  ("puissance" (speech-superscript))
  ("exposant" (make 'rsup))

  ("prime" (make-rprime "'"))
  ("double prime" (make-rprime "'") (make-rprime "'"))
  ("triple prime" (make-rprime "'") (make-rprime "'") (make-rprime "'"))

  ("chapeau" (speech-accent "^"))
  ("tilde" (speech-accent "~"))
  ("barre" (speech-accent "<bar>"))
  ("large chapeau" (speech-wide "^"))
  ("large tilde" (speech-wide "~"))
  ("large barre" (speech-wide "<bar>"))
  ("dessous" (speech-under))

  ("de" (speech-of))
  ("ouvrir" (speech-open "(" ")"))
  ("fermer" (speech-close))
  ("parenthèses" (speech-brackets "(" ")"))
  ("crochets" (speech-brackets "[" "]"))
  ("accolades" (speech-brackets "{" "}"))
  ("chevrons" (speech-brackets "<langle>" "<rangle>"))
  ("partie entière" (speech-brackets "<lfloor>" "<rfloor>"))
  ("ouvrir parenthèses" (speech-open "(" ")"))
  ("ouvrir crochets" (speech-open "[" "]"))
  ("ouvrir accolades" (speech-open "{" "}"))
  ("ouvrir chevrons" (speech-open "<langle>" "<rangle>"))
  ("ouvrir partie entière" (speech-open "<lfloor>" "<rfloor>"))

  ("(" (speech-open "(" ")"))
  ("[" (speech-open "[" "]"))
  ("{" (speech-open "{" "}"))
  (")" (speech-close))
  ("]" (speech-close))
  ("}" (speech-close))
  
  ("arc cos" (speech-insert-operator "arccos"))
  ("arc sin" (speech-insert-operator "arcsin"))
  ("arc tan" (speech-insert-operator "arctan"))
  ("arg" (speech-insert-operator "arg"))
  ("cos" (speech-insert-operator "cos"))
  ("deg" (speech-insert-operator "deg"))
  ("det" (speech-insert-operator "det"))
  ("dim" (speech-insert-operator "dim"))
  ("exp" (speech-insert-operator "exp"))
  ("gcd" (speech-insert-operator "gcd"))
  ("log" (speech-insert-operator "log"))
  ("hom" (speech-insert-operator "hom"))
  ("inf" (speech-insert-operator "inf"))
  ("ker" (speech-insert-operator "ker"))
  ("lcm" (speech-insert-operator "lcm"))
  ("lim" (speech-insert-operator "lim"))
  ("lim inf" (speech-insert-operator "liminf"))
  ("lim sup" (speech-insert-operator "limsup"))
  ("ln" (speech-insert-operator "ln"))
  ("log" (speech-insert-operator "log"))
  ("max" (speech-insert-operator "max"))
  ("min" (speech-insert-operator "min"))
  ("Pr" (speech-insert-operator "Pr"))
  ("sin" (speech-insert-operator "sin"))
  ("supremum" (speech-insert-operator "sup"))
  ("tan" (speech-insert-operator "tan"))
  
  ("plus points plus" (speech-dots "+" "<cdots>"))
  ("moins points moins" (speech-dots "-" "<cdots>"))
  ("fois points fois" (speech-dots "*" "<cdots>"))
  ("virgule points virgule" (speech-dots "," "<ldots>"))
  ("et points et" (speech-dots "<wedge>" "<cdots>"))
  ("ou points ou" (speech-dots "<vee>" "<cdots>"))
  ("égal points égal" (speech-dots "=" "<cdots>"))
  ("similaire points similaire" (speech-dots "<sim>" "<cdots>"))
  ("inférieur points inférieur" (speech-dots "<less>" "<cdots>"))
  ("inférieur égal points inférieur égal" (speech-dots "<leqslant>" "<cdots>"))
  ("supérieur points supérieur" (speech-dots "<gtr>" "<cdots>"))
  ("supérieur égal points supérieur égal" (speech-dots "<geqslant>" "<cdots>"))
  ("rond points rond" (speech-dots "<circ>" "<cdots>"))
  ("tenseur points tenseur" (speech-dots "<otimes>" "<cdots>"))

  ("somme" (math-big-operator "sum"))
  ("produit" (math-big-operator "prod"))
  ("intégrale" (math-big-operator "int"))
  ("intégrale de contours" (math-big-operator "oint"))
  ("intégrale double" (math-big-operator "iint"))
  ("intégral triple" (math-big-operator "iiint"))
  ("pour" (speech-for))
  ("jusqu'à" (speech-until))

  ("racine carrée" (speech-sqrt))
  ("racine carrée de" (speech-sqrt-of))
  ("racine carrée deux/de" (speech-sqrt-of))
  ("fraction" (speech-fraction))
  ("sur" (speech-over))
  ("numérateur" (go-to-fraction :numerator))
  ("dénominateur" (go-to-fraction :denominator))

  ("matrice" (make 'matrix))
  ("déterminant" (make 'det))
  ("choix" (make 'choice))
  ("points horizontaux" (insert "<cdots>"))
  ("points verticaux" (insert "<vdots>"))
  ("points diagonaux" (insert "<ddots>"))
  ("points montants" (insert "<udots>"))

  ;;("more" "var")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speech reductions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-reduce french math
  ("lettre a" "a")
  ("lettre b" "b")
  ("lettre c" "c")
  ("lettre d" "d")
  ("lettre e" "e")
  ("lettre f" "f")
  ("lettre g" "g")
  ("lettre h" "h")
  ("lettre i" "i")
  ("lettre j" "j")
  ("lettre k" "k")
  ("lettre l" "l")
  ("lettre m" "m")
  ("lettre n" "n")
  ("lettre o" "o")
  ("lettre p" "p")
  ("lettre q" "q")
  ("lettre r" "r")
  ("lettre s" "s")
  ("lettre t" "t")
  ("lettre u" "u")
  ("lettre v" "v")
  ("lettre w" "w")
  ("lettre x" "x")
  ("lettre y" "y")
  ("lettre z" "z")

  ("la constante" "constante")

  ("les complexes" "complexes")
  ("les entiers positifs" "entiers positifs")
  ("les rationnels" "rationnels")
  ("les réels" "réels")
  ("les entiers" "entiers")
  ("nombres complexes" "complexes")
  ("nombres entiers positifs" "entiers positifs")
  ("nombres rationnels" "rationnels")
  ("nombres réels" "réels")
  ("nombres entiers" "entiers")

  ("partie entière de" "partie entière")
  ("partie entière deux/de" "partie entière")
  ("fermer parenthèses" "fermer")
  ("fermer crochets" "fermer")
  ("fermer accolades" "fermer")
  ("fermer chevrons" "fermer")
  ("fermer partie entière" "fermer")

  ("ouvrir le" "ouvrir")
  ("ouvrir la" "ouvrir")
  ("ouvrir les" "ouvrir")
  ("ouvrir un" "ouvrir")
  ("ouvrir une" "ouvrir")
  ("ouvrir des" "ouvrir")
  ("fermer le" "fermer")
  ("fermer la" "fermer")
  ("fermer les" "fermer")
  ("fermer un" "fermer")
  ("fermer une" "fermer")
  ("fermer des" "fermer")

  ("ensemble" "accolades")
  ("ensemble de" "accolades")
  ("ensemble deux/de" "accolades")
  ("ensemble des" "accolades")
  ("l'ensemble" "accolades")
  ("l'ensemble de" "accolades")
  ("l'ensemble deux/de" "accolades")
  ("l'ensemble des" "accolades")

  ("égal à" "égal")
  ("est égal à" "égal")

  ("pas égal" "non égal")
  ("pas égal à" "non égal")
  ("n'est pas égal à" "non égal")
  ("différent" "non égal")
  ("différent de" "non égal")
  ("différent deux/de" "non égal")
  ("est différent de" "non égal")
  ("est différent deux/de" "non égal")

  ("plus petit" "inférieur")
  ("plus petit que" "inférieur")
  ("est plus petit" "inférieur")
  ("est plus petit que" "inférieur")
  ("est inférieur" "inférieur")
  ("inférieur à" "inférieur")
  ("inférieur ou égal" "inférieur égal")

  ("plus grand" "supérieur")
  ("plus grand que" "supérieur")
  ("est plus grand" "supérieur")
  ("est plus grand que" "supérieur")
  ("est supérieur" "supérieur")
  ("supérieur à" "supérieur")
  ("supérieur ou égal" "supérieur égal")

  ("est dans" "dans")
  ("sous ensemble de" "sous ensemble")
  ("sous ensemble deux/de" "sous ensemble")
  ("un sous ensemble" "sous ensemble")
  ("est un sous ensemble" "sous ensemble")
  ("contient" "sur ensemble")
  ("vers" "flèche")

  ("il existe" "existe")
  ("existe un" "existe")
  ("si et seulement si" "équivaut")

  ("appliquer à" "appliquer")
  ("la racine carrée" "racine carrée")

  ("arc cosinus" "arc cos")
  ("arc sinus" "arc sin")
  ("arc tangente" "arc tan")
  ("arccosinus" "arc cos")
  ("arcsinus" "arc sin")
  ("arctangente" "arc tan")
  ("argument" "arg")
  ("cosinus" "cos")
  ("degré" "deg")
  ("degrée" "deg")
  ("degrés" "deg")
  ("degrées" "deg")
  ("date" "det")
  ("dette" "det")
  ("dettes" "det")
  ("dimension" "dim")
  ("exponentielle" "exp")
  ("noyau" "ker")
  ("limite" "lim")
  ("limite inférieure" "lim inf")
  ("limite supérieure" "lim sup")
  ("lim inférieure" "lim inf")
  ("lim supérieure" "lim sup")
  ("logarithme naturel" "ln")
  ("logarithme naturels" "ln")
  ("logarithme népérien" "ln")
  ("logarithme" "log")
  ("maximum" "max")
  ("minimum" "min")
  ("probabilité" "Pr")
  ("sinus" "sin")
  ("tangente" "tan")

  ("l'arg" "arg")
  ("l'argument" "arg")
  ("le cos" "cos")
  ("le deg" "deg")
  ("le det" "det")
  ("la dim" "dim")
  ("la dimension" "dim")
  ("l'exp" "exp")
  ("l'exponentielle" "exp")
  ("le gcd" "gcd")
  ("le ker" "ker")
  ("le lcm" "lcm")
  ("la lim" "lim")
  ("la lim inf" "lim inf")
  ("la lim sup" "lim sup")
  ("la limite" "limite")
  ("la limite inférieure" "lim inf")
  ("la limite supérieure" "lim sup")
  ("le ln" "ln")
  ("le log" "log")
  ("le max" "max")
  ("le min" "min")
  ("la Pr" "Pr")
  ("la pr" "Pr")
  ("la probabilité" "Pr")
  ("le sin" "sin")
  ("la tan" "tan")
  ("la tangente" "tan")

  ("etc." "points")
  ("petit point" "points")
  ("petits points" "points")
  ("trois petits points" "points")
  ("point point point" "points")
  ("plus plus" "plus points plus")
  ("fois fois" "fois points fois")
  ("virgule virgule" "virgule points virgule")
  ("rond rond" "rond points rond")
  ("tenseur tenseur" "tenseur points tenseur")
  ("plus jusqu'à" "plus points plus")
  ("fois jusqu'à" "fois points fois")
  ("virgule jusqu'à" "virgule points virgule")
  ("et jusqu'à" "et points et")
  ("ou jusqu'à" "ou points ou")
  ("égal jusqu'à" "égal points égal")
  ("similaire jusqu'à" "similaire points similaire")
  ("inférieur jusqu'à" "inférieur points inférieur")
  ("inférieur égal jusqu'à" "inférieur égal points inférieur égal")
  ("supérieur jusqu'à" "supérieur points supérieur")
  ("supérieur égal jusqu'à" "supérieur égal points supérieur égal")
  ("rond jusqu'à" "rond points rond")
  ("tenseur jusqu'à" "tenseur points tenseur")

  ("similaire à" "similaire")
  ("est similaire à" "similaire")
  ("équivalent" "similaire")
  ("équivalent à" "similaire")
  ("est équivalent" "similaire")
  ("est équivalent à" "similaire")
  ("asymptotique à" "asymptotique")
  ("est asymptotique à" "asymptotique")
  ("isomorphe à" "isomorphe")
  ("est isomorphe à" "isomorphe")
  
  ("négligeable devant" "négligeable")
  ("négligeable par rapport à" "négligeable")
  ("strictement dominé par" "négligeable")
  ("est strictement dominé par" "négligeable")
  ("dominé par" "dominé")
  ("est dominé par" "dominé")

  ("la somme" "somme")
  ("le produit" "produit")
  ("l'intégral" "intégrale")
  ("l'intégrale" "intégrale")
  ("l'infini" "infini")

  ("grand chapeau" "large chapeau")
  ("grand tilde" "large tilde")
  ("grand barre" "large barre")
  ("en dessous" "dessous")

  ("une" "un")
  ("en" "un")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disambiguation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (french-de s)
  (with prev (expr-before-cursor)
    (cond ((not prev) (speech-insert-symbol s))
          ((or (and (string? prev)
                    (== (math-symbol-type prev) "symbol"))
               (tm-in? prev '(math-ss math-tt wide wide*)))
           (if (or (stats-role? `(concat ,prev (rsub ,s)))
                   (stats-role? `(concat ,prev (rsup ,s)))
                   (stats-role? `(concat ,prev (around "(" ,s ")")))
                   (stats-role? `(concat ,prev (around* "(" ,s ")"))))
               (speech-insert-symbol s)
               (speech-of)))
          (else (speech-insert-symbol s)))))

(define (french-m/n)
  (if (stats-prefer? "n" "m" :strong)
      (speech-insert-symbol "n")
      (speech-insert-symbol "m")))

(speech-map french math
  ("deux/de" (french-de "2"))
  ("d/de" (french-de "d"))
  ("t/de" (french-de "t"))

  ("m/n" (french-m/n))
  )

(speech-reduce french math
  ("à" "a")
  ("qu'à" "k")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust wrongly recognized words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust french math
  ("ah" "a")
  ("a-ha" "a")
  ("bae" "b")
  ("bébé" "b")
  ("ben" "b")
  ("c'est" "c")
  ("say" "c")
  ("day" "d")
  ("des" "d")
  ("eux" "e")
  ("œuf" "f")
  ("j'ai" "g")
  ("gay" "g")
  ("ashe" "h")
  ("caca" "k")
  ("car" "k")
  ("cara" "k")
  ("casse" "k")
  ("kaaris" "k")
  ("j'y" "j")
  ("el" "l")
  ("elle" "l")
  ("aime" "m")
  ("aisne" "n")
  ("haine" "n")
  ("and" "n")
  ("au" "o")
  ("beau" "o")
  ("haut" "o")
  ("homepod" "o")
  ("oh" "o")
  ("os" "o")
  ("paye" "p")
  ("pays" "p")
  ("cul" "q")
  ("her" "r")
  ("air" "r")
  ("est-ce" "s")
  ("t'es" "t")
  ("stay" "t")
  ("tes" "t")
  ("eu" "u")
  ("vay" "v")
  ("value" "v")
  ("vert" "v")
  ("vii" "v")
  ("that" "z")

  ("bêta" "beta")
  ("bête un" "beta")
  ("bête a" "beta")
  ("bête à" "beta")
  ("d'état" "beta")
  ("gama" "gamma")
  ("gamin" "gamma")
  ("k ma" "gamma")
  ("za" "zeta")
  ("zêta" "zeta")
  ("est un" "eta")
  ("est a" "eta")
  ("êta" "eta")
  ("état" "eta")
  ("éteins" "eta")
  ("greta" "eta")
  ("pétard" "theta")
  ("teta" "theta")
  ("tête a" "theta")
  ("tête à" "theta")
  ("têtard" "theta")
  ("thêta" "theta")
  ("tintin" "theta")
  ("cap a" "kappa")
  ("cap à" "kappa")
  ("capa" "kappa")
  ("qu'à pas" "kappa")
  ("lakhdar" "lambda")
  ("lanta" "lambda")
  ("lampe a" "lambda")
  ("lampe à" "lambda")
  ("lampe ta" "lambda")
  ("lampe tard" "lambda")
  ("lampe torche" "lambda")
  ("lent a" "lambda")
  ("lent à" "lambda")
  ("mieux" "mu")
  ("mou" "mu")
  ("mûr" "mu")
  ("mus" "mu")
  ("nue" "nu")
  ("nul" "nu")
  ;;("si" "xi")
  ("haut microns" "omicron")
  ("pie" "pi")
  ("pis" "pi")
  ("pipi" "pi")
  ("raux" "rho")
  ("raw" "rho")
  ("rhô" "rho")
  ("rock" "rho")
  ("rome" "rho")
  ("rose" "rho")
  ("row" "rho")
  ("chic ma" "sigma")
  ("sixma" "sigma")
  ("taux" "tau")
  ("to" "tau")
  ("toe" "tau")
  ("tony" "tau")
  ("tôt" "tau")
  ("town" "tau")
  ("options" "upsilon")
  ("upside down" "upsilon")
  ("fille" "phi")
  ("fit" "phi")
  ("psy" "psi")
  ;;("si" "psi")
  ("qui" "chi")
  ("oméga" "omega")

  ("assez" "a c")
  ("an" "a n")

  ("bédé" "b de")
  ("belle des" "b de")
  ("c'est des" "c de")
  ("d'aider" "d de")
  ("j'ai des" "g de")
  ("j'ai des aide" "g de z")
  ("j'aide" "g de")
  ("j'ai dit" "g de")
  ("j'ai w w" "g de w")
  ("idée" "i de")
  ("i de ée" "i de")
  ("j'y d" "j de")
  ("qu'à de" "k de")
  ("qu'à deux/de" "k de")
  ("elle d" "l d/de")
  ("un d" "n de")
  ("cul des" "q de")
  ("que des" "q de")
  ("est-ce d" "s de")
  ("ac/dc" "s de c")
  ("u d x" "u de x")
  ("wylix" "w de x")
  ("qu'à mad" "gamma de")
  ("gamin d" "gamma d/de")
  ("epsilon d" "epsilon d/de")
  ("zeta d" "zeta d/de")
  ("iota a de" "iota de")
  ("iota d" "iota d/de")
  ("kappa d" "kappa d/de")
  ("lambda d" "lambda d/de")
  ("medics" "mu de x")
  ("mieux des aides" "mu de z")
  ("mu d" "mu d/de")
  ("nu d" "nu d/de")
  ("omicron d" "omicron d/de")
  ("au micro-ondes" "omicron de")
  ("road" "rho de")
  ("rodè" "rho de")
  ("rodé" "rho de")
  ("rho de deux/de" "rho de")
  ("site mad" "sigma de")
  ("upsilon d" "upsilon d/de")
  ("chi d" "chi d/de")
  ("omega d" "omega d/de")
  ("décès" "de c")
  ("dédé" "de d")
  ("ducat" "de k")
  ("du cul" "de q")
  ("dévé" "de v")
  ("des aides" "de z")
  ("de êtes" "de eta")
  ("décapas" "de kappa")
  ("the road" "de rho")

  ("constante dans une heure" "constante d'euler")
  ("constante de eyelar" "constante d'euler")

  ("youssef" "plus f")
  ("plusi" "plus i")

  ("rang" "rond")
  ("rend" "rond")
  ("ron" "rond")
  ("run" "rond")
  ("aaron" "a rond")
  ("beyoncé" "b rond c")
  ("irons" "i rond")
  ("giron" "j rond")
  ("caron" "k rond")
  ("aileron" "l rond")
  ("huron" "u rond")
  ("véron" "v rond")
  ("verrons" "v rond")
  ("rondé" "rond d")
  ("ranger" "rond g")
  ("ronger" "rond g")
  ("ron ginny" "rond j")
  ("rongie" "rond j")
  ("ronca" "rond k")
  ("rompez" "rond p")
  ("remonter" "rond t")
  ("ranvée" "rond v")
  ("ranger z" "rond z")

  ("au carré" "carré")
  ("est carré" "carré")
  ("chaos carré" "k carré")
  ("the contours" "de contour")

  ("sûr" "sur")
  ("assure" "a sur")
  ("belle sur" "b sur")
  ("qu'à sur" "k sur")
  ("tête sur" "theta sur")
  ("sur ces" "sur c")
  ("sur ses" "sur c")
  ("sur siri" "sur c")
  ("surgé" "sur g")
  ("sur elle" "sur l")
  ("sur aisne" "sur n")
  ("sur où" "sur o")
  ("sur terre" "sur r")
  ("sur veille" "sur v")
  ("surveille" "sur v")

  ("bars" "barre")
  ("bar" "barre")
  ("bahr" "barre")
  ("var" "barre")
  ("chapo" "chapeau")
  ("chapeaux" "chapeau")
  ("quatre chapeau" "k chapeau")
  ("ou chapeau" "u chapeau")
  ("veille chapeau" "v chapeau")
  ("vieille chapeau" "v chapeau")
  ("six chapeau" "psi chapeau")
  ("bathilde" "b tilde")
  ("the tilde" "e tilde")
  ("ou tilde" "o tilde")
  ("est tilde" "s tilde")
  ("ti tilde" "t tilde")
  ("utile" "u tilde")
  ("utilité" "u tilde")
  ("veille tilde" "v tilde")
  ("vieille tilde" "v tilde")
  ("gars mathilde" "gamma tilde")
  ("ibar" "i barre")
  ("gibard" "j barre")
  ("khabar" "k barre")
  ("mbar" "m barre")
  ("au bar" "o barre")
  ("herbart" "r barre")
  ("esbart" "s barre")
  ("hubbard" "u barre")
  ("vieil bar" "w barre")
  ("lampe tabar" "lambda barre")
  ("bibar" "pi barre")
  ("bibard" "pi barre")
  ("tibar" "pi barre")
  ("tibard" "pi barre")
  ("pivar" "pi barre") 
  ("pivard" "pi barre")
  ("robar" "rho barre")
  ("robart" "rho barre")

  ("un 10" "indice")
  ("un dix" "indice")

  (". diagonaux" "points diagonaux")
  (". montant" "points montants")

  ("telle" "tel")
  ("tel qu'" "tel que")
  ("a-t-elle" "a tel")

  ("the" "de")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further, more dangerous adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust french math
  ("six barre" "psi barre")
  ("d' sur" "delta sur")
  )
