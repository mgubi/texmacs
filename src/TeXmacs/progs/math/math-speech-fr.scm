
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
  (:use (math math-speech-en)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sanitize input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-sanitize lan mode s)
  (:require (and (== lan 'french) (== mode 'math)))
  (set! s (locase-all s))
  (set! s (list->tmstring (clean-letter-digit (tmstring->list s))))
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
  (set! s (string-replace s "  " " "))
  (set! s (string-replace s "  " " "))
  (set! s (tm-string-trim-both s))
  s)

(speech-collection dont-break french
  "ah" "an" "au" "el" "en" "es" "et" "ex" "il" "oh" "ou" "un"
  "ma" "ta" "za" "de" "he" "le" "se" "te"
  "ai" "bi" "hi" "ji" "pi" "si" "ti" "xi"
  "ho" "no" "to" "du" "mu" "nu" "ou")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General speech commands for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-map french math
  ("zéro" (speech-insert-number "0"))
  ("un" (speech-insert-number "1"))
  ("deux" (speech-insert-number "2"))
  ("trois" (speech-insert-number "3"))
  ("quatre" (speech-insert-number "4"))
  ("cinq" (speech-insert-number "5"))
  ("six" (speech-insert-number "6"))
  ("sept" (speech-insert-number "7"))
  ("huit" (speech-insert-number "8"))
  ("neuf" (speech-insert-number "9"))
  ("dix" (speech-insert-number "10"))
  ("cent" (speech-insert-number "100"))
  ("mille" (speech-insert-number "1000"))
  ("million" (speech-insert-number "1000000"))
  ("milliard" (speech-insert-number "1000000000"))

  ("a" (speech-insert-letter "a"))
  ("b" (speech-insert-letter "b"))
  ("c" (speech-insert-letter "c"))
  ("d" (speech-insert-letter "d"))
  ("e" (speech-insert-letter "e"))
  ("f" (speech-insert-letter "f"))
  ("g" (speech-insert-letter "g"))
  ("h" (speech-insert-letter "h"))
  ("i" (speech-insert-letter "i"))
  ("j" (speech-insert-letter "j"))
  ("k" (speech-insert-letter "k"))
  ("l" (speech-insert-letter "l"))
  ("m" (speech-insert-letter "m"))
  ("n" (speech-insert-letter "n"))
  ("o" (speech-insert-letter "o"))
  ("p" (speech-insert-letter "p"))
  ("q" (speech-insert-letter "q"))
  ("r" (speech-insert-letter "r"))
  ("s" (speech-insert-letter "s"))
  ("t" (speech-insert-letter "t"))
  ("u" (speech-insert-letter "u"))
  ("v" (speech-insert-letter "v"))
  ("w" (speech-insert-letter "w"))
  ("x" (speech-insert-letter "x"))
  ("y" (speech-insert-letter "y"))
  ("z" (speech-insert-letter "z"))

  ("alpha" (speech-insert-letter "<alpha>"))
  ("beta" (speech-insert-letter "<beta>"))
  ("gamma" (speech-insert-letter "<gamma>"))
  ("delta" (speech-insert-letter "<delta>"))
  ("epsilon" (speech-insert-letter "<varepsilon>"))
  ("zeta" (speech-insert-letter "<zeta>"))
  ("eta" (speech-insert-letter "<eta>"))
  ("theta" (speech-insert-letter "<theta>"))
  ("iota" (speech-insert-letter "<iota>"))
  ("kappa" (speech-insert-letter "<kappa>"))
  ("lambda" (speech-insert-letter "<lambda>"))
  ("mu" (speech-insert-letter "<mu>"))
  ("nu" (speech-insert-letter "<nu>"))
  ("xi" (speech-insert-letter "<xi>"))
  ("omicron" (speech-insert-letter "<omicron>"))
  ("pi" (speech-insert-letter "<pi>"))
  ("rho" (speech-insert-letter "<rho>"))
  ("sigma" (speech-insert-letter "<sigma>"))
  ("tau" (speech-insert-letter "<tau>"))
  ("upsilon" (speech-insert-letter "<upsilon>"))
  ("phi" (speech-insert-letter "<varphi>"))
  ("psi" (speech-insert-letter "<psi>"))
  ("chi" (speech-insert-letter "<chi>"))
  ("omega" (speech-insert-letter "<omega>"))

  ("constante e" (speech-insert-letter "<mathe>"))
  ("constante i" (speech-insert-letter "<mathi>"))
  ("constante pi" (speech-insert-letter "<mathpi>"))
  ("constante gamma" (speech-insert-letter "<mathgamma>"))
  ("constante d'euler" (speech-insert-letter "<mathgamma>"))

  ("majuscule" (speech-alter-letter :big))
  ("minuscule" (speech-alter-letter :small))
  ("gras" (speech-alter-letter :bold))
  ("droit" (speech-alter-letter :up))
  ("calligraphique" (speech-alter-letter :cal) (speech-alter-letter :big))
  ("fraktur" (speech-alter-letter :frak))
  ("tableau noir gras" (speech-alter-letter :bbb) (speech-alter-letter :big))
  ("sans serif" (speech-alter-letter :ss))
  ("machine à écrire" (speech-alter-letter :tt))
  ("opérateur" (speech-operator))

  ("infini" (speech-insert-symbol "<infty>"))
  ("les complexes" (speech-insert-symbol "<bbb-C>"))
  ("les entiers positifs" (speech-insert-symbol "<bbb-N>"))
  ("les rationnels" (speech-insert-symbol "<bbb-Q>"))
  ("les réels" (speech-insert-symbol "<bbb-R>"))
  ("les entiers" (speech-insert-symbol "<bbb-Z>"))

  ("virgule" (insert ","))
  ("tel que" (insert "<suchthat>"))
  ("point" (insert "."))

  ("plus" (insert "+"))
  ("moins" (insert "-"))
  ("fois" (insert "*"))
  ("facteur" (speech-factor))
  ("croix" (insert "<times>"))
  ("inverse" (speech-insert-superscript "-1"))
  ("carré" (speech-insert-superscript "2"))
  ("cube" (speech-insert-superscript "3"))
  ("indice" (speech-subscript))
  ("puissance" (speech-superscript))
  ("e puissance" (insert "<mathe>") (speech-superscript))
  ;;("subscript" (make 'rsub))
  ("exposant" (make 'rsup))
  ("e exposant" (insert "<mathe>") (make 'rsup))

  ("prime" (make-rprime "'"))
  ("double prime" (make-rprime "'") (make-rprime "'"))
  ("triple prime" (make-rprime "'") (make-rprime "'") (make-rprime "'"))
  ;;("dagger" (make-rprime "<dag>"))
  ;;("star" (make-rprime "<asterisk>"))

  ("chapeau" (speech-accent "^"))
  ("tilde" (speech-accent "~"))
  ("barre" (speech-accent "<bar>"))
  ("large chapeau" (speech-wide "^"))
  ("large tilde" (speech-wide "~"))
  ("large barre" (speech-wide "<bar>"))
  ("chapeau dessous" (speech-accent-under "^"))
  ("tilde dessous" (speech-accent-under "~"))
  ("barre dessous" (speech-accent-under "<bar>"))
  ("large chapeau dessous" (speech-wide-under "^"))
  ("large tilde dessous" (speech-wide-under "~"))
  ("large barre dessous" (speech-wide-under "<bar>"))

  ("de" (speech-of))
  ("parenthèses" (math-bracket-open "(" ")" 'default))
  ("crochets" (math-bracket-open "[" "]" 'default))
  ("accolades" (math-bracket-open "{" "}" 'default))

  ("égal" (insert "="))
  ("non égal" (insert "<neq>"))

  ("inférieur" (insert "<less>"))
  ("inférieur égal" (insert "<leqslant>"))
  ("supérieur" (insert "<gtr>"))
  ("supérieur égal" (insert "<geqslant>"))

  ("dans" (insert "<in>"))
  ("contient l'élément" (insert "<ni>"))
  ("sous ensemble" (insert "<subset>"))
  ("sur ensemble" (insert "<supset>"))
  ("flèche" (insert "<rightarrow>"))
  ("longue flèche" (insert "<rightarrow>"))

  ("pour tout" (insert "<forall>"))
  ("existe" (insert "<exists>"))
  ("ou" (insert "<vee>"))
  ("et" (insert "<wedge>"))
  ("implique" (insert "<Rightarrow>"))
  ("équivaut" (insert "<Leftrightarrow>"))

  ("exponentielle" (speech-insert-operator "exp"))
  ("exponentielle de" (speech-insert-function "exp"))
  ("logarithme" (speech-insert-operator "log"))
  ("logarithme de" (speech-insert-function "log"))
  ("sinus" (speech-insert-operator "sin"))
  ("sinus de" (speech-insert-function "sin"))
  ("cosinus" (speech-insert-operator "cos"))
  ("cosinus de" (speech-insert-function "cos"))
  ("tangente" (speech-insert-operator "tan"))
  ("tangente de" (speech-insert-function "tan"))
  ("factoriel" (insert "!"))
  
  ("plus points plus" (insert "+<cdots>+"))
  ("moins points moins" (insert "-<cdots>-"))
  ("fois points fois" (insert "*<cdots>*"))
  ("virgule points virgule" (insert ",<ldots>,"))
  ("et points et" (insert "<wedge><cdots><wedge>"))
  ("ou points ou" (insert "<vee><cdots><vee>"))
  ("égal points égal" (insert "=<cdots>="))
  ("similar points similar" (insert "<sim><cdots><sim>"))
  ("inférieur points inférieur" (insert "<less><cdots><less>"))
  ("inférieur égal points inférieur égal" (insert "<leqslant><cdots><leqslant>"))
  ("supérieur points supérieur" (insert "<gtr><cdots><gtr>"))
  ("supérieur égal points supérieur égal" (insert "<geqslant><cdots><geqslant>"))
  ("tenseur points tenseur" (insert "<otimes><cdots><otimes>"))

  ("somme" (math-big-operator "sum"))
  ("produit" (math-big-operator "prod"))
  ("intégrale" (math-big-operator "int"))
  ("intégrale de contours" (math-big-operator "oint"))
  ("intégrale double" (math-big-operator "iint"))
  ("intégral triple" (math-big-operator "iiint"))
  ("pour" (speech-for))
  ("jusqu'à" (speech-until))

  ("racine carrée" (make 'sqrt))
  ("racine carrée de" (speech-sqrt))
  ("fraction" (make 'frac))
  ("sur" (speech-over))
  ("numérateur" (go-to-fraction :numerator))
  ("dénominateur" (go-to-fraction :denominator))

  ("similaire" (insert "<sim>"))
  ("asymptotique" (insert "<asymp>"))
  ;;("approx" (insert "<approx>"))
  ("isomorphe" (insert "<cong>"))
  ("négligeable" (insert "<prec>"))
  ("dominé" (insert "<preccurlyeq>"))
  ("domine" (insert "<succcurlyeq>"))
  ("domine strictement" (insert "<succ>"))

  ("matrice" (make 'matrix))
  ("déterminant" (make 'det))
  ("choix" (make 'choice))
  ("points horizontaux" (insert "<cdots>"))
  ("points verticaux" (insert "<vdots>"))
  ("points diagonaux" (insert "<ddots>"))
  ("points montants" (insert "<udots>"))

  ("tenseur" (insert "<otimes>"))
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

  ("grand" "majuscule")
  ("petit" "minuscule")
  ("majuscules" "majuscule")
  ("minuscules" "minuscule")
  ("la constante" "constante")
  
  ("ensemble" "accolades")
  ("ensemble de" "accolades")
  ("ensemble des" "accolades")
  ("l'ensemble" "accolades")
  ("l'ensemble de" "accolades")
  ("l'ensemble des" "accolades")

  ("égal à" "égal")
  ("est égal à" "égal")

  ("pas égal" "non égal")
  ("pas égal à" "non égal")
  ("n'est pas égal à" "non égal")
  ("différent" "non égal")
  ("différent de" "non égal")
  ("est différent de" "non égal")

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
  ("un sous ensemble" "sous ensemble")
  ("est un sous ensemble" "sous ensemble")
  ("contient" "sur ensemble")
  ("vers" "flèche")

  ("il existe" "existe")
  ("existe un" "existe")
  ("si et seulement si" "équivaut")

  ("exp" "exponential")
  ("log" "logarithm")
  ("l'exponentielle de" "exponential of")
  ("le logarithme de" "logarithm of")
  ("le sinus de" "sine of")
  ("le cosinus de" "cosine of")
  ("la tangente de" "tangent of")
  ("la racine carrée de" "square root of")

  ("etc." "points")
  ("petits points" "points")
  ("point point point" "points")
  ("plus plus" "plus points plus")
  ("fois fois" "fois points fois")
  ("virgule virgule" "virgule points virgule")
  ("tenseur tenseur" "tenseur points tenseur")

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
  ("intégral" "intégrale")
  ("l'intégral" "intégrale")
  ("l'intégrale" "intégrale")
  ("l'infini" "infini")

  ("une" "un")
  ("égale" "égal")
  ("petite" "petit")
  ("petits" "petit")
  ("petites" "petit")
  ("grande" "grand")
  ("grands" "grand")
  ("grandes" "grand")
  ("inférieure" "inférieur")
  ("inférieurs" "inférieur")
  ("inférieures" "inférieur")
  ("supérieure" "supérieur")
  ("supérieurs" "supérieur")
  ("supérieures" "supérieur")
  ("équivalente" "équivalent")
  ("équivalents" "équivalent")
  ("équivalentes" "équivalent")
  ("dominée" "dominé")
  ("dominés" "dominé")
  ("dominées" "dominé")

  ("grand chapeau" "large chapeau")
  ("grand tilde" "large tilde")
  ("grand barre" "large barre")
  ("en dessous" "dessous")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust wrongly recognized words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust french math
  ("à" "a")
  ("ah" "a")
  ("bae" "b")
  ("bébé" "b")
  ("ben" "b")
  ("c'est" "c")
  ("say" "c")
  ("day" "d")
  ("des" "d")
  ("œuf" "f")
  ("j'ai" "g")
  ("gay" "g")
  ("qu'à" "k")
  ("caca" "k")
  ("car" "k")
  ("casse" "k")
  ("j'y" "j")
  ("el" "l")
  ("elle" "l")
  ("aime" "m")
  ("haine" "n")
  ("and" "n")
  ("oh" "o")
  ("haut" "o")
  ("beau" "o")
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
  ("that" "z")

  ("bêta" "beta")
  ("bête un" "beta")
  ("gamin" "gamma")
  ("k ma" "gamma")
  ("za" "zeta")
  ("éteins" "eta")
  ("est un" "eta")
  ("est a" "eta")
  ("état" "eta")
  ("capa" "kappa")
  ("lakhdar" "lambda")
  ("lanta" "lambda")
  ("lampe ta" "lambda")
  ("lampe tard" "lambda")
  ("lampe torche" "lambda")
  ("mus" "mu")
  ("nue" "nu")
  ;;("si" "xi")
  ("haut microns" "omicron")
  ("pie" "pi")
  ("pis" "pi")
  ("pipi" "pi")
  ("rose" "rho")
  ("raux" "rho")
  ("row" "rho")
  ("chic ma" "sigma")
  ("to" "tau")
  ("toe" "tau")
  ("tôt" "tau")
  ("taux" "tau")
  ("tony" "tau")
  ("options" "upsilon")
  ("upside down" "upsilon")
  ("fille" "phi")
  ("fit" "phi")
  ("psy" "psi")
  ;;("si" "psi")
  ("qui" "chi")
  ("oméga" "omega")

  ("an" "a n")
  ("assez" "a c")
  ("constante dans une heure" "constante d'euler")
  ("constante de eyelar" "constante d'euler")

  ("sommes" "somme")
  ("au carré" "carré")
  ("est carré" "carré")
  ("chaos carré" "k carré")
  ("the contours" "de contour")

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

  ("point horizontaux" "points horizontaux")
  ("point verticaux" "points verticaux")
  ("point diagonaux" "points diagonaux")
  ("point montant" "points montants")
  (". diagonaux" "points diagonaux")
  (". montant" "points montants")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further, more dangerous adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust french math
  ("puissance deux" "puissance d")
  ("six barre" "psi barre")
  )
