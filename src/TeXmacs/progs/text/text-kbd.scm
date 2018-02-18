
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-kbd.scm
;; DESCRIPTION : keystrokes in text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-kbd)
  (:use (generic generic-kbd)
	(utils edit auto-close)
	(text text-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special symbols in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("\"" (insert-quote))
  ("\" var" "\"")
  ("- var" (make 'nbhyph))
  ("<" "<less>")
  (">" "<gtr>")
  ("< var" "")
  ("> var" "")
  ("`" "<#2018>")
  ("'" (insert-apostrophe #f))
  ("` var" "`")
  ("' var" (insert-apostrophe #t))

  ("< <" "")
  ("> >" "")
  ("' '" "")
  ("` `" "")
  (", ," "")
  ("- -" "")
  ("- - -" "")
  ("< < var" "<less><less>")
  ("> > var" "<gtr><gtr>")
  ("' ' var" "''")
  ("` ` var" "``")
  ("- - var" "--")
  ("- - - var" "---")

  ("space var" (make 'nbsp))
  ("space var var" (make-space "1em"))
  ("_" "_")
  ("_ var" (make-script #f #t))
  ("^" "^")
  ("^ var" (make-script #t #t))
  ("accent:deadhat var" (make-script #t #t))
  ("sz" "ˇ")

  ("text:symbol s" "ˇ")
  ("text:symbol S" "ﬂ")
  ("text:symbol a" "Ê")
  ("text:symbol a e" "Ê")
  ("text:symbol o" "¯")
  ("text:symbol o e" "˜")
  ("text:symbol A" "∆")
  ("text:symbol A E" "∆")
  ("text:symbol O" "ÿ")
  ("text:symbol O E" "◊")
  ("text:symbol !" "Ω")
  ("text:symbol ?" "æ")
  ("text:symbol p" "ü")
  ("text:symbol P" "ø")
  ("text:symbol m" (make 'masculine))
  ("text:symbol M" (make 'varmasculine))
  ("text:symbol f" (make 'ordfeminine))
  ("text:symbol F" (make 'varordfeminine))

  ("accent:tilde" "~")
  ("accent:tilde space" "~")
  ("accent:tilde A" "√")
  ("accent:tilde N" "—")
  ("accent:tilde O" "’")
  ("accent:tilde a" "„")
  ("accent:tilde n" "Ò")
  ("accent:tilde o" "ı")

  ("accent:hat" "^")
  ("accent:hat space" "^")
  ("accent:hat A" "¬")
  ("accent:hat E" " ")
  ("accent:hat I" "Œ")
  ("accent:hat O" "‘")
  ("accent:hat U" "€")
  ("accent:hat a" "‚")
  ("accent:hat e" "Í")
  ("accent:hat i" "Ó")
  ("accent:hat o" "Ù")
  ("accent:hat u" "˚")
  ("accent:deadhat" "^")
  ("accent:deadhat space" "^")
  ("accent:deadhat A" "¬")
  ("accent:deadhat E" " ")
  ("accent:deadhat I" "Œ")
  ("accent:deadhat O" "‘")
  ("accent:deadhat U" "€")
  ("accent:deadhat a" "‚")
  ("accent:deadhat e" "Í")
  ("accent:deadhat i" "Ó")
  ("accent:deadhat o" "Ù")
  ("accent:deadhat u" "˚")

  ("accent:umlaut" "")
  ("accent:umlaut space" "")
  ("accent:umlaut A" "ƒ")
  ("accent:umlaut E" "À")
  ("accent:umlaut I" "œ")
  ("accent:umlaut O" "÷")
  ("accent:umlaut U" "‹")
  ("accent:umlaut Y" "ò")
  ("accent:umlaut a" "‰")
  ("accent:umlaut e" "Î")
  ("accent:umlaut i" "Ô")
  ("accent:umlaut o" "ˆ")
  ("accent:umlaut u" "¸")
  ("accent:umlaut y" "∏")

  ("accent:acute" "'")
  ("accent:acute space" "'")
  ("accent:acute A" "¡")
  ("accent:acute C" "Ç")
  ("accent:acute E" "…")
  ("accent:acute I" "Õ")
  ("accent:acute L" "à")
  ("accent:acute N" "ã")
  ("accent:acute O" "”")
  ("accent:acute R" "è")
  ("accent:acute S" "ë")
  ("accent:acute U" "⁄")
  ("accent:acute Y" "›")
  ("accent:acute Z" "ô")
  ("accent:acute a" "·")
  ("accent:acute c" "¢")
  ("accent:acute e" "È")
  ("accent:acute i" "Ì")
  ("accent:acute l" "®")
  ("accent:acute n" "´")
  ("accent:acute o" "Û")
  ("accent:acute r" "Ø")
  ("accent:acute s" "±")
  ("accent:acute u" "˙")
  ("accent:acute y" "˝")
  ("accent:acute z" "π")

  ("accent:grave" "`")
  ("accent:grave space" "`")
  ("accent:grave A" "¿")
  ("accent:grave E" "»")
  ("accent:grave I" "Ã")
  ("accent:grave O" "“")
  ("accent:grave U" "Ÿ")
  ("accent:grave a" "‡")
  ("accent:grave e" "Ë")
  ("accent:grave i" "Ï")
  ("accent:grave o" "Ú")
  ("accent:grave u" "˘")

  ("accent:cedilla" "")
  ("accent:cedilla space" "")
  ("accent:cedilla C" "«")
  ("accent:cedilla S" "ì")
  ("accent:cedilla T" "ï")
  ("accent:cedilla c" "Á")
  ("accent:cedilla s" "≥")
  ("accent:cedilla t" "µ")

  ("accent:breve" "")
  ("accent:breve space" "")
  ("accent:breve A" "Ä")
  ("accent:breve G" "á")
  ("accent:breve a" "†")
  ("accent:breve g" "ß")

  ("accent:check" "")
  ("accent:check space" "")
  ("accent:check C" "É")
  ("accent:check D" "Ñ")
  ("accent:check E" "Ö")
  ("accent:check L" "â")
  ("accent:check N" "å")
  ("accent:check R" "ê")
  ("accent:check S" "í")
  ("accent:check T" "î")
  ("accent:check U" "ó")
  ("accent:check Z" "ö")
  ("accent:check c" "£")
  ("accent:check d" "§")
  ("accent:check e" "•")
  ("accent:check l" "©")
  ("accent:check n" "¨")
  ("accent:check r" "∞")
  ("accent:check s" "≤")
  ("accent:check t" "¥")
  ("accent:check u" "∑")
  ("accent:check z" "∫")

  ("accent:doubleacute" "")
  ("accent:doubleacute space" "")
  ("accent:doubleacute O" "é")
  ("accent:doubleacute U" "ñ")
  ("accent:doubleacute o" "Æ")
  ("accent:doubleacute u" "∂")

  ("accent:abovering" "")
  ("accent:abovering space" "")
  ("accent:abovering A" "≈")
  ("accent:abovering U" "ó")
  ("accent:abovering a" "Â")
  ("accent:abovering u" "∑")

  ("accent:abovedot" "
")
  ("accent:abovedot space" "
")
  ("accent:abovedot Z" "õ")
  ("accent:abovedot I" "ù")
  ("accent:abovedot z" "ª")

  ("accent:ogonek" "")
  ("accent:ogonek space" "")
  ("accent:ogonek a" "°")
  ("accent:ogonek A" "Å")
  ("accent:ogonek e" "¶")
  ("accent:ogonek E" "Ü")

  ("exclamdown" "Ω")
  ("cent" (make 'cent))
  ("sterling" "ø")
  ("currency" (make 'currency))
  ("yen" (make 'yen))
  ("section" "ü")
  ("copyright" (make 'copyright))
  ("copyright var" (make 'copyleft))
  ("guillemotleft" "")
  ("registered" (make 'registered))
  ("degree" (make 'degreesign))
  ("twosuperior" (make 'twosuperior))
  ("threesuperior" (make 'threesuperior))
  ("paragraph" (make 'paragraphsign))
  ("onesuperior" (make 'onesuperior))
  ("guillemotright" "")
  ("onequarter" (make 'onequarter))
  ("onehalf" (make 'onehalf))
  ("threequarters" (make 'threequarters))
  ("questiondown" "æ")
  ("euro" (make 'euro))
  ("masculine" (make 'masculine))
  ("ordfeminine" (make 'ordfeminine))
  ("masculine var" (make 'varmasculine))
  ("ordfeminine var" (make 'varordfeminine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language dependent shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-cyrillic?)
  ("modeswitch"
   (make-with "language" "english")
   (make-with "font" "roman")))

; This breaks ﬂ for german keyboards on all systems.
; Maybe it was an old fix for bug #2092 ? It now seems incorrect.
;(kbd-map
;  (:mode in-german?)
;  ("ﬂ" "ˇ")
;  ("ˇ" "∏"))

(kbd-map
  (:mode in-esperanto?)
  ("c var" "<#109>")
  ("g var" "<#11D>")
  ("h var" "<#125>")
  ("j var" "<#135>")
  ("s var" "<#15D>")
  ("u var" "<#16D>")
  ("C var" "<#108>")
  ("G var" "<#11C>")
  ("H var" "<#124>")
  ("J var" "<#134>")
  ("S var" "<#15C>")
  ("U var" "<#16C>"))

(kbd-map
  (:mode in-chinese?)
  ("a var" "<#0101>")
  ("a var var" "<#00E1>")
  ("a var var var" "<#01CE>")
  ("a var var var var" "<#00E0>")
  ("o var" "<#014D>")
  ("o var var" "<#00F3>")
  ("o var var var" "<#01D2>")
  ("o var var var var" "<#00F2>")
  ("e var" "<#0113>")
  ("e var var" "<#00E9>")
  ("e var var var" "<#011B>")
  ("e var var var var" "<#00E8>")
  ("i var" "<#012B>")
  ("i var var" "<#00ED>")
  ("i var var var" "<#01D0>")
  ("i var var var var" "<#00EC>")
  ("u var" "<#016B>")
  ("u var var" "<#00FA>")
  ("u var var var" "<#01D4>")
  ("u var var var var" "<#00F9>")
  ("v var" "<#00FC>")
  ("v var var" "<#01D6>")
  ("v var var var" "<#01D8>")
  ("v var var var var" "<#01DA>")
  ("v var var var var var" "<#01DC>")
  ("m var" "<#1E3F>")
  ("n var" "<#0144>")
  ("n var var" "<#0148>")
  ("n var var var" "<#01F9>")
  ("< var" "<#300A>")
  ("> var" "<#300B>")
  ("' var" "<#2018>")
  ("` var" "<#2019>")
  ("\" var" "<#201C>")
  ("\" var var" "<#201D>")
  ("[ var" "<#300C>")
  ("[ var var" "<#300E>")
  ("] var" "<#300D>")
  ("] var var" "<#300F>"))

(kbd-map
  (:mode in-hungarian?)
  ("text:symbol O" "é")
  ("text:symbol U" "ñ")
  ("text:symbol o" "Æ")
  ("text:symbol u" "∂")
  ("text:symbol O var" "ÿ")
  ("text:symbol o var" "¯"))

(kbd-map
  (:mode in-spanish?)
  ("°" "Ω")
  ("ø" "æ")
  ("! var" "Ω")
  ("? var" "æ")
  ("! `" "Ω")
  ("? `" "æ")
  ("! accent:grave" "Ω")
  ("? accent:grave" "æ"))

(kbd-map
  (:mode in-polish?)
  ("text:symbol a" "°")
  ("text:symbol A" "Å")
  ("text:symbol c" "¢")
  ("text:symbol C" "Ç")
  ("text:symbol e" "¶")
  ("text:symbol E" "Ü")
  ("text:symbol l" "™")
  ("text:symbol L" "ä")
  ("text:symbol n" "´")
  ("text:symbol N" "ã")
  ("text:symbol o" "Û")
  ("text:symbol O" "”")
  ("text:symbol s" "±")
  ("text:symbol S" "ë")
  ("text:symbol x" "π")
  ("text:symbol X" "ô")
  ("text:symbol z" "ª")
  ("text:symbol Z" "õ")
  ("text:symbol a var" "Ê")
  ("text:symbol A var" "∆")
  ("text:symbol o var" "¯")
  ("text:symbol O var" "ÿ")
  ("text:symbol s var" "ˇ")
  ("text:symbol S var" "ﬂ")
  ("text:symbol z var" "π")
  ("text:symbol Z var" "ô"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Greek symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("alpha" "<#3B1>")
  ("beta" "<#3B2>")
  ("gamma" "<#3B3>")
  ("delta" "<#3B4>")
  ("epsilon" "<#3F5>")
  ("varepsilon" "<#3B5>")
  ("zeta" "<#3B6>")
  ("eta" "<#3B7>")
  ("theta" "<#3B8>")
  ("vartheta" "<#3D1>")
  ("iota" "<#3B9>")
  ("kappa" "<#3BA>")
  ("varkappa" "<#3F0>")
  ("lambda" "<#3BB>")
  ("mu" "<#3BC>")
  ("nu" "<#3BD>")
  ("xi" "<#3BE>")
  ("omicron" "<#3BF>")
  ("pi" "<#3C0>")
  ("varpi" "<#3D6>")
  ("rho" "<#3C1>")
  ("varrho" "<#3F1>")
  ("sigma" "<#3C3>")
  ("varsigma" "<#3C2>")
  ("tau" "<#3C4>")
  ("upsilon" "<#3C5>")
  ("phi" "<#3C6>")
  ("varphi" "<#3D5>")
  ("chi" "<#3C7>")
  ("psi" "<#3C8>")
  ("omega" "<#3C9>")
  ("Alpha" "<#391>")
  ("Beta" "<#392>")
  ("Gamma" "<#393>")
  ("Delta" "<#394>")
  ("Epsilon" "<#395>")
  ("Zeta" "<#396>")
  ("Eta" "<#397>")
  ("Theta" "<#398>")
  ("Iota" "<#399>")
  ("Kappa" "<#39A>")
  ("Lambda" "<#39B>")
  ("Mu" "<#39C>")
  ("Nu" "<#39D>")
  ("Xi" "<#39E>")
  ("Omicron" "<#39F>")
  ("Pi" "<#3A0>")
  ("Rho" "<#3A1>")
  ("Sigma" "<#3A3>")
  ("Tau" "<#3A4>")
  ("Upsilon" "<#3A5>")
  ("Phi" "<#3A6>")
  ("Chi" "<#3A7>")
  ("Psi" "<#3A8>")
  ("Omega" "<#3A9>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overwrite shortcuts which are inadequate in certain contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-variants-disabled?)
  ("- var" (begin (insert "-") (kbd-tab)))
  ("space var" (begin (insert " ") (kbd-tab)))
  ("space var var" (begin (insert " ") (kbd-tab) (kbd-tab))))

(kbd-map
  (:mode in-verbatim?)
  ("space var" (insert-tabstop))
  ("space var var" (begin (insert-tabstop) (insert-tabstop)))
  ("$" (insert "$"))
  ("$ var" (make 'math))
  ("\\" "\\")
  ("\\ var" (make 'hybrid))
  ("\"" "\"")
  ("`" "`")
  ("` var" "<#2018>")
  ("'" "'")
  ("' var" "<#2019>")
  ("< <" "<less><less>")
  ("> >" "<gtr><gtr>")
  ("' '" "''")
  ("` `" "``")
  ("- -" "--")
  ("- - -" "---")
  ("< < var" "")
  ("> > var" "")
  ("' ' var" "")
  ("` ` var" "")
  (", , var" "")
  ("- - var" "")
  ("- - - var" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the text format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("font ^" (make-script #t #t))
  ("font hat" (make-script #t #t))
  ("font _" (make-script #f #t))
  ("font s" (make-with "font-family" "ss"))
  ("font t" (make-with "font-family" "tt"))
  ("font b" (make-with "font-series" "bold"))
  ("font m" (make-with "font-series" "medium"))
  ("font r" (make-with "font-shape" "right"))
  ("font i" (make-with "font-shape" "italic"))
  ("font l" (make-with "font-shape" "slanted"))
  ("font o" (make 'overline))
  ("font p" (make-with "font-shape" "small-caps"))
  ("font u" (make 'underline)))

(kbd-map
  (:profile macos)
  (:mode in-text?)
  ("macos {" (make-line-with "par-mode" "left"))
  ("macos |" (make-line-with "par-mode" "center"))
  ("macos }" (make-line-with "par-mode" "right"))
  ("macos C-{" (make-line-with "par-mode" "justify")))

(kbd-map
  (:profile windows)
  (:mode in-text?)
  ("windows 1" (make-line-with "par-line-sep" "0fn"))
  ("windows 2" (make-line-with "par-line-sep" "1fn"))
  ("windows 5" (make-line-with "par-line-sep" "0.5fn"))
  ("windows l" (make-line-with "par-mode" "left"))
  ("windows e" (make-line-with "par-mode" "center"))
  ("windows r" (make-line-with "par-mode" "right"))
  ("windows j" (make-line-with "par-mode" "justify"))
  ("windows t" (make 'indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard markup in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-std-text?)
  ("text $" (make-equation*))
  ("text &" (make-eqnarray*))

  ("text a" (make 'abbr))
  ("text d" (make-tmlist 'description))
  ("text e" (make-tmlist 'enumerate))
  ("text i" (make-tmlist 'itemize))
  ("text m" (make 'em))
  ("text n" (make 'name))
  ("text p" (make 'samp))
  ("text s" (make 'strong))
  ("text v" (make 'verbatim))
  ("text ;" (make-item))
  ("text 0" (make-section 'chapter))
  ("text 1" (make-section 'section))
  ("text 2" (make-section 'subsection))
  ("text 3" (make-section 'subsubsection))
  ("text 4" (make-section 'paragraph))
  ("text 5" (make-section 'subparagraph))

  ("F5" (make 'em))
  ("F6" (make 'strong))
  ("F7" (make 'verbatim))
  ("F8" (make 'samp))
  ("S-F6" (make 'name)))
