;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My personal initialization file: $TEXMACS_HOME_PATH/progs/my-init-texmacs.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Two new menus in the menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-extra-menu
  (former)
  (=> "AAA" (link aaa-menu))
  (=> "BBB" (link aaa-menu)))

(menu-bind aaa-menu
  ("AAA 111" (insert "Menu: aaa 111"))
  ("AAA 222" (insert "Menu: aaa 222"))
  ---
  (-> "AAA 333" (link file-menu))
  (-> "AAA 444" (link file-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Keyboard shortcuts only valid in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("M-A-w" (insert "Welcome to TeXmacs!"))
  ("e q u ." (insert-go-to "Eq. ()" '(5)) (make 'reference) (insert "equ:"))
  ("f i g ." (insert "Fig. ") (make 'reference) (insert "fig:"))
  ("t a b ." (insert "Table ") (make 'reference) (insert "tab:"))
  ("r e f ." (make 'cite) (insert "bib:")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Keyboard shortcuts only valid in math mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-math?)
  ("g r a d ." (insert "<nabla>"))
  ("d i v ." (insert "<nabla><cdot>"))
  ("r o t ." (insert "<nabla><times>"))
  ("e q u ." (make 'label) (insert "equ:")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. User defined icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-extra-icons
  (former)
  (=> (balloon (icon "my_lowgre.png") "Lowercase Greek")
      (tile 6 (link my-lowercase-greek)))
  (=> (balloon (icon "my_uppgre.png") "Uppercase Greek")
      (tile 4 (link my-uppercase-greek)))
  |
  (=> (balloon (icon "my_binope.png") "Binary operations")
      (tile 5 (link my-binary-operations)))
  (=> (balloon (icon "my_binrel.png") "Binary relations")
      (tile 5 (link my-binary-relations)))
  (=> (balloon (icon "my_negrel.png") "Negated relations")
      (tile 5 (link my-negated-relations)))
  |
  (=> (balloon (icon "my_arrows.png") "Arrows")
      (tile 5 (link my-arrows)))
  (=> (balloon (icon "my_missym.png") "Miscellaneous symbols")
      (tile 5 (link my-miscellaneous-symbols))))

(menu-bind my-lowercase-greek
  (symbol "<alpha>") (symbol "<beta>") (symbol "<gamma>")
  (symbol "<delta>") (symbol "<epsilon>") (symbol "<varepsilon>")
  (symbol "<zeta>") (symbol "<eta>") (symbol "<theta>")
  (symbol "<vartheta>") (symbol "<iota>") (symbol "<kappa>")
  (symbol "<lambda>") (symbol "<mu>") (symbol "<nu>")
  (symbol "<xi>") (symbol "<pi>") (symbol "<varpi>")
  (symbol "<rho>") (symbol "<varrho>") (symbol "<sigma>")
  (symbol "<varsigma>") (symbol "<tau>") (symbol "<upsilon>")
  (symbol "<phi>") (symbol "<varphi>") (symbol "<chi>")
  (symbol "<psi>") (symbol "<omega>") (symbol "<varkappa>"))

(menu-bind my-uppercase-greek
  (symbol "<Gamma>") (symbol "<Delta>") (symbol "<Theta>")
  (symbol "<Lambda>") (symbol "<Xi>") (symbol "<Pi>")
  (symbol "<Sigma>") (symbol "<Upsilon>") (symbol "<Phi>")
  (symbol "<Psi>") (symbol "<Omega>") (symbol "<digamma>"))

(menu-bind my-binary-operations
  (symbol "<pm>") (symbol "<mp>") (symbol "<times>")
  (symbol "<div>") (symbol "<ast>") (symbol "<star>")
  (symbol "<circ>") (symbol "<bullet>") (symbol "<cdot>")
  (symbol "<cap>") (symbol "<cup>") (symbol "<wedge>")
  (symbol "<vee>") (symbol "<oplus>") (symbol "<otimes>"))

(menu-bind my-binary-relations
  (symbol "<leqslant>") (symbol "<geqslant>") (symbol "<ll>")
  (symbol "<gg>") (symbol "<sim>") (symbol "<simeq>")
  (symbol "<approx>") (symbol "<cong>") (symbol "<equiv>")
  (symbol "<propto>") (symbol "<subset>") (symbol "<supset>")
  (symbol "<subseteq>") (symbol "<supseteq>") (symbol "<in>"))

(menu-bind my-negated-relations
  (symbol "<neq>") (symbol "<nless>") (symbol "<ngtr>")
  (symbol "<nleqslant>") (symbol "<ngeqslant>") (symbol "<nsim>")
  (symbol "<napprox>") (symbol "<ncong>") (symbol "<nequiv>")
  (symbol "<nin>") (symbol "<nsubset>") (symbol "<nsupset>")
  (symbol "<nsubseteq>") (symbol "<nsupseteq>") (symbol "<nmid>"))

(menu-bind my-arrows
  (symbol "<leftarrow>") (symbol "<rightarrow>") (symbol "<leftrightarrow>")
  (symbol "<Leftarrow>") (symbol "<Rightarrow>")
  (symbol "<Leftrightarrow>") (symbol "<longrightarrow>")
  (symbol "<mapsto>") (symbol "<longmapsto>") (symbol "<hookrightarrow>")
  (symbol "<uparrow>") (symbol "<downarrow>") (symbol "<nearrow>")
  (symbol "<searrow>") (symbol "<rightleftharpoons>"))

(menu-bind my-miscellaneous-symbols
  (symbol "<infty>") (symbol "<partial>") (symbol "<nabla>")
  (symbol "<forall>") (symbol "<exists>") (symbol "<emptyset>")
  (symbol "<aleph>") (symbol "<hbar>") (symbol "<ell>")
  (symbol "<wp>") (symbol "<Re>") (symbol "<Im>")
  (symbol "<angle>") (symbol "<top>") (symbol "<bot>"))
