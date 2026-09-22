;; the icons: one row of buttons per icon set, to check the vector icons
;; (TeXmacs/misc/pixmaps/light and .../dark) against the rasters.
;; Run it under TEXMACS_VUE_THEME=light and TEXMACS_VUE_THEME=dark.
(tm-widget (icons-test)
  (padded
    (vlist
      (text "24x24/main")
      (hlist ((icon "tm_new.xpm") (noop)) ((icon "tm_open.xpm") (noop)) ((icon "tm_save.xpm") (noop)) ((icon "tm_cut.xpm") (noop)) ((icon "tm_copy.xpm") (noop)) ((icon "tm_paste.xpm") (noop)) ((icon "tm_undo.xpm") (noop)) ((icon "tm_redo.xpm") (noop)) ((icon "tm_find.xpm") (noop)) >>>)
      ===
      (text "20x20/mode")
      (hlist ((icon "tm_math.xpm") (noop)) ((icon "tm_text.xpm") (noop)) ((icon "tm_prog.xpm") (noop)) ((icon "tm_table.xpm") (noop)) ((icon "tm_left.xpm") (noop)) ((icon "tm_right.xpm") (noop)) ((icon "tm_up.xpm") (noop)) ((icon "tm_down.xpm") (noop)) >>>)
      ===
      (text "16x16/focus")
      (hlist ((icon "tm_emphasize.xpm") (noop)) ((icon "tm_strong.xpm") (noop)) ((icon "tm_verbatim.xpm") (noop)) ((icon "tm_sansserif.xpm") (noop)) ((icon "tm_british.xpm") (noop)) ((icon "tm_german.xpm") (noop)) ((icon "tm_italian.xpm") (noop)) ((icon "tm_greek.xpm") (noop)) >>>)
      ===
      (text "32x32 settings and table")
      (hlist ((icon "tm_cell_width.xpm") (noop)) ((icon "tm_cell_height.xpm") (noop)) ((icon "tm_cell_hcenter.xpm") (noop)) ((icon "tm_cell_vcenter.xpm") (noop)) >>>)
      ===
      (text "traditional")
      (hlist ((icon "tm_acute.xpm") (noop)) ((icon "tm_grave.xpm") (noop)) ((icon "tm_hat.xpm") (noop)) ((icon "tm_tilda.xpm") (noop)) ((icon "tm_bar.xpm") (noop)) ((icon "tm_check.xpm") (noop)) ((icon "tm_breve.xpm") (noop)) ((icon "tm_vect.xpm") (noop)) >>>)
      )))

(delayed (:pause 2500) (top-window icons-test "Icons"))
