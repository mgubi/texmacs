;; the MuPDF renderer: PDF figures drawn as drawing (draw_scalable through
;; a form XObject), upright and with /Rotate 90, and at twice the size, where
;; a figure turned into pixels would show it; a figure at half opacity
;; is translucent as a whole (a transparency group); an EPS figure is
;; drawn as a drawing as well (made a PDF once)
(delayed (:pause 3000)
  (with dir (url->string (url-append (url-pwd) "src/Plugins/MuPDF/tests"))
    (insert (stree->tree
      `(document
         "An upright PDF figure, the same with /Rotate 90, and one at half opacity (the overlap pale blue, not purple):"
         (concat (image ,(string-append dir "/vector.pdf") "6cm" "" "" "")
                 " "
                 (image ,(string-append dir "/vector-rotated.pdf") "" "4cm" "" "")
                 " "
                 (with "opacity" "50%"
                   (image ,(string-append dir "/overlap.pdf") "3cm" "" "" "")))
         "At three times the size, hairlines must stay thin:"
         (image ,(string-append dir "/vector.pdf") "16cm" "" "" "")
         "A PostScript figure, made a PDF once and drawn as a drawing too:"
         (image ,(url->string (url-append (url-pwd) "TeXmacs/misc/pixmaps/unknown.eps")) "14cm" "" "" ""))))))
