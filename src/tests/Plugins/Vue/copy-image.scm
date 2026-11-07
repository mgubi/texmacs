;; a selection copied as an image to the system clipboard ("Copy to >
;; Image" of the Edit menu: graphics-file-to-clipboard), as a PNG, and
;; pasted back, which inserts it as a picture. The format is given here
;; rather than read from the preference texmacs->image:format, which a
;; test must not change. Note: this replaces the contents of the clipboard
(delayed (:pause 2500)
  (insert "Hello image")
  (select-all)
  (let ((u (url-glue (url-temp) ".png")))
    (export-selection-as-graphics u)
    (display* "got: exported " (url-exists? u) "\n")
    (display* "got: on clipboard " (graphics-file-to-clipboard u) "\n")
    (system-remove u))
  (go-end)
  (insert-return)
  (clipboard-paste "primary")
  (delayed (:pause 500)
    (display* "got: pasted "
              (with t (tree-ref (buffer-tree) :last)
                (if t (tree->stree t) "nothing")) "\n")))
