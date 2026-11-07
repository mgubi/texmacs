
;; the GUI entry points which used to be stubs: the wait indicator
;; (system-wait, shown while a long operation runs, popped by an empty
;; message) and the help balloon (show-balloon, dismissed by a key or a
;; pointer motion). The beep and image-gc have no Scheme binding and are
;; exercised from C++ by their callers.
(use-modules (kernel gui menu-widget))
(delayed (:pause 3000)
  (display* "ENTRY wait on\n")
  (system-wait "Working" "please wait")
  (delayed (:pause 1200)
    (display* "ENTRY wait off\n")
    (system-wait "" "")
    (delayed (:pause 800)
      (display* "ENTRY balloon\n")
      (show-balloon (make-menu-widget '(vertical (text "A help balloon")) 0)
                    400 400)
      (delayed (:pause 1200) (display* "ENTRY done\n")))))
