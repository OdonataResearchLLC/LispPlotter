
(in-package :plotter)

;; ------------------------------------------
;; Convenience macros

(defmacro with-color ((pane color) &body body)
  `(gp:with-graphics-state
       (,pane
        :foreground ,color)
     ,@body))
  
(defmacro with-mask ((pane mask) &body body)
  `(gp:with-graphics-state
       (,pane
        :mask ,mask)
     ,@body))

;;; Useful macros

(defun rcurry (fn &rest suf-args)
  (lambda (&rest pref-args)
    (apply fn (nconc pref-args suf-args))))
