
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

(defun raw-mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun mkstr (&rest args)
  (with-standard-io-syntax
    (apply 'raw-mkstr args)))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun single (arg)
  (and
   (consp arg)
   (null (cdr (the cons arg)))))

(defun last1 (lst)
  (car (the cons (last lst))))

(defun curry (fn &rest pref-args)
  (lambda (&rest suf-args)
    (apply fn (append pref-args suf-args))))

(defun rcurry (fn &rest suf-args)
  (lambda (&rest pref-args)
    (apply fn (nconc pref-args suf-args))))

(defmacro expanded-curry ((&rest suf-args) f &rest pref-args)
  `(lambda ,suf-args
     (funcall ,f ,@pref-args ,@suf-args)))

(defun foldl (fn init seq)
  ;; fn should be a function of (accum item)
  (reduce fn seq :initial-value init))

(defun foldr (fn seq init)
  ;; fn should be a function of (item accum)
  (reduce fn seq :from-end t :initial-value init))

(defun compose (&rest fns)
  (cond
   ((null fns)   'identity)
   ((single fns) (car fns))
   ((single (rest fns))
    (destructuring-bind (fn1 fn2) fns
      (lambda (&rest args)
        (funcall fn1 (apply fn2 args)))))
   (t (let ((fn1 (last1 fns))
            (fns (butlast fns)))
        (lambda (&rest args)
          (foldr 'funcall fns (apply fn1 args)))))))
