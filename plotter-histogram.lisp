
(in-package :plotter)

;; ---------------------------------------------------------
;; org can be a list of (type xorg yorg), e.g., '(:frac 0.9 0.96)
;; or a pair of typed values ((type xorg) (type yorg)), e.g., '((:frac 0.9) (:data 14.3))
;;
;; convert to a list of typed pairs, e.g., '((:frac 0.9) (:data 14.3))
;;
(defun get-xy-orgs (org)
  (if (= 3 (length org))
      (list (list (first org) (second org))
            (list (first org) (third org)))
    org))

(defun draw-text (pane str org &rest args)
  (destructuring-bind (xorg yorg) (get-xy-orgs org)
    (apply #'outsxy pane xorg yorg str (append args *default-args*))))

;; ------------------------------------------

(defun framp (nel)
  (declare (type fixnum nel))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel
                       :initial-element 0.0f0
                       :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix) (coerce ix 'single-float)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-args-from-bindings (arg-bindings)
    (let ((argnames (mapcar #'(lambda (binding)
                                (if (consp binding)
                                    (first binding)
                                  binding))
                            arg-bindings))
          (argvalues (mapcar #'(lambda (binding)
                                 (if (consp binding)
                                     (second binding)
                                   binding))
                             arg-bindings)))
      (values argnames argvalues))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun need-symbolic-index-var (index)
    (unless (symbolp index)
      (error "symbol index var expected, got: ~A" index))))

(defmacro vectorwise (arg-bindings body-form &key dest index)
  ;; dest is name of destination variable. If missing or nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (multiple-value-bind (argnames argvalues) (get-args-from-bindings arg-bindings)
    (if index
        (progn
          (need-symbolic-index-var index)
          (if dest
              `(indexed-dest-vwise #'(lambda (,index ,@argnames) ,body-form)
                                   ,dest ,@argvalues)
              `(indexed-vwise #'(lambda (,index ,@argnames) ,body-form)
                              ,@argvalues)))
        (if dest
            `(dest-vwise #'(lambda ,argnames ,body-form)
                         ,dest ,@argvalues)
            `(vwise #'(lambda ,argnames ,body-form)
                    ,@argvalues)))))

(defmethod vector-of (x)
  (make-array 1 :initial-element x))

(defmethod vector-of ((lst cons))
  (coerce lst 'vector))

(defmethod vector-of ((v vector))
  (if (array-has-fill-pointer-p v)
      (subseq v 0 (length v))
      v))

(defmethod vector-of ((arr array))
  (make-array (array-total-size-of arr)
              :displaced-to arr
              :element-type (array-element-type arr)))

(defun histogram (arr &key min max range nbins binwidth)
  (let* ((v (vector-of arr))
         (minv (if range
                   (elt range 0)
                   (or min
                       (reduce #'min v))))
         (maxv (if range
                   (elt range 1)
                   (or max
                       (reduce #'max v))))
         (range (- maxv minv))
         (nbins (or nbins
                    (and binwidth
                         (truncate range binwidth))
                    200))
         (binwidth (let ((bw (or binwidth
                                 (/ range nbins))))
                     (if (zerop bw)
                         1
                         bw)))
         (h (make-array (+ 2 nbins)
                        :initial-element 0
                        :element-type    'integer))
         (x (let ((xs     (framp (1+ nbins))))
              (vectorwise (xs)
                          (coerce (+ minv (* binwidth xs))
                                  'single-float)
                          :dest xs))))
    (dotimes (ix  (length v))
      (let* ((val (aref v ix))
             (jx  (round (- val minv) binwidth)))
        (when (<= minv val maxv)
          (incf (aref h jx)))))
    (values x h binwidth)))

(defun make-overlay-vector (arr)
  (if (vectorp arr)
      arr
      (make-array (array-total-size-of arr)
                  :displaced-to arr
                  :element-type (array-element-type arr))))

(defun dest-vwise (fn dest &rest args)
  (let ((args  (mapcar #'make-overlay-vector args))
        (vdest (make-overlay-vector dest)))
    (apply #'map-into vdest fn args)
    dest))

(defun do-plot-histogram (pane v &rest args
                            &key min max range nbins binwidth
                            ylog cum (norm t)
                            (line-type :stepped)
                            &allow-other-keys)
  (multiple-value-bind (x h bw)
      (histogram v
                 :min      min
                 :max      max
                 :range    range
                 :nbins    nbins
                 :binwidth binwidth)
    (let* ((nel (array-total-size v))
           (tot (* nel bw))
           minnz)
      (when norm
        (loop for v across h
              for ix from 0
              do
              (setf (aref h ix) (/ v tot))
              ))
      (when cum
        (loop for vy across h
              for ix from 0
              for sf = (if norm bw (/ nel))
              for sum = (* sf vy) then (+ sum (* sf vy))
              do
              (setf (aref h ix) sum)
              (unless (or minnz
                          (zerop sum))
                (setf minnz sum))
              ))
      (when ylog
        (let ((zlim (cond (cum  minnz)
                          (norm (/ 0.9 tot))
                          (t     0.9)
                          )))
          (loop for v across h
                for ix from 0
                do
                (when (zerop v)
                  (setf (aref h ix) zlim)))
          ))
      (apply #'plot pane x h :line-type line-type args)
      )))

;; user callable routine
(defun histogram (pane v &rest args)
  (apply #'do-plot-histogram pane (coerce v 'vector) (append args *default-args*)))
