
(in-package :plotter)

;; ------------------------------------------
;; generalized operators to accommodate <carrays> and others
;;

;; -------------------------------------------------------------------
(defmethod coerce-to-vector ((v vector))
  v)

(defmethod coerce-to-vector ((lst list))
  (coerce lst 'vector))

(defmethod coerce-to-vector ((a array))
  (make-array (array-total-size a)
              :element-type (array-element-type a)
              :displaced-to a))

;;---------
(defmethod length-of (arg)
  (length arg))

(defmethod length-of ((arg array))
  (array-total-size arg))

;;---------
(defmethod vmax-of ((arg vector))
  (loop for item on arg maximize item))

(defmethod vmax-of ((arg list))
  (loop for item in arg maximize item))

(defmethod vmax-of ((arg array))
  (loop for ix from 0 below (array-total-size arg)
        maximize (row-major-aref arg ix)))

;;---------
(defmethod vmin-of ((arg vector))
  (loop for item on arg minimize item))

(defmethod vmin-of ((arg list))
  (loop for item in arg minimize item))

(defmethod vmin-of ((arg array))
  (loop for ix from 0 below (array-total-size arg)
        minimize (row-major-aref arg ix)))

;;---------
(defmethod array-total-size-of (arg)
  (array-total-size arg))

;;---------
(defmethod array-dimension-of (arg n)
  (array-dimension arg n))

;;---------
(defmethod aref-of (arg &rest indices)
  (apply #'aref arg indices))

;;---------
(defmethod subseq-of (arg start &optional end)
  (subseq arg start end))

(defmethod subseq-of ((arg array) start &optional end)
  (let* ((limit (array-total-size arg))
         (nel   (- (or end limit) start))
         (ans   (make-array nel :element-type (array-element-type arg))))
    (loop for ix from start below (or end limit)
          for jx from 0
          do
          (setf (aref ans jx) (row-major-aref arg ix)))
    ans))
