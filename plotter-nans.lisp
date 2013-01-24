
(in-package :plotter)

(defun infinitep (v)
  "Return true if non-zero numeric arg with zero reciprocal.

Works for plus or minus infinity. As a secondary effect, the truth
value will be the largest double precision value."
  (when (and (not (zerop v)) (zerop (/ v)))
    (if (plusp v)
        most-positive-double-float
        most-negative-double-float)))

(defun nanp (v)
  "nanp true if numeric v not equal to itself"
  (/= v v))

(defun inf-nan-p (v)
  (or (infinitep v) (nanp v)))

(defun simple-real-number (v)
  (and (realp v) (not (inf-nan-p v))))

(defun real-eval-with-nans (fn &rest args)
  (handler-case
      (let ((v (apply fn args)))
        (if (simple-real-number v) v :nan))
    (arithmetic-error (err)
      (declare (ignore err))
      :nan)))

(defun nan-or-infinite-p (v)
  (not (simple-real-number v)))

;;; NaN and Infinity filtering

;;; FIXME: Consider using a pair scanner
(defgeneric filter-x-y-nans-and-infinities (xs ys)
  (:documentation
   "Filter out the NaNs and Infinities."))

(defmethod filter-x-y-nans-and-infinities ((xs list) (ys list))
  "Remove paired values if either of the (x,y) pair is NaN or
infinite."
  (loop
   for x in xs
   and y in ys
   when (and (simple-real-number x) (simple-real-number y))
   collect x into filtered-x and collect y into filtered-y
   finally (return (values filtered-x filtered-y))))

(defmethod filter-x-y-nans-and-infinities ((xs vector) (ys vector))
  "Remove paired values if either of the (x,y) pair is NaN or
infinite."
  (loop
   for x across xs
   and y across ys
   when (and (simple-real-number x) (simple-real-number y))
   collect x into filtered-x and collect y into filtered-y
   finally (return (values filtered-x filtered-y))))

(defmethod filter-x-y-nans-and-infinities (xs ys)
  "Remove paired values if either of the (x,y) pair is NaN or
infinite."
  (filter-x-y-nans-and-infinities
   (coerce-to-vector xs) (coerce-to-vector ys)))

(defun filter-nans-and-infinities (xs)
  "Remove values from the sequence if they are NaNs or infinities."
  (remove-if (complement #'simple-real-number) xs))

(defun acceptable-value (v islog)
  "Filter out potential nans and infinities for logarithmic axes."
  (and
   (simple-real-number v)
   (or (not islog) (and islog (plusp v)))))

(defgeneric filter-potential-x-y-nans-and-infinities (xs ys xlog ylog)
  (:documentation
   "Remove paired values if either of the (x,y) pair is NaN or
infinite."))

(defmethod filter-potential-x-y-nans-and-infinities
           ((xs cons) (ys cons) xlog ylog)
  "Remove paired values if either of the (x,y) pair is nan or
infinite."
  (loop
   for x in xs
   and y in ys
   when (and (acceptable-value x xlog) (acceptable-value y ylog))
   collect x into filtered-x and collect y into filtered-y
   finally (return (values filtered-x filtered-y))))

(defmethod filter-potential-x-y-nans-and-infinities
           ((xs vector) (ys vector) xlog ylog)
  "Remove paired values if either of the (x,y) pair is nan or
infinite."
  (loop
   for x across xs
   and y across ys
   when (and (acceptable-value x xlog) (acceptable-value y ylog))
   collect x into filtered-x and collect y into filtered-y
   finally (return (values filtered-x filtered-y))))

(defmethod filter-potential-x-y-nans-and-infinities (xs ys xlog ylog)
  "Remove paired values if either of the (x,y) pair is nan or
infinite."
  (filter-x-y-nans-and-infinities
   (coerce-to-vector xs) (coerce-to-vector ys)))

(defun filter-potential-nans-and-infinities (xs islog)
  "Remove values from the sequence if they are nans or infinities."
  (remove-if (complement (rcurry #'acceptable-value islog)) xs))
