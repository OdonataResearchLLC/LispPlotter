
(in-package :plotter)

;;; Scanner classes

(defclass <scanner> ()
  ()
  (:documentation
   "Abstract superclass <scanner> represent objects that respond to
the NEXT-ITEM method"))

(defclass <limited-scanner> (<scanner>)
  ((limit
    :initarg :limit
    :accessor scanner-limit)
   (pos
    :accessor scanner-position
    :initform 0))
  (:documentation
   "A scanner with a finite length, the limit."))

(defclass <counting-scanner> (<limited-scanner>)
  ()
  (:documentation
   "Count up to the limit."))

(defclass <vector-scanner> (<limited-scanner>)
  ((vec
    :initarg :vector
    :accessor scanner-vector))
  (:documentation
   "Scan the vector."))

(defclass <list-scanner> (<limited-scanner>)
  ((lst
    :initarg :list
    :accessor scanner-list)
   (lst-backup
    :accessor scanner-list-backup))
  (:documentation
   "Scan the list."))

(defmethod initialize-instance :after ((self <list-scanner>)
                                       &rest initargs)
  ;; FIXME: Either COPY-LIST or use ELT for next-item.
  (setf (scanner-list-backup self) (scanner-list self)))

(defclass <array-scanner> (<limited-scanner>)
  ((arr
    :accessor scanner-array
    :initarg :array))
  (:documentation
   "Scan the array."))

;;; Constructors

(defgeneric make-scanner (object &key max-items)
  (:documentation
   "Return the appropriate scanner instance for the object."))

(defmethod make-scanner ((object integer) &key (max-items limit))
  (make-instance '<counting-scanner> :limit (min object max-items)))

(defmethod make-scanner ((object vector) &key max-items)
  (let* ((len (length object))
         (max-items (or max-items len)))
    (make-instance
     '<vector-scanner>
     :limit (min len max-items)
     :vector object)))

(defmethod make-scanner ((object list) &key max-items)
  (let* ((len (length object))
         (max-items (or max-items len)))
    (make-instance
     '<list-scanner>
     :list object
     :limit (min len max-items))))

(defmethod make-scanner ((object array) &key max-items)
  (let* ((len (array-total-size object))
         (max-items (or max-items len)))
    (make-instance
     '<array-scanner>
     :array object
     :limit (min len max-items))))

;;; NEXT-ITEM
;;; All scanners pass through NIL as the terminal value

(defgeneric next-item (scanner)
  (:documentation
   "Return the next item from the scanner and increment the
position."))

(defmethod next-item ((cscanner <counting-scanner>))
  (with-accessors
      ((position scanner-position)
       (limit scanner-limit))
      cscanner
    (let ((item position))
      (when (< item limit)
        (incf position)
        item))))

(defmethod next-item ((lscanner <list-scanner>))
  (with-accessors
      ((limit scanner-limit)
       (position scanner-position)
       (its-list scanner-list))
      lscanner
    (when (< position limit)
      (incf position)
      (pop its-list))))

(defmethod next-item ((vscanner <vector-scanner>))
  (with-accessors
      ((position scanner-position)
       (limit scanner-limit)
       (its-vector scanner-vector))
      vscanner
    (when (< position limit)
      (let ((item (aref its-vector position)))
        (incf position)
        item))))

(defmethod next-item ((ascanner <array-scanner>))
  (with-accessors
      ((position scanner-position)
       (limit scanner-limit)
       (its-array scanner-array))
      ascanner
    (when (< position limit)
      (let ((item (row-major-aref its-array position)))
        (incf position)
        item))))

;;; Reset the scanner

(defgeneric reset-scanner (scanner)
  (:documentation
   "Reset the scanner to the original position."))

(defmethod reset-scanner ((scanner <limited-scanner>))
  (setf (scanner-position scanner) 0))

(defmethod reset-scanner :after ((scanner <list-scanner>))
  (setf (scanner-list scanner) (scanner-list-backup scanner)))

;; Transforming scanner

(defclass <transformer> (<scanner>)
  ((src
    :initarg :source
    :accessor transformer-source)
   (xform
    :initarg :xform
    :accessor transformer-xform)))

(defgeneric make-transformer (object function)
  (:documentation
   "A scanner that transforms each item using the function."))

(defmethod make-transformer (object (xform function))
  (make-instance
   '<transformer>
   :source (make-scanner object)
   :xform xform))

(defmethod make-transformer ((src <scanner>) (xform function))
  (make-instance '<transformer> :source src :xform xform))

(defmethod next-item ((xf <transformer>))
  (with-accessors
      ((source transformer-source)
       (xform transformer-xform))
      xf
    (let ((item (next-item source)))
      (when item
        (funcall xform item)))))

(defmethod reset-scanner ((xf <transformer>))
  (reset-scanner (transformer-source xf)))

;;; Pair Scanner

(defclass <pair-scanner> (<scanner>)
  ((xsrc
    :initarg :xsrc
    :accessor pair-scanner-xsrc)
   (ysrc
    :initarg :ysrc
    :accessor pair-scanner-ysrc)
   (pair
    :initform (make-array 2)
    :accessor pair-scanner-values))
  (:documentation
   "A linked pair of scanners."))

(defgeneric make-pair-scanner (scanner1 scanner2)
  (:documentation
   "Link the pair of scanners."))

(defmethod make-pair-scanner (scanner1 scanner2)
  (make-instance
   '<pair-scanner>
   :xsrc (make-scanner scanner1)
   :ysrc (make-scanner scanner2)))

(defmethod make-pair-scanner ((xs <scanner>) (ys <scanner>))
  (make-instance '<pair-scanner> :xsrc xs :ysrc ys))

(defmethod next-item ((pairs <pair-scanner>))
  (with-accessors
      ((xs pair-scanner-xsrc)
       (ys pair-scanner-ysrc)
       (pair pair-scanner-values))
      pairs
    (let ((x (next-item xs))
          (y (next-item ys)))
      (when (and x y)
        (setf
         (svref pair 0) x
         (svref pair 1) y)
        ;; Return the pair
        pair))))

(defmethod reset-scanner ((pairs <pair-scanner>))
  (reset-scanner (pair-scanner-xsrc pairs))
  (reset-scanner (pair-scanner-ysrc pairs)))
