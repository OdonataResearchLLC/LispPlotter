
(in-package :plotter)

;;; Generalized operators

(defgeneric coerce-to-vector (object)
  (:method (object) (make-array 1 :initial-element object))
  (:method ((object vector)) object)
  (:method ((object list)) (coerce object 'vector))
  (:method ((object array))
   (make-array (array-total-size object)
               :element-type (array-element-type object)
               :displaced-to object))
  (:documentation
   "Create a vector from the object."))

;;; FIXME: Shadow LENGTH
(defgeneric length-of (object)
  (:method (object) (length object))
  (:method ((object array)) (array-total-size object))
  (:documentation
   "Return the length of the object, total-size of arrays."))

;;; FIXME: Rename vmax
(defgeneric vmax-of (object)
  (:method ((object vector))
   (loop for item across object maximize item))
  (:method ((object list))
   (loop for item in object maximize item))
  (:method ((object array))
   (loop for item below (array-total-size object)
         maximize (row-major-aref object item)))
  (:documentation
   "Return the maximum item of the object."))

;;; FIXME: Rename vmin
(defgeneric vmin-of (object)
  (:method ((object vector))
   (loop for item across object minimize item))
  (:method ((object list))
   (loop for item in object minimize item))
  (:method ((object array))
   (loop for item below (array-total-size object)
         minimize (row-major-aref object item)))
  (:documentation
   "Return the minimum item of the object."))
