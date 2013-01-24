
;;; Collector objects that perform rapid nconc list accumulation

(in-package :plotter)

(defclass <collector> ()
  ((hd :accessor collector-hd)
   (tl :accessor collector-tl)))

(defmethod collector-discard-contents ((c <collector>))
  (let ((v (list nil)))
    (setf (collector-hd c) v
          (collector-tl c) v)))

(defmethod collector-stuff-contents ((c <collector>) lst)
  (setf (collector-hd c) (cons nil lst)
        (collector-tl c) (last lst)))

(defmethod initialize-instance :after ((c <collector>) &key &allow-other-keys)
  (collector-discard-contents c))

(defmethod collector-contents ((c <collector>) &key (discard t))
  ;; make readout destructive to the collector object
  ;; so that we can't accidentally hand off a list that
  ;; is still undergoing active mutation
  ;;
  ;; If user doesn't want to discard contents,
  ;; then hand him a copy of the contents as they exist at this moment.
  (let ((lst (cdr (the cons (collector-hd c)))))
    (declare (list lst))
    (if discard
        (progn
          (collector-discard-contents c)
          lst)
        (copy-list lst))))
    
(defmethod collector-ncontents ((c <collector>))
  (length (cdr (the cons (collector-hd c)))))

(defmethod collector-empty-p ((c <collector>))
  (null (cdr (the cons (collector-hd c)))))

(defmethod collector-append-item ((c <collector>) item)
  (setf
   (collector-tl c)
   (cdr (the cons (rplacd (the cons (collector-tl c)) (list item))))))

(defmethod collector-push-item ((c <collector>) item)
  (setf
   (collector-hd c)
   (cons nil (the cons (rplaca (the cons (collector-hd c)) item)))))

(defmethod collector-pop ((c <collector>))
  (let* ((lst (cdr (the cons (collector-hd c))))
         (v   (car lst)))
    (declare (list lst))
    (unless (endp lst)
      (setf (collector-hd c) lst
            (car lst)        nil)) ;; unhook for GC
    v))

(defun make-collector ()
  (make-instance '<collector>))

;;; Monitored collector

(defclass <monitored-collector-mixin> ()
  ((changed :accessor monitored-collector-mixin-changed :initform nil)))

(defmethod changed-p ((m <monitored-collector-mixin>))
  "Asking if changed also resets the monitor"
  (shiftf (monitored-collector-mixin-changed m) nil))

(defclass <monitored-collector> (<monitored-collector-mixin>
                                 <collector>)
  ())

(defmethod collector-append-item :after ((c <monitored-collector-mixin>) item)
  (declare (ignore item))
  (setf (monitored-collector-mixin-changed c) t))

(defmethod collector-push-item :after ((c <monitored-collector-mixin>) item)
  (declare (ignore item))
  (setf (monitored-collector-mixin-changed c) t))

(defmethod collector-pop :after ((c <monitored-collector-mixin>))
  (setf (monitored-collector-mixin-changed c) t))

(defmethod collector-discard-contents :after ((c <monitored-collector-mixin>))
  (setf (monitored-collector-mixin-changed c) t))

(defmethod collector-stuff-contents :after ((c <monitored-collector-mixin>) lst)
  (declare (ignore lst))
  (setf (monitored-collector-mixin-changed c) t))

(defun make-monitored-collector ()
  (make-instance '<monitored-collector>))

;;; Multi-processor safe collector

(defclass <mpsafe-collector-mixin> ()
  ((lock
    :accessor mpsafe-lock
    :initform (mp:make-lock :name "MPSafe Mixin Lock"))))

(defclass <mpsafe-collector> (<mpsafe-collector-mixin>
                              <collector>)
  ())

(defclass <mpsafe-monitored-collector> (<mpsafe-collector-mixin>
                                        <monitored-collector-mixin>
                                        <collector>)
  ())

(defmethod collector-discard-contents :around ((c <mpsafe-collector-mixin>))
  (mp:with-lock ((mpsafe-lock c))
    (call-next-method)))

(defmethod collector-stuff-contents :around ((c <mpsafe-collector-mixin>) lst)
  (declare (ignore lst))
  (mp:with-lock ((mpsafe-lock c))
    (call-next-method)))

(defmethod collector-contents :around ((c <mpsafe-collector-mixin>)
                                       &key &allow-other-keys)
  (mp:with-lock ((mpsafe-lock c))
    (call-next-method)))
    
(defmethod collector-ncontents :around ((c <mpsafe-collector-mixin>))
  (mp:with-lock ((mpsafe-lock c))
    (call-next-method)))

(defmethod collector-append-item :around ((c <mpsafe-collector-mixin>) item)
  (declare (ignore item))
  (mp:with-lock ((mpsafe-lock c))
    (call-next-method)))

(defmethod collector-push-item :around ((c <mpsafe-collector-mixin>) item)
  (declare (ignore item))
  (mp:with-lock ((mpsafe-lock c))
    (call-next-method)))

(defmethod collector-pop :around ((c <mpsafe-collector-mixin>))
  (mp:with-lock ((mpsafe-lock c))
    (call-next-method)))

(defun make-mpsafe-collector ()
  (make-instance '<mpsafe-collector>))

(defun make-mpsafe-monitored-collector ()
  (make-instance '<mpsafe-monitored-collector>))
