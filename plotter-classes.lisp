
(in-package :plotter)

(defclass <line-style> ()
  ((line-thick
    :initarg :thick
    :accessor line-thick)
   (line-dashing
    :initarg  :dashing
    :accessor line-dashing)
   (line-color
    :initarg :color
    :accessor line-color)
   (line-alpha
    :initarg :alpha
    :accessor line-alpha)
   (line-type
    :initarg :type
    :accessor line-type))
  (:default-initargs
   :thick 1
   :dashing nil
   :color :darkgreen
   :alpha nil
   :type :interpolated))

(defclass <symbol-style> ()
  ((plot-symbol
    :initarg :symbol
    :accessor plot-symbol)
   (fill-color
    :initarg :fill-color
    :accessor fill-color)
   (fill-alpha
    :initarg :fill-alpha
    :accessor fill-alpha)
   (border-color
    :initarg :border-color
    :accessor border-color)
   (border-alpha
    :initarg :border-alpha
    :accessor border-alpha)
   (border-thick
    :initarg :border-thick
    :accessor border-thick)
   (bar-width
    :initarg :bar-width
    :accessor bar-width)
   (bar-offset
    :initarg :bar-offset
    :accessor bar-offset))
  (:default-initargs
   :symbol :circle
   :fill-color nil
   :fill-alpha nil
   :border-color :black
   :border-alpha nil
   :border-thick 1
   :bar-width nil
   :bar-offset nil))

(defclass <plot-style> ()
  ((line-style
    :initarg :line-style
    :accessor line-style)
   (symbol-style
    :initarg :symbol-style
    :accessor symbol-style)
   (legend
    :initarg :legend
    :accessor legend))
  (:default-initargs
   :line-style (make-instance '<line-style>)
   :symbol-style nil
   :legend nil))

(defclass <legend> ()
  ((activep
    :initform t
    :accessor activep)
   (has-content
    :initform nil
    :accessor has-content)
   (highlighted
    :initform nil
    :accessor highlighted)
   (x
    :initform 0
    :accessor x)
   (y
    :initform 0
    :accessor y)
   (width
    :initform 0
    :accessor width)
   (height
    :initform 0
    :accessor height)
   (dragging
    :initform nil
    :accessor dragging)
   (dx
    :initform 0
    :accessor dx)
   (dy
    :initform 0
    :accessor dy)))

(defclass <plotter-mixin> ()
  ;; stuff used by 2-D plot scaling and plotting
  ;; The mixin has all the information needed to produce plots
  ;; but has nothing to draw on...
  ((lock
    :initform (mp:make-lock)
    :accessor plotter-lock)
   (xlog
    :initform nil
    :accessor plotter-xlog)
   (xmin
    :initform 0D0
    :accessor plotter-xmin)
   (xmax
    :initform 1D0
    :accessor plotter-xmax)
   (ylog
    :initform nil
    :accessor plotter-ylog)
   (ymin
    :initform 0D0
    :accessor plotter-ymin)
   (ymax
    :initform 1D0
    :accessor plotter-ymax)
   (box :accessor plotter-box)
   (xform
    :initform (list 1 0 0 1 0 0)
    :accessor plotter-xform)
   (inv-xform
    :initform (list 1 0 0 1 0 0)
    :accessor plotter-inv-xform)
   (dlist
    :initform (make-mpsafe-monitored-collector)
    :accessor plotter-display-list)
   (delayed
    :initform 0
    :accessor plotter-delayed-update)
   ;; info for nice looking zooming
   (def-wd
    :initarg :nominal-width
    :accessor plotter-nominal-width)
   (def-ht
    :initarg :nominal-height
    :accessor plotter-nominal-height)
   (sf
    :initform 1
    :accessor plotter-sf)
   (magn
    :initform 1
    :accessor plotter-magn)
   (legend-info
    :initform (make-collector)
    :accessor plotter-legend-info)
   (legend-x
    :initform (list :frac 0.95)
    :accessor plotter-legend-x)
   (legend-y
    :initform (list :frac 0.95)
    :accessor plotter-legend-y)
   (legend-anchor
    :initform :auto
    :accessor plotter-legend-anchor)
   (legend
    :initform (make-instance '<legend>)
    :accessor plotter-legend)
   (preferred-x
    :initform nil
    :accessor preferred-x)
   (preferred-y
    :initform nil
    :accessor preferred-y)
   (dirty
    :initform nil
    :accessor plotter-dirty)
   (needs-legend
    :initform nil
    :accessor plotter-needs-legend)
   (reply-mbox
    :initform nil
    :accessor reply-mbox))
  (:default-initargs
   :nominal-width 400
   :nominal-height 300))

(defclass <plotter-pane> (<plotter-mixin> capi:output-pane)
  ;; stuff used by 2-D plot scaling and plotting
  ;; The pane adds something to draw on...
  ;; And it also adds some user gestures and any display related items
  ;; like cross hairs, cursors, backing images, etc.
  ((backing-image
    :initform nil
    :accessor plotter-backing-image)
   (timer
    :initform nil
    :accessor plotter-resize-timer)
   (backing-pixmap
    :initform nil
    :accessor plotter-backing-pixmap)
   (delay-backing
    :initform nil
    :accessor plotter-delay-backing)
   (full-crosshair
    :initarg :full-crosshair
    :accessor plotter-full-crosshair)
   (prev-x
    :initform nil
    :accessor plotter-prev-x)
   (prev-y
    :initform nil
    :accessor plotter-prev-y)
   (x-ro-hook
    :initform #'identity
    :accessor plotter-x-readout-hook)
   (y-ro-hook
    :initform #'identity
    :accessor plotter-y-readout-hook)
   (plotter-valid
    :initform t
    :accessor plotter-valid)
   (mark-x
    :initform nil
    :accessor mark-x)
   (mark-y
    :initform nil
    :accessor mark-y)
   (mark-x-raw
    :initform nil
    :accessor mark-x-raw)
   (mark-y-raw
    :initform nil
    :accessor mark-y-raw))
  (:default-initargs
   :full-crosshair nil
   :display-callback 'display-callback
   :resize-callback  'resize-callback
   :destroy-callback 'destroy-callback
   :pane-menu 'popup-menu
   :input-model
   '((:motion mouse-move)
     ((:button-1 :motion) drag-legend)
     ((:button-1 :press) show-x-y-at-cursor)
     ((:button-1 :release) undrag-legend)
     ((:gesture-spec "Backspace") maybe-remove-legend)
     ((:gesture-spec "Delete") maybe-remove-legend)
     ((:gesture-spec "Control-c") copy-image-to-clipboard)
     ((:gesture-spec "Control-p") print-plotter-pane)
     ((:gesture-spec "Control-s") save-image-from-menu)
     ((:gesture-spec "C") toggle-full-crosshair)
     ((:gesture-spec "c") toggle-full-crosshair)
     ((:gesture-spec "x") mark-x-at-cursor)
     ((:gesture-spec "y") mark-y-at-cursor)
     ((:gesture-spec "m") mark-x-y-at-cursor)
     ((:gesture-spec "u") unmark-x-y))
   :cursor :crosshair
   :visible-min-width  200
   :visible-min-height 150
   :visible-max-width  800
   :visible-max-height 600
   :background :white
   :foreground :black))

#+:WIN32
(defmethod initialize-instance :after
  ((pane <plotter-pane>) &rest initargs)
  (when (slot-value pane 'full-crosshair)
    (let ((color (slot-value pane 'full-crosshair)))
      (setf
       (slot-value pane 'full-crosshair)
       (complementary-color
        pane color (capi:simple-pane-background pane))))))
                               
(defun destroy-callback (pane)
  (setf (plotter-valid pane) nil)
  (discard-backing-image pane)
  (discard-backing-pixmap pane))

;;; FIXME : Replace the backquoted code in popup-menu

(defun popup-menu (pane selection x y)
  (declare (ignore selection))
  (make-instance
   'capi:menu :items
   `(,(make-instance
       'capi:menu-component :items
       `(,(make-instance
           'capi:menu-item
           :data :toggle-cursor
           :text "Toggle crosshair")))
     ,(make-instance
       'capi:menu-component :items
       `(,(make-instance
           'capi:menu-item
           :data :copy-image
           :text "Copy to clipboard")
         ,(make-instance
           'capi:menu-item
           :data :print-image
           :text "Print image")
         ,(make-instance
           'capi:menu-item
           :data :save-image
           :text "Save image")))
     ,@(when (or (on-legend pane x y)
                 (not (activep (plotter-legend pane))))
         `(,(make-instance
             'capi:menu-component :items
             `(,@(when (on-legend pane x y)
                   `(,(make-instance
                       'capi:menu-item
                       :data :remove-legend
                       :text "Remove Legend")))
               ,@(when (not (activep (plotter-legend pane)))
                   `(,(make-instance
                       'capi:menu-item
                       :data :restore-legend
                       :text "Restore Legend")))))))
     ,(make-instance
       'capi:menu-component :items
       `(,(make-instance 'capi:menu-item :data :mark-x :text "Mark X")
         ,(make-instance 'capi:menu-item :data :mark-y :text "Mark Y")
         ,(make-instance
           'capi:menu-item :data :mark-x-y :text "Mark X & Y")
         ,@(when (or (mark-x pane) (mark-y pane))
             `(,(make-instance
                 'capi:menu-item
                 :data :remove-mark
                 :text "Remove marker"))))))
   :callback
   (lambda (key intf)
     (declare (ignore intf))
     (case key
       (:toggle-cursor
        (toggle-full-crosshair pane))
       (:copy-image
        (copy-image-to-clipboard pane))
       (:print-image
        (print-plotter-pane pane))
       (:save-image
        (save-image-from-menu pane))
       (:remove-legend
        (let ((legend (plotter-legend pane)))
          (setf (activep legend) nil)
          (restore-legend-background pane legend)))
       (:restore-legend
        (let ((legend (plotter-legend pane)))
          (setf (activep legend) t)
          (draw-existing-legend pane pane)))
       (:mark-x
        (mark-x-at-cursor pane x y))
       (:mark-y
        (mark-y-at-cursor pane x y))
       (:mark-x-y
        (mark-x-y-at-cursor pane x y))
       (:remove-mark
        (unmark-x-y pane))))))

(defun maybe-remove-legend (pane x y &rest args)
  (declare (ignore args))
  (let ((legend (plotter-legend pane)))
    (when (and (activep legend) (on-legend pane x y))
      (setf (activep legend) nil)
      (restore-legend-background pane legend))))

(defgeneric plotter-mixin-of (pane-rep &optional args)
  (:documentation
   "pane-rep might be a <plotter-pane>, a subclass of a
capi:interface, or a symbolic name of a window."))

(defmethod plotter-mixin-of ((pane <plotter-mixin>) &optional args)
  (declare (ignore args))
  pane)

(defmethod display-pane-of (pane)
  pane)

(defmethod display-pane-of ((obj capi:pinboard-object))
  (display-pane-of (capi:element-parent obj)))

(defun append-display-list (pane item)
  (collector-append-item (plotter-display-list pane) item))

(defun discard-display-list (pane)
  (collector-discard-contents (plotter-display-list pane))
  (collector-discard-contents (plotter-legend-info pane)))

(defun display-list-items (pane &key discard)
  (collector-contents (plotter-display-list pane) :discard discard))

(defun display-list-empty-p (pane)
  (collector-empty-p (plotter-display-list pane)))

(defun append-legend (pane item)
  (collector-append-item (plotter-legend-info pane) item))

(defun all-legends (pane &key discard)
  (collector-contents (plotter-legend-info pane) :discard discard))

(defun discard-legends (pane)
  (collector-discard-contents (plotter-legend-info pane)))
