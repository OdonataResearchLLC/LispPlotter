
(asdf:defsystem "plotter"
  :description "plotter: Scientific data plotting"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :serial t
  :components
  ((:file "packages")
   (:file "scanner")
   (:file "collector")
   (:file "plotter-classes")
   (:file "plotter-compat")
   (:file "plotter-macros")
   (:file "plotter-nans")
   (:file "plotter-mp")
   (:file "plotter-arrays")
   (:file "plotter-boxes")
   (:file "plotter-autorange")
   (:file "pane-locations")
   (:file "plotter-strings")
   (:file "plotter-drawing")
   (:file "plotter-watermark")
   (:file "plotter-axes")
   (:file "plotter-legends")
   (:file "plotter-styles")
   (:file "plotter-commands")
   (:file "plotter-callbacks")
   (:file "plotter-windows")
   (:file "plotter-histogram")
   (:file "plotter-images")
   (:file "parametric-plotting")
   (:file "plotting")))
