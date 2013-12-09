(defsystem #:cl-nneat-demo
  :version "0.0.2"
  :maintainer "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :description "A library for defining and running neural networks whos structure can be modified genetically."
  :depends-on (#:cl-nneat #:cl-opengl #:cl-glu #:lispbuilder-sdl #:bordeaux-threads)
  :components
  ((:module demo
    :serial t
    :components
    ((:file "ext/sdl-window")
     (:file "ext/hash-util")
     (:file "package")
     (:file "config")
     (:file "util")
     (:file "game-object")
     (:file "food")
     (:file "animal")
     (:file "scavenger")
     (:file "predator")
     (:file "population")
     (:file "world")
     (:file "window")
     (:file "main")))))

