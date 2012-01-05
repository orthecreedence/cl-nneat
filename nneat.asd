(in-package :asdf)

(defsystem #:nneat
  :version "0.1.1"
  :maintainer "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :description "A library for defining and running neural networks whos structure can be modified genetically."
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "util")
               (:file "base")
               (:file "neuron")
               (:file "connection")
               (:file "genome")
               (:file "net")))
