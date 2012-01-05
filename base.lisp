;;; defines a base class for all objects that exist inside a network. basically
;;; provides the "(id ...)" accessor since everything in a network must have an
;;; id.
(in-package :nneat)

(defclass base ()
  ((id :accessor id :initarg :id :initform 0))
  (:documentation "Base class for any object inside a network. Basically holds
  an id."))
