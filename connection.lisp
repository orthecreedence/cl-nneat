;;; defines the class and methods for connections, which are the glue to a
;;; network. connections connect neurons between eachother in various patterns.
;;; connections can be forward or backward (ie temporal) and the system makes no
;;; distinction between the two types. in other words, a connection can connect
;;; any regular neuron to any regular neuron, eve itself.
(in-package :cl-nneat)

(defclass connection (base)
  ((from :accessor connection-from :initarg :from :initform nil)
   (to :accessor connection-to :initarg :to :initform nil)
   (weight :accessor connection-weight :initarg :weight :initform 1)
   (output :accessor connection-output :initarg :output :initform 0))
  (:documentation "The conncetion class defines a connection betwenn two neurons
  and handles the passing of values between them. A connection also holds a
  weight value, which is multipled by its input when the neuron is summing
  inputs (the weight lives in the connection, not the neuron)."))

;; An input or output neuron can only have one ougoing/incoming (respectively)
;; connection since they are pass-thru and do no summing.
(define-condition node-neuron-multiple-connections (error)
  ((text :initarg :text :reader text)))

;; A neuron must have at least one outoing connection and one incoming
;; connection.
(define-condition connection-is-required (error)
  ((text :initarg :text :reader :text)))

(defun create-connection (from to &key weight (initial-value 0))
  "Given two neurons, set up connections between them."
  (when (and (eql (neuron-type to) :input)
             (< 0 (length (neuron-inputs to))))
    (error 'node-neuron-multiple-connections
           :text "A node neuron can only have one incoming/outgoing connection"))
  (let ((connection (make-instance 'connection
                                   :from from
                                   :to to
                                   :output initial-value
                                   :weight (or weight (let ((weight (random *connection-weight-initial-max*)))
                                                        (if *connection-allow-negative-weights*
                                                            (* weight (1- (* 2 (random 2))))
                                                            weight))))))
    (when to
      (vector-push-extend connection (neuron-inputs to)))
    (when from
      (push connection (neuron-outputs from)))
    connection))

(defmethod remove-connection ((c connection) &key ignore-errors)
  "Given a connection, unregister it with the two neurons it connects to. Cannot
  be called while the network is being processed. Well, it can, but I wouldn't.
  It also checks if the neuron's last incoming/outgoing connection is being
  removed, and if so fires a 'connection-is-required error that the genetic
  system must handle (probably by ignoring and trying a different mutation)."
  (let ((to (connection-to c))
        (from (connection-from c)))
    (when (and (not ignore-errors)
               (or (<= (length (neuron-outputs from)) 1)
                   (<= (length (neuron-inputs to)) 1)))
      (error 'connection-is-required
             :text "You are trying to remove the last connection to a neuron."))
    (let ((inputs (remove-if (lambda (rc) (equal rc c))
                             (neuron-inputs to))))
      (setf (neuron-inputs to)
            (make-array (length inputs)
                        :initial-contents inputs
                        :fill-pointer t
                        :adjustable t)))
    (setf (neuron-outputs from)
          (remove-if (lambda (rc) (equal rc c)) (neuron-outputs from)))))

(defmethod remove-neuron-connection ((from neuron) (to neuron) &key remove-all)
  "Remove a connection given a pair of two (allegedly) connected neurons. Finds
  any (and all) matching connections and uses the other remove-connection to do
  the actual remove."
  (let ((found-connections (remove-if (lambda (nc)
                                        (not (equal to (connection-to nc))))
                                      (neuron-outputs from))))
    (dolist (c found-connections)
      (remove-connection c)
      (unless remove-all (return)))))

(defun activate-connection (connection value &key (propagate t))
  "Take the output of a neuron that fired (or didn't) and enter it as the input
  to the connection's TO neuron in the correct slot. This function takes care of
  weighting the value value (inp * weight) before entering the input to the
  resulting neuron. It also calls run-neuron on its outgoing neuron connection
  if propagate is T."
  (declare (optimize (speed 3) (safety 1))
           (type connection connection)
           (type number value)
           (type boolean propagate))
  (let ((neuron (connection-to connection)))
    (when neuron
      (let ((output (* (connection-weight connection) value)))
        (setf (connection-output connection) output)
        (when (and propagate
                   (not (neuron-has-run neuron)))
          (run-neuron neuron))))))
  
