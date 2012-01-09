;;; Defines the neuron class, which provides a simple way to sum weighted values
;;; from other connected neurons and fire its outgoing connections with the 
;;; resulting values. The inputs/outputs are of type 'connection, which is where
;;; the weight values live.
;;;
;;; There are three types of neurons: :input :output :neuron. :input provides
;;; no mathematical processesing, and must have ONLY ONE output. :output and
;;; :neuron are normal neuron types, provides sigmoid summation and processing.
;;; :output cannot have any outgoing connections, whereas :neuron can have an
;;; arbitrary number of incoming/outgoing connections (it can even connect to itself).

(in-package :nneat)

(defclass neuron (base)
  ((inputs :accessor neuron-inputs :initform (make-array 0 :adjustable t :fill-pointer t))
   (outputs :accessor neuron-outputs :initform nil)
   (output :accessor neuron-output :initform nil)
   (threshold :accessor neuron-threshold :initarg :threshold :initform *neuron-default-threshold*)
   (has-run :accessor neuron-has-run :initform nil)
   (type :accessor neuron-type :initarg :type :initform :neuron)
   (stimulate :accessor neuron-stimulate :initform 0))
  (:documentation "The neuron class is a model of a single neuron. It sums its
  weighted inputs and runs the amount through the activation function. to decide
  whether or not to fire."))

(defmethod create-neuron (&key (type :neuron) (threshold *neuron-default-threshold*))
  "Wrapper around neuron creation."
  (let ((neuron (make-instance 'neuron :type type :threshold threshold)))
    (when (eql type :input)
      (setf (neuron-inputs neuron) (make-array 1 :adjustable t :fill-pointer 1)))
    neuron))

(defmethod run-neuron ((n neuron) &key (propagate t))
  "Take this neuron's inputs and calculate the output value."
  (when (all-inputs-recieved n)
    (case (neuron-type n)
          (:input
            (setf (neuron-output n) (elt (neuron-inputs n) 0)))
          ((:neuron :output)
            ;; regular neuron, sum up and threshold the inputs
            (let* ((sig (sigmoid (sum-inputs n))))
              (if (or *neuron-always-output*
                      (< (neuron-threshold n) sig)
                      (and (eql (neuron-type n) :output)
                           *neuron-output-passthrough*))
                  (progn (setf (neuron-output n) (if *neuron-binary-output* 1 sig))
                         (when *dynamic-neuron-self-govern* 
                           (incf (neuron-stimulate n))))
                  (progn (setf (neuron-output n) 0)
                         (when *dynamic-neuron-self-goven*
                           (decf (neuron-stimulate n))))))))
    (when (and *dynamic-neuron*
               (not (eql (neuron-type n) :input))
               (not (zerop (neuron-stimulate n))))
      (let* ((stimulate (if (< 0 (neuron-stimulate n)) t nil))
             (threshold-enforce (if stimulate *dynamic-neuron-threshold-delta* (/ 1 *dynamic-neuron-threshold-delta*)))
             (weight-enforce (if stimulate *dynamic-neuron-weight-delta* (/ 1 *dynamic-neuron-weight-delta*))))
        (setf (neuron-threshold n) (* (neuron-threshold n) threshold-enforce))
        (loop for inp across (neuron-inputs n) do
              (setf (connection-weight inp) (* (connection-weight inp) weight-enforce)))
        (if stimulate
            (decf (neuron-stimulate n))
            (incf (neuron-stimulate n)))))
    ;; neuron ran, mark it as such
    (setf (neuron-has-run n) t)
    (clear-inputs n)
    ;; push the output value to all outgoing connections
    (run-outputs n :propagate propagate)))

(defmethod run-outputs ((n neuron) &key (propagate t))
  "Process all outgoing outputs for this neuron."
  (dolist (c (neuron-outputs n))
    (activate-connection c (neuron-output n) :propagate propagate)))

(defmethod sum-inputs ((n neuron))
  "Given a neuron, grab all the values from its inputs and return the sum."
  (let ((sum 0))
    (loop for inp across (neuron-inputs n) do
          (let ((value (case (type-of inp) (connection (connection-output inp))
                                           ('nil 0)
                                           (t inp))))
            (incf sum value)))
    sum))

(defmethod clear-inputs ((n neuron))
  "Set the incoming connections to nil (marks them as not having fired yet),
  which allows the neuron to track whether it should fire or not."
  (let* ((inputs (neuron-inputs n))
         (num-inputs (length inputs)))
    (if (eql (neuron-type n) :input)
        ;; just set to nil for input neurons
        (loop for i from 0 to (1- num-inputs) do (setf (elt inputs i) nil))
        ;; for all others, set the actual connection value to nil.
        (loop for inp across inputs do
              (setf (connection-output inp) nil)))))

(defmethod all-inputs-recieved ((n neuron))
  "Find out if all inputs have been recieved or not. Non-recieved inputs will be
  nil. All inputs are reset to nil after a neuron has processed."
  (case (neuron-type n)
        (:input
          (not (not (elt (neuron-inputs n) 0))))
        ((:output :neuron)
         (zerop (length (remove-if (lambda (c) (connection-output c))
                                   (neuron-inputs n)))))))

(defun sigmoid (sum &key (max-sum 80) (slope *neuron-sigmoid-slope*))
  "Sigmoid function."
  (let* ((sum (max (min (* sum slope) max-sum) (- max-sum)))
         (sig (/ 1 (+ 1 (exp (- sum))))))
    (* (if *neuron-sigmoid-negatives*
           (* 2 (- sig 1/2))
           sig)
       *neuron-sigmoid-multiplier*)))

