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
   (type :accessor neuron-type :initarg :type :initform :neuron))
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
            (let* ((sig (sigmoid (sum-inputs n)))
                   (sig (if *neuron-abs-sigmoid*
                               (abs sig)
                               sig)))
              (if (or (< (neuron-threshold n) sig)
                      (and (eql (neuron-type n) :output)
                           *neuron-output-passthrough*))
                  (setf (neuron-output n) (if *neuron-binary-output* 1 sig))
                  (setf (neuron-output n) 0)))))
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
            (format t "neuron ~a here, summing value ~a~%" (id n) value)
            (incf sum value)))
    sum))

(defmethod clear-inputs ((n neuron))
  "Set the incoming connections to nil (marks them as not having fired yet),
  which allows the neuron to track whether it should fire or not."
  (let* ((inputs (neuron-inputs n))
         (num-inputs (length inputs)))
    (dotimes (i num-inputs)
      (cond ((numberp (elt inputs i))
             (setf (elt inputs i) nil))
            ((eql (type-of (elt inputs i)) 'connection)
             (setf (connection-output (elt inputs i)) nil))))))

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

