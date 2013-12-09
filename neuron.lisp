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

(in-package :cl-nneat)

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

(defun create-neuron (&key (type :neuron) (threshold *neuron-default-threshold*))
  "Wrapper around neuron creation."
  (let ((neuron (make-instance 'neuron :type type :threshold threshold)))
    (when (eql type :input)
      (setf (neuron-inputs neuron) (make-array 1 :adjustable t :fill-pointer 1)))
    neuron))

(defun run-neuron (neuron &key (propagate t))
  "Take this neuron's inputs and calculate the output value."
  (declare (optimize (speed 3) (safety 1))
           (type neuron neuron )
           (type boolean propagate))
  (when (all-inputs-recieved neuron)
    (case (neuron-type neuron)
          (:input
            (setf (neuron-output neuron) (aref (neuron-inputs neuron) 0)))
          ((:neuron :output)
            ;; regular neuron, sum up and threshold the inputs
            (let* ((sig (sigmoid (sum-inputs neuron))))
              (if (or *neuron-always-output*
                      (< (neuron-threshold neuron) sig)
                      (and (eql (neuron-type neuron) :output)
                           *neuron-output-passthrough*))
                  (progn (setf (neuron-output neuron) (if *neuron-binary-output* 1 sig))
                         (when *dynamic-neuron-self-govern* 
                           (incf (neuron-stimulate neuron))))
                  (progn (setf (neuron-output neuron) 0)
                         (when *dynamic-neuron-self-govern*
                           (decf (neuron-stimulate neuron))))))))
    (when (and *dynamic-neuron*
               (not (eql (neuron-type neuron) :input))
               (not (zerop (neuron-stimulate neuron))))
      (let* ((stimulate (if (< 0 (neuron-stimulate neuron)) t nil))
             (threshold-enforce (if stimulate *dynamic-neuron-threshold-delta* (/ 1 *dynamic-neuron-threshold-delta*)))
             (weight-enforce (if stimulate *dynamic-neuron-weight-delta* (/ 1 *dynamic-neuron-weight-delta*))))
        (setf (neuron-threshold neuron) (* (neuron-threshold neuron) threshold-enforce))
        (loop for inp across (neuron-inputs neuron) do
              (setf (connection-weight inp) (* (connection-weight inp) weight-enforce)))
        (if stimulate
            (decf (neuron-stimulate neuron))
            (incf (neuron-stimulate neuron)))))
    ;; neuron ran, mark it as such
    (setf (neuron-has-run neuron) t)
    (clear-inputs neuron)
    ;; push the output value to all outgoing connections
    (run-outputs neuron :propagate propagate)))

(defun run-outputs (neuron &key (propagate t))
  "Process all outgoing outputs for this neuron."
  (declare (optimize (speed 3) (safety 1))
           (type neuron neuron)
           (type boolean propagate))
  (dolist (c (neuron-outputs neuron))
    (activate-connection c (neuron-output neuron) :propagate propagate)))

(defun sum-inputs (neuron)
  "Given a neuron, grab all the values from its inputs and return the sum."
  (declare (optimize (speed 3) (safety 1))
           (type neuron neuron))
  (let ((sum 0)
        (inputs (neuron-inputs neuron)))
    (dotimes (i (length inputs))
      (let ((inp (aref inputs i)))
        (incf sum (case (type-of inp) (connection (connection-output inp))
                                      ('nil 0)
                                      (t inp)))))
    sum))

(defun clear-inputs (neuron)
  "Set the incoming connections to nil (marks them as not having fired yet),
  which allows the neuron to track whether it should fire or not."
  (declare (optimize (speed 3) (safety 1))
           (type neuron neuron))
  (let* ((inputs (neuron-inputs neuron))
         (num-inputs (length inputs)))
    (case (neuron-type neuron)
          (:input
            (loop for i from 0 to (1- num-inputs) do (setf (aref inputs i) nil)))
          ((:output :neuron)
           (dotimes (i (length inputs))
             (setf (connection-output (aref inputs i)) nil))))))

(defun all-inputs-recieved (neuron)
  "Find out if all inputs have been recieved or not. Non-recieved inputs will be
  nil. All inputs are reset to nil after a neuron has processed."
  (declare (optimize (speed 3) (safety 1))
           (type neuron neuron))
  (case (neuron-type neuron)
        (:input
          (when (aref (neuron-inputs neuron) 0) t))
        ((:output :neuron)
         (let ((inputs (neuron-inputs neuron)))
           (dotimes (i (length inputs))
             (unless (connection-output (aref inputs i))
               (return-from all-inputs-recieved nil)))
           t))))

(defun sigmoid (sum &key (max-sum 80) (slope *neuron-sigmoid-slope*))
  "Sigmoid function."
  (declare (optimize (speed 3) (safety 1))
           (type number sum)
           (type number max-sum)
           (type number slope))
  (let* ((sum (max (min (* sum slope) max-sum) (- max-sum))))
    (/ 1 (+ 1 (exp (- sum))))))


