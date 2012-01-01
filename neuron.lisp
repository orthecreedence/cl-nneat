(defclass neuron ()
  ((inputs :accessor neuron-inputs :initform (make-array 0 :adjustable t :fill-pointer t))
   (outputs :accessor neuron-outputs :initform nil)
   (output :accessor neuron-output :initform nil)
   (threshold :accessor neuron-threshold :initform .5)
   (has-run :accessor neuron-has-run :initform nil)
   (is-node :accessor neuron-is-node :initarg :is-node :initform nil)))

(defmethod run-neuron ((n neuron) &key (propagate t))
  "Take this neuron's inputs and calculate the output value."
  (when (all-inputs-recieved n)
    ;(format t "~%running ~a ~a~%" (if (neuron-is-node n) "node" "neuron") n)
    (if (neuron-is-node n)
        ;; neuron is a node, meaning it blindly passes its input to output (only
        ;; one input allowed, obviously). this is good for input/output neurons
        (setf (neuron-output n)
              (get-neuron-input n 0))
        ;; regular neuron, sum up and threshold the inputs
        (let ((result (sig (reduce (lambda (inp1 inp2) (+ (if (numberp inp1) inp1 (connection-output inp1))
                                                          (if (numberp inp2) inp2 (connection-output inp2))))
                                   (neuron-inputs n)))))
          (if (< (neuron-threshold n) result)
              (setf (neuron-output n) (if *neuron-binary-output* 1 result))
              (setf (neuron-output n) 0))))
    ;; neuron ran, mark it as such
    (setf (neuron-has-run n) t)
    (clear-inputs n)
    ;; push the output value to all outgoing connections
    (run-outputs n :propagate propagate)))

(defmethod get-neuron-input ((n neuron) (index number))
  (let ((input (elt (neuron-inputs n) index)))
    (cond ((numberp input)
           input)
          ((eql (type-of input) 'connection)
           (connection-output input))
          (t input))))

(defmethod run-outputs ((n neuron) &key (propagate t))
  "Process all outgoing outputs for this neuron."
  ;(format t "running outputs for ~a ~a:~%~a~%" (if (neuron-is-node n) "node" "neuron") n (neuron-outputs n))
  (dolist (c (neuron-outputs n))
    (activate-connection c (neuron-output n) :propagate propagate)))

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
  (if (neuron-is-node n)
      (not (not (elt (neuron-inputs n) 0)))
      (zerop (length (remove-if (lambda (c) (connection-output c))
                                (neuron-inputs n))))))

(defun sig (sum)
  "Sigmoid function."
  (let ((sig (/ 1 (+ 1 (exp (- (if (< sum 80) sum 80)))))))
    (if *neuron-sigmoid-negatives*
        (* 2 (- sig 1/2))
        sig)))

