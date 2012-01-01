(defclass net ()
  ((inputs :accessor net-inputs :initform nil)
   (outputs :accessor net-outputs :initform nil)))

(defun create-basic-net (&key (inputs 1) (outputs 1))
  "Create a basic network consisting of one neuron with the specified number of
  inputs and outputs. No structure, no funny stuff."
  (let ((net (make-instance 'net))
        (neuron (make-instance 'neuron)))
    (dotimes (i inputs)
      (let ((input (make-instance 'neuron :is-node t)))
        (setf (neuron-inputs input) (make-array 1 :adjustable t :fill-pointer 1))
        (create-connection input neuron :initial-value nil)
        (push input (net-inputs net))))
    (dotimes (i outputs)
      (let ((output (make-instance 'neuron :is-node t)))
        (create-connection neuron output :initial-value nil)
        (push output (net-outputs net))))
    (values net
            neuron)))

(defmethod run-net ((n net) (inputs list))
  "Set the network inputs and run the net recursively, returning all outputs as
  a list."
  (loop for value in inputs
        for input-neuron in (net-inputs n) do
        (setf (elt (neuron-inputs input-neuron) 0) value)
        (run-neuron input-neuron))
  (traverse-net n (lambda (neuron)
                    (setf (neuron-has-run neuron) nil)))
  (mapcar (lambda (output) (neuron-output output)) (net-outputs n)))

(defmethod traverse-net ((net net) (fn function) &key (avoid-duplicates t))
  (let ((run-neurons nil))
    (labels ((unique-neurons (connections)
               (remove-duplicates (loop for c in connections collect (connection-to c))))
             (do-traverse (neuron)
               (unless (and avoid-duplicates
                            (contains run-neurons neuron))
                 (push neuron run-neurons)
                 (funcall fn neuron)
                 (dolist (outgoing-neuron (unique-neurons (neuron-outputs neuron)))
                   (when outgoing-neuron
                     (do-traverse outgoing-neuron))))))
      (dolist (n (net-inputs net))
        (do-traverse n)))))

