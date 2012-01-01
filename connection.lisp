(defclass connection ()
  ((from :accessor connection-from :initarg :from :initform nil)
   (to :accessor connection-to :initarg :to :initform nil)
   (to-slot :accessor connection-to-slot :initform nil)
   (weight :accessor connection-weight :initarg :weight :initform 1)
   (output :accessor connection-output :initarg :output :initform 0)))

(define-condition node-neuron-multiple-connections (error)
  ((text :initarg :text :reader text)))

(defmethod create-input ((to neuron) &key weight (initial-value 0))
  "Wrapper around create-connection to create a connection with no from, meaning
  an input-only neuron."
  (create-connection nil to :weight weight :initial-value initial-value))

(defmethod create-output ((from neuron) &key weight (initial-value 0))
  "Wrapper around create-neuron to create a connection with no to, resulting in
  an output-only neuron."
  (create-connection from nil :weight weight :initial-value initial-value))

(defun create-connection (from to &key weight (initial-value 0))
  "Given two neurons, set up connections between them."
  (when (or (and (neuron-is-node from)
                 (< 0 (length (neuron-outputs from))))
            (and (neuron-is-node to)
                 (< 0 (length (neuron-inputs to)))))
    (error 'node-neuron-multiple-connections
           :text "A node neuron can only have one incoming/outgoing connection"))
  (let ((connection (make-instance 'connection
                                   :from from
                                   :to to
                                   :output initial-value
                                   :weight (or weight (* 2 (- (random 1.0) 0.5))))))
    (when to
      (vector-push-extend connection (neuron-inputs to)))
    (when from
      (push connection (neuron-outputs from)))
    connection))

(defmethod remove-connection ((c connection))
  "Given a connection, unregister it with the two neurons it connects to. Cannot
  be called while the network is being processed. Well, it can, but I wouldn't."
  (let ((to (connection-to c))
        (from (connection-from c)))
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

(defmethod split-connection ((c connection) (n neuron))
  "Split a connection an put a neuron between the new parts, wiring everything
  back up as it was before. Also preserves the weight value for both of the new
  connections."
  (let ((weight (connection-weight c))
        (old-from (connection-from c))
        (old-to (connection-to c)))
    (remove-connection c)
    (create-connection old-from n :weight weight)
    (create-connection n old-to :weight weight)))

(defmethod activate-connection ((c connection) (value number) &key (propagate t))
  "Take the output of a neuron that fired (or didn't) and enter is as the input
  to the connection's TO neuron in the correct slot. This function takes care of
  weighting the value value (inp * weight) before entering the input to the
  resulting neuron."
  (let ((neuron (connection-to c))
        (output (* (connection-weight c) value)))
    (setf (connection-output c) output)
    ;(format t "connection output (~a): ~a~%" c output)
    (when neuron
      (when (and propagate
                 (not (neuron-has-run neuron)))
        (run-neuron neuron)))))
  
