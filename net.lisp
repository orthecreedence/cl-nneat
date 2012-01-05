(in-package :nneat)

(defclass net ()
  ((inputs :accessor net-inputs :initform nil)
   (outputs :accessor net-outputs :initform nil)
   (genome :accessor net-genome :initform nil)
   (ids :accessor net-ids :initform 0)))

(defun create-basic-net (&key (inputs 1) (outputs 1))
  "Create a basic network consisting of one neuron with the specified number of
  inputs and outputs. No structure, no funny stuff."
  (let ((net (make-instance 'net)))
    (setf (net-genome net) (make-instance 'genome))
    (let ((neuron (modify-net net :create-neuron)))
      (dotimes (i inputs)
        (let ((input (modify-net net :create-neuron :type :input)))
          (setf (neuron-inputs input) (make-array 1 :adjustable t :fill-pointer 1))
          (modify-net net :create-connection input neuron :initial-value nil)
          (push input (net-inputs net))))
      (dotimes (i outputs)
        (let ((output (modify-net net :create-neuron :type :output)))
          (modify-net net :create-connection neuron output :initial-value nil)
          (push output (net-outputs net))))
      (values net
              neuron))))

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
  "Run a function on each neuron in the given network (in no particular order,
  and once per neuron)."
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

(defmethod next-id ((net net))
  "Grab the next available id from this network for assignment to an object."
  (1- (incf (net-ids net))))

(defmethod modify-net ((net net) (action keyword) &rest args)
  (let ((obj-id nil)
        (obj nil)
        (obj-meta nil))
    (labels ((create-with-id (fn args)
               "Call the given function, give a unique id to the resulting
               object, and set the object, its id, and any metadata into our top
               level bindings."
               (let ((new-obj (apply fn args)))
                  (setf obj-id (next-id net))
                  (setf (id new-obj) obj-id)
                  (setf obj new-obj)
                  obj)))
      ;; create the object/object-id for whatever action we're taking
      (case action
            (:create-neuron
              (let ((neuron (create-with-id #'create-neuron args)))
                (setf obj-meta (list :type (neuron-type neuron)
                                     :threshold (neuron-threshold neuron)))))
            (:create-connection
              (let ((connection (create-with-id #'create-connection args)))
                (setf obj-meta (list :from (id (connection-from connection))
                                     :to (id (connection-to connection))
                                     :weight (connection-weight connection)))))
            (:remove-connection
              (let ((connection (car args)))
                (setf obj-id (id connection))
                (appendf obj-meta (list :ignore-errors (c-getf args :ignore-errors)))
                (apply #'remove-connection args)))
            (:split-connection
              ;; batching is hard, so let's just recursively call each operation
              ;; of split-connection to break it down to its pieces.
              (let* ((connection (car args))
                     (from (connection-from connection))
                     (to (connection-to connection))
                     (weight (connection-weight connection)))
                (let ((neuron (modify-net net :create-neuron)))
                  (modify-net net :remove-connection connection :ignore-errors t)
                  (modify-net net :create-connection from neuron :weight weight)
                  (modify-net net :create-connection neuron to))))))
    (case action
          ((:create-neuron :create-connection)
           (add-gene (net-genome net) 
                     `(:action ,action
                       :id ,obj-id
                       :object ,obj
                       :meta ,obj-meta)))
          ((:remove-neuron :remove-connection)
           (add-gene (net-genome net) 
                     `(:action ,action
                       :id ,obj-id
                       :meta ,obj-meta)))
          (:split-connection
            ;; no action needed, since the sub-actions were encoded already lol
            nil))
    obj))

(defmethod create-net-from-genome ((genome genome))
  (let* ((net (make-instance 'net))
         (genes (progn (setf (net-genome net) (make-instance 'genome)) (net-genome net))))
    (loop for gene across (genome-genes genome) do
      (let ((action (getf gene :action))
            (id (getf gene :id))
            (meta (getf gene :meta)))
        (case action
              (:create-neuron
                (let ((neuron (apply #'modify-net (append (list net :create-neuron) meta)))
                      (type (getf meta :type)))
                  (case type (:input
                               (push neuron (net-inputs net)))
                             (:output
                               (push neuron (net-outputs net))))))
              (:create-connection
                (apply #'modify-net (append (list net
                                                  :create-connection
                                                  (get-object-in-genome genes (getf meta :from))
                                                  (get-object-in-genome genes (getf meta :to)))
                                            (remprops meta '(:from :to)))))
              (:remove-connection
                (apply #'modify-net (append (list net
                                                  :remove-connection
                                                  (get-object-in-genome genes id))
                                            (remprops meta '(:from :to))))))))
    net))
