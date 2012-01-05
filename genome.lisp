(in-package :nneat)

(defclass genome ()
  ((genes :accessor genome-genes :initarg :genes :initform (make-array 0 :fill-pointer t :adjustable t)))
  (:documentation "A genome defines a sequential set of instructions for
   building a network from scratch. Genomes can be crossed-over and mutated,
   and the resulting genomes can be turned back into a network."))

(defmethod add-gene ((genome genome) (gene list))
  "Add a gene to the genome."
  (vector-push-extend gene (genome-genes genome)))

(defmethod get-object-in-genome ((genome genome) (id number))
  "Given a genome and a numeric id, find the corresponding object in the
  genes (usually it exists in a :create-* action)."
  (loop for gene across (genome-genes genome) do
    (let ((action (getf gene :action))
          (obj-id (getf gene :id)))
      (when (and (= id obj-id)
                 (or (eql action :create-neuron)
                     (eql action :create-connection)))
        (return-from get-object-in-genome (getf gene :object))))))

(defmethod update-genome-meta ((genome genome) (id integer) &rest meta)
  (let ((meta-list (plist-iter meta))
        (gene (find-if (lambda (g) (eql (getf g :id) id))
                       (genome-genes genome))))
    (if gene
        (progn (dolist (item meta-list)
                 (setf (getf (getf gene :meta) (car item)) (cadr item)))
               t)
        nil)))

(defmethod crossover ((mom-genome genome) (dad-genome genome) &key position)
  (let* ((mom (genome-genes mom-genome))
         (dad (genome-genes dad-genome))
         (length-mom (length mom))
         (length-dad (length dad))
         (position (or position
                       (/ (min length-mom length-dad) 2))))
    (let ((newmom (make-array 0 :fill-pointer t :adjustable t))
          (newdad (make-array 0 :fill-pointer t :adjustable t)))
      (dotimes (i (max length-mom length-dad))
        (let ((gene-m (if (< i length-mom)
                          (elt mom i)))
              (gene-d (if (< i length-dad)
                          (elt dad i))))
          (if (< i position)
              (progn (unless (null gene-m) (vector-push-extend gene-m newmom))
                     (unless (null gene-d) (vector-push-extend gene-d newdad)))
              (progn (unless (null gene-m) (vector-push-extend gene-m newdad))
                     (unless (null gene-d) (vector-push-extend gene-d newmom))))))
      (values (make-instance 'genome :genes newmom)
              (make-instance 'genome :genes newdad)))))

(defmethod mutate (net &key (probabilities *mutate-probabilities*))
  "Out of the given possible actions (and their probabilities of occuring) in
  probabilities, pick an action based on a weighted-random selection and perform
  the selected action."
  (let* ((probs (sort (plist-iter probabilities) (lambda (a b) (< (cadr b) (cadr a)))))
         (prob-sum (reduce (lambda (a b) (+ (if (numberp a) a (cadr a))
                                            (if (numberp b) b (cadr b))))
                           probs))
         (rand (random prob-sum))
         (test-sum 0))
    (multiple-value-bind (action id)
        (block :do-mutate
          (dolist (prob probs)
            (let ((action (car prob))
                  (probability (cadr prob)))
              (incf test-sum probability)
              (when (< rand test-sum)
                ;; define a handler in case mutate fails (trying to remove a 
                ;; required connection, etc) so the next operation can be
                ;; performed.
                (handler-case
                  (return-from :do-mutate 
                               (values action
                                       (do-mutate net action)))
                  (error () nil))))))
      (values net action id))))

(defmethod do-mutate (net (action keyword))
  (case action
        ;(:create-neuron
        ;  (id (modify-net net :create-neuron)))
        (:create-connection
          (let ((neurons nil))
            (traverse-net net (lambda (n) (when (eql (neuron-type n) :neuron)
                                            (push n neurons))))
            (let ((n1 (nth (random (length neurons)) neurons))
                  (n2 (nth (random (length neurons)) neurons)))
              (id (modify-net net :create-connection n1 n2)))))
        ((:split-connection :mutate-connection-weight)
          (let ((connections nil))
            (traverse-net net (lambda (n)
                                (when (eql (neuron-type n) :neuron)
                                  (loop for c across (neuron-inputs n) do
                                        (unless (contains connections c)
                                          (push c connections)))
                                  (loop for c in (neuron-outputs n) do
                                        (unless (contains connections c)
                                          (push c connections))))))
            (let ((connection (nth (random (length connections)) connections)))
              (case action
                    (:split-connection
                      (modify-net net :split-connection connection))
                    (:mutate-connection-weight
                      (incf (connection-weight connection)
                            (- (random (* 2 *connection-weight-mutate-max*))
                               *connection-weight-mutate-max*))
                      (update-genome-meta (net-genome net) (id connection) :weight (connection-weight connection))))
              (id connection))))
        (:remove-connection
          (let ((connections nil))
            (traverse-net net (lambda (n)
                                (when (eql (neuron-type n) :neuron)
                                  (loop for c across (neuron-inputs n) do
                                        (unless (eql (neuron-type (connection-from c)) :input)
                                          (push c connections)))
                                  (loop for c in (neuron-outputs n) do
                                        (unless (eql (neuron-type (connection-to c)) :output)
                                          (push c connections))))))
            (let ((connection (nth (random (length connections)) connections)))
              (modify-net net :remove-connection connection)
              (id connection))))
        (:mutate-neuron-threshold
          (let ((neurons nil))
            (traverse-net net (lambda (n) (when (eql (neuron-type n) :neuron)
                                            (push n neurons))))
            (let ((neuron (nth (random (length neurons)) neurons)))
              (incf (neuron-threshold neuron)
                    (- (random (* 2 *neuron-threshold-mutate-max*))
                       *neuron-threshold-mutate-max*))
              (update-genome-meta (net-genome net) (id neuron) :threshold (neuron-threshold neuron))
              (id neuron))))))
