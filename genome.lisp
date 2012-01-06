;;; Defines the genome class. The genome system works not by describing a
;;; network state, but instead describing the instructions on how to build the
;;; network. This was the only way I could think of to serialize a network into
;;; a one-dimensional array. Seems to work well for now.
;;;
;;; Note that the only problem with this approach is an ever-growing genome...
;;; even if a connection is added then removed later on, both instructions are
;;; still encoded into the genome indefinitely.
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
  "Given a genome and an object id, update the meta for that object with the
  given metadata. This is useful when mutating if you want to update a network's
  structure, but also want to make sure the metadata for that item is up to
  date."
  (let ((meta-list (plist-iter meta))
        (gene (find-if (lambda (g) (eql (getf g :id) id))
                       (genome-genes genome))))
    (if gene
        (progn (dolist (item meta-list)
                 (setf (getf (getf gene :meta) (car item)) (cadr item)))
               t)
        nil)))

(defmethod crossover ((mom-genome genome) (dad-genome genome) &key position)
  "Run the genetic crossover function on two genomes. This specific version
  finds the mid-point of the shortest genome, and swaps the genes after it with
  the paired genome. For ex:

            | midpoint of shortest gene
     aabacbddbaddba
     aabdccdcbaaadacca

  would yield the genomes:

            | ends of genes are swapped after this point
     aabacbdcbaaadacca
     aabdccddbaddba

  Note that these are not what real genomes look like, this is just a functional
  example."
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
  the selected action.
  
  Listens for errors in do-mutate and tries the next available mutation if one
  occurs."
  (let* ((probs (sort (plist-iter probabilities) (lambda (a b) (< (cadr b) (cadr a)))))
         (prob-sum (reduce (lambda (a b) (+ (if (numberp a) a (cadr a))
                                            (if (numberp b) b (cadr b))))
                           probs))
         (rand (random prob-sum))
         (test-sum 0))
    (multiple-value-bind (action id)
        (block do-mutate
          (dolist (prob probs)
            (let ((action (car prob))
                  (probability (cadr prob)))
              (incf test-sum probability)
              (when (< rand test-sum)
                ;; define a handler in case mutate fails (trying to remove a 
                ;; required connection, etc) so the next operation can be
                ;; performed.
                (handler-case
                  (return-from do-mutate 
                               (values action
                                       (do-mutate net action)))
                  (error (err)
                    (format t "Mutation error occured, action: ~a, error: ~a~%" action err)))))))
      (values net action id))))

(defmethod do-mutate (net action)
  "Perform mutation on a network. Does its best to follow all given genetic
  rules, but does no work to catch errors. This must be done by the caller.
  This returns the id of whatever object was operated on."
  (case action
        ;; creating neurons is tricky business, it's best to just split a 
        ;; connection.
        ;(:create-neuron
        ;  (id (modify-net net :create-neuron)))

        ;; create a new connection. new connections are only allowed to connect
        ;; non-input/output neurons to non-input neurons.
        (:create-connection
          (let ((neurons-from nil)
                (neurons-to nil))
            ;; grab all eligible neurons
            (traverse-net net (lambda (n) (when (not (eql (neuron-type n) :input))
                                            (push n neurons-to))
                                          (when (eql (neuron-type n) :neuron)
                                            (push n neurons-from))))
            ;; pick two random neurons from the eligible pool and connect them
            (let ((n1 (nth (random (length neurons-from)) neurons-from))
                  (n2 (nth (random (length neurons-to)) neurons-to)))
              (id (modify-net net :create-connection n1 n2)))))

        ;; split an existing connection, or mutate a connection weight
        ((:split-connection :mutate-connection-weight)
          (let ((connections nil))
            ;; grab a unique list of all existing connections
            (traverse-net net (lambda (n)
                                (when (eql (neuron-type n) :neuron)
                                  (loop for c across (neuron-inputs n) do
                                        (unless (contains connections c)
                                          (push c connections)))
                                  (loop for c in (neuron-outputs n) do
                                        (unless (contains connections c)
                                          (push c connections))))))
            ;; pick a connection at random out of the eligible connection pool
            (let ((connection (nth (random (length connections)) connections)))
              (case action
                    ;; split the connection with a new neuron
                    (:split-connection
                      (modify-net net :split-connection connection))
                    ;; modify the connection's weight value
                    (:mutate-connection-weight
                      (incf (connection-weight connection)
                            (- (random (* 2 *connection-weight-mutate-max*))
                               *connection-weight-mutate-max*))
                      ;; if negatives weights aren't allowed, enforce here
                      (unless *connection-allow-negative-weights*
                        (setf (connection-weight connection)
                              (max 0 (connection-weight connection))))
                      ;; make sure the weight isn't exceeding its maximum(s)
                      (let* ((weight (connection-weight connection))
                             (weight-abs (abs weight)))
                        (when (< *connection-weight-max* weight-abs)
                          (if (< 0 weight)
                              (setf (connection-weight connection) *connection-weight-max*)
                              (setf (connection-weight connection) (- *connection-weight-max*)))))
                      ;; keep the weight value in sync with the genome's 
                      ;; metadata (used to rebuild the net from the genome)
                      (update-genome-meta (net-genome net) (id connection) :weight (connection-weight connection))))
              (id connection))))

        ;; remove a connection from the network. removed connection must not be
        ;; between any neuron and an input/output, and a connection which is the
        ;; last input or output of a neuron cannot be removed either (although
        ;; this is enforced in (remove-connection) and not here).
        (:remove-connection
          (let ((connections nil))
            ;; grab a list of eligible connections
            (traverse-net net (lambda (n)
                                (unless (eql (neuron-type n) :input)
                                  (loop for c across (neuron-inputs n) do
                                        (unless (contains connections c)
                                          (push c connections))))))
            ;; pick a connection and modify it
            (let ((connection (nth (random (length connections)) connections)))
              (modify-net net :remove-connection connection)
              (id connection))))

        ;; mutate the threshold value of a neuron. 
        (:mutate-neuron-threshold
          (let ((neurons nil))
            ;; grab all neurons/outputs
            (traverse-net net (lambda (n) (when (not (eql (neuron-type n) :input))
                                            (push n neurons))))
            ;; pick a random neuron and modify its threshold
            (let ((neuron (nth (random (length neurons)) neurons)))
              (incf (neuron-threshold neuron)
                    (- (random (* 2 *neuron-threshold-mutate-max*))
                       *neuron-threshold-mutate-max*))
              ;; keep the threshold above 0
              (setf (neuron-threshold neuron)
                    (max 0 (neuron-threshold neuron)))
              ;; keep the threshold in the genome metadata in sync the the 
              ;; network's value
              (update-genome-meta (net-genome net) (id neuron) :threshold (neuron-threshold neuron))
              (id neuron))))))
