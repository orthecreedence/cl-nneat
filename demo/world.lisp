(in-package :cl-nneat-demo)

(defclass world ()
  ((objects :accessor world-objects :initform (make-array 0 :fill-pointer t :adjustable t))
   (ticks :accessor world-ticks :initform 0)
   (pause :accessor world-pause :initform nil)
   (generations :accessor world-generations :initform 0)))

(defun create-world (&key world (num-food *num-food*))
  (let ((world (or world (make-instance 'world))))
    (setf (world-objects world) (make-array 0 :fill-pointer t :adjustable t))
    (setf (world-ticks world) 0)
    (dotimes (i num-food)
      (vector-push-extend (make-instance 'food) (world-objects world)))
    (loop for type being the hash-keys of *animal-config*
          for config being the hash-values of *animal-config* do
          (world-replace-animal world type nil :num-objects (hash-get config '(:population))))
    world))

(defmethod step-world ((world world))
  (when (world-pause world)
    (return-from step-world))
  (let ((objects (world-objects world)))
    (loop for obj across objects do
          (run obj objects)))
  (incf (world-ticks world))
  (when (<= *ticks-in-generation* (world-ticks world))
    (setf (world-ticks world) 0)
    (incf (world-generations world))
    (mating-season world)))

(defmethod mating-season ((world world))
  (format t "Generation: ~a~%" (world-generations world))
  (let ((mate-types '(scavenger predator)))
    (dolist (obj-type mate-types)
      (let* ((animals (get-objects-of-type (world-objects world) obj-type))
             (num-animals (length animals))
             (animals (sort animals
                            (lambda (a b) (> (if (numberp a) a (animal-fitness a))
                                             (if (numberp b) b (animal-fitness b))))))
             (animals (remove-if (lambda (a) (animal-dead a)) animals))
             ;(FUCK (setf (animal-fitness (elt animals 0)) 5))
             (fitness-sum (if (< (length animals) 2)
                              0
                              (reduce (lambda (a b) (+ (if (numberp a) a (animal-fitness a))
                                                       (if (numberp b) b (animal-fitness b))))
                                      animals))))
        (cond ((zerop num-animals) 
               (format t "~a has empty starting population, doing nothing.~%" obj-type))
              ((or (zerop fitness-sum)
                   (< (length animals) 2))
               (format t "~a has a fitness sum of zero. Killing all of them.~%" obj-type)
               (world-replace-animal world obj-type nil :num-objects num-animals))
              (t
                ;(format t "Evolving ~a~%" obj-type)
                (print-generation-stats animals obj-type)
                ;; loop over allowed mates and for each of the best, select one (not
                ;; itself) and mate with it
                (let ((children (if *world-dynamic-populations*
                                  (create-dynamic-generation animals obj-type :fitness-sum fitness-sum)
                                  (create-static-generation animals obj-type num-animals :fitness-sum fitness-sum))))
                  (let ((avg (/ (reduce (lambda (a b) (+ (if (numberp a) a (length (genome-genes (net-genome (animal-net a)))))
                                                         (if (numberp b) b (length (genome-genes (net-genome (animal-net b)))))))
                                        children)
                                (length children))))
                    (format t "~a avg genome length: ~f~%" obj-type avg))
                    ;(format t "~a ~as killed, new pop size: ~a~%" num-killed obj-type (length children)))
                  ;; clear out all objects of type obj-type and append our children in.
                  ;; the old generation is dead and gone.
                  (world-replace-animal world obj-type children))))))
    (format t "------------~%")))

(defmethod world-replace-animal ((world world) obj-type objects &key num-objects)
  (unless objects
    ;; no objects were given, create "maximum" count objects
    (dotimes (i num-objects)
      (let ((obj (create-animal-of-type obj-type)))
        (push obj objects))))
  (let ((no-more-of-those-types (remove-if (lambda (o) (eql (type-of o) obj-type))
                                           (world-objects world))))
    (setf (world-objects world)
          (make-array (length no-more-of-those-types)
                      :fill-pointer t
                      :adjustable t
                      :initial-contents no-more-of-those-types))
    ;; objects are clear of all types obj-type, add our children back in
    (loop for o in objects do
          (vector-push-extend o (world-objects world)))))

(defun create-dynamic-generation (animals animal-type &key (fitness-sum 1))
  (let ((fitness-avg (/ fitness-sum (length animals)))
        (children nil))
    (dotimes (i (length animals))
      (let ((mom (elt animals i)))
        (when (< (animal-fitness mom) (* fitness-avg *fitness-survival-threshold*))
          (setf (animal-dead mom) t))
        (unless (animal-dead mom)
          (let* ((allowed-children (floor (* (/ (animal-fitness mom) fitness-avg)
                                             *fitness-children-multiplier*)))
                 (all-but-mom (remove-if (lambda (a) (equal a mom)) animals)))
            (loop for i from 1 to allowed-children
                  for dad across all-but-mom do
                  (let ((child-genomes (multiple-value-list
                                         (crossover (net-genome (animal-net mom))
                                                    (net-genome (animal-net dad))
                                                    :position (random (min (length (genome-genes (net-genome (animal-net mom))))
                                                                           (length (genome-genes (net-genome (animal-net dad))))))
                                                    :probability *crossover-rate*))))
                    ;; mom and dad found, "mate" them (crossover) to yield two children
                    ;; who are then mutated genetically and pushed into the "children"
                    ;; var for later injection back into the population
                    (dolist (child-genome child-genomes)
                      (let* ((net (create-net-from-genome child-genome)))
                        (when (< (random 1.0) *mutation-rate*)
                          (format t "  ~a mutation occured: ~a~%" animal-type (cadr (multiple-value-list (mutate net)))))
                        (when (<= allowed-children i)
                          (return-from create-dynamic-generation children))
                        (push (make-instance animal-type :net net
                                             :initial-genome-length (animal-initial-genome-length mom))
                              children)))))))))
    children))

(defun create-static-generation (animals animal-type num-children &key (fitness-sum 1))
  (let ((children nil))
    (block do-mate
      (loop do
        (dotimes (i (length animals))
          (let* ((mom (elt animals i))
                 (all-but-mom (remove-if (lambda (a) (or (equal a mom)
                                                         (animal-dead a))) animals))
                 (fitness-avg (/ fitness-sum (length animals)))
                 (allowed-mates (floor (* (/ (animal-fitness mom) fitness-avg)
                                          *fitness-children-multiplier*)))
                 (rand (random fitness-sum))
                 (tmp-sum 0))
            ;(format t "Alowed mates: ~a~%" allowed-mates)
            ;(format t "all-but ~a  rnd ~a  sum ~a  so-far ~a~%" (length all-but-mom) rand fitness-sum (length children))
            (loop for i from 1 to allowed-mates
                  for dad across all-but-mom do
                  (incf tmp-sum (animal-fitness dad))
                  (when (<= tmp-sum rand)
                    (let ((child-genomes (multiple-value-list
                                           (crossover (net-genome (animal-net mom))
                                                      (net-genome (animal-net dad))
                                                      :position (random (min (length (genome-genes (net-genome (animal-net mom))))
                                                                             (length (genome-genes (net-genome (animal-net dad))))))
                                                      :probability *crossover-rate*))))
                      (dolist (child-genome child-genomes)
                        (let ((net (create-net-from-genome child-genome)))
                          (when (< (random 1.0) *mutation-rate*)
                            (format t "  ~a mutation occured: ~a~%" animal-type (cadr (multiple-value-list (mutate net)))))
                          (when (<= num-children (length children))
                            (return-from do-mate))
                          (push (make-instance animal-type :net net
                                               :initial-genome-length (animal-initial-genome-length mom))
                                children))))))
            (when (<= num-children (length children))
              (return))))
        while (< (length children) num-children)))
    (when (< (length children) num-children)
      (let ((diff (- num-children (length children))))
        (format t "  - ~a population is ~a children short, creating.~%" animal-type diff)
        (dotimes (i diff)
          (push (create-animal-of-type animal-type) children))))
    children))

(defun print-generation-stats (animals animal-type)
  (let* ((max (reduce (lambda (a b) (max (if (numberp a) a (animal-fitness a))
                                         (if (numberp b) b (animal-fitness b))))
                      animals))
         (min (reduce (lambda (a b) (min (if (numberp a) a (animal-fitness a))
                                         (if (numberp b) b (animal-fitness b))))
                      animals))
         (sum (reduce (lambda (a b) (+ (if (numberp a) a (animal-fitness a))
                                       (if (numberp b) b (animal-fitness b))))
                      animals))
         (avg (/ sum (length animals))))
    (format t "~a had max: ~a, min: ~a, avg: ~a~%" animal-type max min (coerce avg 'single-float))))

(defmethod draw-world ((world world))
  (let ((objects (world-objects world)))
    (loop for obj across objects do
          (draw obj))))
