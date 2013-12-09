(in-package :nneat-animals)

(defparameter *window-x* 600)
(defparameter *window-y* 600)
(defparameter *window-padding* 10)

(defparameter *ticks-in-generation* 500)

(defparameter *object-speed-constant* 6)
(defparameter *angle-multiplier* 40)

(defparameter *world-dynamic-populations* nil)
(defparameter *fitness-survival-threshold* 0.4)
(defparameter *fitness-children-multiplier* 1.5)

(defparameter *num-food* 32)
(defparameter *food-size* 2)
(defparameter *chew-ticks* 20)
(defparameter *animal-awareness* 90)

(defparameter *search-distance* 8)
(defparameter *animal-config* (hash ('scavenger 
                                     (hash (:population 24)
                                           (:vision (hash (:distance 100)
                                                          (:distance-resolution 10)
                                                          (:fov 60)
                                                          (:angle-resolution 5)))
                                           (:hidden 4)
                                           (:outputs 2)
                                           (:classifiers (hash ('scavenger -1)
                                                               ('predator 2)
                                                               ('food -2)
                                                               ('empty .1)))))
                                    ('predator
                                     (hash (:population 4)
                                           (:vision (hash (:distance 100)
                                                          (:distance-resolution 10)
                                                          (:fov 60)
                                                          (:angle-resolution 5)))
                                           (:hidden 4)
                                           (:outputs 2)
                                           (:classifiers (hash ('scavenger -3)
                                                               ('predator -1)
                                                               ('food -2)
                                                               ('empty .1)))))))

(defparameter *scavenger-size* 3.5)

(defparameter *predator-size* 3.5)
(defparameter *predator-chomp-value* 1)
(defparameter *predator-satiation-penalty* 0.4)

;; nneat parameters
(setf *neuron-sigmoid-negatives* nil)
(setf *connection-allow-negative-weights* t)
(setf *neuron-sigmoid-slope* 1);4.924273)
(setf *neuron-always-output* nil)
(setf *neuron-default-threshold* 0.30)
(setf *neuron-output-passthrough* t)
(setf *connection-weight-initial-max* 1.0)

;; neuron config
(setf *dynamic-neuron* nil)
(setf *dynamic-neuron-self-govern* nil)
(setf *dynamic-neuron-threshold-delta* 1.02)
(setf *dynamic-neuron-weight-delta* 1.002)
(defparameter *stimulate-amount* 10)

(defparameter *crossover-rate* 0.7)

(defparameter *mutation-rate* 0.02)
(setf *connection-weight-mutate-max* 0.6)
(setf *neuron-threshold-mutate-max* 0.1)
(setf *mutate-probabilities* '(;:create-neuron 0
                               :create-connection 1
                               :split-connection 1
                               :remove-connection 1
                               :mutate-neuron-threshold 100
                               :mutate-connection-weight 100))
