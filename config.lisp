(in-package :nneat)

(defparameter *neuron-sigmoid-negatives* t)
(defparameter *neuron-sigmoid-slope* 1)
(defparameter *neuron-sigmoid-multiplier* 1)
(defparameter *neuron-abs-sigmoid* t)
(defparameter *neuron-binary-output* nil)

(defparameter *mutate-rate* 0.01)
(defparameter *connection-weight-mutate-max* 0.3)
(defparameter *neuron-threshold-mutate-max* 0.01)
(defparameter *mutate-probabilities* '(;:create-neuron 0
                                       :create-connection 1
                                       :split-connection 1
                                       :remove-connection 1
                                       :mutate-connection-weight 10
                                       :mutate-neuron-threshold 10))

