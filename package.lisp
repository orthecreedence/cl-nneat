(defpackage :nneat
  (:use :cl :cl-user)
  (:export ;; network exports
           #:net
           #:net-genome
           #:create-basic-net
           #:create-net-from-genome
           #:run-net
           #:traverse-net
           #:modify-net
           #:print-net

           ;; neuron exports
           #:neuron
           #:run-neuron
           #:sigmoid

           ;; connection exports
           #:connection
           #:node-neuron-multiple-connections
           #:connection-is-required
           
           ;; genome exports
           #:genome
           #:crossover
           #:mutate

           ;; config exports
           #:*neuron-sigmoid-negatives*
           #:*neuron-sigmoid-slope*
           #:*neuron-sigmoid-multiplier*
           #:*neuron-abs-sigmoid*
           #:*neuron-default-threshold*
           #:*neuron-binary-output*
           #:*neuron-threshold-mutate-max*
           #:*connection-allow-negative-weights*
           #:*connection-weight-initial-max*
           #:*connection-weight-max*
           #:*connection-weight-mutate-max*
           #:*mutate-probabilities*))
