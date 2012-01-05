(defpackage :nneat
  (:use :cl :cl-user)
  (:export #:net
           #:create-basic-net
           #:create-net-from-genome
           #:run-net
           #:traverse-net
           #:modify-net
           #:print-net

           #:neuron
           #:run-neuron
           #:sigmoid

           #:connection
           #:node-neuron-multiple-connections
           #:connection-is-required
           
           #:genome
           #:crossover
           #:mutate

           #:*neuron-sigmoid-negatives*
           #:*neuron-sigmoid-slope*
           #:*neuron-sigmoid-multiplier*
           #:*neuron-abs-sigmoid*
           #:*neuron-binary-output*
           #:*mutate-rate*
           #:*connection-weight-mutate-max*
           #:*neuron-threshold-mutate-max*
           #:*mutate-probabilities*))
