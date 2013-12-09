(defpackage :cl-nneat
  (:use :cl :cl-user)
  (:export ;; network exports
           #:net
           #:net-genome
           #:create-basic-net
           #:create-net-from-genome
           #:run-net
           #:stimulate-net
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
           #:genome-genes
           #:crossover
           #:mutate

           ;; config exports
           #:*neuron-sigmoid-negatives*
           #:*neuron-sigmoid-slope*
           #:*neuron-sigmoid-multiplier*
           #:*neuron-default-threshold*
           #:*neuron-binary-output*
           #:*neuron-always-output*
           #:*neuron-output-passthrough*
           #:*dynamic-neuron*
           #:*dynamic-neuron-self-govern*
           #:*dynamic-neuron-threshold-delta*
           #:*dynamic-neuron-weight-delta*
           #:*neuron-threshold-mutate-max*
           #:*connection-allow-negative-weights*
           #:*connection-weight-initial-max*
           #:*connection-weight-max*
           #:*connection-weight-mutate-max*
           #:*mutate-probabilities*))
