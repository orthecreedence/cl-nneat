;;; this defines the configuration values for the nneat system: neuron
;;; activation function parameters, mutation probabilities, etc.
(in-package :nneat)

;; neuron defines, mainly having to do with the activation function (sigmoid)
;; and initial neuron parameters.
(defvar *neuron-sigmoid-negatives* nil
  "Does the sigmoid allow negatives? ie (sig -4) might be -0.62672")
(defvar *neuron-sigmoid-slope* 1
  "Define the slope parameter for the sigmoid function. 1 is a good value if
  you are unsure how this will affect the output, but playing around is always
  a good idea too.")
(defvar *neuron-sigmoid-multiplier* 1
  "Maximum value of the sigmoid output. This can be adjusted to fit your data,
  although it's mostly arbitrary and you'll get better results changing this in
  conjuction with *neuron-default-threshold*")
(defvar *neuron-default-threshold* 1/2
  "The initial threshold value for a new neuron (may be modified by mutation as
  the net progresses).")
(defvar *neuron-binary-output* nil
  "When set to true, a neuron may only output 1 (fired) or 0 (didn't fire). If
  nil, a neuron will output whatever its sigmoidal sum is.")
(defvar *neuron-always-output* t
  "When set to true, neurons don't fire per-se, but instead pass along their
  activation value to the next neurons in line.")
(defvar *neuron-output-passthrough* t
  "When true, an output neuron will always set its output to the final sigmoid,
  regardless of whether over the threshold or not (this effectively makes all
  output neurons have a threshold of 0).")

;; connection defines
(defvar *connection-allow-negative-weights* nil
  "Whether or not negative connection weights are allowed.")
(defvar *connection-weight-initial-max* 1.0
  "The ceiling for weight values when weights are initialized randomly.")
(defvar *connection-weight-max* 5.0
  "Maximum value a connection weight can be. If negative weights are allowed,
  the weight is run through (abs) before enforcing...ie if this is -5 and
  negatives are allowed, the weight can be between -5 and 5, if negatives aren't
  allowed, between 0 and 5.")

;; mutation parameters. controls how much each mutation is allowed to change,
;; and also defines the retio different types of mutations can happen.
(defvar *connection-weight-mutate-max* 0.3
  "When the mutation action :mutate-connection-weight occurs, this defines how
  much the connection weight of the chosen connection can be modified, up or
  down.")
(defvar *neuron-threshold-mutate-max* 0.01
  "When :mutate-neuron-threshold is selected, this defines how much, up or down,
  the selected neuron's threshold can be mutated.")
(defvar *mutate-probabilities* '(;:create-neuron 0
                                 :create-connection 1
                                 :split-connection 1
                                 :remove-connection 1
                                 :mutate-connection-weight 10
                                 :mutate-neuron-threshold 10)
  "Defines a property list for each mutation type and its likelyhood of occuring
  when a mutation happens. The probabilties are relative, so the actual values
  are not as important as their ratio to each other.")

