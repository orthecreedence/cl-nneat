(in-package :cl-nneat-demo)

(defpackage :cl-nneat-demo
  (:use :cl :cl-nneat :cl-hash-util))
(in-package :cl-nneat-demo)

(setf *random-state* (make-random-state t))

;; profile!!
#|
nneat::contains
nneat::all-inputs-recieved
nneat:traverse-net
nneat:run-net
nneat::activate-connection
nneat::run-outputs
nneat::clear-inputs
nneat::sum-inputs
nneat:sigmoid
nneat:run-neuron
;; accessors...
nneat::neuron-has-run
nneat::connection-to
nneat::connection-output
nneat::connection-weight
nneat::neuron-outputs
nneat::neuron-inputs
nneat::neuron-type

;; animals
nneat-animals::run
nneat-animals::get-objects-of-interest
nneat-animals::raytrace-nearby-objects
|#
;; /profile!!


(defparameter *world* nil)
(defparameter *main-thread* nil)
(defun run-app ()
  (let* ((window-cl (lambda (w)
                      (declare (ignore w))
                      (sdl:with-events (:poll)
                        (:quit-event () t)
                        (:video-expose-event () (sdl:update-display))
                        (:key-down-event (:key key)
                          (when (sdl:key= key :sdl-key-q)
                            (sdl:push-quit-event))
                          (when (sdl:key= key :sdl-key-escape)
                            (sdl:push-quit-event))
                          (when (sdl:key= key :sdl-key-p)
                            (toggle-bool (world-pause *world*)))
                          (when (sdl:key= key :sdl-key-r)
                            (setf *world* (create-world))))
                        ;; when not processing events, step the world
                        (:idle ()
                          (gl:clear :color-buffer :depth-buffer)
                          (step-world *world*)
                          (draw-world *world*)
                          (sdl:update-display))))))
    (setf *world* (create-world))
    (sdl-window:create-window window-cl
                              :title "NNEAT animals"
                              :width (+ *window-x* (* 2 *window-padding*))
                              :height (+ *window-y* (* 2 *window-padding*))
                              :background '(1 1 1 0)))
  (stop))

(defun start ()
  (unless *main-thread*
    (setf *main-thread* (bt:make-thread #'run-app))))

(defun stop ()
  (when (and *main-thread*
             (bt:threadp *main-thread*))
    (unless (equal *main-thread* (bt:current-thread))
      (when (bt:thread-alive-p *main-thread*)
        (bt:destroy-thread *main-thread*)))
    (setf *main-thread* nil)))
