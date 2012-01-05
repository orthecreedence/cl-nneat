(defpackage :nneat
  (:use :cl :cl-user))
(in-package :nneat)

#|
(defparameter *x* 300)
(defparameter *y* 300)

(defun draw (net)
  (let ((out (run-net net (list (/ *x* 100) (/ *y* 100)))))
    (incf *x* (car out))
    (incf *y* (cadr out)))
  (gl:color 0 0 0 1)
  (gl:push-matrix)
  (gl:begin :polygon)
    (gl:vertex (1- *x*) (1- *y*))
    (gl:vertex (1+ *x*) (1- *y*))
    (gl:vertex (1+ *x*) (1+ *y*))
    (gl:vertex (1- *x*) (1+ *y*))
  (gl:end)
  (gl:pop-matrix))

(defun start()
  (multiple-value-bind (net n1) (create-basic-net :inputs 2 :outputs 2)
    (let ((window-cl (lambda (w)
                       (declare (ignore w))
                       (sdl:with-events (:poll)
                         (:quit-event () t)
                         (:video-expose-event () (sdl:update-display))
                         (:key-down-event (:key key)
                           (when (sdl:key= key :sdl-key-q)
                             (sdl:push-quit-event))
                           (when (sdl:key= key :sdl-key-escape)
                             (sdl:push-quit-event)))
                         ;; when not processing events, step the world
                         (:idle ()
                           (gl:clear :color-buffer :depth-buffer)
                           (draw net)
                           (sdl:update-display))))))
      (sdl-window:create-window window-cl
                                :title "NNEAT test"
                                :width 600
                                :height 600
                                :background '(1 1 1 0)))))
|#
