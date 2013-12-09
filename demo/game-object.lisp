(in-package :nneat-animals)

(defclass game-object ()
  ((x :accessor x :initarg :x :initform (random *window-x*))
   (y :accessor y :initarg :y :initform (random *window-y*))
   (angle :accessor angle :initarg :angle :initform (random 360))
   (speed :accessor speed :initarg :speed :initform 0)))

(defmethod run ((obj game-object) (objects vector))
  (let ((dir-rad (* (+ 90 (angle obj)) (/ pi 180)))
        (speed (* (speed obj) *object-speed-constant*)))
    (incf (x obj) (coerce (* (sin dir-rad) speed) 'single-float))
    (incf (y obj) (coerce (* (cos dir-rad) speed) 'single-float)))
  (constrain obj))

(defmethod constrain ((obj game-object))
  (let ((x (x obj))
        (y (y obj))
        (angle (angle obj)))
    (when (< *window-x* x) (setf (x obj) *window-x*))
    (when (< x 0) (setf (x obj) 0))
    (when (< *window-y* y) (setf (y obj) *window-y*))
    (when (< y 0) (setf (y obj) 0))
    (when (< 360 angle) (decf (angle obj) 360))
    (when (< angle 0) (incf (angle obj) 360)))
  obj)

(defmethod reset-object ((obj game-object) &key (x (random *window-x*))
                                           (y (random *window-y*))
                                           (speed 0)
                                           (angle 0))
  (setf (x obj) x
        (y obj) y
        (angle obj) angle
        (speed obj) speed))

(defun get-nearest-object (x y objects)
  (let ((n-dist 999999999)
        (nearest-object nil))
    (loop for o across objects do
          (let* ((ox (x o))
                 (oy (y o))
                 (dist (+ (pow (- x ox) 2) (pow (- y oy) 2))))
            (when (< dist n-dist)
              (setf n-dist dist)
              (setf nearest-object o))))
    nearest-object))
