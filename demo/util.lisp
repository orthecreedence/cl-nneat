(in-package :cl-nneat-demo)

(defmacro toggle-bool (frm)
  `(let ((val ,frm))
     (if val
         (setf ,frm nil)
         (setf ,frm t))))

(defun contains (sequence value &key (test #'equal))
  (if (listp sequence)
      (loop for v in sequence do (when (funcall test v value) (return-from contains t)))
      (loop for v across sequence do (when (funcall test v value) (return-from contains t)))))

(defun get-objects-of-type (objects type &key qualifier)
  (remove-if (lambda (o)
               (and (not (eql (type-of o) type))
                    (if qualifier
                        (funcall qualifier o)
                        t)))

             objects))

(defun map-type (sequence type)
  (cond ((arrayp sequence)
         (coerce (loop for x across sequence collect (coerce x type))
                 'vector))
        ((listp sequence)
         (loop for x in sequence collect (coerce x type)))))


(defmacro pow (val power)
  `(let ((cval ,val))
     (* ,@(loop for i from 1 to power collect 'cval))))

(defun angle-diff (x1 y1 x2 y2 cur-angle)
  "Calculates the angle of (x1,y1) -> (x2,y2) and diffs it with cur-angle. All
  angles are given and returned as degrees, convert to radians later on if you
  so wish."
  (let* ((x1 (if (zerop (- x1 x2))
                 (+ x1 1/1000)
                 x1))
         (angle (* (atan (- y2 y1) (- x2 x1))
                   (/ 180 pi)
                   -1)))
    (let ((angle-diff (- angle cur-angle)))
      (coerce (cond ((< 180 angle-diff)
                     (- angle-diff 360))
                    ((< 180 (- angle-diff))
                     (+ angle-diff 360))
                    (t angle-diff)) 'single-float))))
