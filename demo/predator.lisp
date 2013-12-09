(in-package :nneat-animals)

(defclass predator (animal) ())

(defmethod process-outputs ((animal predator) objects outputs)
  (let* ((angle-diff (car outputs))
         (speed (cadr outputs)))
    (if *neuron-sigmoid-negatives*
        (progn
          (incf (angle animal) (* *angle-multiplier* angle-diff))
          (setf (speed animal) speed))
        (progn
          (incf (angle animal) (* (* 2 (- angle-diff 1/2)) *angle-multiplier*))
          (let* (;(speed (1- (* 2 speed)))
                 (speed (if (< (animal-chewing animal) 1)
                          speed
                          (* speed *predator-satiation-penalty*))))
            (setf (speed animal) speed)))))
  (eat animal (get-nearest-object (x animal)
                                  (y animal)
                                  (get-objects-of-type objects 'scavenger))))

#|
(defmethod run_ ((animal predator) (objects vector))
  (let* (;(food (get-objects-of-type objects 'food))
         (scavengers (get-objects-of-type objects 'scavenger))
         ;(scavengers (remove-if (lambda (a) (animal-dead a)) scavengers))
         (x (x animal))
         (y (y animal))
         ;(nearest-food (get-nearest-object x y food))
         (nearest-scavenger (get-nearest-object x y scavengers)))
    ;; grab the nearest food item
    (let* (;(fx (x nearest-food))
           ;(fy (y nearest-food))
           (sx (x nearest-scavenger))
           (sy (y nearest-scavenger))
           (animal-angle (angle animal))
           ;(food-angle-diff (/ (angle-diff x y fx fy animal-angle) 90))
           ;(food-dist (/ (sqrt (+ (pow (- x fx) 2) (pow (- y fy) 2))) *window-x*))
           ;(food-amount (food-amount nearest-food))
           (scavenger-angle-diff (/ (angle-diff x y sx sy animal-angle) 90))
           (scavenger-dist (/ (sqrt (+ (pow (- x sx) 2) (pow (- y sy) 2))) *window-x*))
           (scavenger-fitness (animal-fitness nearest-scavenger))
           ;(scavenger-dead (if (animal-dead nearest-scavenger) 1 -1))
           (chew (/ (animal-chewing animal) *chew-ticks*))
           (inputs (list ;food-angle-diff
                         ;food-dist
                         ;food-amount
                         scavenger-angle-diff
                         scavenger-dist
                         scavenger-fitness
                         ;scavenger-dead
                         chew)))
      ;(format t "a: (~a, ~a) < ~a      f: (~a, ~a), < ~a~%" x y animal-angle (x nearest-food) (y nearest-food) food-angle-diff)
      (let ((outputs (run-net (animal-net animal) inputs)))
        ;(format t "inputs: ~a, outputs: ~a~%" inputs outputs)
        (let* ((angle-diff (car outputs))
               (speed (cadr outputs)))
          (if *neuron-sigmoid-negatives*
              (progn
                (incf (angle animal) (* *angle-multiplier* angle-diff))
                (setf (speed animal) speed))
              (progn
                (incf (angle animal) (* (* 2 (- angle-diff 1/2)) *angle-multiplier*))
                (let* ((speed (1- (* 2 speed)))
                       (speed (if (< (animal-chewing animal) 1)
                                  speed
                                  (* speed *predator-satiation-penalty*))))
                  (setf (speed animal) speed))))))
      (call-next-method animal objects)
      (eat animal nearest-scavenger))))
|#

(defmethod eat ((animal predator) (scavenger scavenger))
  (let* ((x (x animal))
         (y (y animal))
         (sx (x scavenger))
         (sy (y scavenger))
         (dist (sqrt (+ (pow (- x sx) 2) (pow (- y sy) 2)))))
    (if (< (animal-chewing animal) 1)
        (when (and (< dist (+ 4 *predator-size* *scavenger-size*))
                   (not (animal-dead scavenger)))
          (stimulate-net (animal-net animal) *stimulate-amount*)
          (stimulate-net (animal-net scavenger) (- *stimulate-amount*))
          (setf (animal-chewing animal) (* 2 *chew-ticks*))
          (incf (animal-fitness animal) 1)
          (decf (animal-fitness scavenger) *predator-chomp-value*)
          (when (< (animal-fitness scavenger) 0)
            ;; if they killed the animal, give them a fitness boost
            (incf (animal-fitness animal) 1)
            (kill scavenger)))
        (decf (animal-chewing animal)))))

(defmethod draw ((animal predator))
  (if (< 0 (animal-chewing animal))
      (gl:color 0.8 0 0.8)
      (gl:color 0.8 0 0))
  (gl:push-matrix)
  (gl:translate (+ *window-padding* (x animal))
                (+ *window-padding* (y animal))
                0)
  (gl:rotate (+ 270 (- (angle animal))) 0 0 1)
  (gl:begin :polygon)
  ;; pring body
  (let* ((ss *scavenger-size*)
         (ssn (- *scavenger-size*))
         (length (+ (* 2 ss)
                    (abs (* 4 (speed animal))))))
    (gl:vertex ssn ssn)
    (gl:vertex ss ssn)
    (gl:vertex 0 length))
  (gl:end)
  (gl:pop-matrix))
