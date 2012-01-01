(defun contains (list value &key (test #'equal))
  (not (zerop (length (remove-if (lambda (item)
                                   (not (funcall test item value)))
                                 list)))))
  
(defmacro push-end (list item)
  `(if (null ,list)
     (setf ,list (list ,item))
     (nconc ,list (list ,item))))

;; unused
(defun resize-array (array new-size)
  (let ((new-array (make-array new-size))
        (old-size (length array)))
    (dotimes (i new-size)
      (if (< i old-size)
          (setf (elt new-array i)
                (elt array i))
          (return)))
    new-array))

