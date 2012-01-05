(in-package :nneat)

(defun contains (list value &key (test #'equal))
  (not (zerop (length (remove-if (lambda (item)
                                   (not (funcall test item value)))
                                 list)))))

(defmacro appendf (append-to list-items)
  `(setf ,append-to (append ,append-to ,list-items)))

(defun c-getf (plist key)
  "Given a set of funciton arguments, pull out the value for a
  keyword argument. If it isn't present, return nil."
  (let ((next-item nil))
    (dolist (item plist)
      (when next-item
        (return-from c-getf (values item t)))
      (when (equal item key)
        (setf next-item t)))
    (values nil nil)))

(defun plist-iter (plist)
  "Turns (:id 16 :friends 0 :fav-animal 'pelican) into
  ((:id 16) (:friends 0) (:fav-animal 'pelican))"
  (let ((collection nil)
        (next-val nil)
        (tmp nil))
    (dolist (p plist)
      (if next-val
          (progn (setf next-val nil)
                 (push (append tmp (list p)) collection))
          (progn (setf next-val t)
                 (setf tmp (list p)))))
    (reverse collection)))


(defun remprops (plist props)
  (let ((newlist nil)
        (ignore-next nil))
    (dolist (item plist)
      (block :fist
        (when ignore-next
          (setf ignore-next nil)
          (return-from :fist nil))
        (if (contains props item)
            (setf ignore-next t)
            (push item newlist))))
    (reverse newlist)))
  
(defun print-net (net)
  (format t "~%---------------------------~%")
  (traverse-net
    (create-net-from-genome (net-genome net))
    (lambda (n)
      (format t "~a(id:~a)~%" (neuron-type n) (id n))
      (unless (eql (neuron-type n) :input)
        (format t "   inp:~%")
        (loop for inp across (neuron-inputs n) do
              (let ((from (connection-from inp)))
                (format t "       ~a(id:~a), id:~a, weight: ~a~%" (neuron-type from)
                                                                  (id from)
                                                                  (id inp)
                                                                  (connection-weight inp)))))
      (format t "   out:~%")
      (loop for out in (neuron-outputs n) do
            (let ((to (connection-to out)))
              (format t "       ~a(id:~a), id:~a, weight: ~a~%" (neuron-type to)
                                                                (id to)
                                                                (id out)
                                                                (connection-weight out))))))
  (format t "---------------------------~%~%"))
