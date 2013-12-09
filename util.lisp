;;; Provides basic utility functions used through the system.

(in-package :nneat)

(defun contains (sequence value &key (test #'equal))
  "Test if a value exists in the given list. Default test is #'equal"
  (declare (optimize (speed 3) (safety 1))
           (type sequence sequence)
           (type function test))
  (if (listp sequence)
      (dolist (v sequence)
        (when (funcall test v value)
          (return-from contains t)))
      (dolist (i (length sequence))
        (when (funcall test (aref sequence i) value)
          (return-from contains t)))))

(defmacro appendf (append-to list-items)
  "Destructively appends one list to another."
  `(setf ,append-to (append ,append-to ,list-items)))

(defun c-getf (plist key)
  "Given a set of function arguments, pull out the value for a
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
  ((:id 16) (:friends 0) (:fav-animal 'pelican))
  which is a lot easier to operate on as a set of pairs."
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
  "Removes a set of properties and their values from a property list."
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
  "Shitty function to print out a network. The output is hard to parse unless
  you know exactly how the traverse-net function iterates over a network. Event
  then, a complicated network is really hard to visualize from this."
  (format t "~%---------------------------~%")
  (traverse-net
    (create-net-from-genome (net-genome net))
    (lambda (n)
      (format t "~a (id: ~a, thresh: ~a)~%" (neuron-type n) (id n) (neuron-threshold n))
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
