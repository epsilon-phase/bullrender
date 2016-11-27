(in-package #:bullrender-test)
(use-package :fiveam)
(defun generate-multi-dim-array(n)
  (let* ((size (loop for i from 1 to n
                     collect(1+ (random 12) )))
         (result (make-array size :element-type 'integer )))
    (loop for i from 0 to (1- (array-total-size result))
          do(setf (row-major-aref result i) (random 40)))
    result))
(def-suite bullrender-tests :description "")
(in-suite bullrender-tests)
(test array-clone
  (is (array-clone-func) "Arrays were not equal"))
(defun make-index(array &key (start nil))
  (let ((dimensions (array-dimensions array)))
    (if start
        (map 'list #'+ start (loop for j in dimensions for i in start collect(max 0 (random (1- (- j i))))))
        (loop for i in dimensions collect(random (1- i))))))
(defun array-clone-func()
  (loop named outer for i from 1 to 3
    do(let* ((original-array (generate-multi-dim-array i))
             (start-point (make-index original-array))
             (end-point (make-index original-array :start start-point)
                        )
             (sizes (map 'list #'- end-point start-point))
           (subset
             (bullrender:aslice-multi
              original-array start-point end-point)))
        (print original-array)
        (loop with index = (make-list i :initial-element 0)
                until (equal sizes index)
              do(progn
                  (if (not (equal
                            (apply #'aref original-array (map 'list #'+ start-point index))
                            (apply #'aref subset index)))
                      (progn
                        (print original-array)
                        (print subset)
                        (return-from outer nil)))
                  (setf index (bullrender:interpolate-lists index sizes)))
                )
        t)
        finally (return-from outer t)))

