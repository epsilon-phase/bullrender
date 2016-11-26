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
(defun array-clone()
  (loop for i from 1 to 3
    do(let* ((original-array (generate-multi-dim-array i))
           (sizes
             (array-dimensions original-array))
           (start-point
             (loop for j from 0 to i
                   collect(random (1- (elt sizes j))))
             )
           (end-point (loop for j from 0 to i
                            collect(+ (elt start-point j)
                                      (random (1-
                                            (- (elt sizes j)
                                               (elt start-point j))))))
                      )
           (subset
             (bullrender:aslice-multi
              original-array start-point end-point)))
        (loop for index = (make-list i :initial-element 0) then (bullrender:interpolate-lists index (array-dimensions subset))
              do(print index)
                until (equal index (map 'list #'- end-point start-point))
            do(if (equal
               (apply #'aref original-array (map 'list #'+ start-point index))
               (apply #'aref subset index))
                  (print "WORKED")))
      )))
