;;;; bullrender.lisp

(in-package #:bullrender)

;;; "bullrender" goes here. Hacks and glory await!

(defstruct color
  "A simple rgb color"
  (r 0 :type unsigned-byte)
  (g 0 :type unsigned-byte)
  (b 0 :type unsigned-byte)
  )
(defstruct vertex
  "A Vertex similar to the sort found in the SFML library"
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (texture-x 0.0 :type single-float)
  (texture-y 0.0 :type single-float)
  (color (make-color :r 0 :g 0 :b 0) :type color))
(defun make-transform-matrix()
  (make-array '(3 3) :element-type 'single-float :initial-contents '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))))
(deftype matrix(a)
  (and (= 3 (array-dimension a 0))
       (= 3 (array-dimension a 1))))
(defun column(array index)
  (loop for i from 0 to (- (array-dimension array 0) 1)
        collect (aref array i index )))
(defun row(array index)
  (loop for i from 0 to (- (array-dimension array 1) 1)
        collect (aref array index i)))
(defun dot(a b)
  (reduce #'+ (map (type-of a) #'* a b) ))
(defun make-translate-matrix(dx dy)
  (declare (type single-float dx dy))
  (make-array '(3 3) :element-type 'single-float :initial-contents
              (list
               (list 1.0 0.0 dx)
               (list 0.0 1.0 dy)
               (list 0.0 0.0 1.0))))
(defun make-rotation-matrix(theta)
  (declare (type single-float theta) )
  (make-array '(3 3) :element-type 'single-float :initial-contents (list (list (cos theta) (- 0(sin theta)) 0.0) (list (sin theta) (cos theta) 0.0)
                                                                         (list 0.0 0.0 1.0))))
(defun make-scale-matrix(sx sy)
  (declare (type single-float sx sy))
  (make-array '(3 3) :element-type 'single-float :initial-contents (list (list sx 0.0 0.0) (list 0.0 sy 0.0) '(0.0 0.0 1.0))))
(defun mat-mult(a b)
  "Perform matrix multiplication. No checks are performed here, so take care to make sure matrix multiplication is applicable to both arguments"
  (let ((result (make-array (list (array-dimension a 0) (array-dimension b 1)))))
    (loop for i from 0 to (- (array-dimension a 0) 1)
          do(loop for j from 0 to (- (array-dimension b 1) 1)
                                  do(setf (aref result i j) (dot (row a i) (column b j))))
          )
    result))
(defgeneric transform(element transform))
(defmethod transform((element vertex) m)
  (let ((coords (make-array '(1 3) :initial-contents (list (list (vertex-x element) (vertex-y element) 1.0)))))
    (mat-mult coords m )))

