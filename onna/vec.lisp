(in-package :onna)

(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(defstruct (vec3 (:constructor make-vec3
                     (x y z
                      &aux
                        (x (coerce x 'double-float))
                        (y (coerce y 'double-float))
                        (z (coerce z 'double-float)))))
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (z 0d0 :type double-float))

(declaim (ftype (function (vec3) (double-float 0d0))
                vec3-length-squared
                vec3-length))
(defun vec3-length-squared (v)
  "The length of V, squared."
  (let ((x (vec3-x v))
        (y (vec3-y v))
        (z (vec3-z v)))
    (+ (* x x) (* y y) (* z z))))
(defun vec3-length (v)
  (sqrt (vec3-length-squared v)))

(defun vec3+ (&rest vs)
  "Returns a new `vec3' representing the sum of VS."
  (let ((s (make-vec3 0 0 0)))
    (dolist (v vs s)
      (incf (vec3-x s) (vec3-x v))
      (incf (vec3-y s) (vec3-y v))
      (incf (vec3-z s) (vec3-z v)))))

(defun vec3- (v &rest vs)
  "See `vec3+'."
  (if (null vs)
      (make-vec3 (- (vec3-x v)) (- (vec3-y v)) (- (vec3-z v)))
      (let ((s (copy-vec3 v)))
        (dolist (v vs s)
          (decf (vec3-x s) (vec3-x v))
          (decf (vec3-y s) (vec3-y v))
          (decf (vec3-z s) (vec3-z v))))))

(defun scaled-vec3 (v d)
  "Returns a new `vec3' where the components of V are scaled by D."
  (declare (type double-float d))
  (make-vec3 (* (vec3-x v) d) (* (vec3-y v) d) (* (vec3-z v) d)))

(defun dot-product (u v)
  "Dot product of U and V."
  (+ (* (vec3-x u) (vec3-x v))
     (* (vec3-y u) (vec3-y v))
     (* (vec3-z u) (vec3-z v))))

(defun cross-product (u v)
  "Cross product of U and V."
  (make-vec3 (- (* (vec3-y u) (vec3-z v)) (* (vec3-z u) (vec3-y v)))
             (- (* (vec3-z u) (vec3-x v)) (* (vec3-x u) (vec3-z v)))
             (- (* (vec3-x u) (vec3-y v)) (* (vec3-y u) (vec3-x v)))))

(defun unit-vec3 (v)
  "V scaled to be a unit vector."
  (scaled-vec3 v (/ (vec3-length v))))

;;; As in the book, points are vec3s, but a little more type safe here.
(defstruct (point (:constructor make-point
                      (x y z
                       &aux
                         (x (coerce x 'double-float))
                         (y (coerce y 'double-float))
                         (z (coerce z 'double-float))))
                  (:include vec3)))
