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

(defun point+ (p &rest vs)
  "The point offset from point P by vectors VS."
  (let ((v (apply #'vec3+ vs)))
    (make-point (+ (point-x p) (vec3-x v))
                (+ (point-y p) (vec3-y v))
                (+ (point-z p) (vec3-z v)))))

(defun point- (a b)
  "The vector representing A - B."
  (make-vec3 (- (point-x a) (point-x b))
             (- (point-y a) (point-y b))
             (- (point-z a) (point-z b))))

;;; This doesn't really have anything to do with vectors but I guess it
;;; doesn't not make sense here, if you consider this a generic "math
;;; primitives" dumping ground.
(defstruct (interval (:constructor make-interval
                       (&key min max
                        &aux
                          (min (coerce min 'double-float))
                          (max (coerce max 'double-float)))))
  ;; These are stupid defaults because it means the default interval is
  ;; simultaneously empty and infinitely large (due to the implementation of
  ;; interval::size in the C++. I'm assuming we won't care about the size of
  ;; an interval for a while though, so I just won't implement that.
  (min #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY :type double-float)
  (max #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY :type double-float))

(defun interval-contains (i x)
  (declare (type double-float x))
  (<= (interval-min i) x (interval-max i)))

(defun interval-surrounds (i x)
  (declare (type double-float x))
  (< (interval-min i) x (interval-max i)))

;;; TODO: defconst? But let's see how it's used first?
(defun universe-interval ()
  (make-interval :min #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
                 :max #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY))
