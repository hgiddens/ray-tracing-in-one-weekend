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

(defun random-unit-vec3 ()
  "A random vector of unit length."
  (loop for p = (make-vec3 (+ -1d0 (random 2d0)) (+ -1d0 (random 2d0)) (+ -1d0 (random 2d0)))
        as length-squared = (vec3-length-squared p)
        ;; 1d-160 is approximately (sqrt least-positive-double-float)
        until (and (< 1d-160 length-squared) (<= length-squared 1d0))
        finally (return (scaled-vec3 p (/ (sqrt length-squared))))))

(defun reflect (v n)
  "Reflects vector V with regard to surface normal N."
  (vec3- v (scaled-vec3 n (* 2 (dot-product v n)))))

(defun refract (uv n eta-i/eta-t)
  "Refracts vector UV with regard to surface normal N and refactive index ratio ETA-I/ETA-T."
  (let* ((cos-theta (min (dot-product (vec3- uv) n) 1d0))
         (r-out-perp (scaled-vec3 (vec3+ uv (scaled-vec3 n cos-theta)) eta-i/eta-t))
         (r-out-parallel (scaled-vec3 n (- (sqrt (abs (- 1d0 (vec3-length-squared r-out-perp))))))))
    (vec3+ r-out-perp r-out-parallel)))

;;; As in the book, points are vec3s, but a little more type safe here.
(defstruct (point3 (:constructor make-point3
                       (x y z
                        &aux
                          (x (coerce x 'double-float))
                          (y (coerce y 'double-float))
                          (z (coerce z 'double-float))))
                   (:include vec3)))

(defun point3+ (p &rest vs)
  "The point offset from point P by vectors VS."
  (let ((v (apply #'vec3+ vs)))
    (make-point3 (+ (point3-x p) (vec3-x v))
                 (+ (point3-y p) (vec3-y v))
                 (+ (point3-z p) (vec3-z v)))))

(defun point3- (a b)
  "The vector representing A - B."
  (make-vec3 (- (point3-x a) (point3-x b))
             (- (point3-y a) (point3-y b))
             (- (point3-z a) (point3-z b))))

;;; This doesn't really have anything to do with vectors but I guess it
;;; doesn't not make sense here, if you consider this a generic "math
;;; primitives" dumping ground.
(defstruct (interval
            (:constructor empty-interval)
            (:constructor make-interval
                (min max
                 &aux
                   (min (coerce min 'double-float))
                   (max (coerce max 'double-float)))))
  ;; These are frustrating defaults because it means the default interval is
  ;; empty but the size is non-zero (it's negative infinity instead).
  (min #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY :type double-float)
  (max #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY :type double-float))

(defun combine-intervals (a b)
  "Builds an `interval' tightly enclosing the input intervals A and B."
  (make-interval (min (interval-min a) (interval-min b))
                 (max (interval-max a) (interval-max b))))

(defun interval-contains (i x)
  (declare (type double-float x))
  (<= (interval-min i) x (interval-max i)))

(defun interval-surrounds (i x)
  (declare (type double-float x))
  (< (interval-min i) x (interval-max i)))

(defun universe-interval ()
  (make-interval #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
                 #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY))

(defun clamp-to-interval (i d)
  (declare (type double-float d))
  (alexandria:clamp d (interval-min i) (interval-max i)))

(defun expand-interval (i delta)
  (let ((padding (/ delta 2d0)))
    (make-interval (- (interval-min i) padding)
                   (+ (interval-max i) padding))))

(defun interval-size (i)
  (- (interval-max i) (interval-min i)))

(defun interval+ (i d)
  "Shifts interval I by delta D."
  (make-interval (+ (interval-min i) d)
                 (+ (interval-max i) d)))
