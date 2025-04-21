(in-package :onna)

(defgeneric texture-value (texture u v p)
  (:documentation "Colour of TEXTURE at point P and texture coördinates U,V."))

;;;; Solid colours

(defmethod texture-value ((colour colour) u v p)
  (declare (ignore u v p))
  colour)

;;;; Spatial chequer

(defstruct (chequer (:constructor make-chequer
                        (&key scale even odd
                         &aux
                           (1/scale (coerce (/ scale) 'double-float)))))
  (1/scale 0d0 :type double-float :read-only t)
  (even nil :read-only t)
  (odd nil :read-only t))

(defmethod texture-value ((chequer chequer) u v p)
  (flet ((component-floor (x)
           (floor (* (chequer-1/scale chequer) x))))
    (let ((x (component-floor (point3-x p)))
          (y (component-floor (point3-y p)))
          (z (component-floor (point3-z p))))
      (if (evenp (+ x y z))
          (texture-value (chequer-even chequer) u v p)
          (texture-value (chequer-odd chequer) u v p)))))

;;;; Image

;;; We need to define a wrapper type because all arrays in Common Lisp have
;;; the same type, regardless of their dimensions.
(defstruct (image-texture)
  (image (make-array '(0 0) :element-type 'colour) :type (array colour (* *)) :read-only t))

(defmethod texture-value ((image image-texture) u v p)
  (setf v (- 1d0 v))                    ; Flip V to image coördinates
  (destructuring-bind (image-height image-width) (array-dimensions (image-texture-image image))
    (let ((i (floor (* u image-width)))
          (j (floor (* v image-height))))
      (aref (image-texture-image image) j i))))

;;;; Perlin noise

(defconstant +perlin-point-count+ 256)

(defstruct (perlin (:constructor))
  (random-vecs (map-into (make-array (list +perlin-point-count+)
                                     :element-type 'vec3
                                     :initial-element (make-vec3 0 0 0))
                         #'random-unit-vec3)
   :type (vector vec3 #.+perlin-point-count+)
   :read-only t)
  (x (alexandria:shuffle (coerce (alexandria:iota +perlin-point-count+)
                                 '(vector (integer 0 #.(1- +perlin-point-count+)))))
   :type (vector (integer 0 #.(1- +perlin-point-count+)) #.+perlin-point-count+)
   :read-only t)
  (y (alexandria:shuffle (coerce (alexandria:iota +perlin-point-count+)
                                 '(vector (integer 0 #.(1- +perlin-point-count+)))))
   :type (vector (integer 0 #.(1- +perlin-point-count+)) #.+perlin-point-count+)
   :read-only t)
  (z (alexandria:shuffle (coerce (alexandria:iota +perlin-point-count+)
                                 '(vector (integer 0 #.(1- +perlin-point-count+)))))
   :type (vector (integer 0 #.(1- +perlin-point-count+)) #.+perlin-point-count+)
   :read-only t)
  (scale 1d0 :type double-float :read-only t))

(defun perlin-noise (perlin point)
  (flet ((interpolate (c u v w)
           (loop with uu = (* u u (- 3 (* 2 u)))
                 and vv = (* v v (- 3 (* 2 v)))
                 and ww = (* w w (- 3 (* 2 w)))
                 for i below 2
                 sum (loop for j below 2
                           sum (loop for k below 2
                                     as weight = (make-vec3 (- u i) (- v j) (- w k))
                                     sum (* (alexandria:lerp i (- 1 uu) uu)
                                            (alexandria:lerp j (- 1 vv) vv)
                                            (alexandria:lerp k (- 1 ww) ww)
                                            (dot-product (aref c i j k) weight)))))))
    (loop with (i u) = (multiple-value-list (floor (point3-x point)))
          and (j v) = (multiple-value-list (floor (point3-y point)))
          and (k w) = (multiple-value-list (floor (point3-z point)))
          and c = (make-array '(2 2 2) :element-type 'vec3 :initial-element (make-vec3 0 0 0))
          for di below 2
          as i-index = (aref (perlin-x perlin) (logand (+ i di) +perlin-point-count+))
          do (loop for dj below 2
                   as j-index = (aref (perlin-y perlin) (logand (+ j dj) +perlin-point-count+))
                   do (loop for dk below 2
                            as k-index = (aref (perlin-z perlin) (logand (+ k dk) +perlin-point-count+))
                            as float-index = (logxor i-index j-index k-index)
                            do (setf (aref c di dj dk)
                                     (aref (perlin-random-vecs perlin) float-index))))
          finally (return (interpolate c u v w)))))

(defun perlin-turbulence (perlin point max-depth)
  (abs (loop with weight = 1
             for i below max-depth
             sum (* weight (perlin-noise perlin point))
             do (setf weight (* weight 0.5)
                      point (scaled-vec3 point 2d0)))))

(defmethod texture-value ((perlin perlin) u v p)
  (declare (ignore u v))
  (let* ((n (sin (+ (* (perlin-scale perlin) (vec3-z p))
                    (* (perlin-turbulence perlin p 7) 10))))
         (c (* 0.5 (1+ n))))
    (make-colour c c c)))
