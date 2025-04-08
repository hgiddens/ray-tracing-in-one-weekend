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
  (1/scale 0d0 :type double-float)
  even
  odd)

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
  (image (make-array '(0 0) :element-type 'colour) :type (array colour (* *))))

(defmethod texture-value ((image image-texture) u v p)
  (setf v (- 1d0 v))                    ; Flip V to image coördinates
  (destructuring-bind (image-height image-width) (array-dimensions (image-texture-image image))
    (let ((i (floor (* u image-width)))
          (j (floor (* v image-height))))
      (aref (image-texture-image image) j i))))
