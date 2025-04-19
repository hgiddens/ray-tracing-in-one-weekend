(in-package :onna)

(defgeneric pdf-value (pdf direction)
  (:documentation "The distribution value of PDF in DIRECTION."))

(defgeneric pdf-generate (pdf)
  (:documentation "Generates a random direction weight by the PDF distribution."))

;;; TODO: Just have this? For fuck's sake.
(defun pdf-direction (pdf)
  (let ((direction (pdf-generate pdf)))
    (values direction (pdf-value pdf direction))))

;;;; Uniform density over the unit sphere

;;; TODO: This kinda calls into question whether this should be using generics.
(defstruct sphere-pdf)

(defmethod pdf-value ((pdf sphere-pdf) direction)
  (/ (* 4 pi)))

(defmethod pdf-generate ((pdf sphere-pdf))
  (random-unit-vec3))

;;;; Cosine density

(defstruct (cosine-pdf (:constructor make-cosine-pdf (w &aux (uvw (make-onb w)))))
  (uvw (make-onb (make-vec3 0 0 0)) :type onb))

(defmethod pdf-value ((pdf cosine-pdf) direction)
  (let ((cosine-θ (dot-product (unit-vec3 direction) (onb-w (cosine-pdf-uvw pdf)))))
    (max 0d0 (/ cosine-θ pi))))

(defmethod pdf-generate ((pdf cosine-pdf))
  (flet ((random-cosine-direction ()
           "A random direction on the hemisphere weighted by cos(θ)."
           (let* ((r1 (random 1d0))
                  (r2 (random 1d0))
                  (φ (* 2d0 pi r1))
                  (x (* (cos φ) (sqrt r2)))
                  (y (* (sin φ) (sqrt r2)))
                  (z (sqrt (- 1d0 r2))))
             (make-vec3 x y z))))
    (onb-transform (cosine-pdf-uvw pdf) (random-cosine-direction))))

;;;; Sampling directions towards a hittable

(defstruct hittable-pdf
  objects
  (origin (make-point3 0 0 0) :type point3))

(defmethod pdf-value ((pdf hittable-pdf) direction)
  (object-pdf-value (hittable-pdf-objects pdf) (hittable-pdf-origin pdf) direction))

(defmethod pdf-generate ((pdf hittable-pdf))
  (point3- (random-point (hittable-pdf-objects pdf)) (hittable-pdf-origin pdf)))

;;;; Weighted mixture of PDFs

(defstruct (mixture-pdf (:constructor make-mixture-pdf (p0 p1)))
  p0
  p1)

(defmethod pdf-value ((pdf mixture-pdf) direction)
  (+ (* 0.5 (pdf-value (mixture-pdf-p0 pdf) direction))
     (* 0.5 (pdf-value (mixture-pdf-p1 pdf) direction))))

(defmethod pdf-generate ((pdf mixture-pdf))
  (pdf-generate (if (< (random 1d0) 0.5)
                    (mixture-pdf-p0 pdf)
                    (mixture-pdf-p1 pdf))))
