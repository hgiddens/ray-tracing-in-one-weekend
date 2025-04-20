(in-package :onna)

(defstruct (scatter-record
            (:constructor)
            (:constructor make-diffuse-scatter-record (&key attenuation pdf))
            (:constructor make-specular-scatter-record (&key attenuation ray
                                                        &aux (skip-pdf-ray ray))))
  (attenuation (make-colour 0 0 0) :type colour)
  pdf
  (skip-pdf-ray nil :type (or ray null)))

(defgeneric emitted (material ray hit u v p)
  (:documentation "Colour emitted by MATERIAL at point P and texture coördinates U,V."))

(defgeneric scatter* (material ray hit-point hit-normal hit-front-face hit-u hit-v))

;;; TODO: I'm only bothering to define this where the book tells me to for
;;; now, so e.g. the null material isn't going to have an implementation for
;;; this. Don't know if that means I want to remove the null material or work
;;; out what to do here.
(defgeneric scattering-pdf (material ray hit scattered-ray))

(defun scatter (ray hit)
  "Scatters RAY according to HIT.

Returns a `scatter-record' or `nil'."
  ;; TODO: There maybe is a nicer way to do this with CLOS? I don't know if we
  ;; can dispatch on a slot of an argument, and since we have to get the
  ;; material out of the hit-record anyway it seems dumb to then pass the
  ;; hit-record into scatter*? On the other hand, that's what the C++ code
  ;; does, it's basically:
  ;;     hit->material->scatter(ray, hit);
  (scatter* (hit-record-material hit)
            ray
            (hit-record-point hit)
            (hit-record-normal hit)
            (hit-record-front-face hit)
            (hit-record-u hit)
            (hit-record-v hit)))

;;;; Lambertian

(defstruct lambertian
  (texture (make-colour 0 0 0)))

(defmethod emitted ((material lambertian) ray hit u v p)
  (declare (ignore ray hit u v p))
  (make-colour 0 0 0))

(defmethod scatter* ((material lambertian) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (declare (ignore hit-front-face))
  ;; TODO: We used to have stuff here using near-zero-vec3-p that is no longer
  ;; necessary (because now we're using the PDF for generating scatter
  ;; directions, we don't need to worry about degenerate directions). Maybe
  ;; this can be cleaned up now?
  (make-diffuse-scatter-record
   :attenuation (texture-value (lambertian-texture material) hit-u hit-v hit-point)
   :pdf (make-cosine-pdf hit-normal)))

(defmethod scattering-pdf ((material lambertian) ray hit scattered-ray)
  (declare (ignore ray))
  (let ((cos-θ (dot-product (hit-record-normal hit)
                            (unit-vec3 (ray-direction scattered-ray)))))
    (if (minusp cos-θ) 0d0 (/ cos-θ pi))))

;;;; Metal

(defstruct metal
  (albedo (make-colour 0 0 0) :type colour)
  (fuzz 0d0 :type (double-float 0d0 1d0)))

(defmethod emitted ((material metal) ray hit u v p)
  (declare (ignore ray hit u v p))
  (make-colour 0 0 0))

(defmethod scatter* ((material metal) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (declare (ignore hit-front-face hit-u hit-v))
  (let* ((reflected (vec3+ (unit-vec3 (reflect (ray-direction ray) hit-normal))
                           (scaled-vec3 (random-unit-vec3) (metal-fuzz material)))))
    ;; TODO: we used to care about scattering into the object (there was a
    ;; test making sure the dot product of the scatter ray and the hit normal
    ;; was positive) but we've lost that and I'm not sure why. I also think
    ;; this means that all the scatter methods always return a non-nil value,
    ;; so we could lose the test.
    (make-specular-scatter-record :attenuation (metal-albedo material)
                                  :ray (make-ray :origin hit-point
                                                 :direction reflected
                                                 :time (ray-time ray)))))

;;;; Dielectric

(defstruct dielectric
  ;; Refractive index in vacuum or air, or the ratio of the material's
  ;; refractive index over the refractive index of the enclosing media.
  (refraction-index 0d0 :type double-float))

(defmethod emitted ((material dielectric) ray hit u v p)
  (declare (ignore ray hit u v p))
  (make-colour 0 0 0))

(defun reflectance (cosine refraction-index)
  "Schlick's approximation for reflectance."
  (let ((r0 (/ (- 1d0 refraction-index) (+ 1d0 refraction-index))))
    (setf r0 (* r0 r0))
    (+ r0 (* (- 1d0 r0) (expt (- 1 cosine) 5)))))

(defmethod scatter* ((material dielectric) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (declare (ignore hit-u hit-v))
  (let* ((ri (let ((r (dielectric-refraction-index material)))
               (if hit-front-face (/ r) r)))
         (unit-direction (unit-vec3 (ray-direction ray)))
         (cos-θ (min (dot-product (vec3- unit-direction) hit-normal) 1d0))
         (sin-θ (sqrt (- 1d0 (* cos-θ cos-θ))))
         (cannot-refract (> (* ri sin-θ) 1d0))
         (direction (if (or cannot-refract
                            (> (reflectance cos-θ ri) (random 1d0)))
                        (reflect unit-direction hit-normal) ; Cannot refract
                        (refract unit-direction hit-normal ri))))
    (make-specular-scatter-record :attenuation (make-colour 1 1 1)
                                  :ray (make-ray :origin hit-point
                                                 :direction direction
                                                 :time (ray-time ray)))))

;;;; Diffuse light

(defstruct diffuse-light
  texture)

;;; TODO: it's probably a code smell that all these materials have either an
;;; implementation for emitted or scatter, but not both.
(defmethod emitted ((light diffuse-light) ray hit u v p)
  (declare (ignore ray))
  (if (hit-record-front-face hit)
      (texture-value (diffuse-light-texture light) u v p)
      (make-colour 0 0 0)))

(defmethod scatter* ((light diffuse-light) ray hit-point hit-normal hit-front-face hit-u hit-v)
  nil)

;;;; Isotropic

(defstruct isotropic texture)

(defmethod emitted ((iso isotropic) ray hit u v p)
  (declare (ignore ray hit u v p))
  (make-colour 0 0 0))

(defmethod scatter* ((iso isotropic) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (declare (ignore ray hit-normal hit-front-face))
  (make-diffuse-scatter-record :attenuation (texture-value (isotropic-texture iso) hit-u hit-v hit-point)
                               :pdf (make-sphere-pdf)))

(defmethod scattering-pdf ((iso isotropic) ray hit scattered-ray)
  (declare (ignore ray hit scattered-ray))
  (/ (* 4d0 pi)))
