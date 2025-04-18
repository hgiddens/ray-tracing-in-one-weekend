(in-package :onna)

(defstruct scatter-record
  (scattered (make-ray) :type ray)
  (attenuation (make-colour 0 0 0) :type colour))

(defgeneric emitted (material u v p)
  (:documentation "Colour emitted by MATERIAL at point P and texture coördinates U,V."))

(defgeneric scatter* (material ray hit-point hit-normal hit-front-face hit-u hit-v))

(defun scatter (ray hit)
  "Scatters RAY according to HIT.

Returns a `scatter-record' or `nil'."
  ;; TODO: There maybe is a nicer way to do this with CLOS? I don't know if we
  ;; can dispatch on a slot of an argument, and since we have to get the
  ;; material out of the hit-record anyway it seems dumb to then pass the
  ;; hit-record into scatter*?
  (scatter* (hit-record-material hit)
            ray
            (hit-record-point hit)
            (hit-record-normal hit)
            (hit-record-front-face hit)
            (hit-record-u hit)
            (hit-record-v hit)))

;;;; Null material

(defmethod emitted ((material null) u v p)
  (declare (ignore u v p))
  (make-colour 0 0 0))

;;; I'm not sure this is a good idea, but means that old scenes keep working?
(defmethod scatter* ((material null) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (let ((direction (vec3+ hit-normal (random-unit-vec3))))
    (make-scatter-record :scattered (make-ray :origin hit-point
                                              :direction direction)
                         :attenuation (make-colour 0.5 0.5 0.5))))

;;;; Lambertian

(defstruct lambertian
  (texture (make-colour 0 0 0)))

(defmethod emitted ((material lambertian) u v p)
  (declare (ignore u v p))
  (make-colour 0 0 0))

(defmethod scatter* ((material lambertian) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (declare (ignore hit-front-face))
  (let ((scatter-direction (vec3+ hit-normal (random-unit-vec3))))
    (when (near-zero-vec3-p scatter-direction)
      ;; Stop degenerate scatter direction: the random vector cancelling out
      ;; the normal to a near-zero vector.
      (setf scatter-direction hit-normal))
    (make-scatter-record
     :scattered (make-ray :origin hit-point
                          :direction scatter-direction
                          :time (ray-time ray))
     :attenuation (texture-value (lambertian-texture material) hit-u hit-v hit-point))))

;;;; Metal

(defstruct metal
  (albedo (make-colour 0 0 0) :type colour)
  (fuzz 0d0 :type (double-float 0d0 1d0)))

(defmethod emitted ((material metal) u v p)
  (declare (ignore u v p))
  (make-colour 0 0 0))

(defmethod scatter* ((material metal) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (declare (ignore hit-front-face hit-u hit-v))
  (let* ((reflected (vec3+ (unit-vec3 (reflect (ray-direction ray) hit-normal))
                           (scaled-vec3 (random-unit-vec3) (metal-fuzz material))))
         (scattered (make-ray :origin hit-point
                              :direction reflected
                              :time (ray-time ray))))
    (when (> (dot-product (ray-direction scattered) hit-normal) 0)
      (make-scatter-record :scattered scattered
                           :attenuation (metal-albedo material)))))

;;;; Dielectric

(defstruct dielectric
  ;; Refractive index in vacuum or air, or the ratio of the material's
  ;; refractive index over the refractive index of the enclosing media.
  (refraction-index 0d0 :type double-float))

(defmethod emitted ((material dielectric) u v p)
  (declare (ignore u v p))
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
         (cos-theta (min (dot-product (vec3- unit-direction) hit-normal) 1d0))
         (sin-theta (sqrt (- 1d0 (* cos-theta cos-theta))))
         (cannot-refract (> (* ri sin-theta) 1d0))
         (direction (if (or cannot-refract
                            (> (reflectance cos-theta ri) (random 1d0)))
                        (reflect unit-direction hit-normal) ; Cannot refract
                        (refract unit-direction hit-normal ri))))
    (make-scatter-record :scattered (make-ray :origin hit-point
                                              :direction direction
                                              :time (ray-time ray))
                         :attenuation (make-colour 1 1 1))))

;;;; Diffuse light

(defstruct diffuse-light
  texture)

;;; TODO: it's probably a code smell that all these materials have either an
;;; implementation for emitted or scatter, but not both.
(defmethod emitted ((light diffuse-light) u v p)
  (texture-value (diffuse-light-texture light) u v p))

(defmethod scatter* ((light diffuse-light) ray hit-point hit-normal hit-front-face hit-u hit-v)
  nil)

;;;; Isotropic

(defstruct isotropic texture)

(defmethod emitted ((iso isotropic) u v p)
  (declare (ignore u v p))
  (make-colour 0 0 0))

(defmethod scatter* ((iso isotropic) ray hit-point hit-normal hit-front-face hit-u hit-v)
  (make-scatter-record :scattered (make-ray :origin hit-point
                                            :direction (random-unit-vec3)
                                            :time (ray-time ray))
                       :attenuation (texture-value (isotropic-texture iso) hit-u hit-v hit-point)))
