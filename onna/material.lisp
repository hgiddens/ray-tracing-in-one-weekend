(in-package :onna)

(defstruct (scatter-record
            (:constructor make-diffuse-scatter-record
                (&key attenuation pdf))
            (:constructor make-specular-scatter-record
                (&key attenuation ray
                 &aux (skip-pdf-ray ray))))
  (attenuation (make-colour 0 0 0) :type colour :read-only t)
  (pdf nil :read-only t)
  (skip-pdf-ray nil :type (or ray null) :read-only t))

(defgeneric emitted-by-material (material ray hit)
  (:documentation "Colour emitted by MATERIAL for RAY hitting at HIT.")
  ;; All materials are capable of emitting light, so we need to able to test
  ;; everything, but at the moment only a single defined material actually
  ;; emits light, so I'm just going to define the default here rather than
  ;; manually specifying it for every material.
  ;;
  ;; TODO: feels weird that things either have emitted-by-material or
  ;; scatter-by-material implemented. The no-op scatter-by-material
  ;; implementation for diffuse-light is the only thing that returns nil from
  ;; scatter-by-material, so a better approach here would alow that to be
  ;; tidied up.
  (:method (material ray hit)
    (declare (ignore ray hit))
    (make-colour 0 0 0)))

(defun emitted (ray hit)
  "The emitted colour from RAY hitting at HIT."
  (emitted-by-material (hit-record-material hit) ray hit))

(defgeneric scatter-by-material (material ray hit))

(defun scatter (ray hit)
  "Scatters RAY according to HIT.

Returns a `scatter-record' or `nil'."
  (scatter-by-material (hit-record-material hit) ray hit))

;;; TODO: I'm only bothering to define this where the book tells me to for
;;; now, so e.g. the specular materials aren't going to have an implementation
;;; for this. I'm not sure that this should exist at all? Should the
;;; scatter-record just contain a scattering-pdf value as well?
(defgeneric scattering-pdf (material ray hit scattered-ray))

;;;; Lambertian

(defstruct lambertian
  (texture (make-colour 0 0 0) :read-only t))

(defmethod scatter-by-material ((material lambertian) ray hit)
  (declare (ignore ray))
  (make-diffuse-scatter-record :attenuation (texture-value (lambertian-texture material)
                                                           (hit-record-u hit)
                                                           (hit-record-v hit)
                                                           (hit-record-point hit))
                               :pdf (make-cosine-pdf (hit-record-normal hit))))

(defmethod scattering-pdf ((material lambertian) ray hit scattered-ray)
  (declare (ignore ray))
  (let ((cos-θ (dot-product (hit-record-normal hit)
                            (unit-vec3 (ray-direction scattered-ray)))))
    (if (minusp cos-θ) 0d0 (/ cos-θ pi))))

;;;; Metal

(defstruct metal
  (albedo (make-colour 0 0 0) :type colour :read-only t)
  (fuzz 0d0 :type (double-float 0d0 1d0) :read-only t))

(defmethod scatter-by-material ((material metal) ray hit)
  (let ((reflected (vec3+ (unit-vec3 (reflect (ray-direction ray) (hit-record-normal hit)))
                          (scaled-vec3 (random-unit-vec3) (metal-fuzz material)))))
    (make-specular-scatter-record :attenuation (metal-albedo material)
                                  :ray (make-ray :origin (hit-record-point hit)
                                                 :direction reflected
                                                 :time (ray-time ray)))))

;;;; Dielectric

(defstruct dielectric
  ;; Refractive index in vacuum or air, or the ratio of the material's
  ;; refractive index over the refractive index of the enclosing media.
  (refraction-index 0d0 :type double-float :read-only t))

(defun reflectance (cosine refraction-index)
  "Schlick's approximation for reflectance."
  (let ((r0 (/ (- 1d0 refraction-index) (+ 1d0 refraction-index))))
    (setf r0 (* r0 r0))
    (+ r0 (* (- 1d0 r0) (expt (- 1 cosine) 5)))))

(defmethod scatter-by-material ((material dielectric) ray hit)
  (let* ((ri (let ((r (dielectric-refraction-index material)))
               (if (hit-record-front-face hit) (/ r) r)))
         (unit-direction (unit-vec3 (ray-direction ray)))
         (cos-θ (min (dot-product (vec3- unit-direction) (hit-record-normal hit)) 1d0))
         (sin-θ (sqrt (- 1d0 (* cos-θ cos-θ))))
         (direction (if (or (> (* ri sin-θ) 1d0) ; can't refract
                            (> (reflectance cos-θ ri) (random 1d0)))
                        (reflect unit-direction (hit-record-normal hit))
                        (refract unit-direction (hit-record-normal hit) ri))))
    (make-specular-scatter-record :attenuation (make-colour 1 1 1)
                                  :ray (make-ray :origin (hit-record-point hit)
                                                 :direction direction
                                                 :time (ray-time ray)))))

;;;; Diffuse light

(defstruct diffuse-light
  (texture nil :read-only t))

(defmethod emitted-by-material ((light diffuse-light) ray hit)
  (declare (ignore ray))
  (if (hit-record-front-face hit)
      (texture-value (diffuse-light-texture light)
                     (hit-record-u hit)
                     (hit-record-v hit)
                     (hit-record-point hit))
      (make-colour 0 0 0)))

(defmethod scatter-by-material ((light diffuse-light) ray hit)
  (declare (ignore ray hit))
  nil)

;;;; Isotropic

(defstruct isotropic
  (texture nil :read-only t))

(defmethod scatter-by-material ((iso isotropic) ray hit)
  (declare (ignore ray))
  (make-diffuse-scatter-record :attenuation (texture-value (isotropic-texture iso)
                                                           (hit-record-u hit)
                                                           (hit-record-v hit)
                                                           (hit-record-point hit))
                               :pdf (make-sphere-pdf)))

(defmethod scattering-pdf ((iso isotropic) ray hit scattered-ray)
  (declare (ignore ray hit scattered-ray))
  (/ (* 4d0 pi)))
