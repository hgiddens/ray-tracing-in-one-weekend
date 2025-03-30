(in-package :onna)

(defstruct scatter-record
  (scattered (make-ray) :type ray)
  (attenuation (make-colour 0 0 0) :type colour))

(defgeneric scatter* (material ray hit-point hit-normal))

(defun scatter (ray hit)
  "Scatters RAY according to HIT.

Returns a `scatter-record' or `nil'."
  ;; TODO: There maybe is a nicer way to do this with CLOS? I don't know if we
  ;; can dispatch on a slot of an argument, and since we have to get the
  ;; material out of the hit-record anyway it seems dumb to then pass the
  ;; hit-record into scatter*?
  (scatter* (hit-record-material hit) ray (hit-record-point hit) (hit-record-normal hit)))

;;;; Null material

;;; I'm not sure this is a good idea, but means that old scenes keep working?
(defmethod scatter* ((material null) ray hit-point hit-normal)
  (let ((direction (vec3+ hit-normal (random-unit-vec3))))
    (make-scatter-record :scattered (make-ray :origin hit-point
                                              :direction direction)
                         :attenuation (make-colour 0.5 0.5 0.5))))

;;;; Lambertian

(defstruct lambertian
  (albedo (make-colour 0 0 0) :type colour))

(defmethod scatter* ((material lambertian) ray hit-point hit-normal)
  (let ((scatter-direction (vec3+ hit-normal (random-unit-vec3))))
    (when (near-zero-vec3-p scatter-direction)
      ;; Stop degenerate scatter direction: the random vector cancelling out
      ;; the normal to a near-zero vector.
      (setf scatter-direction hit-normal))
    (make-scatter-record :scattered (make-ray :origin hit-point :direction scatter-direction)
                         :attenuation (lambertian-albedo material))))

;;;; Metal

(defstruct metal
  (albedo (make-colour 0 0 0) :type colour)
  (fuzz 0d0 :type (double-float 0d0 1d0)))

(defmethod scatter* ((material metal) ray hit-point hit-normal)
  (let* ((reflected (vec3+ (unit-vec3 (reflect (ray-direction ray) hit-normal))
                           (scaled-vec3 (random-unit-vec3) (metal-fuzz material))))
         (scattered (make-ray :origin hit-point :direction reflected)))
    (when (> (dot-product (ray-direction scattered) hit-normal) 0)
      (make-scatter-record :scattered scattered
                           :attenuation (metal-albedo material)))))
