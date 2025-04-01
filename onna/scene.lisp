(in-package :onna)

;;;; This is roughly analogous to the hittable class in the book.

(defstruct (hit-record
            (:constructor make-hit-record
                (&key ray point outward-normal time material
                 &aux
                   (front-face (< (dot-product (ray-direction ray) outward-normal) 0))
                   (normal (if front-face outward-normal (vec3- outward-normal))))))
  (point (make-point3 0 0 0) :type point3)
  ;; The normal, facing against the incident ray
  (normal (make-vec3 0 0 0) :type vec3)
  (time 0d0 :type double-float)
  front-face
  material)

;;; TODO: I probably want to do the monomorphisation I did before, but I
;;; wonder if there's a way to automatically do it and/or whether it's
;;; beneficial when intervals and BVH trees are taken into account. Going to
;;; ignore it for now.
(defgeneric hit-test (ray object ray-interval)
  (:documentation
   "Tests OBJECT for a hit from RAY between RAY-TIME-MIN and RAY-TIME-MAX.

Returns a `hit-record' or `nil'."))

;;;; Sequences of objects

(defmethod hit-test (ray (seq sequence) ray-interval)
  (setf ray-interval (copy-interval ray-interval))
  (let ((length (length seq))
        closest)
    (dotimes (i length closest)
      (alexandria:when-let ((record (hit-test ray (elt seq i) ray-interval)))
        (setf closest record
              (interval-max ray-interval) (hit-record-time record))))))

;;;; Spheres

(defun sphere-centre-ray (centre from to)
  (assert (or (and (null centre) from to)
              (and centre (null from) (null to)))
          (centre from to)
          "Specify either CENTRE or both of FROM and TO")
  (let ((direction (if centre
                       (make-vec3 0 0 0)
                       (point3- to from))))
    (make-ray :origin (or centre from) :direction direction)))

(defstruct (sphere
            (:constructor make-sphere
                (&key centre radius material from to
                 &aux
                   (radius (coerce radius 'double-float))
                   (centre (sphere-centre-ray centre from to)))))
  (centre (make-ray) :type ray)
  (radius 0d0 :type (double-float 0d0))
  material)

(defmethod hit-test (ray (sphere sphere) ray-interval)
  (flet ((hit-record-for-root (root centre)
           (let ((hit-point (point-at-time ray root)))
             (make-hit-record
              :ray ray
              :point hit-point
              :outward-normal (scaled-vec3 (point3- hit-point centre)
                                           (/ (sphere-radius sphere)))
              :time root
              :material (sphere-material sphere)))))
    (let* ((current-centre (point-at-time (sphere-centre sphere) (ray-time ray)))
           (oc (point3- current-centre (ray-origin ray)))
           (a (vec3-length-squared (ray-direction ray)))
           (h (dot-product (ray-direction ray) oc))
           (c (- (vec3-length-squared oc) (* (sphere-radius sphere) (sphere-radius sphere))))
           (discriminant (- (* h h) (* a c))))
      (unless (< discriminant 0)
        (let* ((sqrt-discriminant (sqrt discriminant))
               (root (/ (- h sqrt-discriminant) a)))
          (if (interval-surrounds ray-interval root)
              (hit-record-for-root root current-centre)
              (progn
                (setf root (/ (+ h sqrt-discriminant) a))
                (when (interval-surrounds ray-interval root)
                  (hit-record-for-root root current-centre)))))))))
