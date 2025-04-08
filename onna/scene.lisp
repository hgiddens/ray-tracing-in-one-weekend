(in-package :onna)

;;;; This is roughly analogous to the hittable class in the book.

(defstruct (hit-record
            (:constructor make-hit-record
                (&key ray point outward-normal time material u v
                 &aux
                   (front-face (< (dot-product (ray-direction ray) outward-normal) 0))
                   (normal (if front-face outward-normal (vec3- outward-normal))))))
  (point (make-point3 0 0 0) :type point3)
  ;; The normal, facing against the incident ray
  (normal (make-vec3 0 0 0) :type vec3)
  (time 0d0 :type double-float)
  (u 0d0 :type double-float)
  (v 0d0 :type double-float)
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

(defgeneric bounding-box (object))

;;;; BVH

(defstruct (bvh-node (:constructor nil))
  left
  right
  (aabb (make-aabb) :type aabb))

(defun make-bvh-node (objects)
  ;; TODO: as before, this destroys the objects structure. Seems surprising.
  (let ((object-span (length objects))
        (node (make-instance 'bvh-node)))
    (setf (bvh-node-aabb node)
          (reduce #'make-aabb-from-aabbs objects
                  :initial-value (make-aabb)
                  :key #'bounding-box))
    (cond
      ((= object-span 1)
       ;; TODO: this is still dumb.
       (setf (bvh-node-left node) (elt objects 0)
             (bvh-node-right node) (elt objects 0)))
      ((= object-span 2)
       (setf (bvh-node-left node) (elt objects 0)
             (bvh-node-right node) (elt objects 1)))
      (t
       (setf objects (sort objects #'< :key (alexandria:compose #'interval-min
                                                                (longest-axis (bvh-node-aabb node))
                                                                #'bounding-box)))
       (let ((mid (floor object-span 2)))
         (setf (bvh-node-left node) (make-bvh-node (subseq objects 0 mid))
               (bvh-node-right node) (make-bvh-node (subseq objects mid))))))
    node))

(defmethod hit-test (ray (node bvh-node) ray-interval)
  (when (hit-test-aabb ray (bvh-node-aabb node) ray-interval)
    (let* ((hit-left (hit-test ray (bvh-node-left node) ray-interval))
           (right-interval (if hit-left
                               (make-interval :min (interval-min ray-interval)
                                              :max (hit-record-time hit-left))
                               ray-interval))
           (hit-right (hit-test ray (bvh-node-right node) right-interval)))
      (or hit-right hit-left))))

(defmethod bounding-box ((node bvh-node))
  (bvh-node-aabb node))

;;;; Sequences of objects

(defmethod hit-test (ray (seq sequence) ray-interval)
  (setf ray-interval (copy-interval ray-interval))
  (let ((length (length seq))
        closest)
    (dotimes (i length closest)
      (alexandria:when-let ((record (hit-test ray (elt seq i) ray-interval)))
        (setf closest record
              (interval-max ray-interval) (hit-record-time record))))))

(defmethod bounding-box ((seq sequence))
  (let ((aabb (make-aabb)))
    (dotimes (i (length seq))
      (setf aabb (make-aabb-from-aabbs aabb (bounding-box (elt seq i)))))))

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
                   (centre (sphere-centre-ray centre from to))
                   (aabb (let* ((radius-vector (make-vec3 radius radius radius))
                                (box-0 (let ((c (point-at-time centre 0d0)))
                                         (make-aabb-from-points (point3+ c (vec3- radius-vector))
                                                                (point3+ c radius-vector))))
                                (box-1 (let ((c (point-at-time centre 1d0)))
                                         (make-aabb-from-points (point3+ c (vec3- radius-vector))
                                                                (point3+ c radius-vector)))))

                           (make-aabb-from-aabbs box-0 box-1))))))
  (centre (make-ray) :type ray)
  (radius 0d0 :type (double-float 0d0))
  material
  ;; TODO: Why is this stored here and not in the BVH structure?
  (aabb (make-aabb) :type aabb))

(defmethod hit-test (ray (sphere sphere) ray-interval)
  (flet ((hit-record-for-root (root centre)
           (let* ((hit-point (point-at-time ray root))
                  (outward-normal (scaled-vec3 (point3- hit-point centre)
                                               (/ (sphere-radius sphere))))
                  (theta (acos (- (vec3-y outward-normal))))
                  (phi (+ (atan (- (vec3-z outward-normal)) (vec3-x outward-normal)) pi)))
             (make-hit-record
              :ray ray
              :point hit-point
              :outward-normal outward-normal
              :time root
              :u (/ phi (* 2 pi))
              :v (/ theta pi)
              ;; TODO: I was here! Up to section 4.5 in the book, loading image texture data.
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

(defmethod bounding-box ((sphere sphere))
  (sphere-aabb sphere))
