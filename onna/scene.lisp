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

(defgeneric bounding-box (object)
  (:documentation
   "The axis-aligned bounding box for OBJECT; see `aabb'."))

;;; TODO: I hate this name. Maybe make pdf-value take (maybe with keywords?)
;;; an origin too and then just have pdf-value everywhere?
(defgeneric object-pdf-value (object origin direction)
  (:documentation
   "The distribution value of a ray from ORIGIN in DIRECTION hitting OBJECT."))

;;; TODO: On the surface of? In the volume of? Probably the latter if I want
;;; an easy implementation for e.g. sequences?
(defgeneric random-point (object)
  (:documentation "Generates a random point on OBJECT."))

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
    (dotimes (i (length seq) aabb)
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

;;;; Quadrilaterals

(defstruct (quad
            (:constructor make-quad
              (&key q u v material
               &aux
                 (n (cross-product u v))
                 ;; This will "deliberately" divide by zero if the u and v
                 ;; basis vectors are parallel.
                 (w (scaled-vec3 n (/ (dot-product n n))))
                 (aabb (let ((diagonal-1 (make-aabb-from-points q (point3+ q u v)))
                             (diagonal-2 (make-aabb-from-points (point3+ q u) (point3+ q v))))
                         (make-aabb-from-aabbs diagonal-1 diagonal-2)))
                 (normal (unit-vec3 n))
                 (d (dot-product normal q))
                 (area (vec3-length n)))))
  (q (make-point3 0 0 0) :type point3)
  (u (make-vec3 0 0 0) :type vec3)
  (v (make-vec3 0 0 0) :type vec3)
  (w (make-vec3 0 0 0) :type vec3)
  material
  ;; TODO: Same question as with spheres above: why is this here?
  (aabb (make-aabb) :type aabb)
  (normal (make-vec3 0 0 0) :type vec3)
  (d 0d0 :type double-float)
  (area 0d0 :type double-float))

(defmethod hit-test (ray (quad quad) ray-interval)
  (let ((denom (dot-product (quad-normal quad) (ray-direction ray))))
    ;; No hit if the ray is parallel to the plane.
    (unless (< (abs denom) 1d-8)
      (let ((time (/  (- (quad-d quad) (dot-product (quad-normal quad) (ray-origin ray))) denom)))
        ;; No hit if the time parameter is outside the ray interval.
        (when (interval-contains ray-interval time)
          (let* ((intersection (point-at-time ray time))
                 (planar-hit-point-vector (point3- intersection (quad-q quad)))
                 (alpha (dot-product (quad-w quad)
                                     (cross-product planar-hit-point-vector (quad-v quad))))
                 (beta (dot-product (quad-w quad)
                                    (cross-product (quad-u quad) planar-hit-point-vector))))
            ;; No hit if the intersection is outside the planar shape.
            (when (and (<= 0d0 alpha 1d0)
                       (<= 0d0 beta 1d0))
              (make-hit-record :ray ray
                               :point intersection
                               :outward-normal (quad-normal quad)
                               :time time
                               :u alpha
                               :v beta
                               :material (quad-material quad)))))))))

(defmethod bounding-box ((quad quad))
  (quad-aabb quad))

(defmethod object-pdf-value ((quad quad) origin direction)
  ;; TODO: This doesn't "need" to specify a time because quads can't move, but
  ;; this is just filthy code.
  ;;
  ;; I also don't understand how this works (the results get bigger as the
  ;; quad gets further from the origin, or am I missing something)? How does
  ;; this some to 1? What the actual fuck is going on here?
  (alexandria:if-let ((hit (hit-test (make-ray :origin origin :direction direction)
                                     quad
                                     (make-interval :min 0.001
                                                    :max #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY))))
    (let ((distance-squared (* (hit-record-time hit)
                               (hit-record-time hit)
                               (vec3-length-squared direction)))
          (cosine (abs (/ (dot-product direction (hit-record-normal hit))
                          (vec3-length direction)))))
      (/ distance-squared (* cosine (quad-area quad))))
    0))

(defmethod random-point ((quad quad))
  (point3+ (quad-q quad)
           (scaled-vec3 (quad-u quad) (random 1d0))
           (scaled-vec3 (quad-v quad) (random 1d0))))

(defun make-box (&key a b material)
  ;; TODO: Should this be called make-box? Or just box?
  (let* ((min (make-point3 (min (point3-x a) (point3-x b))
                           (min (point3-y a) (point3-y b))
                           (min (point3-z a) (point3-z b))))
         (max (make-point3 (max (point3-x a) (point3-x b))
                           (max (point3-y a) (point3-y b))
                           (max (point3-z a) (point3-z b))))
         (dx (make-vec3 (- (point3-x max) (point3-x min)) 0 0))
         (dy (make-vec3 0 (- (point3-y max) (point3-y min)) 0))
         (dz (make-vec3 0 0 (- (point3-z max) (point3-z min)))))
    (vector
     ;; front
     (make-quad :q (make-point3 (point3-x min) (point3-y min) (point3-z max)) :u dx :v dy :material material)
     ;; right
     (make-quad :q (make-point3 (point3-x max) (point3-y min) (point3-z max)) :u (vec3- dz) :v dy :material material)
     ;; back
     (make-quad :q (make-point3 (point3-x max) (point3-y min) (point3-z min)) :u (vec3- dx) :v dy :material material)
     ;; left
     (make-quad :q (make-point3 (point3-x min) (point3-y min) (point3-z min)) :u dz :v dy :material material)
     ;; top
     (make-quad :q (make-point3 (point3-x min) (point3-y max) (point3-z max)) :u dx :v (vec3- dz) :material material)
     ;; bottom
     (make-quad :q (make-point3 (point3-x min) (point3-y min) (point3-z min)) :u dx :v dz :material material))))

;;;; Translate

;;; TODO: This is a dumb constructor name, but I think the struct
;;; representation makes sense? Hah, although another representation that
;;; (might) work would be having instances (translation, rotation etc) as CLOS
;;; mixins and using method combinations to handle the translations between
;;; world and object spaces. It's probably dumb but I'm interested to see how
;;; it would work and/or look.
(defstruct (translate
            (:constructor make-translate
                (&key object offset
                 &aux
                   (aabb (aabb+ (bounding-box object) offset)))))
  object
  (offset (make-vec3 0 0 0) :type vec3)
  (aabb (make-aabb) :type aabb))

(defmethod hit-test (ray (translate translate) ray-interval)
  (let ((offset-ray (copy-ray ray)))
    (setf (ray-origin offset-ray) (point3+ (ray-origin ray) (vec3- (translate-offset translate))))
    (alexandria:when-let ((hit (hit-test offset-ray (translate-object translate) ray-interval)))
      (setf (hit-record-point hit) (point3+ (hit-record-point hit) (translate-offset translate)))
      hit)))

(defmethod bounding-box ((translate translate))
  (translate-aabb translate))

;;;; Rotations

;; TODO: As the name suggests, rotates only in the Y axis. Surely we just use
;; matrices here and move on with our lives, right? Right?
(defstruct (rotate-y
            (:constructor make-rotate-y
                (&key object angle
                 &aux
                   (radians (/ (* angle pi) 180d0))
                   (sin-theta (sin radians))
                   (cos-theta (cos radians))
                   ;; TODO: This is an extremely literal translation of the C++
                   ;; and consequently gross.
                   (aabb (loop with bbox = (bounding-box object)
                               with min = (make-point3 #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
                                                       #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
                                                       #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)
                               with max = (make-point3 #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
                                                       #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
                                                       #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY)
                               for i below 2 do
                                 (loop for j below 2 do
                                   (loop for k below 2
                                         as x = (let ((n (aabb-x bbox)))
                                                  (+ (* (interval-max n) i) (* (interval-min n) (- 1 i))))
                                         as y = (let ((n (aabb-y bbox)))
                                                  (+ (* (interval-max n) j) (* (interval-min n) (- 1 j))))
                                         as z = (let ((n (aabb-z bbox)))
                                                  (+ (* (interval-max n) k) (* (interval-min n) (- 1 k))))
                                         as new-x = (+ (* cos-theta x) (* sin-theta z))
                                         as new-z = (+ (* (- sin-theta) x) (* cos-theta z))
                                         do (setf (point3-x min) (min (point3-x min) new-x)
                                                  (point3-x max) (max (point3-x max) new-x)
                                                  (point3-y min) (min (point3-y min) y)
                                                  (point3-y max) (max (point3-y max) y)
                                                  (point3-z min) (min (point3-z min) new-z)
                                                  (point3-z max) (max (point3-z max) new-z))))
                               finally (return (make-aabb-from-points min max)))))))
  object
  (sin-theta 0d0 :type double-float)
  (cos-theta 0d0 :type double-float)
  (aabb (make-aabb) :type aabb))

(defmethod hit-test (ray (rotate rotate-y) ray-interval)
  ;; Transform the ray from world space to object space.
  (flet ((transform (o)
           ;; Because point3 includes vec3 and has no extra slots, we can just
           ;; do the same operation to both points and vectors.
           (setf o (copy-structure o))
           (psetf
            (vec3-x o) (- (* (rotate-y-cos-theta rotate) (vec3-x o))
                          (* (rotate-y-sin-theta rotate) (vec3-z o)))
            (vec3-z o) (+ (* (rotate-y-sin-theta rotate) (vec3-x o))
                          (* (rotate-y-cos-theta rotate) (vec3-z o))))
           o)
         (untransform (o)
           (setf o (copy-structure o))
           (psetf
            (vec3-x o) (+ (* (rotate-y-cos-theta rotate) (vec3-x o))
                          (* (rotate-y-sin-theta rotate) (vec3-z o)))
            (vec3-z o) (+ (* (- (rotate-y-sin-theta rotate)) (vec3-x o))
                          (* (rotate-y-cos-theta rotate) (vec3-z o))))
           o))
    (let ((transformed-ray (copy-ray ray)))
      (setf (ray-origin transformed-ray) (transform (ray-origin ray))
            (ray-direction transformed-ray) (transform (ray-direction ray)))
      (alexandria:when-let ((hit (hit-test transformed-ray (rotate-y-object rotate) ray-interval)))
        (setf (hit-record-point hit) (untransform (hit-record-point hit))
              (hit-record-normal hit) (untransform (hit-record-normal hit)))
        hit))))

(defmethod bounding-box ((rotate rotate-y))
  (rotate-y-aabb rotate))

;;;; Volumes

(defstruct (constant-medium
            (:constructor make-constant-medium
              (&key boundary density texture
               &aux
                 (negative-inverse-density (/ -1d0 (coerce density 'double-float)))
                 (phase-function (make-isotropic :texture texture)))))
  boundary
  (negative-inverse-density 0d0 :type double-float)
  phase-function)


(defmethod hit-test (ray (medium constant-medium) ray-interval)
  ;; Note: this assumes a convex boundary; see the end of §9.1 in book 2.
  (alexandria:when-let
      ((rec-1 (hit-test ray
                        (constant-medium-boundary medium)
                        (universe-interval))))
    (alexandria:when-let
        ((rec-2 (hit-test ray
                          (constant-medium-boundary medium)
                          (make-interval :min (+ (hit-record-time rec-1) 0.0001d0)
                                         :max #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY))))
      (alexandria:maxf (hit-record-time rec-1) (interval-min ray-interval))
      (alexandria:minf (hit-record-time rec-2) (interval-max ray-interval))
      (unless (>= (hit-record-time rec-1) (hit-record-time rec-2))
        (alexandria:maxf (hit-record-time rec-1) 0)
        (let* ((ray-length (vec3-length (ray-direction ray)))
               (distance-inside-boundary (* (- (hit-record-time rec-2)
                                               (hit-record-time rec-1))
                                            ray-length))
               (hit-distance (* (constant-medium-negative-inverse-density medium)
                                (log (random 1d0))))
               (hit-time (+ (hit-record-time rec-1) (/ hit-distance ray-length))))
          (unless (> hit-distance distance-inside-boundary)
            (make-hit-record :ray ray
                             :point (point-at-time ray hit-time)
                             ;; The outward normal is arbitrary, and it
                             ;; doesn't matter what front-face gets set to.
                             :outward-normal (make-vec3 1 0 0)
                             :time hit-time
                             :material (constant-medium-phase-function medium))))))))

(defmethod bounding-box ((medium constant-medium))
  (bounding-box (constant-medium-boundary medium)))
