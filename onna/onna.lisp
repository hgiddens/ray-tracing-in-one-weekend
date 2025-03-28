(in-package #:onna)

;; (declaim (optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0) (compilation-speed 0)))

(defstruct (colour (:constructor make-colour (r g b)))
  (r 0 :type (real 0 1))
  (g 0 :type (real 0 1))
  (b 0 :type (real 0 1)))

(defun attenuate (by colour)
  (make-colour (* (colour-r by) (colour-r colour))
               (* (colour-g by) (colour-g colour))
               (* (colour-b by) (colour-b colour))))

(defun blend-colours (colours)
  (when colours
    (let ((r 0) (g 0) (b 0) (l 0))
      (dolist (colour colours)
        (incf r (colour-r colour))
        (incf g (colour-g colour))
        (incf b (colour-b colour))
        (incf l))
      (make-colour (/ r l) (/ g l) (/ b l)))))

(defun colour-8bit (c)
  (flet ((8bit (x) (floor (* 256 (alexandria:clamp x 0 0.999)))))
    (list (8bit (colour-r c)) (8bit (colour-g c)) (8bit (colour-b c)))))

(defun gamma2 (c)
  (with-slots (r g b) c
    (make-colour (sqrt r) (sqrt g) (sqrt b))))

(defstruct (vec (:constructor make-vec (x y z &aux
                                                (x (coerce x 'double-float))
                                                (y (coerce y 'double-float))
                                                (z (coerce z 'double-float)))))
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (z 0d0 :type double-float))

(declaim (ftype (function (vec) (double-float 0d0)) vec-squared-length))
(defun vec-squared-length (v)
  (let ((x (vec-x v))
        (y (vec-y v))
        (z (vec-z v)))
    (+ (* x x) (* y y) (* z z))))

(defun cross (a b)
  "Cross product of two vectors"
  (make-vec (- (* (vec-y a) (vec-z b)) (* (vec-z a) (vec-y b)))
            (- (* (vec-z a) (vec-x b)) (* (vec-x a) (vec-z b)))
            (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b)))))

(defun dot (a b)
  "Dot product of two vectors"
  (+ (* (vec-x a) (vec-x b))
     (* (vec-y a) (vec-y b))
     (* (vec-z a) (vec-z b))))

(defun random-vec (&optional min max)
  (if (and min max)
      (progn
        (assert (< min max))
        (flet ((random-in-range () (+ min (random (- max min)))))
          (make-vec (random-in-range) (random-in-range) (random-in-range))))
      (make-vec (random 1d0) (random 1d0) (random 1d0))))

(defun random-in-unit-sphere ()
  (loop for p = (random-vec -1d0 1d0)
        as squared-length = (vec-squared-length p)
        ;; I wonder if I'm making a non-Common Lispy assumption about the
        ;; implementation of double-float here? This value is an approximation
        ;; of (sqrt least-positive-double-float) which might be nicer to use?
        ;; The book even describes numbers that, when squared, become zero as
        ;; "fallling into a 'black hole'" so we can even call it the
        ;; Schwarzschild value or something cute.
        until (and (< 1d-160 squared-length) (<= squared-length 1))
        finally (return (scaled-vec p (sqrt squared-length)))))

(defun random-in-unit-hemisphere (normal)
  ;; TODO: does anything ever use this?
  (let ((on-unit-sphere (random-in-unit-sphere)))
    (if (> (dot on-unit-sphere normal) 0)
        on-unit-sphere
        (vec- on-unit-sphere))))

(defun random-in-unit-disc ()
  (flet ((vec-in-unit-disc-p (v)
           (and (< (dot v v) 1) v)))
    (loop for x = (1- (random 2.0))
          for y = (1- (random 2.0))
          as v = (make-vec x y 0)
            thereis (vec-in-unit-disc-p v))))

(defun reflect (v n)
  "Returns a new VEC corresponding to V reflected according to surface normal N"
  (vec- v (scaled-vec n (* 2 (dot v n)))))

(defun refract (v n eta-i/eta-t)
  ;; Something something Snell's law?
  (let* ((cos-theta (min (dot (vec- v) n) 1d0))
         (r-out-perpendicular (scaled-vec (vec+ v (scaled-vec n cos-theta)) eta-i/eta-t))
         (r-out-parallel (scaled-vec n (- (sqrt (abs (- 1d0 (vec-squared-length r-out-perpendicular))))))))
    (vec+ r-out-perpendicular r-out-parallel)))

(declaim (ftype (function (vec double-float) vec) scaled-vec))
(defun scaled-vec (v n)
  "Returns a new VEC corresponding to V scaled by N"
  (make-vec (* (vec-x v) n)
            (* (vec-y v) n)
            (* (vec-z v) n)))

(defun vec+ (&rest vs)
  "Returns a new VEC representing the sum of VS"
  (let ((s (make-vec 0 0 0)))
    (dolist (v vs s)
      (incf (vec-x s) (vec-x v))
      (incf (vec-y s) (vec-y v))
      (incf (vec-z s) (vec-z v)))))

(defun vec- (&rest vs)
  "See vec+"
  (cond
    ((null vs) (make-vec 0 0 0))
    ((null (cdr vs)) (with-slots (x y z) (car vs)
                       (make-vec (- x) (- y) (- z))))
    (t (let ((s (copy-vec (car vs))))
         (dolist (v (cdr vs) s)
           (decf (vec-x s) (vec-x v))
           (decf (vec-y s) (vec-y v))
           (decf (vec-z s) (vec-z v)))))))

(defun unit-vec (v)
  "Returns a new VEC representing V scaled to a unit length"
  ;; We ignore the possibility of vec-length = 0 here. This mostly isn't a
  ;; problem because it's unlikely, and in the main program we have floating
  ;; traps masked so we end up with a NaN which is presumably what the C++ has
  ;; happen, but e.g. when we initialise the Perlin state, we're currently
  ;; running code in a defparameter form which doesn't currently have the
  ;; traps masked.
  ;;
  ;; TODO: think about this: mask traps there? Don't have a parameter?
  (let ((l (vec-length v)))
    (make-vec (/ (vec-x v) l)
              (/ (vec-y v) l)
              (/ (vec-z v) l))))

(declaim (ftype (function (vec) (values double-float &optional)) vec-length))
(defun vec-length (v)
  (sqrt (vec-squared-length v)))

(defun vec-near-zero-p (v)
  (let ((limit 1d-8))
    ;; This limit seems weirdly enormous? Maybe if it's *just* for catching
    ;; degenerate Lambertian scattering it's ok?
    (and (< (abs (vec-x v)) limit)
         (< (abs (vec-y v)) limit)
         (< (abs (vec-z v)) limit))))

(defstruct (point (:constructor make-point (x y z &aux
                                                    (x (coerce x 'double-float))
                                                    (y (coerce y 'double-float))
                                                    (z (coerce z 'double-float))))
                  (:include vec)))

(defun offset-point (p v)
  "Returns a new POINT offset from P by V"
  (make-point (+ (point-x p) (vec-x v))
              (+ (point-y p) (vec-y v))
              (+ (point-z p) (vec-z v))))

(defun point-difference (a b)
  "Returns the vector representing A - B"
  (make-vec (- (point-x a) (point-x b))
            (- (point-y a) (point-y b))
            (- (point-z a) (point-z b))))

(defstruct perlin
  (random-vecs (let ((samples (make-array 256 :element-type 'vec :initial-element (make-vec 0 0 0))))
                 (map-into samples #'random-in-unit-sphere))
   :type (simple-array vec (256)))
  (x
   (coerce (alexandria:shuffle (alexandria:iota 256)) '(simple-array (unsigned-byte 8) (256)))
   :type (simple-array (unsigned-byte 8) (256)))
  (y
   (coerce (alexandria:shuffle (alexandria:iota 256)) '(simple-array (unsigned-byte 8) (256)))
   :type (simple-array (unsigned-byte 8) (256)))
  (z
   (coerce (alexandria:shuffle (alexandria:iota 256)) '(simple-array (unsigned-byte 8) (256)))
   :type (simple-array (unsigned-byte 8) (256))))

(defparameter *perlin* (make-perlin))

(defun hermite-cubic (n)
  ;; This fixes Mach bands, apparently.
  (* n n (- 3 (* n 2))))

(declaim (ftype (function ((simple-array vec (2 2 2))
                           double-float
                           double-float
                           double-float)
                          double-float)
                perlin-interpolation))
(defun perlin-interpolation (c u v w)
  (loop with uu = (hermite-cubic u)
        and vv = (hermite-cubic v)
        and ww = (hermite-cubic w)
        for i from 0 below 2
        sum (loop for j from 0 below 2
                  sum (loop for k from 0 below 2
                            as weight = (make-vec (- u i) (- v j) (- w k))
                            sum (* (alexandria:lerp i (- 1 uu) uu)
                                   (alexandria:lerp j (- 1 vv) vv)
                                   (alexandria:lerp k (- 1 ww) ww)
                                   (dot (aref c i j k) weight))))))

(defun noise (perlin p)
  ;; I'm not sure what P is supposed to represent so I'm going to treat it like
  ;; a vector rather than a point
  (loop with (i u) = (multiple-value-list (floor (vec-x p)))
        and (j v) = (multiple-value-list (floor (vec-y p)))
        and (k w) = (multiple-value-list (floor (vec-z p)))
        and c = (make-array '(2 2 2) :element-type 'vec :initial-element (make-vec 0 0 0))
        for di from 0 below 2
        as i-idx = (elt (perlin-x perlin) (logand (+ i di) 255))
        do (loop for dj from 0 below 2
                 as j-idx = (elt (perlin-y perlin) (logand (+ j dj) 255))
                 do (loop for dk from 0 below 2
                          as k-idx = (elt (perlin-z perlin) (logand (+ k dk) 255))
                          as rf-idx = (logxor i-idx j-idx k-idx)
                          do (setf (aref c di dj dk) (elt (perlin-random-vecs perlin) rf-idx))))
        finally (return (perlin-interpolation c u v w))))

(defun turbulence (perlin p depth)
  (let ((accum 0d0)
        (weight 1d0))
    (dotimes (i depth)
      (setf accum (+ accum (* weight (noise perlin p)))
            weight (* weight 0.5d0)
            p (scaled-vec p 2d0)))
    (abs accum)))

(defgeneric texture-value (texture u v p))

(defmethod texture-value ((colour colour) u v p)
  (declare (ignore u v p))
  colour)

(defstruct chequer even odd)

(defmethod texture-value ((chequer chequer) u v p)
  (flet ((sin10 (x) (sin (* 10 x))))
    (let ((sines (* (sin10 (point-x p)) (sin10 (point-y p)) (sin10 (point-z p)))))
      (texture-value (if (minusp sines) (chequer-odd chequer) (chequer-even chequer)) u v p))))

(defstruct noise-texture (perlin nil :type perlin) (scale 1d0 :type double-float))

(defmethod texture-value ((texture noise-texture) u v p)
  (let* ((phase (+ (* (noise-texture-scale texture) (point-z p))
                   (* 10 (turbulence (noise-texture-perlin texture) p 7))))
         (grey (* 0.5 (1+ (sin phase)))))
    (make-colour grey grey grey)))

(defstruct ray
  (origin (make-point 0 0 0) :type point)
  (direction (make-vec 0 0 0) :type vec)
  (time 0d0 :type double-float))

(defstruct aabb
  (min (make-point 0 0 0) :type point)
  (max (make-point 0 0 0) :type point))

(defun surrounding-box (a b)
  (let ((small (let ((a (aabb-min a))
                     (b (aabb-min b)))
                 (make-point (min (point-x a) (point-x b))
                             (min (point-y a) (point-y b))
                             (min (point-z a) (point-z b)))))
        (big (let ((a (aabb-max a))
                   (b (aabb-max b)))
               (make-point (max (point-x a) (point-x b))
                           (max (point-y a) (point-y b))
                           (max (point-z a) (point-z b))))))
    (make-aabb :min small :max big)))

(declaim (ftype (function (ray aabb interval) boolean) hit-test-aabb))
(defun hit-test-aabb (ray aabb interval)
  ;; Not part of the hit-test generic function because it returns a bool
  ;; rather than a hit-record. Although it could? There's no requirement that
  ;; all the methods in a generic function return the same type?
  (let ((n-min (interval-min interval))
        (n-max (interval-max interval)))
    ;; This can obviously be improved, but at the moment I don't care to do
    ;; so. Defining a function that takes a slot name as an argument is slow;
    ;; macros seem pretty much as complicated for this simple use case.
    ;;
    ;; Actually, now I'm using the infinities, this can be even more obviously
    ;; improved.
    (let* ((inv-d (/ 1d0 (vec-x (ray-direction ray))))
           (t0 (* (- (point-x (aabb-min aabb))
                     (point-x (ray-origin ray)))
                  inv-d))
           (t1 (* (- (point-x (aabb-max aabb))
                     (point-x (ray-origin ray)))
                  inv-d)))
      (when (minusp inv-d)
        (rotatef t0 t1))
      (alexandria:maxf n-min t0)
      (alexandria:minf n-max t1)
      (when (<= n-max n-min)
        (return-from hit-test-aabb nil)))

    (let* ((inv-d (/ 1d0 (vec-y (ray-direction ray))))
           (t0 (* (- (point-y (aabb-min aabb))
                     (point-y (ray-origin ray)))
                  inv-d))
           (t1 (* (- (point-y (aabb-max aabb))
                     (point-y (ray-origin ray)))
                  inv-d)))
      (when (minusp inv-d)
        (rotatef t0 t1))
      (alexandria:maxf n-min t0)
      (alexandria:minf n-max t1)
      (when (<= n-max n-min)
        (return-from hit-test-aabb nil)))

    (let* ((inv-d (/ 1d0 (vec-z (ray-direction ray))))
           (t0 (* (- (point-z (aabb-min aabb))
                     (point-z (ray-origin ray)))
                  inv-d))
           (t1 (* (- (point-z (aabb-max aabb))
                     (point-z (ray-origin ray)))
                  inv-d)))
      (when (minusp inv-d)
        (rotatef t0 t1))
      (alexandria:maxf n-min t0)
      (alexandria:minf n-max t1)
      (when (<= n-max n-min)
        (return-from hit-test-aabb nil))))

  t)

(defstruct (sphere (:constructor
                       make-sphere (&key
                                      centre
                                      radius
                                      material
                                    &aux
                                      (radius (coerce radius 'double-float))
                                      (from-centre centre)))
                   (:constructor
                       make-moving-sphere (&key
                                             from
                                             to
                                             radius
                                             material
                                           &aux
                                             (from-time (coerce (car from) 'double-float))
                                             (from-centre (cdr from))
                                             (to-time (coerce (car to) 'double-float))
                                             (to-centre (cdr to))
                                             (radius (coerce radius 'double-float)))))
  (from-time 0d0 :type double-float)
  (from-centre (make-point 0 0 0) :type point)
  (to-time 0d0 :type double-float)
  (to-centre nil :type (or point null))
  (radius 0d0 :type double-float)
  material)

(declaim (ftype (function (sphere double-float) point) sphere-centre-at))
(defun sphere-centre-at (sphere time)
  (if (null (sphere-to-centre sphere))
      (sphere-from-centre sphere)
      (let ((scaled-time (/ (- time (sphere-from-time sphere))
                            (- (sphere-to-time sphere) (sphere-from-time sphere))))
            (centre-vec (point-difference (sphere-to-centre sphere) (sphere-from-centre sphere))))
        (offset-point (sphere-from-centre sphere) (scaled-vec centre-vec scaled-time)))))

(defstruct (bvh-node (:constructor empty-bvh-node))
  left right (box nil :type (or aabb null)))

;;; Destructive; I should flag this in some way?
(defun make-bvh-node (objects time0 time1)
  (let ((n (length objects))
        (node (empty-bvh-node))
        (axis (let ((axes #(x y z)))
                (elt axes (random (length axes))))))
    (setf objects (sort objects (lambda (a b)
                                  ;; Difference to book: uses time0 and time1 here
                                  (let ((a-box (bounding-box a time0 time1))
                                        (b-box (bounding-box b time0 time1)))
                                    (unless (and a-box b-box)
                                      (error "no bounding box in bvh-node construction"))
                                    (< (slot-value (aabb-min a-box) axis)
                                       (slot-value (aabb-min b-box) axis))))))
    (assert (> n 0))
    (with-slots (left right) node
      (case n
        (1
         ;; This seems silly but it's what's in the book.
         (setf left (elt objects 0)
               right (elt objects 0)))
        (2
         (setf left (elt objects 0)
               right (elt objects 1)))
        (otherwise
         (let ((split (floor (/ n 2))))
           (setf left (make-bvh-node (subseq objects 0 split) time0 time1)
                 right (make-bvh-node (subseq objects split) time0 time1)))))
      (alexandria:if-let ((box-left (when left (bounding-box left time0 time1)))
                          (box-right (when right (bounding-box right time0 time1))))
        (setf (bvh-node-box node) (surrounding-box box-left box-right))
        ;; Is this actually an error? It just means the node has no bounding box, right?
        (error "no bounding box in bvh-node construction")))
    node))

(defstruct hit-record
  (n 0 :type real)
  (point (make-point 0 0 0) :type point)
  (normal (make-vec 0 0 0) :type vec)
  ;; TODO: need to make sure this is updated for all hittables
  front-face
  material)

(defun set-hit-record-face-normal (hit-record ray outward-normal)
  "Sets the normal and front-face of hit-record."
  ;; TODO: outward-normal is assumed to have unit length, which seems like
  ;; something types could help with? I think I saw something about predicate
  ;; types or something too, although they're slow obviously. Or maybe just
  ;; make the constructor smarter?
  (with-slots (front-face normal) hit-record
    (setf front-face (< (dot (ray-direction ray) outward-normal) 0)
          normal (if front-face outward-normal (vec- outward-normal)))))

(defstruct interval
  ;; TODO: This is a dumb default, for example the default size is negative
  ;; infinity. On the other hand now I've given up I should be able make some
  ;; other code less tremendously annoying.
  (min #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY :type double-float)
  (max #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY :type double-float))

;; TODO: I'll be these want to be inlined on account of how SBCL boxes floats.
(defun interval-size (i)
  (- (interval-max i) (interval-min i)))

(defun interval-contains (i x)
  (<= (interval-min i) x (interval-max i)))

(defun interval-surrounds (i x)
  (< (interval-min i) x (interval-max i)))

(defun interval-clamp (i x)
  (cond
    ((< x (interval-min i)) (interval-min i))
    ((> x (interval-max i)) (interval-max i))
    (x)))

;; TODO: defconst?
(defun universal-interval ()
  (make-interval :min #.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
                 :max #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY))

(defgeneric hit-test (ray object interval)
  (:documentation "Tests OBJECT for a hit from RAY between N-MIN and N-MAX, returning a hit-record."))

(defun hit-test-sphere (ray sphere interval)
  (let* ((sphere-centre (sphere-centre-at sphere (ray-time ray)))
         (oc (point-difference sphere-centre (ray-origin ray)))
         (a (vec-squared-length (ray-direction ray)))
         (h (dot (ray-direction ray) oc))
         (c (- (vec-squared-length oc) (* (sphere-radius sphere) (sphere-radius sphere))))
         (discriminant (- (* h h) (* a c))))
    (when (> discriminant 0)
      (labels ((point-at-parameter (r n)
                 (offset-point (ray-origin r) (scaled-vec (ray-direction r) n)))
               (hit-record-for-root (root)
                 (let* ((hit-point (point-at-parameter ray root))
                        (outward-normal (scaled-vec (point-difference hit-point sphere-centre) (/ (sphere-radius sphere))))
                        (hit-record (make-hit-record :n root :point hit-point :material (sphere-material sphere))))
                   (set-hit-record-face-normal hit-record ray outward-normal)
                   hit-record)))
        (let* ((sqrt-discriminant (sqrt discriminant))
               (root (/ (- h sqrt-discriminant) a)))
          (if (interval-surrounds interval root)
              (hit-record-for-root root)
              (progn
                (setf root (/ (+ h sqrt-discriminant) a))
                (when (interval-surrounds interval root) (hit-record-for-root root)))))))))

(defmethod hit-test (ray (sphere sphere) interval)
  (hit-test-sphere ray sphere interval))

(defun hit-test-seq (ray objects interval)
  (let (closest)
    (dotimes (i (length objects) closest)
      (alexandria:when-let ((hit (hit-test ray (elt objects i) (if closest
                                                                   (make-interval :min (interval-min interval)
                                                                                  :max (hit-record-n closest))
                                                                   interval))))
        (setf closest hit)))))

(defmethod hit-test (ray (objects sequence) interval)
  (hit-test-seq ray objects interval))

(defun hit-test-node (ray node interval)
  (when (let ((box (bvh-node-box node)))
          (and box (hit-test-aabb ray box interval)))
    (flet ((sub-hit (sub) (and sub (hit-test ray sub interval))))
      (let ((left-hit (sub-hit (bvh-node-left node)))
            (right-hit (sub-hit (bvh-node-right node))))
        (cond
          ((and left-hit right-hit)
           (if (< (hit-record-n left-hit) (hit-record-n right-hit)) left-hit right-hit))
          (left-hit)
          (right-hit))))))

(defmethod hit-test (ray (node bvh-node) interval)
  (hit-test-node ray node interval))

(defgeneric bounding-box (object t0 t1)
  (:documentation "Returns the axis-aligned bounding box (AABB) of OBJECT between times t0 and t1."))

;;; I don't think we need split the implementation functions out as the
;;; bounding boxes are built once, at program start.

(defmethod bounding-box ((sphere sphere) t0 t1)
  (let* ((r (sphere-radius sphere))
         (r-vec (make-vec r r r)))
    (flet ((bounding-box* (c)
             (make-aabb :min (offset-point c (vec- r-vec)) :max (offset-point c r-vec))))
      (if (null (sphere-to-centre sphere))
          (bounding-box* (sphere-from-centre sphere))
          (surrounding-box (bounding-box* (sphere-centre-at sphere t0))
                           (bounding-box* (sphere-centre-at sphere t1)))))))

(defmethod bounding-box ((objects sequence) t0 t1)
  (reduce (lambda (&optional a b)
            (and a b (surrounding-box a b)))
          objects
          :key (lambda (o) (bounding-box o t0 t1))))

(defmethod bounding-box ((node bvh-node) t0 t1)
  (bvh-node-box node))

(defstruct (camera (:constructor nil))
  (lower-left-corner (make-point 0 0 0) :type point)
  (horizontal (make-vec 0 0 0) :type vec)
  (vertical (make-vec 0 0 0) :type vec)
  (origin (make-point 0 0 0) :type point)
  (u (make-vec 0 0 0) :type vec)
  (v (make-vec 0 0 0) :type vec)
  (lens-radius 0d0 :type double-float)
  (t0 0d0 :type double-float)
  (t1 0d0 :type double-float))

(defun make-camera (&key from at (up (make-vec 0 1 0)) vertical-fov aspect-ratio lens-radius focus-distance t0 t1)
  "A camera with the specified vertical field of view (in degrees) and aspect ratio"
  (let* ((theta (/ (* vertical-fov pi) 180))
         (height/2 (tan (/ theta 2)))
         (width/2 (* aspect-ratio height/2))
         (camera (make-instance 'camera))
         (w (unit-vec (point-difference from at)))
         (u (unit-vec (cross up w)))
         (v (cross w u)))
    (with-slots (lower-left-corner horizontal vertical origin) camera
      (setf focus-distance (coerce focus-distance 'double-float)
            lower-left-corner (vec- from
                               (scaled-vec u (* width/2 focus-distance))
                               (scaled-vec v (* height/2 focus-distance))
                               (scaled-vec w focus-distance))
            horizontal (scaled-vec u (* 2 width/2 focus-distance))
            vertical (scaled-vec v (* 2 height/2 focus-distance))
            origin from
            (slot-value camera 'u) u
            (slot-value camera 'v) v
            (slot-value camera 'lens-radius) (coerce lens-radius 'double-float)
            (camera-t0 camera) (coerce t0 'double-float)
            (camera-t1 camera) (coerce t1 'double-float)))
    camera))

(defun get-ray (camera s t*)
  (let* ((untransformed-offset (scaled-vec (random-in-unit-disc) (camera-lens-radius camera)))
         (offset (vec+ (scaled-vec (camera-u camera) (vec-x untransformed-offset))
                       (scaled-vec (camera-v camera) (vec-y untransformed-offset)))))
    (make-ray :origin (offset-point (camera-origin camera) offset)
              :direction (vec+ (camera-lower-left-corner camera)
                               (scaled-vec (camera-horizontal camera) s)
                               (scaled-vec (camera-vertical camera) t*)
                               (point-difference (make-point 0 0 0) (camera-origin camera))
                               (vec- offset))
              :time (coerce (alexandria:lerp (random 1.0) (camera-t0 camera) (camera-t1 camera)) 'double-float))))

(defstruct (book-camera (:constructor nil))
  (centre (make-point 0 0 0) :type point)
  ;; Vectors across the horizontal and down the vertical viewport edges.
  (viewport-u (make-vec 0 0 0) :type vec)
  (viewport-v (make-vec 0 0 0) :type vec)
  ;; Upper-left corner of the viewport
  (viewport-upper-left (make-point 0 0 0) :type point)
  ;; Defocus disc horizontal and vertical radius.
  (defocus-disc-u nil :type (or null vec))
  (defocus-disc-v nil :type (or null vec)))
(defun make-book-camera (&key
                           (from (make-point 0 0 0))
                           (at (make-point 0 0 -1))
                           (up (make-vec 0 1 0))
                           ;; Vertical field of view, degrees.
                           (vertical-fov 90)
                           (aspect-ratio 1)
                           ;; Variation of angle of rays through each pixel, degrees.
                           (defocus-angle 0)
                           ;; Distance from camera to plane of perfect focus.
                           (focus-distance 10))
  (setf focus-distance (coerce focus-distance 'double-float))
  ;; defocus-angle used to be lens-radius, which I think I like more?
  (let* ((camera (make-instance 'book-camera))
         (theta (/ (* vertical-fov pi) 180))
         (viewport-height (* 2 (tan (/ theta 2)) focus-distance))
         (viewport-width (* viewport-height aspect-ratio))
         ;; The u, v, w vectors are the dimensional unit basis vectors for the
         ;; camera co-ordinate frame.
         (w (unit-vec (point-difference from at)))
         (u (unit-vec (cross up w)))
         (v (unit-vec (cross w u))))
    (with-slots (centre viewport-u viewport-v viewport-upper-left) camera
      (setf centre from
            viewport-u (scaled-vec u viewport-width)
            viewport-v (scaled-vec (vec- v) viewport-height)
            viewport-upper-left (let ((offset (vec+ (scaled-vec w focus-distance)
                                                    (scaled-vec viewport-u (/ 2d0))
                                                    (scaled-vec viewport-v (/ 2d0)))))
                                  (offset-point centre (vec- offset)))))
    (let ((defocus-radius (* focus-distance (tan (/ (* (/ defocus-angle 2) pi) 180)))))
      (unless (= defocus-radius 0)
        (setf (book-camera-defocus-disc-u camera) (scaled-vec u defocus-radius)
              (book-camera-defocus-disc-v camera) (scaled-vec v defocus-radius))))
    camera))
(defun get-book-camera-ray (camera pixel-centre)
  (let* ((origin (let ((centre (book-camera-centre camera)))
                   (alexandria:if-let ((u (book-camera-defocus-disc-u camera))
                                       (v (book-camera-defocus-disc-v camera)))
                     (let ((p (random-in-unit-disc)))
                       (offset-point centre (vec+ (scaled-vec u (vec-x p)) (scaled-vec v (vec-y p)))))
                     centre)))
         (ray-direction (point-difference pixel-centre origin)))
    (make-ray :origin origin :direction ray-direction)))

(defstruct scatter-record
  (attenuation (make-colour 0 0 0) :type colour)
  (scatter-ray (make-ray) :type ray))

(defgeneric scatter* (material ray-in hit-point hit-normal hit-front-face)
  (:documentation "Materail; scatters RAY-IN at HIT and returns a scatter-record"))

(defun scatter (ray-in hit)
  (scatter* (hit-record-material hit) ray-in (hit-record-point hit) (hit-record-normal hit) (hit-record-front-face hit)))

(defstruct lambertian (albedo (make-colour 0 0 0)))

(defmethod scatter* ((material lambertian) ray-in hit-point hit-normal hit-front-face)
  (let* ((scatter-direction (let ((s (vec+ hit-normal (random-in-unit-sphere))))
                             (if (vec-near-zero-p s) hit-normal s)))
         (target (offset-point hit-point scatter-direction)))
    (make-scatter-record :attenuation (texture-value (lambertian-albedo material) 0 0 hit-point)
                         :scatter-ray (make-ray :origin hit-point
                                                :direction (point-difference target hit-point)
                                                :time (ray-time ray-in)))))

(defstruct (metal (:constructor make-metal (&key albedo fuzz &aux (fuzz (coerce fuzz 'double-float)))))
  (albedo (make-colour 0 0 0) :type colour)
  (fuzz 0d0 :type (double-float 0d0 1d0)))

(defmethod scatter* ((material metal) ray-in hit-point hit-normal hit-front-face)
  (let* ((reflected (reflect (ray-direction ray-in) hit-normal))
         (scattered (make-ray :origin hit-point
                              :direction (vec+ (unit-vec reflected)
                                               (scaled-vec (random-in-unit-sphere)
                                                           (metal-fuzz material)))
                              :time (ray-time ray-in))))
    ;; BROKEN: This is what the book says, and it's fucked?
    (when (> (dot scattered hit-normal) 0)
      (make-scatter-record :attenuation (metal-albedo material)
                           :scatter-ray scattered))))

(defstruct (dielectric (:constructor
                           make-dielectric
                           (&key refractive-index
                            &aux (refractive-index (coerce refractive-index 'double-float)))))
  ;; The ratio of the material's refractive index over the refractive index of
  ;; the enclosing media.
  (refractive-index 0 :type double-float))

(defun reflectance (cosine ref-idx)
  "Schlick's approximation for reflectance."
  (let ((r0 (/ (- 1 ref-idx) (+ 1 ref-idx))))
    (setf r0 (* r0 r0))
    (+ r0 (* (- 1 r0) (expt (- 1 cosine) 5)))))

(defmethod scatter* ((material dielectric) ray-in hit-point hit-normal hit-front-face)
  (let* ((attenuation (make-colour 1 1 1))
         (ref-idx (let ((r (dielectric-refractive-index material)))
                    (if hit-front-face (/ r) r)))
         (unit-direction (unit-vec (ray-direction ray-in)))
         (cos-theta (min (dot (vec- unit-direction) hit-normal) 1d0))
         (sin-theta (sqrt (- 1d0 (* cos-theta cos-theta))))
         (cannot-refract (> (* ref-idx sin-theta) 1d0))
         (scatter-direction (if (or cannot-refract (> (reflectance cos-theta ref-idx) (random 1d0)))
                                (reflect unit-direction hit-normal)
                                (refract unit-direction hit-normal ref-idx))))
    (make-scatter-record :attenuation attenuation
                         :scatter-ray (make-ray :origin hit-point
                                                :direction scatter-direction
                                                :time (ray-time ray-in)))))

;; TODO: This was 50 before! I wonder how much of a difference it makes.
;; TODO: In the book, this is a property of the camera, but that doesn't
;; really match how I want things to be.
(defparameter *ray-recursion-depth-limit* 10)

(defun ray-colour (r world depth)
  (alexandria:if-let ((hit (hit-test r world (make-interval :min 0.001d0 :max #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY))))
    (let ((scatter (scatter r hit)))
      (if (and (< depth *ray-recursion-depth-limit*) scatter)
          (attenuate (scatter-record-attenuation scatter)
                     (ray-colour (scatter-record-scatter-ray scatter) world (1+ depth)))
          (make-colour 0 0 0)))
    (let* ((unit-direction (unit-vec (ray-direction r)))
           (n (* 0.5 (1+ (vec-y unit-direction)))))
      (make-colour (alexandria:lerp n 1 0.5) (alexandria:lerp n 1 0.7) 1))))

(defparameter *antialiasing-sample-count* 10)

(defparameter *scene* (vector (make-sphere :centre (make-point 0 0 -1.2)
                                           :radius 0.5
                                           :material (make-lambertian :albedo (make-colour 0.1 0.2 0.5)))
                              (make-sphere :centre (make-point 0 -100.5 -1)
                                           :radius 100
                                           :material (make-lambertian :albedo (make-colour 0.8 0.8 0.0)))
                              (make-sphere :centre (make-point 1 0 -1)
                                           :radius 0.5
                                           :material (make-metal :albedo (make-colour 0.8 0.6 0.2)
                                                                 :fuzz 1.0))
                              (make-sphere :centre (make-point -1 0 -1)
                                           :radius 0.5
                                           :material (make-dielectric :refractive-index 1.5))
                              (make-sphere :centre (make-point -1 0 -1)
                                           :radius -0.4
                                           :material (make-dielectric :refractive-index 1.5))))

(defun test-scene ()
  (let* ((small-radius 0.2)
         (big-radius 1)
         (clear-centre (make-point 4 small-radius 0))
         (clear-radius 0.9)
         list)
    (push (let ((texture (make-chequer :even (make-colour 0.2 0.3 0.1)
                                       :odd (make-colour 0.9 0.9 0.9))))
            (make-sphere :centre (make-point 0 -1000 0)
                         :radius 1000
                         :material (make-lambertian :albedo texture)))
          list)
    (push (make-sphere :centre (make-point 0 1 0)
                       :radius big-radius
                       :material (make-dielectric :refractive-index 1.5))
          list)
    (push (make-sphere :centre (make-point -4 1 0)
                       :radius big-radius
                       :material (make-lambertian :albedo (make-colour 0.4 0.2 0.1)))
          list)
    (push (make-sphere :centre (make-point 4 1 0)
                       :radius big-radius
                       :material (make-metal :albedo (make-colour 0.7 0.6 0.5) :fuzz 0))
          list)

    (flet ((random-sphere (centre radius)
             (let ((m (random 1.0)))
               (cond
                 ((< m 0.8)
                  (make-moving-sphere :from (cons 0 centre)
                                      :to (cons 1 (offset-point centre (make-vec 0 (random 0.5d0) 0)))
                                      :radius radius
                                      :material (make-lambertian :albedo (make-colour (* (random 1.0) (random 1.0))
                                                                                      (* (random 1.0) (random 1.0))
                                                                                      (* (random 1.0) (random 1.0))))))
                 ((< m 0.95)
                  (make-sphere :centre centre
                               :radius radius
                               :material (make-metal :albedo (make-colour (* 0.5 (1+ (random 1.0)))
                                                                          (* 0.5 (1+ (random 1.0)))
                                                                          (* 0.5 (1+ (random 1.0))))
                                                     :fuzz (random 0.5))))
                 (t
                  (make-sphere :centre centre
                               :radius radius
                               :material (make-dielectric :refractive-index 1.5)))))))
      (loop for a from -11 below 11 do
        (loop for b from -11 below 11
              as centre = (make-point (+ a (random 0.9)) small-radius (+ b (random 0.9)))
              when (> (vec-length (point-difference centre clear-centre)) clear-radius)
                do (push (random-sphere centre small-radius) list))))

    (coerce list 'vector)))

(defun test-image (image-width image-height)
  ;; There's also :invalid to handle (quiet) NaNs, which the book mentions we
  ;; may encounter at some point. There are also predicates for the "fun"
  ;; floating-point values in SB-EXT. There are also reader literals e.g.
  ;; #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY.
  ;;
  ;; This is here rather than closer to where the float stuff actually happens
  ;; because it's apparently slow to toggle (also, apparently it only works on
  ;; the main thread or something, which, yikes, so I guess we might fix it
  ;; differently one day).
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (let* ((a (make-array (list image-height image-width)
                          :element-type 'colour
                          :initial-element (make-colour 0 0 0)))
           ;; (camera (make-camera :from (make-point 13 2 3)
           ;;                      :at (make-point 0 0 0)
           ;;                      :vertical-fov 20
           ;;                      :aspect-ratio (/ image-width image-height)
           ;;                      :lens-radius 0
           ;;                      :focus-distance 10
           ;;                      :t0 0
           ;;                      :t1 1))
           (camera (make-camera :from (make-point -2 2 1)
                                :at (make-point 0 0 -10)
                                :vertical-fov 20
                                :aspect-ratio (/ image-width image-height)
                                :lens-radius 0
                                :focus-distance 3.4d0
                                :t0 0
                                :t1 1))
           (book-camera (make-book-camera :from (make-point -2 2 1)
                                          :at (make-point 0 0 -1)
                                          :vertical-fov 20
                                          :aspect-ratio (/ image-width image-height)
                                          ;; :defocus-angle 10
                                          ;; :focus-distance 3.4
                                          ))
           (pixel-delta-u (scaled-vec (book-camera-viewport-u book-camera)
                                      (/ (coerce image-width 'double-float))))
           (pixel-delta-v (scaled-vec (book-camera-viewport-v book-camera)
                                      (/ (coerce image-height 'double-float))))
           (pixel-0-0-loc (offset-point (book-camera-viewport-upper-left book-camera)
                                        (scaled-vec (vec+ pixel-delta-u pixel-delta-v) 0.5d0))))
      (flet ((sample-pixel (i j)
               (let* ((u (/ (+ i (random 1d0)) image-width))
                      (v (/ (+ j (random 1d0)) image-height))
                      (r (get-ray camera u v)))
                 (ray-colour r *scene* 0)))
             (book-camera-sample-pixel (i j)
               (labels ((sample-square ()
                          ;; I'm not sure this is better? On the other hand, I
                          ;; did literally earlier today see an interesting
                          ;; looking paper called "A pixel is not a little
                          ;; square" or something similar, and the book
                          ;; suggests changing this sample-disk to experiment
                          ;; with non-square pixels.
                          (make-vec (- (random 1d0) 0.5) (- (random 1d0) 0.5) 0)))
                 (let* ((offset (sample-square))
                        (i* (+ (coerce i 'double-float) (vec-x offset)))
                        (j* (+ (coerce j 'double-float) (vec-y offset)))
                        (pixel-centre (offset-point pixel-0-0-loc (vec+ (scaled-vec pixel-delta-u i*)
                                                                        (scaled-vec pixel-delta-v j*))))
                        (r (get-book-camera-ray book-camera pixel-centre)))
                   (ray-colour r *scene* 0)))))
        (loop for j below image-height do
          ;; TODO: the book has a thing that prints out the scanlines here,
          ;; but they use carriage returns to just do it on a single updating
          ;; line. Not sure what a good equivalent for this would be? I
          ;; haven't felt that it's necessary so far.
          (loop for i below image-width do
            (loop repeat *antialiasing-sample-count*
                  collect (book-camera-sample-pixel i j) into samples
                  finally (setf (aref a j i) (blend-colours samples))))))
      a)))

(defun write-image (image)
  "Writes an image in PPM format to *standard-output*."
  (destructuring-bind (image-height image-width) (array-dimensions image)
    (format t "P3~%~D ~D~%255~%" image-width image-height)
    (loop for j from 0 below image-height do
      (loop for i from 0 below image-width
            as (ir ig ib) = (colour-8bit (gamma2 (aref image j i)))
            do (format t "~D ~D ~D~%" ir ig ib)))))
