(in-package #:onna)

;; TODO: This is outrageously slow. However, if we compile with
;; (declaim (optimize (speed 3) (space 0) (compilation-speed 0)))
;; or similar, we get a ton of guidance about _why_ it's so slow,
;; which I guess I should follow one day.

(defstruct (colour (:constructor make-colour (r g b)))
  (r 0 :type (real 0 1))
  (g 0 :type (real 0 1))
  (b 0 :type (real 0 1)))

(defun attenuate (by colour)
  (make-colour (* (colour-r by) (colour-r colour))
               (* (colour-g by) (colour-g colour))
               (* (colour-b by) (colour-b colour))))

(defun blend-colours (&rest colours)
  (when colours
    (let ((r 0) (g 0) (b 0) (l 0))
      (dolist (colour colours)
        (incf r (colour-r colour))
        (incf g (colour-g colour))
        (incf b (colour-b colour))
        (incf l))
      (make-colour (/ r l) (/ g l) (/ b l)))))

(defun colour-8bit (c)
  (flet ((8bit (x) (floor (* x 255.99))))
    (list (8bit (colour-r c)) (8bit (colour-g c)) (8bit (colour-b c)))))

(defun gamma2 (c)
  (with-slots (r g b) c
    (make-colour (sqrt r) (sqrt g) (sqrt b))))

(defstruct (vec (:constructor make-vec (x y z)))
  (x 0 :type real)
  (y 0 :type real)
  (z 0 :type real))

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

(defun random-in-unit-sphere ()
  (flet ((vec-in-unit-cube ()
           (make-vec (1- (random 2.0)) (1- (random 2.0)) (1- (random 2.0))))
         (vec-in-unit-sphere-p (v)
           (and (< (vec-squared-length v) 1) v)))
    (loop for v = (vec-in-unit-cube)
          thereis (vec-in-unit-sphere-p v))))

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

(defun refract (v n ni/nt)
  ;; Something something Snell's law?
  (let* ((uv (unit-vec v))
         (dt (dot uv n))
         (discriminant (- 1 (* ni/nt ni/nt (- 1 (* dt dt))))))
    (when (> discriminant 0)
      (vec- (scaled-vec (vec- uv (scaled-vec n dt)) ni/nt)
            (scaled-vec n (sqrt discriminant))))))

(defun scaled-vec (v n)
  "Returns a new VEC corresponding to V scaled by N"
  (with-slots (x y z) v
    (make-vec (* x n) (* y n) (* z n))))

(defun vec+ (&rest vs)
  "Returns a new VEC representing the sum of VS"
  (let ((s (make-vec 0 0 0)))
    (with-slots (x y z) s
      (dolist (v vs s)
        (incf x (vec-x v))
        (incf y (vec-y v))
        (incf z (vec-z v))))))

(defun vec- (&rest vs)
  "See vec+"
  (cond
    ((null vs) (make-vec 0 0 0))
    ((null (cdr vs)) (with-slots (x y z) (car vs)
                       (make-vec (- x) (- y) (- z))))
    (t (let ((s (copy-vec (car vs))))
         (with-slots (x y z) s
           (dolist (v (cdr vs) s)
             (decf x (vec-x v))
             (decf y (vec-y v))
             (decf z (vec-z v))))))))

(defun unit-vec (v)
  "Returns a new VEC representing V scaled to a unit length"
  (with-slots (x y z) v
    (let ((l (vec-length v)))
      (make-vec (/ x l) (/ y l) (/ z l)))))

(defun vec-length (v)
  (sqrt (vec-squared-length v)))

(defun vec-squared-length (v)
  (with-slots (x y z) v
    (+ (* x x) (* y y) (* z z))))

(defstruct (point (:constructor make-point (x y z)) (:include vec)))

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

(defstruct ray
  (origin (make-point 0 0 0) :type point)
  (direction (make-vec 0 0 0) :type vec))

(defun point-at-parameter (r n)
  "The point at time T along R"
  (with-slots (origin direction) r
    (offset-point origin (scaled-vec direction n))))

(defstruct sphere
  (centre (make-point 0 0 0) :type point)
  (radius 0 :type real)
  material)

(defstruct hit-record
  (n 0 :type real)
  (point (make-point 0 0 0) :type point)
  (normal (make-vec 0 0 0) :type vec)
  material)

(defgeneric hit-test (ray object n-min &optional n-max)
  (:documentation "Tests OBJECT for a hit from RAY between N-MIN and N-MAX, returning a hit-record."))

(defmethod hit-test (ray (sphere sphere) n-min &optional n-max)
  (with-slots (centre radius material) sphere
    (let* ((oc (point-difference (ray-origin ray) centre))
           (a (dot (ray-direction ray) (ray-direction ray)))
           (b (dot oc (ray-direction ray)))
           (c (- (dot oc oc) (* radius radius)))
           (discriminant (- (* b b) (* a c))))
      (when (> discriminant 0)
        (flet ((root-in-bounds (root)
                 (if (null n-max)
                     (< n-min root)
                     (< n-min root n-max)))
               (hit-record-for-root (root)
                 (let* ((hit-point (point-at-parameter ray root))
                        (normal (unit-vec (scaled-vec (point-difference hit-point centre) radius))))
                   (make-hit-record :n root :point hit-point :normal normal :material material))))
          (let ((temp1 (/ (- (- b) (sqrt discriminant)) a))
                (temp2 (/ (+ (- b) (sqrt discriminant)) a)))
            (cond
              ((root-in-bounds temp1) (hit-record-for-root temp1))
              ((root-in-bounds temp2) (hit-record-for-root temp2)))))))))

(defmethod hit-test (ray (objects sequence) n-min &optional n-max)
  (let (closest)
    (dotimes (i (length objects) closest)
      (let ((hit (hit-test ray (elt objects i) n-min (if closest (hit-record-n closest) n-max))))
        (when hit
          (setf closest hit))))))

(defstruct (camera (:constructor nil))
  (lower-left-corner (make-point 0 0 0) :type point)
  (horizontal (make-vec 0 0 0) :type vec)
  (vertical (make-vec 0 0 0) :type vec)
  (origin (make-point 0 0 0) :type point)
  (u (make-vec 0 0 0) :type vec)
  (v (make-vec 0 0 0) :type vec)
  (lens-radius 0 :type real))

(defun make-camera (&key from at (up (make-vec 0 1 0)) vertical-fov aspect-ratio lens-radius focus-distance)
  "A camera with the specified vertical field of view (in degrees) and aspect ratio"
  (let* ((theta (/ (* vertical-fov pi) 180))
         (height/2 (tan (/ theta 2)))
         (width/2 (* aspect-ratio height/2))
         (camera (make-instance 'camera))
         (w (unit-vec (point-difference from at)))
         (u (unit-vec (cross up w)))
         (v (cross w u)))
    (with-slots (lower-left-corner horizontal vertical origin) camera
      (setf lower-left-corner (vec- from
                                    (scaled-vec u (* width/2 focus-distance))
                                    (scaled-vec v (* height/2 focus-distance))
                                    (scaled-vec w focus-distance))
            horizontal (scaled-vec u (* 2 width/2 focus-distance))
            vertical (scaled-vec v (* 2 height/2 focus-distance))
            origin from
            (slot-value camera 'u) u
            (slot-value camera 'v) v
            (slot-value camera 'lens-radius) lens-radius))
    camera))

(defun get-ray (camera s t*)
  (with-slots (lower-left-corner horizontal vertical origin lens-radius u v) camera
    (let* ((untransformed-offset (scaled-vec (random-in-unit-disc) lens-radius))
           (offset (vec+ (scaled-vec u (vec-x untransformed-offset))
                         (scaled-vec v (vec-y untransformed-offset)))))
      (make-ray :origin (offset-point origin offset)
                :direction (vec+ lower-left-corner
                                 (scaled-vec horizontal s)
                                 (scaled-vec vertical t*)
                                 (point-difference (make-point 0 0 0) origin)
                                 (vec- offset))))))

(defstruct scatter-record
  (attenuation (make-colour 0 0 0) :type colour)
  (scatter-ray (make-ray) :type ray))

(defgeneric scatter* (material ray-in hit-point hit-normal)
  (:documentation "Materail; scatters RAY-IN at HIT and returns a scatter-record"))

(defun scatter (ray-in hit)
  (scatter* (hit-record-material hit) ray-in (hit-record-point hit) (hit-record-normal hit)))

(defstruct lambertian (albedo (make-colour 0 0 0) :type colour))

(defmethod scatter* ((material lambertian) ray-in hit-point hit-normal)
  (let ((target (offset-point hit-point (vec+ hit-normal (random-in-unit-sphere)))))
    (make-scatter-record :attenuation (lambertian-albedo material)
                         :scatter-ray (make-ray :origin hit-point
                                                :direction (point-difference target hit-point)))))

(defstruct metal
  (albedo (make-colour 0 0 0) :type colour)
  (fuzz 0 :type (real 0 1)))

(defmethod scatter* ((material metal) ray-in hit-point hit-normal)
  (let* ((reflected (reflect (unit-vec (ray-direction ray-in)) hit-normal))
         (scattered (make-ray :origin hit-point
                              :direction (vec+ reflected
                                               (scaled-vec (random-in-unit-sphere)
                                                           (metal-fuzz material))))))
    (when (> (dot reflected hit-normal) 0)
      (make-scatter-record :attenuation (metal-albedo material)
                           :scatter-ray scattered))))

(defstruct dielectric (refractive-index 0 :type real))

(defun schlick (cosine ref-idx)
  (let ((r0 (/ (- 1 ref-idx) (+ 1 ref-idx))))
    (setf r0 (* r0 r0))
    (+ r0 (* (- 1 r0) (expt (- 1 cosine) 5)))))

(defmethod scatter* ((material dielectric) ray-in hit-point hit-normal)
  (let ((ref-idx (dielectric-refractive-index material))
        (attenuation (make-colour 1 1 1)))
    (destructuring-bind (outward-normal ni/nt cosine)
        (if (> (dot (ray-direction ray-in) hit-normal) 0)
            (list (vec- hit-normal)
                  ref-idx
                  (/ (* ref-idx (dot (ray-direction ray-in) hit-normal))
                     (vec-length (ray-direction ray-in))))
            (list hit-normal
                  (/ 1 ref-idx)
                  (/ (- (dot (ray-direction ray-in) hit-normal))
                     (vec-length (ray-direction ray-in)))))
      (let* ((refracted (refract (ray-direction ray-in) outward-normal ni/nt))
             (scatter-direction (if (or (null refracted) (< (random 1.0) (schlick cosine ref-idx)))
                                    (reflect (ray-direction ray-in) hit-normal)
                                    refracted)))
        (make-scatter-record :attenuation attenuation
                             :scatter-ray (make-ray :origin hit-point
                                                    :direction scatter-direction))))))

(defparameter *ray-recursion-depth-limit* 50)

(defun ray-colour (r world depth)
  (flet ((lerp (start end n)
           "Linear interpolation of N between START and END"
           (+ (* (- 1 n) start) (* n end))))
    (let ((hit (hit-test r world 0.001 nil)))
      (if hit
          (let ((scatter (scatter r hit)))
            (if (and (< depth *ray-recursion-depth-limit*) scatter)
                (attenuate (scatter-record-attenuation scatter)
                           (ray-colour (scatter-record-scatter-ray scatter) world (1+ depth)))
                (make-colour 0 0 0)))
          (let* ((unit-direction (unit-vec (ray-direction r)))
                 (n (* 0.5 (1+ (vec-y unit-direction)))))
            (make-colour (lerp 1 0.5 n) (lerp 1 0.7 n) 1))))))

(defparameter *antialiasing-sample-count* 100)

(defparameter *scene* (vector (make-sphere :centre (make-point 0 0 -1)
                                           :radius 0.5
                                           :material (make-lambertian :albedo (make-colour 0.1 0.2 0.5)))
                              (make-sphere :centre (make-point 0 -100.5 -1)
                                           :radius 100
                                           :material (make-lambertian :albedo (make-colour 0.8 0.8 0.0)))
                              (make-sphere :centre (make-point 1 0 -1)
                                           :radius 0.5
                                           :material (make-metal :albedo (make-colour 0.8 0.6 0.2)
                                                                 :fuzz 0.1))
                              (make-sphere :centre (make-point -1 0 -1)
                                           :radius 0.5
                                           :material (make-dielectric :refractive-index 1.5))
                              (make-sphere :centre (make-point -1 0 -1)
                                           :radius -0.45
                                           :material (make-dielectric :refractive-index 1.5))))

(defun test-scene ()
  (let* ((small-radius 0.2)
         (big-radius 1)
         (clear-centre (make-point 4 small-radius 0))
         (clear-radius 0.9)
         list)
    (push (make-sphere :centre (make-point 0 -1000 0)
                       :radius 1000
                       :material (make-lambertian :albedo (make-colour 0.5 0.5 0.5)))
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

    (flet ((choose-material ()
             (let ((m (random 1.0)))
               (cond
                 ((< m 0.8)
                  (make-lambertian :albedo (make-colour (* (random 1.0) (random 1.0))
                                                        (* (random 1.0) (random 1.0))
                                                        (* (random 1.0) (random 1.0)))))
                 ((< m 0.95)
                  (make-metal :albedo (make-colour (* 0.5 (1+ (random 1.0)))
                                                   (* 0.5 (1+ (random 1.0)))
                                                   (* 0.5 (1+ (random 1.0))))
                              :fuzz (random 0.5)))
                 (t
                  (make-dielectric :refractive-index 1.5))))))
      (loop for a from -11 below 11 do
        (loop for b from -11 below 11
              as centre = (make-point (+ a (random 0.9)) small-radius (+ b (random 0.9)))
              when (> (vec-length (point-difference centre clear-centre)) clear-radius)
                do (push (make-sphere :centre centre :radius small-radius :material (choose-material)) list))))

    (coerce list 'vector)))

(defun test-image (nx ny)
  (let ((a (make-array (list ny nx) :element-type 'colour :initial-element (make-colour 0 0 0)))
        (camera (let ((from (make-point 15 2 3))
                      (at (make-point -2 0.2 -1)))
                  (make-camera :from from
                               :at at
                               :vertical-fov 13
                               :aspect-ratio (/ nx ny)
                               :lens-radius 0.1
                               :focus-distance (vec-length (point-difference from at))))))
    (loop for j below ny do
      (loop for i below nx do
        (let (samples)
          (dotimes (s *antialiasing-sample-count*)
            (let* ((u (/ (+ i (random 1.0)) nx))
                   (v (/ (+ j (random 1.0)) ny))
                   (r (get-ray camera u v)))
              (push (ray-colour r *scene* 0) samples)))
          (setf (aref a j i) (apply #'blend-colours samples)))))
    a))

(defun write-image (image)
  "Writes an image in PPM format to *standard-output*."
  (destructuring-bind (ny nx) (array-dimensions image)
    (format t "P3~%~D ~D~%255~%" nx ny)
    (loop for j from (1- ny) downto 0 do
      (loop for i from 0 below nx do
        (destructuring-bind (ir ig ib) (colour-8bit (gamma2 (aref image j i)))
          (format t "~D ~D ~D~%" ir ig ib))))))
