(in-package #:onna)

(defstruct (colour (:constructor make-colour (r g b)))
  (r 0 :type (real 0 1))
  (g 0 :type (real 0 1))
  (b 0 :type (real 0 1)))

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

(defun scaled-vec (v n)
  "Returns a new VEC corresponding to V scaled by N"
  (with-slots (x y z) v
    (make-vec (* x n) (* y n) (* z n))))

(defun summed-vecs (&rest vs)
  "Returns a new VEC representing the sum of VS"
  (let ((s (make-vec 0 0 0)))
    (with-slots (x y z) s
      (dolist (v vs s)
        (incf x (vec-x v))
        (incf y (vec-y v))
        (incf z (vec-z v))))))

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
  (radius 0 :type real))

(defstruct hit-record
  (n 0 :type real)
  (point (make-point 0 0 0) :type point)
  (normal (make-vec 0 0 0) :type vec))

(defgeneric hit-test (ray object n-min &optional n-max)
  (:documentation "Tests OBJECT for a hit from RAY between N-MIN and N-MAX, returning a hit-record."))

(defmethod hit-test (ray (sphere sphere) n-min &optional n-max)
  (with-slots (centre radius) sphere
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
                   (make-hit-record :n root :point hit-point :normal normal))))
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

(defstruct camera
  (lower-left-corner (make-point 0 0 0) :type point)
  (horizontal (make-vec 0 0 0) :type vec)
  (vertical (make-vec 0 0 0) :type vec)
  (origin (make-point 0 0 0) :type point))

(defun get-ray (camera u v)
  (with-slots (lower-left-corner horizontal vertical origin) camera
    (make-ray :origin origin
              :direction (summed-vecs lower-left-corner
                                      (scaled-vec horizontal u)
                                      (scaled-vec vertical v)
                                      (point-difference (make-point 0 0 0) origin)))))

(defun ray-colour (r world)
  (flet ((lerp (start end n)
           "Linear interpolation of N between START and END"
           (+ (* (- 1 n) start) (* n end))))
    (let ((hit (hit-test r world 0.001 nil)))
      (if hit
          (let* ((target (offset-point (hit-record-point hit)
                                       (summed-vecs (hit-record-normal hit) (random-in-unit-sphere))))
                 (successor-ray (make-ray :origin (hit-record-point hit)
                                          :direction (point-difference target (hit-record-point hit)))))
            (blend-colours (make-colour 0 0 0)
                           (ray-colour successor-ray world)))
          (let* ((unit-direction (unit-vec (ray-direction r)))
                 (n (* 0.5 (1+ (vec-y unit-direction)))))
            (make-colour (lerp 1 0.5 n) (lerp 1 0.7 n) 1))))))

(defun test-image (nx ny)
  (let ((a (make-array (list ny nx) :element-type 'colour :initial-element (make-colour 0 0 0)))
        (camera (make-camera :lower-left-corner (make-point -2 -1 -1)
                             :horizontal (make-vec 4 0 0)
                             :vertical (make-vec 0 2 0)))
        (world (vector (make-sphere :centre (make-point 0 0 -1) :radius 0.5)
                       (make-sphere :centre (make-point 0 -100.5 -1) :radius 100))))
    (loop for j below ny do
      (loop for i below nx do
        (let (samples)
          (dotimes (s 100)
            (let* ((u (/ (+ i (random 1.0)) nx))
                   (v (/ (+ j (random 1.0)) ny))
                   (r (get-ray camera u v)))
              (push (ray-colour r world) samples)))
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
