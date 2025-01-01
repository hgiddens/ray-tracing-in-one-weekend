(in-package #:onna)

(defstruct (colour (:constructor make-colour (r g b)))
  (r 0 :type (real 0 1))
  (g 0 :type (real 0 1))
  (b 0 :type (real 0 1)))

(defun colour-8bit (c)
  (flet ((8bit (x) (floor (* x 255.99))))
    (list (8bit (colour-r c)) (8bit (colour-g c)) (8bit (colour-b c)))))

(defstruct (vec (:constructor make-vec (x y z)))
  (x 0 :type real)
  (y 0 :type real)
  (z 0 :type real))

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

(defun vec-length (v)
  (with-slots (x y z) v
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun unit-vec (v)
  "Returns a new VEC representing V scaled to a unit length"
  (with-slots (x y z) v
    (let ((l (vec-length v)))
      (make-vec (/ x l) (/ y l) (/ z l)))))

(defstruct (point (:constructor make-point (x y z)) (:include vec)))

(defun offset-point (p v)
  "Returns a new POINT offset from P by V"
  (with-slots (px py pz) p
    (with-slots (vx vy vz) v
      (make-point (+ px vx) (+ py vy) (+ pz vz)))))

(defstruct ray
  (origin (make-point 0 0 0) :type point)
  (direction (make-vec 0 0 0) :type vec))

(defun ray-colour (r)
  (flet ((lerp (start end n)
           (+ (* (- 1 n) start) (* n end))))
    (let* ((unit-direction (unit-vec (ray-direction r)))
           (n (* 0.5 (1+ (vec-y unit-direction)))))
      (make-colour (lerp 1 0.5 n) (lerp 1 0.7 n) 1))))

(defun test-image (nx ny)
  (let ((a (make-array (list ny nx) :element-type 'colour :initial-element (make-colour 0 0 0)))
        (lower-left-corner (make-vec -2 -1 -1))
        (horizontal (make-vec 4 0 0))
        (vertical (make-vec 0 2 0))
        (origin (make-point 0 0 0)))
    (loop for j below ny do
      (loop for i below nx do
        (let* ((u (/ i nx))
               (v (/ j ny))
               (r (make-ray :origin origin :direction (summed-vecs lower-left-corner
                                                                   (scaled-vec horizontal u)
                                                                   (scaled-vec vertical v)))))
          (setf (aref a j i) (ray-colour r)))))
    a))

(defun write-image (image)
  "Writes an image in PPM format to *STANDARD-OUT*."
  (destructuring-bind (ny nx) (array-dimensions image)
    (format t "P3~%~D ~D~%255~%" nx ny)
    (loop for j from (1- ny) downto 0 do
      (loop for i from 0 below nx do
        (destructuring-bind (ir ig ib) (colour-8bit (aref image j i))
          (format t "~D ~D ~D~%" ir ig ib))))))
