(in-package :onna)

;;;; I'm considering the camera to essentially be a transformation from
;;;; viewport coördinates to one or more rays. This means that in contrast to
;;;; the book, the camera itself doesn't know about e.g. anti-aliasing. I'll
;;;; change this later if I need to.
;;;;
;;;; Yeah looking at the start of book 1, chapter 7, we see:
;;;;
;;;;     The camera class will be responsible for two important jobs
;;;;     1. Construct and dispatch rays into the world.
;;;;     2. Use the results of these rays to construct the rendered image.
;;;;
;;;; I'm not exactly wholeheartedly commited to the single responsiblity
;;;; principle, but it seems to me like the class in the book is doing too
;;;; much.

(defstruct (camera (:constructor nil))
  (centre (make-point3 0 0 0) :type point3)
  (samples-per-pixel 1 :type (integer 0))
  (max-depth 0 :type (integer 0))
  (image-width 1 :type (integer 0))
  (image-height 1 :type (integer 0))
  ;; Pixel vectors.
  (pixel-delta-u (make-vec3 0 0 0) :type vec3)
  (pixel-delta-v (make-vec3 0 0 0) :type vec3)
  (pixel-0-0-loc (make-point3 0 0 0) :type point3) ; Centre.
  ;; Defocus disc horizontal and vertical radii.
  (defocus-disc-u (make-vec3 0 0 0) :type vec3)
  (defocus-disc-v (make-vec3 0 0 0) :type vec3))

(defun make-camera (&key
                      (aspect-ratio (/ 16 9))
                      (image-width 400)
                      (samples-per-pixel 10)
                      (max-depth 10)
                      (vertical-fov 90)
                      (look-from (make-point3 0 0 0))
                      (look-at (make-point3 0 0 -1))
                      (up (make-vec3 0 1 0))
                      ;; Variation angle of rays through each pixel.
                      (defocus-angle 0)
                      ;; Distance from look-from to the plane of perfect focus.
                      (focus-distance 10))
  (setf aspect-ratio (coerce aspect-ratio 'double-float)
        vertical-fov (coerce vertical-fov 'double-float)
        defocus-angle (coerce defocus-angle 'double-float)
        focus-distance (coerce focus-distance 'double-float))
  (flet ((degrees-to-radians (degrees)
           ;; pi is technically a long-float, although in SBCL long-float and
           ;; double-float aren't distinct types.
           (coerce (/ (* degrees pi) 180) 'double-float)))
    (let* ((camera (make-instance 'camera))
           (image-height (max (round image-width aspect-ratio) 1))
           (centre look-from)

           ;; Viewport dimensions.
           (theta (degrees-to-radians vertical-fov))
           (h (tan (/ theta 2d0)))
           (viewport-height (* 2d0 h focus-distance))
           (viewport-width (* viewport-height aspect-ratio))

           ;; Calculate the u, v, w basis vectors for the camera coördinate frame.
           (w (unit-vec3 (point3- look-from look-at)))
           (u (unit-vec3 (cross-product up w)))
           (v (cross-product w u))

           ;; Vectors across the horizontal and down the vertical edges.
           (viewport-u (scaled-vec3 u viewport-width))
           (viewport-v (scaled-vec3 v (- viewport-height)))

           ;; Horizontal and vertical vectors from pixel centre to pixel centre.
           (pixel-delta-u (scaled-vec3 viewport-u (coerce (/ image-width) 'double-float)))
           (pixel-delta-v (scaled-vec3 viewport-v (coerce (/ image-height) 'double-float)))

           ;; Upper left pixel location.
           (viewport-upper-left (let ((offset (vec3+ (scaled-vec3 w (- focus-distance))
                                                     (scaled-vec3 viewport-u (/ -2d0))
                                                     (scaled-vec3 viewport-v (/ -2d0)))))
                                  (point3+ centre offset)))
           (pixel-0-0-loc (point3+ viewport-upper-left
                                   (scaled-vec3 pixel-delta-u 0.5d0)
                                   (scaled-vec3 pixel-delta-v 0.5d0)))

           ;; Camera defocus disc basis vectors.
           (defocus-radius (* focus-distance (tan (degrees-to-radians (/ defocus-angle 2)))))
           (defocus-disc-u (scaled-vec3 u defocus-radius))
           (defocus-disc-v (scaled-vec3 v defocus-radius)))
      (setf (camera-centre camera) centre
            (camera-samples-per-pixel camera) samples-per-pixel
            (camera-max-depth camera) max-depth
            (camera-image-width camera) image-width
            (camera-image-height camera) image-height
            (camera-pixel-delta-u camera) pixel-delta-u
            (camera-pixel-delta-v camera) pixel-delta-v
            (camera-pixel-0-0-loc camera) pixel-0-0-loc
            (camera-defocus-disc-u camera) defocus-disc-u
            (camera-defocus-disc-v camera) defocus-disc-v)
      camera)))

(defun ray-colour (ray world depth)
  ;; If we've exceeded the ray bounce limit, no more light is gathered.
  (when (<= depth 0)
    (return-from ray-colour (make-colour 0 0 0)))
  (let ((interval (make-interval :min 0.001 ; to deal with shadow acne
                                 :max #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)))
    (alexandria:if-let ((hit (hit-test ray world interval)))
      (alexandria:if-let ((scattered (scatter ray hit)))
        (attenuate (ray-colour (scatter-record-scattered scattered) world (1- depth))
                   (scatter-record-attenuation scattered))
        (make-colour 0 0 0))
      (let* ((unit-direction (unit-vec3 (ray-direction ray)))
             (a (* 0.5 (1+ (vec3-y unit-direction)))))
        (make-colour (alexandria:lerp a 1 0.5)
                     (alexandria:lerp a 1 0.7)
                     1)))))

(defun defocus-disc-sample (camera)
  (loop for u-scale = (1- (random 2d0))
        for v-scale = (1- (random 2d0))
        until (< (+ (* u-scale u-scale) (* v-scale v-scale)) 1)
        finally (return (point3+
                         (camera-centre camera)
                         (scaled-vec3 (camera-defocus-disc-u camera) u-scale)
                         (scaled-vec3 (camera-defocus-disc-v camera) v-scale)))))

(defun get-ray (camera i j)
  "Constructs a ray from the defocus disc of CAMERA to a sampled point in pixel I, J."
  (flet ((sample-square ()
           "A vector to a random point in the [-.5,-.5]-[+.5,+.5] unit square"
           (make-vec3 (- (random 1d0) 0.5) (- (random 1d0) 0.5) 0)))
    (let* ((offset (sample-square))
           (pixel-sample (point3+ (camera-pixel-0-0-loc camera)
                                  (scaled-vec3 (camera-pixel-delta-u camera) (+ i (vec3-x offset)))
                                  (scaled-vec3 (camera-pixel-delta-v camera) (+ j (vec3-y offset)))))
           (origin (defocus-disc-sample camera)))
      (make-ray :origin origin :direction (point3- pixel-sample origin)))))

(defun render (camera world)
  (let* ((image (make-array (list (camera-image-height camera) (camera-image-width camera))
                            :element-type 'colour
                            :initial-element (make-colour 0 0 0))))
    (loop for j from 0 below (camera-image-height camera) do
      (loop for i from 0 below (camera-image-width camera) do
        (loop repeat (camera-samples-per-pixel camera)
              collecting (ray-colour (get-ray camera i j) world (camera-max-depth camera)) into samples
              finally (setf (aref image j i) (blend-colours samples)))))
    image))
