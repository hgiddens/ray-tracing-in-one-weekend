(in-package :onna)

(defstruct (camera (:constructor nil))
  (centre (make-point3 0 0 0) :type point3)
  (samples-per-pixel 1 :type (integer 0))
  (reciprocal-root-samples-per-pixel 1 :type (real 0 1))
  (max-depth 0 :type (integer 0))
  (background-colour (make-colour 0 0 0) :type colour)
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
                      (samples-per-pixel 9)
                      (max-depth 10)
                      (background-colour (make-colour 0.7 0.8 1))
                      (vertical-fov 90)
                      (look-from (make-point3 0 0 0))
                      (look-at (make-point3 0 0 -1))
                      (up (make-vec3 0 1 0))
                      ;; Variation angle of rays through each pixel.
                      (defocus-angle 0)
                      ;; Distance from look-from to the plane of perfect focus.
                      (focus-distance 10))
  (setf aspect-ratio (coerce aspect-ratio 'double-float)
        samples-per-pixel (expt (floor (sqrt samples-per-pixel)) 2)
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
            (camera-reciprocal-root-samples-per-pixel camera) (/ (floor (sqrt samples-per-pixel)))
            (camera-max-depth camera) max-depth
            (camera-background-colour camera) background-colour
            (camera-image-width camera) image-width
            (camera-image-height camera) image-height
            (camera-pixel-delta-u camera) pixel-delta-u
            (camera-pixel-delta-v camera) pixel-delta-v
            (camera-pixel-0-0-loc camera) pixel-0-0-loc
            (camera-defocus-disc-u camera) defocus-disc-u
            (camera-defocus-disc-v camera) defocus-disc-v)
      camera)))

(defconstant +shadow-acne-minimum-threshold+ 0.001)

(defun ray-colour (ray world lights depth background-colour)
  (labels ((next (r)
             (ray-colour r world lights (1- depth) background-colour))
           (handle-ray ()
             (let ((interval (make-interval +shadow-acne-minimum-threshold+
                                            #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)))
               (alexandria:if-let ((hit (hit-test ray world interval)))
                 (handle-hit hit)
                 background-colour)))
           (handle-hit (hit)
             (let ((colour-from-emission (emitted ray hit)))
               (alexandria:if-let ((scattered (scatter ray hit)))
                 (handle-scatter hit scattered colour-from-emission)
                 colour-from-emission)))
           (handle-scatter (hit scattered colour-from-emission)
             (if (null (scatter-record-pdf scattered))
                 (attenuate (next (scatter-record-skip-pdf-ray scattered))
                            (scatter-record-attenuation scattered))
                 (combine-colours colour-from-emission
                                  (let ((pdf (make-mixture-pdf
                                              (make-hittable-pdf :objects lights
                                                                 :origin (hit-record-point hit))
                                              (scatter-record-pdf scattered))))
                                    (colour-from-scatter hit scattered pdf)))))
           (colour-from-scatter (hit scattered pdf)
             (multiple-value-bind (direction pdf-value) (pdf-direction pdf)
               (let* ((scattered-ray (make-ray :origin (hit-record-point hit)
                                               :direction direction
                                               :time (ray-time ray)))
                      (scattering-pdf (scattering-pdf (hit-record-material hit)
                                                      ray
                                                      hit
                                                      scattered-ray)))
                 (attenuate (next scattered-ray)
                            (scatter-record-attenuation scattered)
                            (let ((c (/ scattering-pdf pdf-value)))
                              (make-colour c c c)))))))
    (if (plusp depth)
        (handle-ray)
        ;; Ray recursion limit exceeded, no more light is generated.
        (make-colour 0 0 0))))

(defun defocus-disc-sample (camera)
  (loop for u-scale = (1- (random 2d0))
        for v-scale = (1- (random 2d0))
        until (< (+ (* u-scale u-scale) (* v-scale v-scale)) 1)
        finally (return (point3+
                         (camera-centre camera)
                         (scaled-vec3 (camera-defocus-disc-u camera) u-scale)
                         (scaled-vec3 (camera-defocus-disc-v camera) v-scale)))))

(defun get-ray (camera i j si sj)
  "A ray from the defocus disc of CAMERA to a sampled point in pixel I, J for sample square SI, SJ."
  (flet ((sample-square-stratified ()
           "A vector to a random point in the square sub-pixel.

Square is specified by grid indices SI and SJ for an idealized unit square
pixel [-.5,-.5] to [+.5,+.5]."
           (make-vec3
            (- (* (+ si (random 1d0)) (camera-reciprocal-root-samples-per-pixel camera)) 0.5)
            (- (* (+ sj (random 1d0)) (camera-reciprocal-root-samples-per-pixel camera)) 0.5)
            0)))
    (let* ((offset (sample-square-stratified))
           (pixel-sample (point3+ (camera-pixel-0-0-loc camera)
                                  (scaled-vec3 (camera-pixel-delta-u camera) (+ i (vec3-x offset)))
                                  (scaled-vec3 (camera-pixel-delta-v camera) (+ j (vec3-y offset)))))
           (origin (defocus-disc-sample camera)))
      (make-ray :origin origin
                :direction (point3- pixel-sample origin)
                :time (random 1d0)))))

(defun render (camera world lights)
  (let* ((image (make-array (list (camera-image-height camera) (camera-image-width camera))
                            :element-type 'colour
                            :initial-element (make-colour 0 0 0)))
         (sqrt-spp (floor (sqrt (camera-samples-per-pixel camera)))))
    (loop for j from 0 below (camera-image-height camera) do
      (loop for i from 0 below (camera-image-width camera) do
        (loop for sj below sqrt-spp
              nconcing (loop for si below sqrt-spp
                             collect (ray-colour (get-ray camera i j si sj)
                                                 world
                                                 lights
                                                 (camera-max-depth camera)
                                                 (camera-background-colour camera)))
                into samples
              finally (setf (aref image j i) (blend-colours samples)))))
    image))
