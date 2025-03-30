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
  (image-width 0 :type (integer 0))
  (image-height 0 :type (integer 0))
  (pixel-delta-u (make-vec3 0 0 0) :type vec3)
  (pixel-delta-v (make-vec3 0 0 0) :type vec3)
  (pixel-0-0-loc (make-point3 0 0 0) :type point3))

(defun make-camera (&key (aspect-ratio (/ 16 9)) (image-width 400))
  (setf aspect-ratio (coerce aspect-ratio 'double-float))
  (let* ((camera (make-instance 'camera))
         (image-height (max (round image-width aspect-ratio) 1))
         (centre (make-point3 0 0 0))

         ;; Viewport dimensions.
         (focal-length 1d0)
         (viewport-height 2d0)
         (viewport-width (* viewport-height aspect-ratio))

         ;; Vectors across the horizontal and down the vertical edges.
         (viewport-u (make-vec3 viewport-width 0 0))
         (viewport-v (make-vec3 0 (- viewport-height) 0))

         ;; Horizontal and vertical vectors from pixel centre to pixel centre.
         (pixel-delta-u (scaled-vec3 viewport-u (coerce (/ image-width) 'double-float)))
         (pixel-delta-v (scaled-vec3 viewport-v (coerce (/ image-height) 'double-float)))

         ;; Upper left pixel location.
         (viewport-upper-left (let ((offset (vec3+ (make-vec3 0 0 (- focal-length))
                                                   (scaled-vec3 viewport-u (/ -2d0))
                                                   (scaled-vec3 viewport-v (/ -2d0)))))
                                (point3+ centre offset)))
         (pixel-0-0-loc (point3+ viewport-upper-left
                                 (scaled-vec3 pixel-delta-u 0.5d0)
                                 (scaled-vec3 pixel-delta-v 0.5d0))))
    (setf (camera-centre camera) centre
          (camera-image-width camera) image-width
          (camera-image-height camera) image-height
          (camera-pixel-delta-u camera) pixel-delta-u
          (camera-pixel-delta-v camera) pixel-delta-v
          (camera-pixel-0-0-loc camera) pixel-0-0-loc)
    camera))

(defun ray-colour (ray world)
  (let ((interval (make-interval :min 0
                                 :max #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)))
    (alexandria:if-let ((hit (hit-test ray world interval)))
      (let ((normal (hit-record-normal hit)))
        (make-colour (* 0.5 (1+ (vec3-x normal)))
                     (* 0.5 (1+ (vec3-y normal)))
                     (* 0.5 (1+ (vec3-z normal)))))
      (let* ((unit-direction (unit-vec3 (ray-direction ray)))
             (a (* 0.5 (1+ (vec3-y unit-direction)))))
        (make-colour (alexandria:lerp a 1 0.5)
                     (alexandria:lerp a 1 0.7)
                     1)))))

(defun render (camera world)
  (let* ((image (make-array (list (camera-image-height camera) (camera-image-width camera))
                            :element-type 'colour
                            :initial-element (make-colour 0 0 0))))
    (loop for j from 0 below (camera-image-height camera) do
      (loop for i from 0 below (camera-image-width camera)
            as pixel-centre = (point3+ (camera-pixel-0-0-loc camera)
                                       (scaled-vec3 (camera-pixel-delta-u camera) (coerce i 'double-float))
                                       (scaled-vec3 (camera-pixel-delta-v camera) (coerce j 'double-float)))
            as ray = (make-ray :origin (camera-centre camera)
                               :direction (point3- pixel-centre (camera-centre camera)))
            do (setf (aref image j i) (ray-colour ray world))))
    image))
