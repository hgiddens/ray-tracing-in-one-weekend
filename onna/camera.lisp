(in-package :onna)

;;;; I'm considering the camera to essentially be a transformation from
;;;; viewport coördinates to one or more rays. This means that in contrast to
;;;; the book, the camera itself doesn't know about e.g. anti-aliasing. I'll
;;;; change this later if I need to.

(defstruct (camera (:constructor nil))
  (centre (make-point 0 0 0) :type point)
  (viewport-upper-left (make-point 0 0 0) :type point)
  ;; The vectors across the horizontal and down the vertical edges.
  (viewport-u (make-vec3 0 0 0) :type vec3)
  (viewport-v (make-vec3 0 0 0) :type vec3))

(defun make-camera (aspect-ratio)
  (setf aspect-ratio (coerce aspect-ratio 'double-float))
  (let* ((camera (make-instance 'camera))
         (focal-length 1d0)
         (viewport-height 2d0)
         (viewport-width (* viewport-height aspect-ratio)))
    (with-slots (centre viewport-u viewport-v viewport-upper-left) camera
      (setf centre (make-point 0 0 0)
            viewport-u (make-vec3 viewport-width 0 0)
            viewport-v (make-vec3 0 (- viewport-height) 0)
            viewport-upper-left (let ((offset (vec3+ (make-vec3 0 0 (- focal-length))
                                                     (scaled-vec3 viewport-u (/ -2d0))
                                                     (scaled-vec3 viewport-v (/ -2d0)))))
                                  (point+ centre offset))))
    camera))

(defun get-ray (camera vc)
  (make-ray :origin (camera-centre camera)
            :direction (point- vc (camera-centre camera))))
