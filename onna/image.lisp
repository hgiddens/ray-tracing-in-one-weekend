(in-package #:onna)

;;; I'm just going to represent images as 2D byte arrays of colour
;;; structs. Arrays are implemented in row-major order, so the most rapidly
;;; component should have the last index; because these are images, that's x.

(defun viewport-coordinate-mapper (camera image)
  "A viewport coördinate mapper for IMAGE in the viewport of CAMERA.

Returns a function taking two arguments, a pixel x and y, and which returns
viewport coördinates for the pixel. Viewport coördinates for a given pixel are
not necessarily unique due to e.g. antialiasing."
  (destructuring-bind (image-height image-width) (array-dimensions image)
    (let* ((pixel-delta-u (scaled-vec3 (camera-viewport-u camera)
                                       (coerce (/ image-width) 'double-float)))
           (pixel-delta-v (scaled-vec3 (camera-viewport-v camera)
                                       (coerce (/ image-height) 'double-float)))
           (pixel-0-0-loc (point+ (camera-viewport-upper-left camera)
                                  (scaled-vec3 pixel-delta-u 0.5d0)
                                  (scaled-vec3 pixel-delta-v 0.5d0))))
      (lambda (i j)
        (point+ pixel-0-0-loc
                (scaled-vec3 pixel-delta-u (coerce i 'double-float))
                (scaled-vec3 pixel-delta-v (coerce j 'double-float)))))))

(defun test-image (image-width image-height)
  "Simple colour gradient."
  (flet ((ray-colour (ray)
           (let* ((unit-direction (unit-vec3 (ray-direction ray)))
                  (a (* 0.5 (1+ (vec3-y unit-direction)))))
             (make-colour (alexandria:lerp a 1 0.5)
                          (alexandria:lerp a 1 0.7)
                          1))))
    (let* ((camera (make-camera (/ image-width image-height)))
           (image (make-array (list image-height image-width)
                              :element-type 'colour
                              :initial-element (make-colour 0 0 0)))
           (vc-mapper (viewport-coordinate-mapper camera image)))
      (loop for j from 0 below image-height do
        (loop for i from 0 below image-width do
          (setf (aref image j i)
                (ray-colour (get-ray camera (funcall vc-mapper i j))))))
      image)))

(defun write-ppm (image &optional (stream *standard-output*))
  "Writes IMAGE to STREAM in PPM format."
  (destructuring-bind (image-height image-width) (array-dimensions image)
    (format stream "P3~%~D ~D ~%255~%" image-width image-height)
    (loop for j from 0 below image-height do
      (loop for i from 0 below image-width do
        (apply #'format stream "~D ~D ~D~%" (colour-8bit (aref image j i)))))))
