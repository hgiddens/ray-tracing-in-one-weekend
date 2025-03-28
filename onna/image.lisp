(in-package #:onna)

;;; I'm just going to represent images as 2D byte arrays of colour
;;; structs. Arrays are implemented in row-major order, so the most rapidly
;;; component should have the last index; because these are images, that's x.

(defun test-image (image-width image-height)
  "Simple colour gradient."
  (let ((image (make-array (list image-height image-width)
                           :element-type 'colour
                           :initial-element (make-colour 0 0 0))))
    (loop for j from 0 below image-height do
      (loop for i from 0 below image-width do
        (setf (aref image j i) (make-colour (/ i (1- image-width))
                                            (/ j (1- image-height))
                                            0))))
    image))

(defun write-ppm (image &optional (stream *standard-output*))
  "Writes IMAGE to STREAM in PPM format."
  (destructuring-bind (image-height image-width) (array-dimensions image)
    (format stream "P3~%~D ~D ~%255~%" image-width image-height)
    (loop for j from 0 below image-height do
      (loop for i from 0 below image-width do
        (apply #'format stream "~D ~D ~D~%" (colour-8bit (aref image j i)))))))
