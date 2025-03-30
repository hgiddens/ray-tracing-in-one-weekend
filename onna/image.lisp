(in-package #:onna)

;;; I'm just going to represent images as 2D byte arrays of colour
;;; structs. Arrays are implemented in row-major order, so the most rapidly
;;; component should have the last index; because these are images, that's x.

(defun write-ppm (image &optional (stream *standard-output*))
  "Writes IMAGE to STREAM in PPM format."
  (destructuring-bind (image-height image-width) (array-dimensions image)
    (format stream "P3~%~D ~D ~%255~%" image-width image-height)
    (loop for j from 0 below image-height do
      (loop for i from 0 below image-width do
        (apply #'format stream "~D ~D ~D~%" (colour-8bit (aref image j i)))))))
