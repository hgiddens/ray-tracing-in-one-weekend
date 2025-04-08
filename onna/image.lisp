(in-package #:onna)

;;; I'm just going to represent images as 2D byte arrays of colour
;;; structs. Arrays are implemented in row-major order, so the most rapidly
;;; component should have the last index; because these are images, that's x.
;;;
;;; TODO: Define, use an image type or similar; I can copy how this is done
;;; from the PNG library.

(defun read-png (stream)
  "Reads an image from STREAM."
  (flet ((adjust-channel (x bit-depth)
           ;; TODO: Look for, use gamma information in the file.
           (expt (/ x (expt 2 bit-depth)) 2)))
    ;; We ignore that the PNG might have a 16 bit channel depth, because we
    ;; convert everything to colour instances anyway, which are reals.
    ;; TODO: Which is lossy, which maybe I care about one day?
    (loop with png = (png:decode stream)
          with depth = (png:image-bit-depth png)
          with image = (make-array (list (png:image-height png)
                                         (png:image-width png))
                                   :element-type 'colour
                                   :initial-element (make-colour 0 0 0))
          for j from 0 below (png:image-height png)
          do (loop for i from 0 below (png:image-width png)
                   as r = (aref png j i 0)
                   as g = (aref png j i 1)
                   as b = (aref png j i 2)
                   as colour = (make-colour (adjust-channel r depth)
                                            (adjust-channel g depth)
                                            (adjust-channel b depth))
                   do (setf (aref image j i) colour))
          finally (return image))))

(defun write-ppm (image &optional (stream *standard-output*))
  "Writes IMAGE to STREAM in PPM format."
  (destructuring-bind (image-height image-width) (array-dimensions image)
    (format stream "P3~%~D ~D ~%255~%" image-width image-height)
    (loop for j from 0 below image-height do
      (loop for i from 0 below image-width do
        (format stream "~{~D ~D ~D~%~}" (colour-8bit (gamma-2 (aref image j i))))))))

(defun write-png (image stream)
  (destructuring-bind (image-height image-width) (array-dimensions image)
    (let ((png (png:make-image image-height image-width 3)))
      (loop for j from 0 below image-height do
        (loop for i from 0 below image-width
              as (r g b) = (colour-8bit (gamma-2 (aref image j i)))
              do (setf (aref png j i 0) r
                       (aref png j i 1) g
                       (aref png j i 2) b)))
      (png:encode png stream))))
