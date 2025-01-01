(in-package #:onna)

(defstruct (colour (:constructor make-colour (r g b)))
  (r 0 :type (real 0 1))
  (g 0 :type (real 0 1))
  (b 0 :type (real 0 1)))

(defun colour-8bit (c)
  (flet ((8bit (x) (floor (* x 255.99))))
    (list (8bit (colour-r c)) (8bit (colour-g c)) (8bit (colour-b c)))))

(defun test-image (nx ny)
  (let ((a (make-array (list ny nx) :element-type 'colour :initial-element (make-colour 0 0 0))))
    (loop for j below ny do
      (loop for i below nx do
        (let ((r (/ i nx))
              (g (/ j ny))
              (b 0.2))
        (setf (aref a j i) (make-colour r g b)))))
    a))

(defun write-image (image)
  "Writes an image in PPM format to *STANDARD-OUT*."
  (destructuring-bind (ny nx) (array-dimensions image)
    (format t "P3~%~D ~D~%255~%" nx ny)
    (loop for j from (1- ny) downto 0 do
      (loop for i from 0 below nx do
        (destructuring-bind (ir ig ib) (colour-8bit (aref image j i))
          (format t "~D ~D ~D~%" ir ig ib))))))
