(in-package #:onna)

(defstruct (colour (:constructor make-colour (r g b)))
  (r 0 :type (real 0 1))
  (g 0 :type (real 0 1))
  (b 0 :type (real 0 1)))

(defun colour-8bit (c)
  "The `colour' C in 8-bit (r g b); applies no gamma."
  (flet ((8-bit (x) (floor (* x 255.999))))
    (with-slots (r g b) c
      (list (8-bit r) (8-bit g) (8-bit b)))))
