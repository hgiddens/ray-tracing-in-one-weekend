(in-package #:onna)

(defstruct (colour (:constructor make-colour (r g b)))
  (r 0 :type (real 0 1))
  (g 0 :type (real 0 1))
  (b 0 :type (real 0 1)))

(defun colour-8bit (c)
  "The `colour' C in 8-bit (r g b); applies no gamma."
  (flet ((8-bit (x)
           (coerce (floor (* 256 (alexandria:clamp x 0 0.999))) '(unsigned-byte 8))))
    (with-slots (r g b) c
      (list (8-bit r) (8-bit g) (8-bit b)))))

(defun blend-colours (cs)
  "Combines multiple colour samples into a single colour."
  (when cs
    (let ((r 0) (g 0) (b 0) (l 0))
      (dolist (c cs)
        (incf r (colour-r c))
        (incf g (colour-g c))
        (incf b (colour-b c))
        (incf l))
      (make-colour (/ r l) (/ g l) (/ b l)))))

(defun gamma-2 (c)
  ;; For gamma X, the X describes the exponent you use when going from gamma
  ;; to linear speace. This function does the opposite, so we just take the
  ;; square root of al the components.
  (with-slots (r g b) c
    (make-colour (sqrt r) (sqrt g) (sqrt b))))

(defun attenuate (c d)
  (make-colour (* (colour-r c) (colour-r d))
               (* (colour-g c) (colour-g d))
               (* (colour-b c) (colour-b d))))
