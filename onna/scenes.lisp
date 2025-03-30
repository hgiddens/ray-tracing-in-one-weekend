(in-package :onna)

(defun two-spheres ()
  "A small sphere on a larger sphere."
  (vector
   (make-sphere :centre (make-point3 0 0 -1) :radius 0.5)
   (make-sphere :centre (make-point3 0 -100.5 -1) :radius 100)))
