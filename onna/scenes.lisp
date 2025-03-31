(in-package :onna)

(defun two-spheres ()
  "A small sphere on a larger sphere."
  (vector
   (make-sphere :centre (make-point3 0 0 -1) :radius 0.5)
   (make-sphere :centre (make-point3 0 -100.5 -1) :radius 100)))

(defun four-spheres ()
  "The classic three spheres in a line on another, bigger sphere."
  (vector
   ;; Ground
   (make-sphere :centre (make-point3 0 -100.5 -1)
                :radius 100
                :material (make-lambertian :albedo (make-colour 0.8 0.8 0)))
   ;; Centre
   (make-sphere :centre (make-point3 0 0 -1.2)
                :radius 0.5
                :material (make-lambertian :albedo (make-colour 0.1 0.2 0.5)))
   ;; Left
   (make-sphere :centre (make-point3 -1 0 -1)
                :radius 0.5
                :material (make-dielectric :refraction-index 1.5d0))
   (make-sphere :centre (make-point3 -1 0 -1)
                :radius 0.4
                :material (make-dielectric :refraction-index (/ 1d0 1.5d0)))
   ;; Right
   (make-sphere :centre (make-point3 1 0 -1)
                :radius 0.5
                :material (make-metal :albedo (make-colour 0.8 0.6 0.2) :fuzz 1d0))))
