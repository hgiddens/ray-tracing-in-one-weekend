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

(defun book-1-cover ()
  "Cover image of the first book."
  (flet ((random-material ()
           (let ((choose-material (random 1.0)))
             (cond
               ((< choose-material 0.8)
                (make-lambertian :albedo (make-colour (* (random 1.0) (random 1.0))
                                                      (* (random 1.0) (random 1.0))
                                                      (* (random 1.0) (random 1.0)))))
               ((< choose-material 0.95)
                (make-metal :albedo (make-colour (+ 0.5 (random 0.5))
                                                 (+ 0.5 (random 0.5))
                                                 (+ 0.5 (random 0.5)))
                            :fuzz (random 0.5d0)))
               (t
                (make-dielectric :refraction-index 1.5d0))))))
    (let (world)
      ;; Ground
      (push (make-sphere :centre (make-point3 0 -1000 0)
                         :radius 1000
                         :material (make-lambertian :albedo (make-colour 0.5 0.5 0.5)))
            world)

      ;; Thousands of luminous spheres
      (loop with exclusion-centre = (make-point3 4 0.2 0)
            for a from -11 below 11
            nconcing (loop for b from -11 below 11
                           as x = (+ a (random 0.9d0))
                           as z = (+ b (random 0.9d0))
                           as sphere-centre = (make-point3 x 0.2 z)
                           when (> (vec3-length (point3- sphere-centre exclusion-centre)) 0.9)
                             collect (make-sphere :centre sphere-centre
                                                  :radius 0.2
                                                  :material (random-material)))
              into little-spheres
            finally (alexandria:nconcf world little-spheres))

      ;; Big spheres
      (push (make-sphere :centre (make-point3 0 1 0)
                         :radius 1
                         :material (make-dielectric :refraction-index 1.5d0))
            world)
      (push (make-sphere :centre (make-point3 -4 1 0)
                         :radius 1
                         :material (make-lambertian :albedo (make-colour 0.4 0.2 0.1)))
            world)
      (push (make-sphere :centre (make-point3 4 1 0)
                         :radius 1
                         :material (make-metal :albedo (make-colour 0.7 0.6 0.5)))
            world)

      (coerce world 'vector))))
