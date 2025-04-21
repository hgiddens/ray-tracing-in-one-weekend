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
                :material (make-lambertian :texture (make-colour 0.8 0.8 0)))
   ;; Centre
   (make-sphere :centre (make-point3 0 0 -1.2)
                :radius 0.5
                :material (make-lambertian :texture (make-colour 0.1 0.2 0.5)))
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

(defun bouncing-spheres ()
  "Cover image of the first book."
  (flet ((random-sphere (sphere-centre)
           (let ((choose-material (random 1.0)))
             (cond
               ((< choose-material 0.8)
                (let ((offset (make-vec3 0 (random 0.5d0) 0)))
                  (make-sphere :from sphere-centre
                               :to (point3+ sphere-centre offset)
                               :radius 0.2
                               :material (make-lambertian
                                          :texture (make-colour (* (random 1.0) (random 1.0))
                                                                (* (random 1.0) (random 1.0))
                                                                (* (random 1.0) (random 1.0)))))))
               ((< choose-material 0.95)
                (make-sphere :centre sphere-centre
                             :radius 0.2
                             :material (make-metal
                                        :albedo (make-colour (+ 0.5 (random 0.5))
                                                             (+ 0.5 (random 0.5))
                                                             (+ 0.5 (random 0.5)))
                                        :fuzz (random 0.5d0))))
               (t
                (make-sphere :centre sphere-centre
                             :radius 0.2
                             :material (make-dielectric :refraction-index 1.5d0)))))))
    (let (world)
      ;; Ground
      (push (let* ((even (make-colour 0.2 0.3 0.1))
                   (odd (make-colour 0.9 0.9 0.9))
                   (chequer (make-chequer :scale 0.32 :even even :odd odd)))
              (make-sphere :centre (make-point3 0 -1000 0)
                    :radius 1000
                    :material (make-lambertian :texture chequer)))
            world)

      ;; Thousands of luminous spheres
      (loop with exclusion-centre = (make-point3 4 0.2 0)
            for a from -11 below 11
            nconcing (loop for b from -11 below 11
                           as x = (+ a (random 0.9d0))
                           as z = (+ b (random 0.9d0))
                           as sphere-centre = (make-point3 x 0.2 z)
                           when (> (vec3-length (point3- sphere-centre exclusion-centre)) 0.9)
                             collect (random-sphere sphere-centre))
              into little-spheres
            finally (alexandria:nconcf world little-spheres))

      ;; Big spheres
      (push (make-sphere :centre (make-point3 0 1 0)
                         :radius 1
                         :material (make-dielectric :refraction-index 1.5d0))
            world)
      (push (make-sphere :centre (make-point3 -4 1 0)
                         :radius 1
                         :material (make-lambertian :texture (make-colour 0.4 0.2 0.1)))
            world)
      (push (make-sphere :centre (make-point3 4 1 0)
                         :radius 1
                         :material (make-metal :albedo (make-colour 0.7 0.6 0.5)))
            world)

      (make-bvh-node (coerce world 'vector)))))

(defun chequered-spheres ()
  "Book 2 chapter 4.3."
  (let ((texture (make-chequer :scale 0.32
                               :even (make-colour 0.2 0.3 0.1)
                               :odd (make-colour 0.9 0.9 0.9))))
    (vector
     (make-sphere :centre (make-point3 0 -10 0)
                  :radius 10
                  :material (make-lambertian :texture texture))
     (make-sphere :centre (make-point3 0 10 0)
                  :radius 10
                  :material (make-lambertian :texture texture)))))

(defun earth ()
  "Book 2 chapter 4.6."
  (let* ((image (with-open-file (stream #P"earthmap.png" :element-type '(unsigned-byte 8))
                  (read-png stream)))
         (texture (make-image-texture :image image)))
    (make-sphere :centre (make-point3 0 0 0) :radius 2 :material (make-lambertian :texture texture))))

(defun perlin-spheres ()
  "Book 2 chapter 5.1."
  (let ((perlin-material (make-lambertian :texture (make-perlin :scale 4d0))))
    (vector
     (make-sphere :centre (make-point3 0 -1000 0) :radius 1000 :material perlin-material)
     (make-sphere :centre (make-point3 0 2 0) :radius 2 :material perlin-material))))

(defun quads ()
  "Book 2 chapter 6.6."
  (vector
   (make-quad :q (make-point3 -3 -2 5)
              :u (make-vec3 0 0 -4)
              :v (make-vec3 0 4 0)
              :material (make-lambertian :texture (make-colour 1 0.2 0.2)))
   (make-quad :q (make-point3 -2 -2 0)
              :u (make-vec3 4 0 0)
              :v (make-vec3 0 4 0)
              :material (make-lambertian :texture (make-colour 0.2 1 0.2)))
   (make-quad :q (make-point3 3 -2 1)
              :u (make-vec3 0 0 4)
              :v (make-vec3 0 4 0)
              :material (make-lambertian :texture (make-colour 0.2 0.2 1)))
   (make-quad :q (make-point3 -2 3 1)
              :u (make-vec3 4 0 0)
              :v (make-vec3 0 0 4)
              :material (make-lambertian :texture (make-colour 1 0.5 0)))
   (make-quad :q (make-point3 -2 -3 5)
              :u (make-vec3 4 0 0)
              :v (make-vec3 0 0 -4)
              :material (make-lambertian :texture (make-colour 0.2 0.8 0.8)))))

(defun simple-light ()
  "Book 2 chapter 7.3."
  (let ((perlin-material (make-lambertian :texture (make-perlin :scale 4d0)))
        (light-material (make-diffuse-light :texture (make-colour 4 4 4))))
    (vector
     (make-sphere :centre (make-point3 0 -1000 0) :radius 1000 :material perlin-material)
     (make-sphere :centre (make-point3 0 2 0) :radius 2 :material perlin-material)
     (make-sphere :centre (make-point3 0 7 0) :radius 2 :material light-material)
     (make-quad :q (make-point3 3 1 -2) :u (make-vec3 2 0 0) :v (make-vec3 0 2 0) :material light-material))))

(defun cornell-box ()
  "Book 2 chapter 7.4."
  (let ((red (make-lambertian :texture (make-colour 0.65 0.05 0.05)))
        (white (make-lambertian :texture (make-colour 0.73 0.73 0.73)))
        (green (make-lambertian :texture (make-colour 0.12 0.45 0.15)))
        (light (make-diffuse-light :texture (make-colour 15 15 15))))
    (values
     ;; World
     (vector
      (make-quad :q (make-point3 555 0 0) :u (make-vec3 0 555 0) :v (make-vec3 0 0 555) :material green)
      (make-quad :q (make-point3 0 0 0) :u (make-vec3 0 555 0) :v (make-vec3 0 0 555) :material red)
      (make-quad :q (make-point3 343 554 332) :u (make-vec3 -130 0 0) :v (make-vec3 0 0 -105) :material light)
      (make-quad :q (make-point3 0 0 0) :u (make-vec3 555 0 0) :v (make-vec3 0 0 555) :material white)
      (make-quad :q (make-point3 555 555 555) :u (make-vec3 -555 0 0) :v (make-vec3 0 0 -555) :material white)
      (make-quad :q (make-point3 0 0 555) :u (make-vec3 555 0 0) :v (make-vec3 0 555 0) :material white)
      (let ((box (make-box :a (make-point3 0 0 0) :b (make-point3 165 330 165) :material white)))
        (make-translate :object (make-rotate-y :object box :angle 15)
                        :offset (make-vec3 265 0 295)))
      (make-sphere :centre (make-point3 190 90 190)
                   :radius 90
                   :material (make-dielectric :refraction-index 1.5d0)))

     ;; Lights
     (vector
      (make-quad :q (make-point3 343 554 332) :u (make-vec3 -130 0 0) :v (make-vec3 0 0 -105))
      (make-sphere :centre (make-point3 190 90 190) :radius 90)))))

(defun cornell-smoke ()
  "Book 2 chapter 9.2."
  (let ((red (make-lambertian :texture (make-colour 0.65 0.05 0.05)))
        (white (make-lambertian :texture (make-colour 0.73 0.73 0.73)))
        (green (make-lambertian :texture (make-colour 0.12 0.45 0.15)))
        (light (make-diffuse-light :texture (make-colour 7 7 7))))
    (vector
     (make-quad :q (make-point3 555 0 0) :u (make-vec3 0 555 0) :v (make-vec3 0 0 555) :material green)
     (make-quad :q (make-point3 0 0 0) :u (make-vec3 0 555 0) :v (make-vec3 0 0 555) :material red)
     (make-quad :q (make-point3 113 554 127) :u (make-vec3 330 0 0) :v (make-vec3 0 0 305) :material light)
     (make-quad :q (make-point3 0 0 0) :u (make-vec3 555 0 0) :v (make-vec3 0 0 555) :material white)
     (make-quad :q (make-point3 555 555 555) :u (make-vec3 -555 0 0) :v (make-vec3 0 0 -555) :material white)
     (make-quad :q (make-point3 0 0 555) :u (make-vec3 555 0 0) :v (make-vec3 0 555 0) :material white)
     (let ((box (make-box :a (make-point3 0 0 0) :b (make-point3 165 330 165) :material white)))
       (setf box (make-rotate-y :object box :angle 15)
             box (make-translate :object box :offset (make-vec3 265 0 295))
             box (make-constant-medium :boundary box :density 0.01d0 :texture (make-colour 0 0 0)))
       box)
     (let ((box (make-box :a (make-point3 0 0 0) :b (make-point3 165 165 165) :material white)))
       (setf box (make-rotate-y :object box :angle -18)
             box (make-translate :object box :offset (make-vec3 130 0 65))
             box (make-constant-medium :boundary box :density 0.01d0 :texture (make-colour 1 1 1)))
       box))))

(defun final-scene ()
  "Book 2 chapter 10."
  (flet ((ground-boxes ()
           (loop with ground = (make-lambertian :texture (make-colour 0.48 0.83 0.53))
                 and boxes-per-side = 20
                 for i below boxes-per-side
                 nconcing (loop for j below boxes-per-side
                                as w = 100
                                as x0 = (+ -1000 (* i w))
                                as y0 = 0
                                as z0 = (+ -1000 (* j w))
                                as x1 = (+ x0 w)
                                as y1 = (1+ (random 100d0))
                                as z1 = (+ z0 w)
                                collect (make-box :a (make-point3 x0 y0 z0)
                                                  :b (make-point3 x1 y1 z1)
                                                  :material ground))
                   into boxes
                 finally (return (coerce boxes 'vector))))
         (sky-boxes ()
           (loop with white = (make-lambertian :texture (make-colour 0.73 0.73 0.73))
                 for j below 1000
                 as x = (random 165d0)
                 as y = (random 165d0)
                 as z = (random 165d0)
                 collecting (make-sphere :centre (make-point3 x y z) :radius 10 :material white)
                   into boxes
                 finally
                    (let ((objects (coerce boxes 'vector)))
                      (setf objects (make-bvh-node objects)
                            objects (make-rotate-y :object objects :angle 15)
                            objects (make-translate :object objects :offset (make-vec3 -100 270 395)))
                      (return objects)))))
    (let (world)
      (push (make-bvh-node (ground-boxes)) world)

      (let ((light (make-diffuse-light :texture (make-colour 7 7 7))))
        (push (make-quad :q (make-point3 123 554 147)
                         :u (make-vec3 300 0 0)
                         :v (make-vec3 0 0 265)
                         :material light)
              world))

      (let* ((from (make-point3 400 400 200))
             (to (point3+ from (make-vec3 30 0 0)))
             (material (make-lambertian :texture (make-colour 0.7 0.3 0.1))))
        (push (make-sphere :from from :to to :radius 50 :material material) world))

      (push (make-sphere :centre (make-point3 260 150 45)
                         :radius 50
                         :material (make-dielectric :refraction-index 1.5d0))
            world)
      (push (make-sphere :centre (make-point3 0 150 145)
                         :radius 50
                         :material (make-metal :albedo (make-colour 0.8 0.8 0.9) :fuzz 1d0))
            world)

      (let ((boundary (make-sphere :centre (make-point3 360 150 145)
                                   :radius 70
                                   :material (make-dielectric :refraction-index 1.5d0))))
        (push boundary world)
        (push (make-constant-medium :boundary boundary
                                    :density 0.2
                                    :texture (make-colour 0.2 0.4 0.9))
              world))
      (let ((boundary (make-sphere :centre (make-point3 0 0 0)
                                   :radius 5000
                                   :material (make-dielectric :refraction-index 1.5d0))))
        (push (make-constant-medium :boundary boundary
                                    :density 0.0001
                                    :texture (make-colour 1 1 1))
              world))

      (let* ((image (with-open-file (stream #P"earthmap.png" :element-type '(unsigned-byte 8))
                      (read-png stream)))
             (emat (make-lambertian :texture (make-image-texture :image image))))
        (push (make-sphere :centre (make-point3 400 200 400) :radius 100 :material emat)
              world))
      (let ((pertext (make-perlin :scale 0.2d0)))
        (push (make-sphere :centre (make-point3 220 280 300)
                           :radius 80
                           :material (make-lambertian :texture pertext))
              world))

      (push (sky-boxes) world)

      (coerce world 'vector))))
