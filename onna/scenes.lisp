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
    (vector
     (make-quad :q (make-point3 555 0 0) :u (make-vec3 0 555 0) :v (make-vec3 0 0 555) :material green)
     (make-quad :q (make-point3 0 0 0) :u (make-vec3 0 555 0) :v (make-vec3 0 0 555) :material red)
     (make-quad :q (make-point3 343 554 332) :u (make-vec3 -130 0 0) :v (make-vec3 0 0 -105) :material light)
     (make-quad :q (make-point3 0 0 0) :u (make-vec3 555 0 0) :v (make-vec3 0 0 555) :material white)
     (make-quad :q (make-point3 555 555 555) :u (make-vec3 -555 0 0) :v (make-vec3 0 0 -555) :material white)
     (make-quad :q (make-point3 0 0 555) :u (make-vec3 555 0 0) :v (make-vec3 0 555 0) :material white))))
