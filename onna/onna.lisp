(in-package #:onna)

(defun example ()
  "Example function rendering whatever I'm working on."
  ;; Not adding a progress bar, because it's not clear to me how I can do that
  ;; well with this being run primarily from Slime.
  (let* ((camera (make-camera :aspect-ratio 1
                              :samples-per-pixel 200
                              :image-width 600
                              :max-depth 50
                              :background-colour (make-colour 0 0 0)
                              :vertical-fov 40
                              :look-from (make-point3 278 278 -800)
                              :look-at (make-point3 278 278 0)
                              :defocus-angle 0
                              :focus-distance 10))
         (world (cornell-smoke))
         (image (time (render camera world))))
    (with-open-file (stream #P"~/Desktop/test.png"
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-png image stream))))
