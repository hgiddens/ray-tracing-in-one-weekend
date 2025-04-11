(in-package #:onna)

(defun example ()
  "Example function rendering whatever I'm working on."
  ;; Not adding a progress bar, because it's not clear to me how I can do that
  ;; well with this being run primarily from Slime.
  (let* ((camera (make-camera :samples-per-pixel 10
                              :image-width 400
                              :max-depth 50
                              :vertical-fov 80
                              :look-from (make-point3 0 0 9)
                              :look-at (make-point3 0 0 0)
                              :defocus-angle 0
                              :focus-distance 10))
         (world (quads))
         (image (time (render camera world))))
    (with-open-file (stream #P"~/Desktop/test.png"
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-png image stream))))
