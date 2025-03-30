(in-package #:onna)

(defun example ()
  "Example function rendering whatever I'm working on."
  ;; Not adding a progress bar, because it's not clear to me how I can do that
  ;; well with this being run primarily from Slime.
  (let* ((camera (make-camera :samples-per-pixel 100
                              :max-depth 50))
         (world (four-spheres))
         (image (time (render camera world))))
    (with-open-file (stream #P"~/Desktop/test.ppm"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-ppm image stream))))
