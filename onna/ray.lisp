(in-package :onna)

(defstruct ray
  (origin (make-point3 0 0 0) :type point3)
  (direction (make-vec3 0 0 0) :type vec3)
  ;; See the notes in book 2 section 2.2 about how to really do this.
  (time 0d0 :type double-float))

(defun point-at-time (ray time)
  "The point along RAY at some time TIME (`t' in the book)."
  (point3+ (ray-origin ray) (scaled-vec3 (ray-direction ray) time)))
