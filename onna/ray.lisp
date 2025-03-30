(in-package :onna)

(defstruct ray
  (origin (make-point3 0 0 0) :type point3)
  (direction (make-vec3 0 0 0) :type vec3))

(defun point-at-time (ray time)
  "The point along RAY at some time TIME (`t' in the book)."
  (point3+ (ray-origin ray) (scaled-vec3 (ray-direction ray) time)))
