(in-package :onna)

;;;; TODO: document what the fuck this thing is

(defstruct (onb (:constructor make-onb
                  (n &aux
                       (w (unit-vec3 n))
                       (a (if (> (abs (vec3-x w)) 0.9)
                              (make-vec3 0 1 0)
                              (make-vec3 1 0 0)))
                       (v (unit-vec3 (cross-product w a)))
                       (u (unit-vec3 (cross-product w v))))))
  (u (make-vec3 0 0 0) :type vec3 :read-only t)
  (v (make-vec3 0 0 0) :type vec3 :read-only t)
  (w (make-vec3 0 0 0) :type vec3 :read-only t))

(defun onb-transform (o v)
  (vec3+ (scaled-vec3 (onb-u o) (vec3-x v))
         (scaled-vec3 (onb-v o) (vec3-y v))
         (scaled-vec3 (onb-w o) (vec3-z v))))

