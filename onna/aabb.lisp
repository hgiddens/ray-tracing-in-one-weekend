(in-package :onna)

(defun pad-to-minimum (interval)
  "Pads INTERVAL not have zero size.

Used to ensure that AABBs don't have zero volume."
  (let ((delta 0.0001))
    (if (< (interval-size interval) delta)
        (expand-interval interval delta)
        interval)))

(defstruct (aabb
            ;; In the book, the default constructor also calls pad-to-minimum
            ;; on its empty intervals, but misses that this is pointless;
            ;; expand-interval is implemented as min = min - delta/2, so the
            ;; infinities remain. However, I don't think this actually matters
            ;; in practice.
            (:constructor)
            (:constructor make-aabb-from-points
                (a b
                 &aux
                   (x (let ((ax (point3-x a)) (bx (point3-x b)))
                        (pad-to-minimum (make-interval :min (min ax bx)
                                                       :max (max ax bx)))))
                   (y (let ((ay (point3-y a)) (by (point3-y b)))
                        (pad-to-minimum (make-interval :min (min ay by)
                                                       :max (max ay by)))))
                   (z (let ((az (point3-z a)) (bz (point3-z b)))
                        (pad-to-minimum (make-interval :min (min az bz)
                                                       :max (max az bz)))))))
            (:constructor make-aabb-from-aabbs
                (a b
                 &aux
                   (x (combine-intervals (aabb-x a) (aabb-x b)))
                   (y (combine-intervals (aabb-y a) (aabb-y b)))
                   (z (combine-intervals (aabb-z a) (aabb-z b))))))
  (x (empty-interval) :type interval)
  (y (empty-interval) :type interval)
  (z (empty-interval) :type interval))

(defun hit-test-aabb (ray aabb time)
  (setf time (copy-interval time))
  (loop with ray-origin = (ray-origin ray)
        with ray-direction = (ray-direction ray)
        ;; TODO: is there a nice way to not use slot-value here?
        for axis in '(x y z)
        as interval = (slot-value aabb axis)
        as 1/direction-axis = (/ (slot-value ray-direction axis))
        as t0 = (* (- (interval-min interval) (slot-value ray-origin axis))
                   1/direction-axis)
        as t1 = (* (- (interval-max interval) (slot-value ray-origin axis))
                   1/direction-axis)
        do (if (< t0 t1)
               (progn
                 (alexandria:maxf (interval-min time) t0)
                 (alexandria:minf (interval-max time) t1))
               (progn
                 (alexandria:maxf (interval-min time) t1)
                 (alexandria:minf (interval-max time) t0)))
        never (<= (interval-max time) (interval-min time))))

(defun longest-axis (node)
  "The interval accessor for the longest axis of NODE."
  (let ((x (interval-size (aabb-x node)))
        (y (interval-size (aabb-y node)))
        (z (interval-size (aabb-z node))))
    (if (> x y)
        (if (> x z) #'aabb-x #'aabb-z)
        (if (> y z) #'aabb-y #'aabb-z))))

(defun aabb+ (aabb v)
  "Offsets AABB by vector V."
  (make-aabb :x (interval+ (aabb-x aabb) (vec3-x v))
             :y (interval+ (aabb-y aabb) (vec3-y v))
             :z (interval+ (aabb-z aabb) (vec3-z v))))
