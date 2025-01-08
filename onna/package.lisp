;;;; package.lisp

(defpackage #:onna
  (:use #:cl
        #:random-state)
  ;; We use random-state for random number generation because SBCL seems
  ;; to commit violent suicide if we ask it to generate too many (?)
  ;; random numbers:
  ;;    fatal error encountered in SBCL pid 12392 pthread 0x1ec198240:
  ;;    unboxed object in scavenge_control_stack: 0x104941b60->1a682a75, start=0x104940000, end=0x104941ef0
  (:shadowing-import-from #:random-state #:random))
