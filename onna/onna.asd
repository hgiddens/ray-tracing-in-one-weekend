;;;; onna.asd

(asdf:defsystem #:onna
  :description "Raytracing in n weekends"
  :author "Hugh Giddens <hgiddens@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :components ((:file "package")
               (:file "colour" :depends-on ("package"))
               (:file "image" :depends-on ("package" "colour" "scene"))
               (:file "vec" :depends-on ("package"))
               (:file "ray" :depends-on ("package" "vec"))
               (:file "camera" :depends-on ("package" "vec" "ray" "colour" "image"))
               (:file "scene" :depends-on ("package" "ray" "vec"))
               (:file "scenes" :depends-on ("package" "scene"))
               (:file "onna" :depends-on ("package" "image")))
  :depends-on (:alexandria
               :random-state))
