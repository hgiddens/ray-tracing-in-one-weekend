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
               (:file "aabb" :depends-on ("package" "vec"))
               (:file "onb" :depends-on ("package" "vec"))
               (:file "pdf" :depends-on ("package" "vec" "onb"))
               (:file "camera" :depends-on ("package" "vec" "ray" "colour" "image" "pdf"))
               (:file "texture" :depends-on ("package" "colour"))
               (:file "scene" :depends-on ("package" "ray" "vec" "aabb" "material"))
               (:file "material" :depends-on ("package" "vec" "colour" "texture" "onb"))
               (:file "scenes" :depends-on ("package" "scene" "vec" "material"))
               (:file "onna" :depends-on ("package" "camera" "scenes" "image")))
  :depends-on (:alexandria
               :png
               :random-state))
