;;;; onna.asd

(asdf:defsystem #:onna
  :description "Raytracing in n weekends"
  :author "Hugh Giddens <hgiddens@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :components ((:file "package")
               (:file "onna"))
  :depends-on (:random-state))
