;;;; trace-painter.asd

(asdf:defsystem #:trace-painter
  :description "Describe trace-painter here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components (("3d-vectors")
               ("3d-matrices")
               (:file "package")
               (:file "trace-painter")))
