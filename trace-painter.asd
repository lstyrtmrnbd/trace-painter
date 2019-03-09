;;;; trace-painter.asd

(asdf:defsystem #:trace-painter
  :description "Ray tracer"
  :author "Your Name <your.name@example.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:3d-vectors
               #:3d-matrices
               #:png
               #:alexandria
               #:trivial-project-pathname)
  :components ((:file "package")
               (:file "trace-painter")))
