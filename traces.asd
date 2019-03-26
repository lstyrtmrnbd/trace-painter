;;;; traces.asd

(asdf:defsystem #:traces
  :description "Ray tracer"
  :author "Date Terumune"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:3d-vectors
               #:3d-matrices
               #:png
               #:alexandria
               #:trivial-project-pathname)
  :components ((:file "package")
               (:file "traces")))
