;;;; trace-painter.lisp

(in-package #:trace-painter)

;; fill object list
;; cast rays
;;  - if hit cast additional rays
;;  - build chain of intersections
;; paint using this info

;; data in play:
;; an object is a list of surfaces and a material those surfaces share
;; 

;; animation:
;; map over object list with transformations at a certain framerate
;; then output it


(defstruct ray
  origin direction)

(defclass sphere ()
  position radius)

(defgeneric intersect (shape ray)
  (:documentation "Check if provided ray intersects shape"))

(defmethod intersect ((shape sphere) ray)
  "Transcription of intersectFast, consider where to use destructive vec ops"
  (with-slots (origin direction) ray
    (with-slots (position radius) shape
      (let* ((rsvec (v- position origin))
             (rad2 (* radius radius)) ;radius^2
             (interA (v. rsvec direction)))     ;cosine angle
        (if (< interA 0.0)
            nil
            (let* ((rslen (v. rsvec rsvec)) ;length^2
                   (interB (- rad2 (+ rslen (* interA interA)))))
              (if (or (< interB 0.0) (< rad2 interB))
                  nil
                  (let* ((dist (- interA (sqrt interB)))
                         (interpt (+ origin (* dist direction)))
                         (normal (vunit (- interpt position))))
                    (make-intersection dist interpt normal direction)))))))))
