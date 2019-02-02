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


(defclass ray ()
  ((origin :initarg :origin :accessor origin :type vec3)
   (direction :initarg :dir :accessor dir :type vec3)))

(defclass sphere ()
  ((position :initarg :pos :accessor pos :type vec3)
   (radius :initarg :r :accessor r :type float)))

(defgeneric intersect (shape ray)
  (:documentation "Check if provided ray intersects shape"))

(defmethod intersect ((shape sphere) ray)
  "Transcription of intersectFast, consider where to use destructive vec ops"
  (with-slots (origin direction) ray
    (with-slots (position radius) shape
      (let* ((rsvec (v- position origin))
             (rad2 (* radius radius))       ;radius^2
             (interA (v. rsvec direction))) ;cosine angle
        (when (> interA 0.0)
            (let* ((rslen (v. rsvec rsvec)) ;length^2
                   (interB (- rad2 (+ rslen (* interA interA)))))
              (when (and (> interB 0.0) (> rad2 interB))
                  (let* ((dist (- interA (sqrt interB)))
                         (interpt (+ origin (* dist direction)))
                         (normal (vunit (v- interpt position))))
                    (make-intersection dist interpt normal direction)))))))))
