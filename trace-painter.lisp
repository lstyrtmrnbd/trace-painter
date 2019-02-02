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

(defstruct ray-intersection
  (distance 0.0 :type float)
  (point (vec3 0 0 0) :type vec3)
  (direction (vec3 0 0 0) :type vec3)
  (normal (vec3 0 0 0) :type vec3))

(defgeneric intersect (shape ray)
  (:documentation "Check if provided ray intersects shape"))

(proclaim '(optimize (debug 3)))
(defmethod intersect ((shape sphere) ray)
  "Transcription of intersectFast, consider where to use destructive vec ops"
  (with-slots (origin direction) ray
    (with-slots (position radius) shape
      (let* ((rsvec (v- position origin))
             (r2 (* radius radius))         ;radius^2
             (interA (v. rsvec direction))) ;cosine angle
        (when (> interA 0.0)
            (let* ((rslen (v. rsvec rsvec)) ;length^2
                   (interB (- r2 (+ rslen (* interA interA)))))
              (when (and (> interB 0.0) (> r2 interB))
                  (let* ((dist (- interA (sqrt interB)))
                         (interpt (v+ origin (* dist direction)))
                         (normal (vunit (v- interpt position))))
                    (make-ray-intersection :distance dist
                                           :point interpt
                                           :direction direction
                                           :normal normal)))))))))

(defun intersect-check-A (origin direction position radius)
  (let* ((rsvec (v- position origin))
         (r2 (* radius radius))         ;radius^2
         (interA (v. rsvec direction)))
    (when (> interA 0.0)
      (values rsvec r2 interA))))

(defun intersect-check-B (rsvec r2 interA)
  (let* ((rslen (v. rsvec rsvec))       ;length^2
         (interB (- r2 (+ rslen (* interA interA)))))
    (when (and (> interB 0.0) (> r2 interB))
      (values interA interB))))

(defun intersect-decompose (sphere ray)
  (with-slots (origin direction) ray
    (with-slots (position radius) sphere
      (multiple-value-call #'intersect-check-B
        (intersect-check-A origin
                           direction
                           position
                           radius)))))


