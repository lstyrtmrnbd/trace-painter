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

(defstruct ray-intersection
  (distance 0.0 :type float)
  (point (vec3 0 0 0) :type vec3)
  (direction (vec3 0 0 0) :type vec3)
  (normal (vec3 0 0 0) :type vec3))

;;; Forms

(defclass sphere ()
  ((position :initarg :pos :accessor pos :type vec3)
   (radius :initarg :r :accessor r :type float)))

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
                   (interB (+ (- r2 rslen) (* interA interA))))
              (when (and (> interB 0.0) (>= r2 interB))
                  (let* ((dist (- interA (sqrt interB)))
                         (interpt (v+ origin (v* dist direction)))
                         (normal (vunit (v- interpt position))))
                    (make-ray-intersection :distance dist
                                           :point interpt
                                           :direction direction
                                           :normal normal)))))))))

;; Decompose sphere intersect to separate checks
(defun intersect-check-A (origin direction position radius)
  (let* ((rsvec (v- position origin))
         (r2 (* radius radius))             ;radius^2
         (interA (v. rsvec direction)))     ;cosine angle
    (when (> interA 0.0)
      (values rsvec r2 interA))))

(defun intersect-check-B (rsvec r2 interA)
  (let* ((rslength (v. rsvec rsvec))        ;length^2
         (interB (+ (- r2 rslength) (* interA interA)))) ;<====
    (if (and (> interB 0.0) (<= interB r2))
        (- interA (sqrt interB))            ;distance to intersection
        (format t "~S"
                (list rsvec rslength interA interB)))))

(defun intersect-decompose (sphere ray)
  (with-slots (origin direction) ray
    (with-slots (position radius) sphere
      (destructuring-bind (rsvec r2 interA)
          (multiple-value-list (intersect-check-A origin direction position radius))
        (when interA
          (intersect-check-B rsvec r2 interA))))))

;;; Casting

(defun focal-length (width height fov)
  (/ (+ (* width width)
        (* height height))
     (* 2 (tan (/ fov 4.0)))))

;; cons a list of rays
(defun generate-rays (w h z origin)
  "w: width of screen, h: height of screen, z: z-pos of screen"
  (let ((result '()))
    (dotimes (x w)
      (dotimes (y h)
        (push (make-instance 'ray :origin origin
                             :dir (vunit (vec (- x (/ w 2.0))
                                              (- y (/ h 2.0))
                                              (- z (vz origin)))))
              result)))
    result))

(defvar width 1280)
(defvar height 720)
(defvar focal-depth (focal-length width height 90))

(defvar sphere0 (make-instance 'sphere :r 64 :pos (vec 0 0 -512)))
(defvar objects (list sphere0))

;; screen at 0 0 0
;; origin at 0 0 -focal
(defvar rays (generate-rays width height 0 (vec 0 0 focal-depth)))

(defun trace-rays (rays objects)
  (mapcar (lambda (ray)
            (mapcar (lambda (object)
                      (intersect object ray))
                    objects))
          rays))

(defvar intersections (trace-rays rays objects))
