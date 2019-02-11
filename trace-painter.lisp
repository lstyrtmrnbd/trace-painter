;;;; trace-painter.lisp

(in-package #:trace-painter)

;;  - fill object list
;;  - cast rays
;;  - if hit cast subrays
;;  - fill intersection record
;;  - paint using this info

;; animation:
;; map over object list with transformations at a certain framerate
;; then output it


(defclass ray ()
  ((origin :initarg :origin :accessor origin :type vec3)
   (direction :initarg :dir :accessor dir :type vec3)))

;;; Records

(defstruct material
  (color (vec3 0 0 0) :type vec3)
  (ambient-k (vec3 0 0 0) :type vec3)
  (diffuse-k (vec3 0 0 0) :type vec3)
  (specular-k (vec3 0 0 0) :type vec3)
  (shininess 0.0 :type float))

(defstruct ray-intersection
  (distance 0.0 :type float)
  (point (vec3 0 0 0) :type vec3)
  (direction (vec3 0 0 0) :type vec3)
  (normal (vec3 0 0 0) :type vec3))

;;; Forms

(defclass shape ()
  ((material :initarg :material :accessor material :type material)))

(defclass sphere (shape)
  ((position :initarg :pos :accessor pos :type vec3)
   (radius :initarg :r :accessor r :type float)))

(defclass plane (shape)
  ((position :initarg :pos :accessor pos :type vec3)
   (normal :initarg :normal :accessor normal :type vec3)))

(defgeneric intersect (form ray)
  (:documentation "Check if provided ray intersects form"))

;;Sphere intersect
;(proclaim '(optimize (debug 3)))
(defmethod intersect ((form sphere) ray)
  "Transcription of intersectFast, consider where to use destructive vec ops"
  (with-slots (origin direction) ray
    (with-slots (position radius) form
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

;; Decompose sphere intersect to separate checks for debugging
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

;; Plane intersect
(defmethod intersect ((form plane) ray)
  (with-slots (origin direction) ray
    (with-slots (position normal) form
      (let ((denominator (v. normal direction)))
        (when (> denominator 1.0e-6)
          (let* ((l (v- position origin))
                 (distance (/ (v. l normal) denominator)))
            (when (> distance 0.0)
              (make-ray-intersection :distance distance
                                     :point (v+ origin (* distance direction))
                                     :direction direction
                                     :normal normal))))))))

;;; Casting

(defun focal-length (width height fov)
  (sqrt (/ (+ (* width width)
              (* height height))
           (* 2 (tan (/ fov 4.0))))))

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

;; these are properties of some 'screen'
(defvar width 1280)
(defvar height 720)
(defvar focal-depth (focal-length width height 90))

;; screen at 0 0 0
;; origin at 0 0 -focal
(defvar rays (generate-rays width height 0 (vec 0 0 focal-depth)))

(defvar sphere0 (make-instance 'sphere :r 128 :pos (vec 0 0 -64)))
(defvar objects (list sphere0))

(defun trace-rays (rays objects)
  (mapcar (lambda (ray)
            (mapcar (lambda (object)
                      (intersect object ray))
                    objects))
          rays))

(defvar intersections (trace-rays rays objects))

(defun count-hits (intersections)
  (count-if (lambda (inter)
              (not (null (car inter))))
            intersections))

(defun get-hits (intersections)
  (remove-if (lambda (inter)
               (null (car inter)))
             intersections))

;;; Output

(defun intersections-to-array (lst width height &optional (fn #'identity))
  "Maps the 1D intersection list to a 2D array, 
   optionally transforming each element by fn"
  (let ((result (make-array (list width height))))
    (labels ((walk (ls ctr)
               (when (< ctr (* width height)) ;assumes length of list >= w*h
                 (setf (aref result (floor ctr height) (mod ctr height))
                       (funcall fn (car ls)))
                 (walk (cdr ls) (1+ ctr)))))
      (walk lst 0))
    result))

(defun array-to-png (arr bit-depth)
  "Maps 3D column-major array to PNG image,
   which is accessed as a row-major array"
  (let* ((w (array-dimension arr 0))
         (h (array-dimension arr 1))
         (d (array-dimension arr 2))
         (result (png:make-image w h d bit-depth)))
    (dotimes (x w)
      (dotimes (y h)
        (dotimes (c d)
          (setf (aref result y x c)
                (aref arr x y c)))))
    result))

(defun basic-hit-fn (intr)
  (if (not (null intr))
      (list 255 255 255)
      (list 0 0 0)))

(defun basic-hit-png (intrs)
  (array-to-png (intersections-to-array intrs width height
                                        #'basic-hit-fn)
                8))

(defun write-png (path png)
  (with-open-file (output path :element-type (png:image-bit-depth png)
                          :direction :output :if-exists :supersede)
    (png:encode png output)))
