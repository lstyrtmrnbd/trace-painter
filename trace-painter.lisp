;;;; trace-painter.lisp

(in-package #:trace-painter)

;;;--Records-----------------------------------------------------------

(defclass ray ()
  ((origin :initarg :origin :accessor origin :type vec3)
   (direction :initarg :dir :accessor dir :type vec3)))

(deftype unit-real ()
  "Real number in [0,1]"
  '(real 0 1))

(defstruct (rgb (:constructor rgb (red green blue)))
  "RGB color"
  (red nil :type unit-real :read-only t)
  (green nil :type unit-real :read-only t)
  (blue nil :type unit-real :read-only t))

(defstruct material
  (color (vec3 0 0 0) :type vec3)
  (ambient-k (vec3 1.0 1.0 1.0) :type vec3)
  (diffuse-k (vec3 0 0 0) :type vec3)
  (specular-k (vec3 0 0 0) :type vec3)
  (shininess 0.0 :type float))

(defvar default-material (make-material))

(defstruct ray-intersection
  (distance 0.0 :type float)
  (point (vec3 0 0 0) :type vec3)
  (direction (vec3 0 0 0) :type vec3)
  (normal (vec3 0 0 0) :type vec3)
  (material default-material :type material))

;;;--Forms------------------------------------------------------------

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
                                           :normal normal
                                           :material (material form))))))))))

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
                                     :point (v+ origin (v* distance direction))
                                     :direction direction
                                     :normal normal
                                     :material (material form)))))))))

;;;--Casting----------------------------------------------------------------

(defstruct screen
  "Screen through which rays are cast"
  (width 1280 :type integer)
  (height 720 :type integer)
  (position (vec 0 0 0) :type vec3))

(defun focal-length (width height fov)
  (sqrt (/ (+ (* width width)
              (* height height))
           (* 2 (tan (/ fov 4.0))))))

(defun screen-focus (screen fov)
  "Returns focal depth for a particular screen and FOV"
  (with-slots (width height) screen
    (focal-length width height fov)))

;; cons a list of rays
(defun generate-rays (screen origin)
  "Uniformly generate rays through a screen from origin"
  (with-slots (width height position) screen
    (let ((z (vz position))
          (result '()))
      (dotimes (x width)
        (dotimes (y height)
          (push (make-instance 'ray :origin origin
                               :dir (vunit (vec (- x (/ width 2.0))
                                                (- y (/ height 2.0))
                                                (- z (vz origin)))))
                result)))
      result)))

(defun closest-intersection (intrl)
  "Reduces a list of intersections to just the closest"
  (reduce (lambda (intr1 intr2)
            (cond
              ((null intr2) intr1)
              ((null intr1) intr2)
              (t
               (let ((dist1 (ray-intersection-distance intr1))
                     (dist2 (ray-intersection-distance intr2)))
                 (if (< dist1 dist2)
                     intr1
                     intr2)))))
          intrl))

(defun trace-rays (rays objects)
  (mapcar (lambda (ray)
            (closest-intersection
             (mapcar (lambda (object)
                       (intersect object ray))
                     objects)))
          rays))

(defun count-hits (intersections)
  (count-if (lambda (inter)
              (not (null inter)))
            intersections))

(defun get-hits (intersections)
  (remove-if (lambda (inter)
               (null inter))
             intersections))

;;;--Lighting--------------------------------------------------------------

(defclass ambient-light ()
  (intensity :initarg :intensity :accessor intensity :type vec3))

(defgeneric contribute (intr light)
  (:documentation "Calculate RGB contribution from light to intersection"))

(defmethod contribute ((ray-intersection intr) (ambient-light light))
  (with-slots (ambient-k) intr
    (with-slots (intensity) light
      (rgb (* (vx ambient-k) (vx intensity))
           (* (vy ambient-k) (vx intensity))
           (* (vz ambient-k) (vz intensity))))))

;;;--Shading---------------------------------------------------------------

;; Shading passes
(defun basic-hit-fn (intr)
  (if (not (null intr))
      (rgb 1.0 1.0 1.0)
      (rgb 0 0 0)))

(defun color-material (intr)
  (if (not (null intr))
      (let ((color (material-color (ray-intersection-material intr))))
        (rgb (vx color) (vy color) (vz color)))
      (rgb 0 0 0)))

(defun intersections-to-array (lst width height
                               &optional (color-fn #'identity))
  "Maps the 1D intersection list to a 2D array, 
   optionally transforming each element by color-fn"
  (let ((result (make-array (list width height))))
    (labels ((walk (ls ctr)
               (when (< ctr (* width height)) ;assumes length of list >= w*h
                 (setf (aref result (floor ctr height) (mod ctr height))
                       (funcall color-fn (car ls)))
                 (walk (cdr ls) (1+ ctr)))))
      (walk lst 0))
    result))

(defun unit-real-to-unsigned-byte (real bit-depth)
  "Turns RGB value into unsigned byte for png output"
  (let ((max (1- (expt 2 bit-depth))))
    (floor (* real max))))

(defun array-to-png (arr bit-depth)
  "Maps 2D row-major array of RGB type to PNG image,
   which is a column-major array of unsigned bytes 8 or 16 bits deep"
  (let* ((w (array-dimension arr 0))
         (h (array-dimension arr 1))
         (result (png:make-image h w 3 bit-depth)))
    (dotimes (x w)
      (dotimes (y h)
        (let* ((color (aref arr x y))
               (red (unit-real-to-unsigned-byte (rgb-red color) bit-depth))
               (green (unit-real-to-unsigned-byte (rgb-green color) bit-depth))
               (blue (unit-real-to-unsigned-byte (rgb-blue color) bit-depth)))
          (setf (aref result y x 0) red
                (aref result y x 1) green
                (aref result y x 2) blue))))
    result))

;; File output
(defun write-png (path png)
  (with-open-file (output path :element-type '(unsigned-byte 8)
                          :direction :output :if-exists :supersede)
    (png:encode png output)))

(defun generate-filename ()
  (pathname (concatenate 'string
                         "c:/Users/user0/src/CL/trace-painter/renders/render-"
                         (write-to-string (get-universal-time))
                         ".png")))

;;;--Test Scene------------------------------------------------------

(defvar test-screen (make-screen))
(defvar test-origin (vec 0 0 (screen-focus test-screen 90)))

(defvar rays (generate-rays test-screen test-origin))

(defvar red-mat (make-material :color (vec 1 0 0)))
(defvar green-mat (make-material :color (vec 0 1 0)))

(defvar sphere0 (make-instance 'sphere :r 128
                               :pos (vec 0 0 -64)
                               :material green-mat))

(defvar plane0 (make-instance 'plane
                              :pos (vec 0 0 -64)
                              :normal (vunit (vec 0 -1 -0.125))
                              :material red-mat))

(defvar objects (list sphere0 plane0))

(defvar intersections (trace-rays rays objects))

(defun test-render (intersections color-fn)
  (write-png (generate-filename)
             (array-to-png (intersections-to-array intersections
                                                   (screen-width test-screen)  ;!!
                                                   (screen-height test-screen) ;!!
                                                   color-fn)
                           8)))

