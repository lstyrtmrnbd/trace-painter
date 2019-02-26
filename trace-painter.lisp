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
  (diffuse-k (vec3 1.0 1.0 1.0) :type vec3)
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
  "Primary ray tracing"
  (mapcar (lambda (ray)
            (closest-intersection
             (mapcar (lambda (object)
                       (intersect object ray))
                     objects)))
          rays))

(defun trace-ray (ray objects)
  "Single ray trace, no filtering"
  (mapcar (lambda (object)
            (intersect object ray))
          objects))

(defun count-hits (intersections)
  (count-if (lambda (inter)
              (not (null inter)))
            intersections))

(defun get-hits (intersections)
  (remove-if (lambda (inter)
               (null inter))
             intersections))

(defun any-hits (intersections)
  (notevery #'null intersections))

(defun no-hits (intersections)
  (every #'null intersections))

;;;--Lighting--------------------------------------------------------------

(defclass ambient-light ()
  ((intensity :initarg :intensity :accessor intensity :type vec3))) ;clamp 0-1

(defclass distant-light ()
  ((intensity :initarg :intensity :accessor intensity :type vec3)   ;clamp 0-1
   (direction :initarg :direction :accessor direction :type vec3))) ;normalize

(defun reflect (incident normal)
  (v- incident (v* 2.0 normal (v. normal incident))))

(defun lambertian (normal direction)
  "Diffuse lighting calculation.
   normal: surface normal, direction: light direction"
  (max 0.0 (v. normal (v* -1.0 direction))))

(defun phong (normal view-dir light-dir shininess)
  "Phong specular highlight calculation."
  (let* ((reflect-dir (reflect (v* -1.0 light-dir) normal))
         (spec-angle (max 0.0 (v. reflect-dir view-dir))))
    (expt spec-angle (/ shininess 4.0))))

(defun blinn-phong (normal view-dir light-dir shininess)
  "Blinn-Phong specular highlight calculation."
  (let* ((half-dir (vunit (v+ light-dir view-dir)))
         (spec-angle (max 0.0 (v. half-dir normal))))
    (expt spec-angle shininess)))

(defgeneric contribute (intr light)
  (:documentation "Calculate contribution from light to intersection"))

(defmethod contribute (intr (light ambient-light))
  (with-slots (ambient-k) (ray-intersection-material intr)
    (with-slots (intensity) light
      (v* ambient-k intensity))))

(defmethod contribute (intr (light distant-light))
  (with-slots (point normal material) intr
    (with-slots (diffuse-k specular-k shininess) material
      (with-slots (intensity direction) light
        (v+ (v* (lambertian normal direction)
                diffuse-k
                intensity)
            (v* (phong normal (vunit (v- point)) direction shininess)
                specular-k
                intensity))))))

(defgeneric direction-to (point light)
  (:documentation "Calculate the direction from a point to a light, for shadow tracing"))

(defmethod direction-to (point (light ambient-light))
  "Ambient light can't be shadowed"
  nil)

(defmethod direction-to (point (light distant-light))
  (v- (direction light)))

(defgeneric distance-to (point light)
  (:documentation "The distance from a point to a light, for shadow tracing"))

(defmethod distance-to (point (light ambient-light))
  "Ambient light has no origin"
  nil)

(defmethod distance-to (point (light distant-light))
  "Distant light has only a directional origin"
  nil)

;;;--Shading---------------------------------------------------------------
;;--produces RGB results per intersection

(defun basic-hit-fn (intr)
  (if (not (null intr))
      (rgb 1.0 1.0 1.0)
      (rgb 0 0 0)))

(defun color-material (intr)
  (if (not (null intr))
      (let ((color (material-color (ray-intersection-material intr))))
        (rgb (vx color) (vy color) (vz color)))
      (rgb 0 0 0)))

(defun ambient-pass (intr light)
  (if (not (null intr))
      (let ((mat-vec (material-color (ray-intersection-material intr)))
            (amb-vec (contribute intr light))
            (ambient (v* mat-vec amb-color)))
        (rgb (vx ambient) (vy ambient) (vz ambient)))
      (with-slots (intensity) light
          (rgb (vx intensity) (vy intensity) (vz intensity)))))

(defun color-ambient (ambient)
  "Returns closure for specific ambient light"
  (lambda (intr)
    (ambient-pass intr ambient)))

(defun compose-color-vectors (vec1 vec2)
  "Vector addition accounting for nil"
  (cond
    ((null vec1) vec2)
    ((null vec2) vec1)
    (t
     (v+ vec1 vec2))))

(defmethod clamp ((val number) floor ceiling)
  (cond
    ((> val ceiling) ceiling)
    ((< val floor) floor)
    (t val)))

(defmethod clamp ((val vec3) floor ceiling)
  (vec (clamp (vx val) floor ceiling)
       (clamp (vy val) floor ceiling)
       (clamp (vz val) floor ceiling)))

;; Per intersection light list should be pre-filtered by shadow casts
;; Clamping the light and material color product is a stopgap,
;;  there should probably be a lerp somewhere
(defun shade (intr lights)
  "The greater shading pipeline, composes all lighting passes for an intersection"
  (if (not (null intr))
      (let* ((color-vec (reduce #'compose-color-vectors
                                (mapcar (lambda (light)
                                          (contribute intr light))
                                        lights)))
             (mat-vec (material-color (ray-intersection-material intr)))
             (product (clamp (v* color-vec mat-vec) 0.0 1.0)))
        (rgb (vx product) (vy product) (vz product)))
      (rgb 0 0 0)))

(defun shade-lights (lights)
  "Closure which shades an intersection for a specific light list"
  (lambda (intr)
    (shade intr lights)))

(defun filter-shadows (intr lights objects)
  "Filters a list of lights through shadowcasting"
  (with-slots (point) intr
    (remove-if #'null
               (mapcar (lambda (light)
                         (unless (alexandria:when-let (direction (direction-to point light))
                                   (any-hits (trace-ray (make-instance 'ray :origin point
                                                                       :dir direction)
                                                        objects)))
                           light))
                       lights))))

;; The higher up the null intersection test in the pipeline the better
(defun shadow-lights (lights objects)
  "Closure which casts shadows before shading intersection"
  (lambda (intr)
    (if intr
        (shade intr (filter-shadows intr lights objects))
        (shade intr lights))))

(defun before-distance (intr distance)
  "Filter for intersections beyond a certain distance"
  (when intr
    (when (> (distance intr) distance)
      nil)))

(defun true (x)
  (not (null x)))

;;;--Formatting-and-Output--------------------------------------------------

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

;;;--Modeling--------------------------------------------------------

(defun make-sphere (radius position material)
  (make-instance 'sphere :r radius :pos position :material material))

(defun make-plane (position normal material)
  (make-instance 'plane :pos position :normal normal :material material))

(defmethod copy-form ((form sphere))
  (make-sphere (r form)
               (pos form)
               (material form)))

(defmethod copy-form ((form plane))
  (make-plane (pos form)
              (normal form)
              (material form)))

(defun move (form newpos)
  (setf (pos form) newpos))

(defun move-all (forms offset)
  (mapcar (lambda (form)
            (move form (v+ (pos form) offset)))
          forms))

;;;--Test Scene------------------------------------------------------

(defvar test-screen (make-screen))
(defvar test-origin (vec 0 0 (screen-focus test-screen 90)))

(defvar rays (generate-rays test-screen test-origin))

(defvar red-mat (make-material :color (vec 1 0 0)))
(defvar green-mat (make-material :color (vec 0 1 0)))
(defvar blue-mat (make-material :color (vec 0 0 1)))

(defvar grey-material (make-material :color (vec 0.5 0.5 0.5)))

(defvar sphere0 (make-instance 'sphere :r 128
                               :pos (vec 0 0 -64)
                               :material green-mat))

(defvar sphere1 (make-instance 'sphere :r 128
                               :pos (vec -256 0 -64)
                               :material blue-mat))

(defvar sphere2 (make-instance 'sphere :r 128
                               :pos (vec 256 0 -64)
                               :material red-mat))

(defvar plane0 (make-instance 'plane
                              :pos (vec 0 -256 -64)
                              :normal (vunit (vec 0 -1 -0.125))
                              :material grey-material))

(defvar objects (list sphere0 sphere1 sphere2 plane0))

(defvar intersections (trace-rays rays objects))

(defvar ambient (make-instance 'ambient-light :intensity (vec 0.5 0.5 0.5)))

(defvar distant (make-instance 'distant-light :intensity (vec 0.25 0.25 0.25)
                               :direction (vec 0 -1 0)))

(defvar lights (list ambient distant))

(defun test-render (intersections color-fn)
  (write-png (generate-filename)
             (array-to-png (intersections-to-array intersections
                                                   (screen-width test-screen)  ;!!
                                                   (screen-height test-screen) ;!!
                                                   color-fn)
                           8)))

