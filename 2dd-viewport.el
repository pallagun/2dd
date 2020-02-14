;;; 2dd-viewport.el --- 2dd viewport object -*- lexical-binding: t -*-

;;; Commentary:
;; A viewport captures a portion of a canvas which should be made
;; visible to the user as well as how it should be made visible to the
;; user (dilation/scaling).
;;
;; You will encounter 3 different coordinate systems in this file:
;; pixel, point and scratch.
;;
;; - point: This is the normal 2 dimensional cartesian coordinate one
;; would expect on a piece of graph paper.  Coordinates can be
;; floating points.  Increasing X coordinate moves right.  Increasing
;; Y coordinate moves up.  There are no contstraints on coordinates
;; execept that they are real numbers.  All drawings and canvases
;; exist in this coordinate system
;;
;; - scratch: In this coordinate system is similar to the point
;; coordinate system but it is tailored for rendering on a screen.
;; Coordinates may still be floating points though it should be
;; understood that they will be rounded to the nearest integer for
;; display purposes (e.g. 2.2 and 2.1 are different coordinates but
;; they will appear at the same 'pixel' when rendered).  Increasing X
;; coordinate moves right.  Increasing Y coordinate moves up.  There
;; are no contstraints on coordinates execept that they are real
;; numbers though it should be understood that any negative
;; coordinates (or coordinates outside of the current viewport window)
;; will be rendered to the screen.
;;
;; The transformation from point to scratch consists of viewport
;; scaling and shifting.
;;
;; - pixel: this coordinate system is similar to what a computer image
;; would use.  All coordinates are integers.  The 'origin' or (0,0)
;; pixel is in the top left.  Increasing X coordinate moves right.
;; Increasing Y coordinate moves down
;;
;; The viewport object facilitates a series of transformations where
;; drawings (in point coordinates) go to scratch coordinates to be
;; drawn by the scratch-renderer.  It also facilitates transforming
;; mouse clicks (or a pixel) to scratch coordinates and then to point
;; coordinates to identify what drawing was clicked on.

;;; Code:
(require '2dg)
(require '2dd-canvas)

(defclass 2dd-viewport (2dg-rect)
  ((scaling :initarg :scaling
            :reader 2dd-scaling
            :type 2dg-point
            :initform (2dg-point :x 1.0 :y 1.0)
            :documentation "Scaling is applied as:
X pixel = X coordinate * X scaling;
Y pixel = Y coordinate * Y scalxng;

X coordinate = X pixel / X scaling
Y coordinate = Y pixel / Y scaling

Scaling goes up and you zoom in, scaling does down and you zoom out.
note: 'pixel' here denotes an scxml-scratch-render.el pixel which is lower-left origin.
TODO: don't say pixel, say scratch-coord"))
  :documentation "An object describing what should be visible to a user and how.

The object itself represents a rectangle of area which should be visible to the user.
The scaling slot represents how that space should be scaled before rendering")

(cl-defgeneric 2dd-reset-scaling ((viewport 2dd-viewport))
  "Reset the scaling of the VIEWPORT to be identity")
(cl-defmethod 2dd-reset-scaling ((viewport 2dd-viewport))
  "Reset the scaling of the VIEWPORT to be identity"
  (oset viewport scaling (2dg-vector :x 1.0 :y 1.0)))

(cl-defgeneric 2dd-build-viewport ((canvas 2dd-canvas))
  "Build a viewport to cover the entire CANVAS with identity scaling")
(cl-defmethod 2dd-build-viewport ((canvas 2dd-canvas))
  "Build a viewport to cover the entire CANVAS with identity scaling"
  ;; TODO - this should really be ceiling, not 1+
  (2dd-viewport :x-min (2dg-x-min canvas)
                :x-max (1+ (2dg-x-max canvas))
                :y-min (2dg-y-min canvas)
                :y-max (1+ (2dg-y-max canvas))))

(cl-defmethod 2dd-set-domain ((viewport 2dd-viewport) (area 2dg-rect))
  "Set the domain of this VIEWPORT to match the CANVAS.

Return the viewport after modification"
  (setf (2dg-x-min viewport) (2dg-x-min area)
        (2dg-x-max viewport) (float (ceiling (+ 2dg--almost-zero (2dg-x-max area))))
        (2dg-y-min viewport) (2dg-y-min area)
        (2dg-y-max viewport) (float (ceiling (+ 2dg--almost-zero (2dg-y-max area))))))
(cl-defmethod 2dd-zoom ((viewport 2dd-viewport) (alpha number))
  "Zoom the VIEWPORT by ALPHA, modifies viewport, returns viewport.

Alpha > 1 zooms in.  Alpha < 1 zooms out."
  (when (<= alpha 0.0)
    (error "2dd-zoom: alpha can't be less than or equal to zero"))
  (let ((centroid (2dg-centroid viewport))
        (x-radius (/ (2dg-width viewport) 2.0 alpha))
        (y-radius (/ (2dg-height viewport) 2.0 alpha)))
    (oset viewport x-min (- (2dg-x centroid) x-radius))
    (oset viewport x-max (+ (2dg-x centroid) x-radius))
    (oset viewport y-min (- (2dg-y centroid) y-radius))
    (oset viewport y-max (+ (2dg-y centroid) y-radius))
    (oset viewport scaling (2dg-scaled (2dd-scaling viewport)
                                       alpha))))
(cl-defmethod 2dd-pan ((viewport 2dd-viewport) (drawing-coord-delta 2dg-point))
  "Move VIEWPORT by DRAWING-COORD-DELTA.

Uses drawing coordinate system."
  (2dg-incf viewport drawing-coord-delta))
(cl-defmethod 2dd-pan-scratch ((viewport 2dd-viewport) (scratch-x integer) (scratch-y integer))
  "Move VIEWPORT by SCRATCH-X and SCRATCH-Y scratch pixels"
  (2dd-pan viewport (2dg-scaled (2dg-point :x (float scratch-x)
                                           :y (float scratch-y))
                                (2dd-get-point-scaling viewport))))

(cl-defmethod 2dd-required-pixel-width ((viewport 2dd-viewport))
  "How many pixels of width are required for this VIEWPORT to be fully visible."
  (ceiling (* (2dg-x (2dd-scaling viewport))
              (2dg-length (2dg-x-span viewport)))))
(cl-defmethod 2dd-required-pixel-height ((viewport 2dd-viewport))
  "How many pixels of height are required for this VIEWPORT to be fully visible."
  (ceiling (* (2dg-y (2dd-scaling viewport))
              (2dg-length (2dg-y-span viewport)))))

(cl-defmethod 2dd-get-pixel-scaling ((viewport 2dd-viewport))
  "Grab the scaling from point to pixel from VIEWPORT"
  (2dd-scaling viewport))
(cl-defmethod 2dd-get-point-scaling ((viewport 2dd-viewport))
  "Grab the scaling from pixel to point from VIEWPORT"
  (with-slots (scaling) viewport
    (2dg-point :x (/ 1.0 (2dg-x scaling))
               :y (/ 1.0 (2dg-y scaling)))))

(cl-defgeneric 2dd-get-pixel ((viewport 2dd-viewport) (drawing-point 2dg-point))
  "Given a drawing coordinate DRAWING-POINT in a VIEWPORT, get the pixel for it")
(cl-defmethod 2dd-get-pixel ((viewport 2dd-viewport) (drawing-point 2dg-point))
  "Given a drawing coordinate DRAWING-POINT in a VIEWPORT, get the pixel for it"
  (let ((scratch-coord (2dd-get-scratch-coord viewport drawing-point)))
    (2dg-pixel :x (floor (2dg-x scratch-coord))
               :y (1- (ceiling (- (2dd-required-pixel-height viewport)
                                  (2dg-y scratch-coord)))))))

(cl-defmethod 2dd-get-scratch-coord ((viewport 2dd-viewport) (drawing-point 2dg-point))
  "Given a DRAWING-POINT on VIEWPORT, determine the proper scratch coordinate."
  (with-slots (scaling x-min y-min) viewport
    (2dg-scaled (2dg-point :x (- (2dg-x drawing-point) x-min)
                           :y (- (2dg-y drawing-point) y-min))
                scaling)))
(cl-defmethod 2dd-get-coord-centroid ((viewport 2dd-viewport) (pixel 2dg-pixel))
  "Given a pixel, return the center of it in drawing coordinates"
  (with-slots (scaling x-min y-min) viewport
    (let ((height (2dd-required-pixel-height viewport))
          (scale-x (/ 1.0 (2dg-x scaling)))
          (scale-y (/ 1.0 (2dg-y scaling))))
      (2dg-point :x (+ x-min (* scale-x (+ 0.5 (2dg-x pixel))))
                 :y (+ y-min (* scale-y (- height (+ 0.5 (2dg-y pixel)))))))))
(cl-defmethod 2dd-get-coord ((viewport 2dd-viewport) (pixel 2dg-pixel))
  "Given a pixel, return the drawing coordinate rectangle for it"
  (with-slots (scaling x-min y-min) viewport
    (let ((height (2dd-required-pixel-height viewport))
          (scale-x (/ 1.0 (2dg-x scaling)))
          (scale-y (/ 1.0 (2dg-y scaling))))
      (2dg-rect :x-min (+ x-min (* scale-x (2dg-x pixel)))
                :x-max (+ x-min (* scale-x (1+ (2dg-x pixel))))
                :y-max (+ y-min (* scale-y (- height (2dg-y pixel))))
                :y-min (+ y-min (* scale-y (- height (1+ (2dg-y pixel)))))))))
(cl-defmethod 2dd-get-scratch-coord ((viewport 2dd-viewport) (pixel 2dg-pixel))
  "Given a pixel, return a rectangle describing the area which was clicked in drawing coordinates"
  (with-slots (x-min y-min) viewport
    (let ((height (2dd-required-pixel-height viewport)))
      (2dg-point :x (float (2dg-x pixel))
                 :y (float (- height (1+ (2dg-y pixel))))))))

(cl-defmethod 2dd-get-scratch-transformers ((viewport 2dd-viewport))
  "Return a cons cell of X and Y transformers that convert from drawing coordinates to scratch coordinates."
  (cons
   ;; X transformer
   (let ((x-offset (2dg-x-min viewport))
         (x-scale (2dg-x (2dd-scaling viewport))))
     (lambda (x-point) (* (- x-point x-offset) x-scale)))
   ;; Y transformer
   (let ((y-offset (2dg-y-min viewport))
         (y-scale (2dg-y (2dd-scaling viewport))))
     (lambda (y-point) (* (- y-point y-offset) y-scale)))))
(cl-defmethod 2dd-get-scratch-int-transformers ((viewport 2dd-viewport))
  "Return a cons cell of X and Y transformers that convert from drawing coordinates to scratch INTEGER coordinates.

for reasons that should be investigated, y must be floored.
"
  (cons
   ;; X transformer
   (let ((x-offset (2dg-x-min viewport))
         (x-scale (2dg-x (2dd-scaling viewport))))
     (lambda (x-point) (floor (* (- x-point x-offset) x-scale))))
   ;; Y transformer
   (let ((y-offset (2dg-y-min viewport))
         (y-scale (2dg-y (2dd-scaling viewport))))
     (lambda (y-point) (floor (* (- y-point y-offset) y-scale))))))

(provide '2dd-viewport)
;;; 2dd-viewport.el ends here
