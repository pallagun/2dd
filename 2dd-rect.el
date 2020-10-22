;;; 2dd-rect.el --- rectangular drawing -*- lexical-binding: t -*-

;;; Commentary:
;; A drawable rectangle has 8 edit indexs.  One for each corner and
;; one at the midpoint of each segment.
;;
;; Edit indices are numbered as:
;; 6 - 5 - 4
;; |       |
;; 7       3
;; |       |
;; 0 - 1 - 2

;;; Code:
(require '2dg)
(require '2dd-drawing)
(require '2dd-scratch-render)

(defclass 2dd-rect (2dd-with-inner-canvas 2dd-editable-drawing 2dd-with-label 2dd-with-parent-relative-location)
  ()
  :documentation "Represents a rectangle which can be drawn on a
canvas.  It has an inner-canvas and a label")
(defsubst 2dd-rect-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-rect)"
  (and (recordp any)
       (object-of-class-p any '2dd-rect)))
(cl-defmethod 2dd-set-geometry :before ((this 2dd-rect) value)
  "Restrict THIS to have a VALUE which is of type 2dg-rect."
  ;; TODO - this can be done using generics, no need to do it here.
  (unless (2dg-rect-p value)
    (error "2dd-rect must use a 2dg-rect as their geometry")))
(defun 2dd--rect-edge-point (rect edge relative-coord)
  "Return the coordinate of absolute coordinate of RELATIVE-COORD along EDGE of RECT.

RECT must be a 2dd-rect, EDGE must be a 2dg edge/direction
enumerator, RELATIVE-COORD must be a floating point between 0.0
and 1.0."
  (let ((geo (oref rect _geometry)))
    (if geo
        (let ((edge-segment (2dg-edge geo edge)))
          (2dg-absolute-coordinates edge-segment relative-coord))
      (error "2dd-rect-edge-point: 2dd-rect has no valid geometry"))))
(cl-defmethod 2dd-pprint ((rect 2dd-rect))
  "Pretty print RECT."
  (with-slots ((geo _geometry) (relative-geo _relative-geometry)) rect
    (format "dr:rect(%s:g->%s:rg->%s)"
            (2dd-get-label rect)
            (if geo (2dg-pprint geo) nil)
            (if relative-geo
                (if (listp relative-geo)
                    (list 'relative-lock (plist-get relative-geo :description))
                  (2dg-pprint relative-geo))
              nil))))

(defmacro 2dd--rect-coord-repair (min max)
  "A macro to ensure min is below max, I should not need this.

TODO - fix this so I don't need this, it's a clear hack."
  `(when (> ,min ,max)
    (let ((mid (round (/ (+ ,min ,max) 2.0))))
      (setq ,min mid)
      (setq ,max mid))))

(defconst 2dd---rect-edit-points (list 2dd---arrow-any
                                       2dd---arrow-down
                                       2dd---arrow-any
                                       2dd---arrow-right
                                       2dd---arrow-any
                                       2dd---arrow-up
                                       2dd---arrow-any
                                       2dd---arrow-left
                                       2dd---arrow-any)
  "Edit idx characters, in order.")

(cl-defmethod 2dd-render ((rect 2dd-rect) scratch x-transformer y-transformer viewport &rest args)
  "Render RECT to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

If ARGS is used the first argument must be a plist containing
style information for the drawing.  Accepted plist keys are:

:outline-style (defaults to no style)
:label-style (defaults to no style)
:edit-idx-style (defaults to no style)
:no-outline (defaults to nil/false)
:label-y-offset (defaults to -1)

Overridable method for ecah drawing to render itself."
  (let ((rectg (2dd-geometry rect))
        (label (2dd-get-label rect))
        (style-plist (first args)))
    (let ((x-min (funcall x-transformer (2dg-x-min rectg)))
          (x-max (funcall x-transformer (2dg-x-max rectg)))
          (y-min (funcall y-transformer (2dg-y-min rectg)))
          (y-max (funcall y-transformer (2dg-y-max rectg))))
      ;; coordinate inversion - when it occurs just draw something tiny for now.
      ;; todo: handle this better?
      (2dd--rect-coord-repair x-min x-max)
      (2dd--rect-coord-repair y-min y-max)
      (unless (plist-get style-plist :no-outline)
        (let ((outline-style (plist-get style-plist :outline-style)))
          (2dd---scratch-line-vert scratch x-min y-min y-max 2dd---vertical outline-style)
          (2dd---scratch-line-vert scratch x-max y-min y-max 2dd---vertical outline-style)
          (2dd---scratch-line-hori scratch x-min x-max y-min 2dd---horizontal outline-style)
          (2dd---scratch-line-hori scratch x-min x-max y-max 2dd---horizontal outline-style)))
      ;; if there is an edit idx set, draw the edit idx points
      (when (2dd-get-edit-idx rect)
        (cl-loop with edit-idx-style = (plist-get style-plist :edit-idx-style)
                 for point in (2dd-edit-idx-points rect)
                 for marker-char in 2dd---rect-edit-points
                 for x-scratch = (funcall x-transformer (2dg-x point))
                 for y-scratch = (funcall y-transformer (2dg-y point))
                 do (2dd---scratch-set scratch
                                       x-scratch
                                       y-scratch
                                       marker-char
                                       edit-idx-style)))

      ;; if there is a label, place it in the top left *pixel*
      ;; Don't draw the label if there is no space (y-max == y-min => no space for label)
      (when (and label (> y-max y-min))
        (let ((label-length (length label))
              (label-y-displacement (or (plist-get style-plist :label-y-offset)
                                        -1)))
          (when (> label-length 0)
            (let ((max-display-length (max 0 (- (- x-max x-min) 1))))
              (2dd---scratch-label scratch
                                   (1+ x-min)
                                   (+ y-max label-y-displacement)
                                   (if (> label-length max-display-length)
                                       (substring label 0 max-display-length)
                                     label)
                                   (plist-get style-plist :label-style)))))))))
(cl-defmethod 2dd-serialize-geometry ((rect 2dd-rect) &optional additional-info)
  "Serialize RECT to a string.

Returns a stringified list in one of two forms:
(:relative <RELATIVE-GEOMETRY>) or (:absolute <ABSOLUTE-GEOMETRY>)."
  (cl-flet ((relative-geometry
             ()
             (let ((raw (2dd--get-raw-rect-relative-geometry rect)))
               (if (and raw (listp raw))
                   ;; It's a relative list describing a parent division
                   (plist-get raw :description)
                 ;; it's a normal rectangle.
                 raw))))
    (let* ((relative-geometry (relative-geometry))
           (output-list (if relative-geometry
                            (list :relative relative-geometry)
                          (list :absolute (2dd-geometry rect)))))
      (prin1-to-string (if additional-info
                           (nconc output-list additional-info)
                         output-list)))))
(defsubst 2dd--clone-2dg-rect (any-rect)
  "Create a clone of ANY-RECT returning only a raw 2dg-rect."
  (2dg-rect :x-min (2dg-x-min any-rect)
            :x-max (2dg-x-max any-rect)
            :y-min (2dg-y-min any-rect)
            :y-max (2dg-y-max any-rect)))
(cl-defmethod 2dd-num-edit-idxs ((rect 2dd-rect))
  "How many edit idx points are there for this RECT.

There are always 8."
  8)
(cl-defmethod 2dd-edit-idx-points ((rect 2dd-rect))
  "Get the locations of the edit idxs for RECT as an ordered list.

Points start at the bottom left and go counter clock wise."
  (with-slots (x-min x-max y-min y-max) (2dd-geometry rect)
    (list (2dg-point :x x-min :y y-min)
          (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-min)
          (2dg-point :x x-max :y y-min)
          (2dg-point :x x-max :y (/ (+ y-max y-min) 2.0))
          (2dg-point :x x-max :y y-max)
          (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-max)
          (2dg-point :x x-min :y y-max)
          (2dg-point :x x-min :y (/ (+ y-max y-min) 2.0)))))
(cl-defmethod 2dd-edit-idx-point ((rect 2dd-rect) (idx integer))
  ;; TODO - figure out how to deduplicate this with points.
  "Get the location of the given edit idx BL is zero, go CCW from there."
  (with-slots (x-min x-max y-min y-max) (2dd-geometry rect)
    (case idx
     ;; BL
     (0 (2dg-point :x x-min :y y-min))
     ;; Bottom
     (1 (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-min))
     ;; BR
     (2 (2dg-point :x x-max :y y-min))
     ;; R
     (3 (2dg-point :x x-max :y (/ (+ y-max y-min) 2.0)))
     ;; TR
     (4 (2dg-point :x x-max :y y-max))
     ;; T
     (5 (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-max))
     ;; TL
     (6 (2dg-point :x x-min :y y-max))
     ;; L
     (7 (2dg-point :x x-min :y (/ (+ y-max y-min) 2.0)))
     ;; err
     (otherwise (error "Invalid edit-mode idx: %s" idx)))))

(cl-defmethod 2dd-build-move-edited-geometry ((rect 2dd-rect) (move-vector 2dg-point))
  "Given a RECT, and a MOVE-DIRECTION, move in one pixel in that direction."
  (2dg-add (oref rect _geometry) move-vector))
(cl-defmethod 2dd-build-idx-edited-geometry ((rect 2dd-rect) (edit-idx integer) (move-vector 2dg-point))
  "Build a rectangle drawing based off RECT having EDIT-IDX moved by MOVE-VECTOR."
  (let ((horizontal-pts)
        (vertical-pts)
        (rectg (oref rect _geometry)))
    (let ((pts (2dg-bounding-pts rectg)))
      (cond ((equal 0 edit-idx)           ;bottom left
             (setq horizontal-pts (list (first pts) (fourth pts)))
             (setq vertical-pts (list (first pts) (second pts))))
            ((equal 1 edit-idx)           ;bottom edge
             (setq vertical-pts (list (first pts) (second pts))))
            ((equal 2 edit-idx)           ;bottom right
             (setq horizontal-pts (list (second pts) (third pts)))
             (setq vertical-pts (list (first pts) (second pts))))
            ((equal 3 edit-idx)           ;right edge
             (setq horizontal-pts (list (second pts) (third pts))))
            ((equal 4 edit-idx)           ;top right
             (setq horizontal-pts (list (second pts) (third pts)))
             (setq vertical-pts (list (third pts) (fourth pts))))
            ((equal 5 edit-idx)           ;top
             (setq vertical-pts (list (third pts) (fourth pts))))
            ((equal 6 edit-idx)           ;top left
             (setq horizontal-pts (list (first pts) (fourth pts)))
             (setq vertical-pts (list (third pts) (fourth pts))))
            ((equal 7 edit-idx)           ;left edge
             (setq horizontal-pts (list (first pts) (fourth pts))))
            ('t
             (error "Invalid edit-idx for 2dd-rect: %s" edit-idx)))
      (with-slots (x y) move-vector
        (when (and horizontal-pts (not (equal x 0.0)))
          (cl-loop for pt in horizontal-pts
                   for delta-x = (2dg-x pt)
                   do (oset pt x (+ delta-x x))))
        (when (and vertical-pts (not (equal y 0.0)))
          (cl-loop for pt in vertical-pts
                   for delta-y = (2dg-y pt)
                   do (oset pt y (+ delta-y y)))))
      (let ((x-min (2dg-x (first pts)))
            (x-max (2dg-x (second pts)))
            (y-min (2dg-y (first pts)))
            (y-max (2dg-y (third pts))))
        ;; Before creating the rectangle, ensure it's not zero size or invalid.
        (if (and (< x-min x-max) (< y-min y-max))
            (2dg-rect :x-min x-min :x-max x-max
                      :y-min y-min :y-max y-max)
          nil)))))

(cl-defmethod 2dd-has-inner-canvas-p ((rect 2dd-rect))
  "Return non-nil if RECT has an inner-canvas.  Will always be 't."
  t)
(cl-defmethod 2dd-get-inner-canvas ((rect 2dd-rect))
  "Return the inner canvas of RECT."
  (let ((rectg (2dd-geometry rect))
        (pad-x (2dd-padding-horizontal rect))
        (pad-y (2dd-padding-vertical rect)))
    (2dd-inner-canvas :parent rect
                      :x-min (+ (2dg-x-min rectg) pad-x)
                      :x-max (- (2dg-x-max rectg) pad-x)
                      :y-min (+ (2dg-y-min rectg) pad-y)
                      :y-max (- (2dg-y-max rectg) pad-y))))
(cl-defmethod 2dd-set-from ((drawing-rect 2dd-rect) (serialized-geom string) &optional parent-canvas)
  "Set the geometry of DRAWING-RECT from SERIALIZED-GEOM.

This function _should_ accept the output of
2dd-serializen-geometry.

It should be of the form \"(:absolute #s(2dg-rect ...)\""
  (let* ((read-sexp (read-from-string serialized-geom))
         (source-geom (car read-sexp)))
    (unless (listp source-geom)
      (error "2dd-set-from for a 2dd-rect expects geometry to be a stringified list"))
    (block is-set
      (when-let ((absolute-geom (plist-get source-geom :absolute)))
        (2dd-set-from drawing-rect absolute-geom parent-canvas)
        (return-from is-set))
      (when-let ((relative-geom (plist-get source-geom :relative)))
        (when (2dg-rect-class-p relative-geom)
          (2dd--rect-set-from-relative drawing-rect relative-geom parent-canvas)
          (return-from is-set))
        ;; (when (and (listp relative-geom)
        ;;            (numberp (plist-get relative-geom :division-idx)))
           
          ;; else
        (return-from is-set))
      (error "2dd-set-from is unable to set geometry for a 2dd-rect given: %s" serialized-geom))))
(defun 2dd--rect-set-from-relative (drawing-rect relative-rect parent-canvas)
  "Set the geometry of DRAWING-RECT from RELATIVE-RECT in PARENT-CANVAS"
  (unless parent-canvas
    (error "Unable to set 2dd-rect relative geometry without a parent canvas reference."))
  (oset drawing-rect
        _relative-geometry
        (2dd--clone-2dg-rect relative-rect))

  ;; _TODO, can probably use an oset for this?
  (2dd-set-geometry drawing-rect (2dg-absolute-coordinates parent-canvas relative-rect))
  ;; (oset drawing-rect
  ;;       _geometry
  ;;       (2dg-absolute-coordinates relative-rect parent-canvas))
        
  )
(cl-defmethod 2dd-set-from ((drawing-rect 2dd-rect) (source-rect 2dg-rect) &optional parent-canvas)
  "Set the x/y min/max coordinates of DRAWING-RECT to match SOURCE-RECT.

NOTE: this function always sets the *absolute* SOURCE-RECT.

When PARENT-CANVAS is supplied the relative coordinate will also
be set.  If PARENT-CANVAS is not suppled any relative coordinates
will be cleared."
  (with-slots ((rect _geometry) (relative-rect _relative-geometry)) drawing-rect
    (if (null rect)
        ;; missing a rect entirely, create one.
        (setf rect (2dd--clone-2dg-rect source-rect))
      ;; rect exists, just set it.
      ;; (2dg-rect-set-from rect source-rect)
      (oset rect x-min (oref source-rect x-min))
      (oset rect x-max (oref source-rect x-max))
      (oset rect y-min (oref source-rect y-min))
      (oset rect y-max (oref source-rect y-max)))
    (if parent-canvas
        (let ((relative (2dg-relative-coordinates parent-canvas source-rect)))
          (if (null relative-rect)
              (setf relative-rect relative)
            (oset relative-rect x-min (oref relative x-min))
            (oset relative-rect x-max (oref relative x-max))
            (oset relative-rect y-min (oref relative y-min))
            (oset relative-rect y-max (oref relative y-max))))
      ;; No parent provided, unable to compute relative coordinates.
      ;; Best to clear them and not be wrong than leave them and be
      ;; incorrect.
      (setf relative-rect nil))))
(cl-defmethod 2dd-set-from ((rect 2dd-rect) (geometry-description list) &optional parent-canvas)
  "Set the geometry of RECT to match what GEOMETRY-DESCRIPTION returns.

Currently GEOMETRY-DESCRIPTION may only take a single form, a
property list with :lambda and :description properties.  The
:lambda value must be a lambda which takes no arguments and
returns an absolute coordinate 2dg-rect for the drawing to take.
Currently. :description can be anything.

PARENT-CANVAS is not used."
  (unless parent-canvas
    (error "Unable to use geometry-description list in 2dd-set-from without a parent canvas"))
  (let ((provider (plist-get geometry-description :lambda)))
    (unless provider
      (error "Unable to set a geomtry by list unless there is a lambda"))
    (with-slots ((rectg _geometry) (relative-rect _relative-geometry)) rect
      (let* ((relative-rect (funcall provider))
             (source-rect (2dg-absolute-coordinates parent-canvas relative-rect)))
        (if (null rectg)
            ;; missing a rect entirely, create one.
            (setf rectg (2dd--clone-2dg-rect source-rect))
          ;; rect exists, just set it.
          (oset rectg x-min (oref source-rect x-min))
          (oset rectg x-max (oref source-rect x-max))
          (oset rectg y-min (oref source-rect y-min))
          (oset rectg y-max (oref source-rect y-max))))
      (2dd-set-relative-geometry rect geometry-description))))

(defun 2dd--get-raw-rect-relative-geometry (rect)
  "Return the raw relative geometry.

TODO - figure out how to do this correctly, I'm going around the
object system."
  (oref rect _relative-geometry))
(cl-defmethod 2dd-get-relative-geometry ((rect 2dd-rect))
  "Return the relative geometry (to parent) of RECT.

Relative geometry for a 2dd-rect may be a 2dg-rect containing the
relative coordinates or it may be a property list with a :lambda
propert which must be funcall'd to get the relative 2dg-rect.
This function will return the existing 2dg-rect or the 2dg-rect
returned by a lambda."
  (let ((relative-geo (cl-call-next-method)))
    (if (and relative-geo (listp relative-geo))
        (funcall (plist-get relative-geo :lambda))
      relative-geo)))

(cl-defmethod 2dd--update-plot ((rect 2dd-rect) (old-parent-canvas 2dd-canvas) (new-parent-canvas 2dd-canvas) child-fn)
  "Update RECT based on a parent drawing changing.

Function will return non-nil when changes are applied and nil
otherwise.

This function should be used when the parent of DRAWING has been
changed and DRAWING must be updated to accomodate this.

OLD-PARENT-CANVAS should contain the previous parent inner-canvas
before any changes were applied.

NEW-PARENT-CANVAS should contain the parent inner-canvas after
the parent drawing was edited.

CHILD-FN should produce a list of all child drawings of a given
 drawing.  It will be called as: (funcall CHILD-FN ROOT-DRAWING)."

  (let* ((relative-coord (or (2dd-get-relative-geometry rect)
                             (2dg-relative-coordinates old-parent-canvas
                                                       (2dd-geometry rect))))
         (new-absolute-coords (2dg-absolute-coordinates new-parent-canvas
                                                        relative-coord)))
    (2dd--set-geometry-and-update-plot rect
                                       new-absolute-coords
                                       child-fn
                                       new-parent-canvas)))

(cl-defmethod 2dd--set-geometry-and-update-plot ((rect 2dd-rect) (rectg 2dg-rect) child-fn &optional parent-canvas)
  "Update RECT to have RECTG geometry, cascade child drawings as needed.

"
  (let ((old-inner-canvas (2dd-get-inner-canvas rect)))
    (2dd-set-from rect rectg parent-canvas)

    (2dd--update-plot-all (2dd--plot-phase rect)
                          (funcall child-fn rect)
                          old-inner-canvas
                          (2dd-get-inner-canvas rect)
                          child-fn)))

(cl-defmethod 2dd-leaving-segment-collision-edge ((drawing-rect 2dd-rect) (pt 2dg-point))
  "If you leave centroid of RECT headed towards PT, which edge do you hit?

Returned as one of 4 symbols: 'up, 'down, 'left, 'right."
  (let* ((rect (oref drawing-rect _geometry))
         (centroid (2dg-centroid rect))
         (path (2dg-segment :start centroid :end pt))
         (char-vector (2dg-characteristic-vector path))
         (to-tl (2dg-segment :start centroid :end (2dg-TL rect)))
         (to-tr (2dg-segment :start centroid :end (2dg-TR rect)))
         (cross-tl (2dg-cross-prod char-vector (2dg-characteristic-vector to-tl)))
         (cross-tr (2dg-cross-prod char-vector (2dg-characteristic-vector to-tr))))
    (cond ((and (>= cross-tl 0.0) (>= cross-tr 0.0))
           'right)
          ((and (<= cross-tl 0.0) (<= cross-tr 0.0))
           'left)
          ((and (>= cross-tl 0.0) (<= cross-tr 0.0))
           'up)
          ((and (<= cross-tl 0.0) (>= cross-tr 0.0))
           'down)
          ('t (error "Impossible?")))))

(provide '2dd-rect)
;;; 2dd-rect.el ends here
