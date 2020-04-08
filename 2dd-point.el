;;; 2dd-point.el --- A "point" drawing -*- lexical-binding: t -*-

;;; Commentary:
;; Draws a "point" - actually a configurable single letter label.

;;; Code:
(require '2dg)
(require '2dd-drawing)

(defclass 2dd-point (2dd-drawing 2dd-with-label 2dd-with-parent-relative-location)
  ()
  :documentation "Represents point based drawing.  Basically a
  single character label floating at a point.")
(defsubst 2dd-point-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-point)"
  (and (recordp any)
       (object-of-class-p any '2dd-point)))
(cl-defmethod 2dd-set-geometry :before ((this 2dd-point) value)
  "Restrict THIS to have a VALUE which is of type 2dg-point."
  (unless (2dg-point-p value)
    (error "2dd-point must use a 2dg-point as their geometry")))

(cl-defmethod 2dd-pprint ((pt 2dd-point))
  "Pretty print PT."
  (format "dr:point(%s:%s)"
          (2dd-get-label pt)
          (2dg-pprint (oref pt _geometry))))
(cl-defmethod cl-print-object ((object 2dd-point) stream)
  "This seems to be used only for edebug sessions."
  (princ (2dd-pprint object) stream))
(cl-defmethod 2dd-set-from ((drawing-point 2dd-point) (rect 2dg-rect) &optional parent-canvas)
  "Set DRAWING-POINT's geometry from RECT.

When PARENT-CANVAS is suppled and the drawing is capable of holding relative coordinate they will be stored as well."
  (2dd-set-from drawing-point (2dg-centroid rect) parent-canvas))

(cl-defmethod 2dd-set-from ((drawing-point 2dd-point) (source-pt 2dg-point) &optional parent-canvas)
  "Set DRAWING-POINT's geometry from SOURCE-PT.

When PARENT-CANVAS is suppled and the drawing is capable of holding relative coordinate they will be stored as well."
  (with-slots (_geometry _relative-geometry) drawing-point
    (if (null _geometry)
        ;; missing a point entirely, create one.
        (setf _geometry (2dg-point :x (2dg-x source-pt)
                                   :y (2dg-y source-pt)))
      ;; point exists, just set it.
      (oset _geometry x (oref source-pt x))
      (oset _geometry y (oref source-pt y)))
    (if parent-canvas
        (let ((relative (2dg-relative-coordinates parent-canvas source-pt)))
          (if (null _relative-geometry)
              (setf _relative-geometry relative)
            (oset _relative-geometry x (oref relative x))
            (oset _relative-geometry y (oref relative y))))
      ;; No parent provided, unable to compute relative coordinates.
      ;; Best to clear them and not be wrong than leave them and be
      ;; incorrect.
      (setf _relative-geometry nil))))

(cl-defmethod 2dd-serialize-geometry ((point 2dd-point) &optional additional-info)
  "Serialize RECT to a string.

Returns a stringified list in one of two forms:
(:relative <RELATIVE-GEOMETRY>) or (:absolute <ABSOLUTE-GEOMETRY>)."
  (with-slots (_geometry _relative-geometry) point
    (prin1-to-string
     (if _relative-geometry
         (list :relative _relative-geometry)
       (list :absolute _geometry)))))

(cl-defmethod 2dd-render ((point 2dd-point) scratch x-transformer y-transformer viewport &rest args)
  "Render POINT to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

If ARGS is used the first argument must be a plist containing
style information for the drawing.  Accepted plist keys are:

:label-style (defaults to no style)

Overridable method for ecah drawing to render itself."
  (let ((pointg (2dd-geometry point))
        (label (2dd-get-label point))
        (style-plist (first args)))
    (let ((x (funcall x-transformer (2dg-x pointg)))
          (y (funcall y-transformer (2dg-y pointg))))

      (2dd---scratch-label scratch x y label (plist-get style-plist :label-style)))))

(cl-defmethod 2dd-build-move-edited ((pt 2dd-point) (move-vector 2dg-point) (viewport 2dd-viewport))
  "Given a PT drawing and a MOVE-VECTOR, apply the movement.

TODO - VIEWPORT is passed in here but not actually needed, remove it."
  (let ((new-pt (clone pt)))
    (oset new-pt _geometry (2dg-point :x (+ (2dg-x pt) (2dg-x move-vector))
                                      :y (+ (2dg-y pt) (2dg-y move-vector))))
    new-pt))

(cl-defmethod 2dd--update-plot ((point 2dd-point) (old-parent-canvas 2dd-canvas) (new-parent-canvas 2dd-canvas) child-fn)
  "Update POINT based on a parent drawing changing.

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

  (let* ((relative-coord (or (2dd-get-relative-geometry point)
                             (2dg-relative-coordinates old-parent-canvas
                                                       (2dd-geometry point))))
         (new-absolute-coords (2dg-absolute-coordinates new-parent-canvas
                                                        relative-coord)))
    (2dd--set-geometry-and-update-plot point
                                       new-absolute-coords
                                       child-fn
                                       new-parent-canvas)))
(cl-defmethod 2dd--set-geometry-and-update-plot ((point 2dd-point) (pointg 2dg-point) child-fn &optional parent-canvas)
  "Update POINT to have POINTG geometry, cascade child drawings as needed."
  (2dd-set-from point pointg parent-canvas)
  (2dd--update-plot-all (2dd--plot-phase point)
                        (funcall child-fn point)
                        parent-canvas
                        parent-canvas
                        child-fn))

(cl-defmethod 2dd-leaving-segment-collision-edge ((source 2dd-point) (dest 2dg-point))
  "If you leave SOURCE headed towards DEST, which edge do you hit?

Returned as one of 4 symbols: 'up, 'down, 'left, 'right."
  (2dg-coarse-direction (2dg-subtract dest (oref source _geometry))))

(provide '2dd-point)
;;; 2dd-point.el ends here
