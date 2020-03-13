;;; 2dd-drawing.el --- 2dd drawing object -*- lexical-binding: t -*-

;;; Commentary:
;; A 2dd drawing is an object that can be drawn in a 2dd-canvas.

;;; Code:
(require 'eieio)
(require '2dd-canvas)
(require '2dd-viewport)

(defconst 2dd-valid-constraints
  '(free
    captive
    exclusive
    captive+exclusive)
  "Possible constrant settings for a 2dd-drawing

free: This drawing is not subject to any constraints.

captive: This is a drawing which is constrained to be inside of a
parent drawing.  An example would be a 'child' rectangle which
must be inside of a 'parent' rectangle.

exclusive: This is a drawing which is constrained to never
intersect any other drawing with the same parent.  No two
drawings which are exclusive and have the same parent may never
overlap partially or fully.

captive+exclusive: This uses the captive and exclusive
constraints at the same time.")

(defclass 2dd-drawing ()
  ((_constraint :initarg :constraint
                :reader 2dd-get-constraint
                :writer 2dd-set-constraint
                :type symbolp
                :documentation "Constraints must be one of tContainment must be one of (captive, free, semicaptive)")
   (_geometry :initarg :geometry
              :initform nil
              :writer 2dd-set-geometry
              :reader 2dd-geometry
              :documentation "The 2dg object backing this drawing"))
  :abstract t
  :documentation "This is a thing which can be drawn.  A rectangle, an arrow, a label, etc.")
(defsubst 2dd-drawing-class-p (any)
  "Same as (object-of-class-p ANY '2dd-drawing)."
  (object-of-class-p any '2dd-drawing))
(cl-defmethod 2dd-set-constraint :before ((this 2dd-drawing) value)
  "Set the constraint flag for THIS to VALUE after validation."
  (unless (memq value 2dd-valid-constraints)
    (error "Invalid containment value: %s, must be one of %s"
           value
           2dd-valid-constraints)))
(cl-defgeneric 2dd-num-edit-idxs ((drawing 2dd-drawing))
  "How many edit idx points are there for this DRAWING.  It may be zero")
(cl-defgeneric 2dd-edit-idx-point ((drawing 2dd-drawing) (idx integer))
  "Get the 2dg-point location of the given edit IDX in DRAWING.

May return nil or error if idx is invalid.")
(cl-defgeneric 2dd-edit-idx-points ((drawing 2dd-drawing))
  "Get an ordered list of all the edit-idx points for this DRAWING.")
(cl-defgeneric 2dd-build-move-edited-geometry ((drawing 2dd-drawing) (move-vector 2dg-point))
  "Return new geometry based off moving DRAWING by MOVE-VECTOR.

In the event that it's not possible to move DRAWING, return nil.

Nothing should be mutated.")
(cl-defgeneric 2dd-build-move-edited ((drawing 2dd-drawing) (move-vector 2dg-point) (viewport 2dd-viewport))
  ;; TODO - remove viewport from this, then from all methods.
  "Build a drawing based off moving DRAWING by MOVE-VECTOR.

This should only build a new drawing and return it (if possible)
and should not mutate anything.")
(cl-defgeneric 2dd-build-hint ((drawing 2dd-drawing) (parent-canvas 2dd-inner-canvas))
  "Given a DRAWING and PARENT-CANVAS generate a drawing 'hint'.

A drawing 'hint' is something that captures the intent of the
drawing but not the exact pixels.  Something like box-on-the-left
instead of an exact set of pixels/segments.  It may or may not be
relative to the parent-canvas.")
(cl-defgeneric 2dd-build-simplified ((drawing 2dd-drawing) (viewport 2dd-viewport))
  "Attempt to build a simplified DRAWING as seen by human eyes in VIEWPORT.

VIEWPORT is used to establish how agressive the simplification can be.")
(cl-defgeneric 2dd-inner-canvas-p ((drawing 2dd-drawing))
  "Return non-nil if this drawing has an inner-canvas.

Having an inner-canvas indicates that a drawing has space within
it to hold other drawings.")
(cl-defgeneric 2dd-render ((drawing 2dd-drawing) scratch x-transformer y-transformer viewport &rest args)
  "Render DRAWING to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

ARGS can be used to pass in additional info to any rendering function.

Overridable method for ecah drawing to render itself."
  (error "Unable to render drawing of type %s"
         (eieio-object-class-name drawing)))
(cl-defgeneric 2dd-serialize-geometry ((drawing 2dd-drawing) &optional additional-info)
  "Serialize DRAWING's geometry to a string for storage."
  (error "Unable to 2dd-serialize object of type: %s"
         (eieio-object-class-name drawing)))
(cl-defgeneric 2dd-set-from ((drawing 2dd-drawing) source-geometry &optional parent-canvas)
  "Set DRAWING's geometry from SOURCE-GEOMETETRY.

When PARENT-CANVAS is suppled and the drawing is capable of holding relative coordinate they will be stored as well.")
(cl-defgeneric 2dd--plot-update ((drawing 2dd-drawing) (old-parent-canvas 2dd-canvas) (new-parent-canvas 2dd-canvas) child-fn)
  "Update DRAWING based on a parent drawing changing.

Function will return non-nil when changes are applied and nil
otherwise.

This function should be used when the parent of DRAWING has been
changed and DRAWING must be updated to accomodate this.

OLD-PARENT-CANVAS should contain the previous parent inner-canvas
before any changes were applied,

NEW-PARENT-CANVAS should contain the parent inner-canvas after
the parent drawing was edited.

CHILD-FN should produce a list of all child drawings of a given
 drawing.  It will be called as: (funcall CHILD-FN ROOT-DRAWING)."
  (error "Unable to 2dd--plot-update for drawing of type: %s"
         (eieio-object-class-name drawing)))
(cl-defgeneric 2dd--set-geometry-and-plot-update ((drawing 2dd-drawing) new-geometry child-fn &optional parent-canvas)
  "Update DRAWING to have NEW-GEOMETRY and cascade any needed updates to child drawings.

NEW-GEOMETRY should be specified in absolute coordinates.

CHILD-FN should produce a list of all child drawings of a given
 drawing.  It will be called as: (funcall CHILD-FN ROOT-DRAWING).

When PARENT-CANVAS is supplied it will be used to set relative
coordinates.

Note: this function assumes that constraints are already
validated."
  (error "Unable to 2dd--set-geometry-and-plot-update for drawing of type: %s"
         (eieio-object-class-name drawing)))

(cl-defmethod 2dd-num-edit-idxs ((drawing 2dd-drawing))
  "Non-editable drawings always have zero edit indices."
  0)
(cl-defmethod 2dd-edit-idx-point ((drawing 2dd-drawing) (idx integer))
  "Non-editable drawings always error when being asked for an edit idx point."
  (error "Non-editable drawings do not have edit idxs"))
(cl-defmethod 2dd-edit-idx-points ((drawing 2dd-drawing))
  "Non-editable drawings do not have any points."
  nil)
(cl-defmethod 2dd-inner-canvas-p ((drawing 2dd-drawing))
  "Return non-nil if this drawing has an inner-canvas.

By default, drawings do not have inner-canvases."
  nil)

(defclass 2dd-editable-drawing (2dd-drawing)
  ((_edit-idx :initarg :edit-idx
              :reader 2dd-get-edit-idx
              :writer 2dd-set-edit-idx
              :initform nil
              :type (or null integer)
              :documentation "Currently selected edit idx of the
              drawing, if any.  May be nil.  Edit idxs start at
              zero and count up."))
  :abstract t
  :documentation "A drawing which could have its shape edited.")
(defsubst 2dd-editable-drawing-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-editable-drawing)."
  (object-of-class-p any '2dd-editable-drawing))
(cl-defgeneric 2dd-build-idx-edited-geometry ((drawing 2dd-editable-drawing) (edit-idx integer) (move-vector 2dg-point))
  "Return new geometry based off moving EDIT-IDX of DRAWING by MOVE-VECTOR.

This should only build a new geometry and return it (if possible)
and should not mutate anything.  When the edit is not possible
this function will return nil.")
(cl-defgeneric 2dd-build-idx-edited ((drawing 2dd-editable-drawing) (edit-idx integer) (move-vector 2dg-point) (viewport 2dd-viewport))
  "Build a drawing based off moving EDIT-IDX of DRAWING by MOVE-VECTOR.

This should only build a new drawing and return it (if possible)
and should not mutate anything.")
(cl-defgeneric 2dd-num-edit-idxs ((drawing 2dd-editable-drawing))
  "Return the number of edit-idxs this DRAWING has."
  (error "2dd-num-edit-idxs: Not implemented for drawing type: %s"
         (eieio-object-class-name drawing)))
(cl-defgeneric 2dd-edit-idx-points ((drawing 2dd-editable-drawing))
  "Get the locations of the edit idxs for DRAWING as an ordered list."
  (error "2dd-edit-idx-points: Not implemented for drawing type: %s"
         (eieio-object-class-name drawing)))
(cl-defgeneric 2dd-edit-idx-point ((drawing 2dd-editable-drawing) (edit-idx integer))
  "Get the coordinate of the given EDIT-IDX for DRAWING."
  (error "2dd-edit-idx-point: Not implemented for drawing type: %s"
         (eieio-object-class-name drawing)))
(cl-defgeneric 2dd-get-closest-edit-idx ((drawing 2dd-editable-drawing) (point 2dg-point))
  "Return DRAWING's closest edit-idx to POINT.

Return is of the form '(EDIT-IDX-NUM . EDIT-IDX-POINT)"
  (error "2dd-get-closest-edit-idx: Not implemented for drawing type: %s"
         (eieio-object-class-name drawing)))

(defclass 2dd-with-label ()
  ((_label :initarg :label
           :initform nil
           :reader 2dd-get-label
           :writer 2dd-set-label
           :type (or null string)))
  :abstract t
  :documentation "A mixin class to give drawings a single string label.")

(defclass 2dd-with-inner-canvas ()
  ((_padding-horizontal :initarg :padding-horizontal
                        :reader 2dd-padding-horizontal
                        :writer 2dd-set-padding-horizontal
                        :initform 0.0
                        :type float)
   (_padding-vertical :initarg :padding-vertical
                       :reader 2dd-padding-vertical
                       :writer 2dd-set-padding-vertical
                       :initform 0.0
                       :type float))
  :abstract t
  :documentation "When a drawing has an inner canvas this class holds the current plotting information relevant to child drawings.")
(cl-defmethod 2dd-has-inner-canvas-p ((drawing-with-inner 2dd-with-inner-canvas))
  "Drawings declared to have an inner canavs will return 't here.

TODO - This could be overridden, I'm not sure if I ever want that.  Figure that out."
  t)
(cl-defgeneric 2dd-get-inner-canvas ((drawing 2dd-drawing))
  "Return the current inner canvas of DRAWING-WITH-INNER.

Default implementation is that there is no inner canvas."
  nil)
(cl-defmethod 2dd-get-inner-canvas ((drawing-with-inner 2dd-with-inner-canvas))
  "Return the current inner canvas of DRAWING-WITH-INNER.

2dd-with-inner-canvas objects must implement this function."
  (error "Must implement 2dd-get-inner-canvas for %s"
         (eieio-object-class-name drawing-with-inner)))
(cl-defmethod 2dd-set-padding ((drawing 2dd-with-inner-canvas) (padding-horizontal number) (padding-vertical number))
  "Set PADDING-HORIZONTAL and PADDING-VERTICAL on this DRAWING."
  (2dd-set-padding-horizontal drawing (float padding-horizontal))
  (2dd-set-padding-vertical drawing (float padding-vertical)))

(defclass 2dd-with-divided-inner-canvas (2dd-with-inner-canvas)
  ()
  :abstract t
  :documentation "A marker class which indicates that the inner
  canvas is a divided one.")
(defsubst 2dd-with-divided-inner-canvas-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-with-divided-inner-canvas)."
  (object-of-class-p any '2dd-with-divided-inner-canvas))


;; TODO: this needs to be enhanced.
;; it should be able to provide
;; -a description (for serialization)
;; -a lambda or value of the parent relative coordinates.
;; -a lambda or value of the absolute coordinates.
;; -a pointer to parent???
;; Currently there's just a weird ad hoc list in division-rect that it generates
;; and 2dd-rect somewhat knows how to use.
(defclass 2dd-with-parent-relative-location ()
  ((_relative-geometry :initform nil
                       :writer 2dd-set-relative-geometry
                       :reader 2dd-get-relative-geometry
                       :documentation "The 2dg object serving as a parent relative geometry"))
  :abstract t
  :documentation "When a drawing has a parent-relative-location class it will be used to set the actual geometry based off the parent and the relative geometry.")
(defsubst 2dd-with-parent-relative-location-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-with-parent-relative-location)"
  (object-of-class-p any '2dd-with-parent-relative-location))



(provide '2dd-drawing)
;;; 2dd-drawing.el ends here
