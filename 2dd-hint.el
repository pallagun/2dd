;; ;;; 2dd-hint.el --- 2dd drawing object -*- lexical-binding: t -*-

;; ;;; Commentary:
;; ;; A 2dd drawing is an object that can be drawn in a 2dd-canvas.

;; ;;; Code:
;; (require 'eieio)
;; (require '2dd-canvas)
;; (require '2dd-viewport)

;; (defclass 2dd-drawing ()
;;   ((_containment :initarg :containment
;;                  :reader 2dd-containment
;;                  :writer 2dd-set-containment
;;                  :initform 'captive
;;                  :type symbolp
;;                  :documentation "Containment must be one of (captive, free, semicaptive)"))
;;   :abstract t
;;   :documentation "This is a thing which can be drawn.  A rectangle, an arrow, a label, etc.")
;; (cl-defmethod 2dd-set-containment :before ((this 2dd-drawing) value)
;;   "Set the containment flag for THIS to VALUE after validation."
;;   (unless (memq value '(captive free semicaptive))
;;     (error "Invalid containment value: %s, must be one of (captive, free, semicaptive)"
;;            value)))
;; (cl-defgeneric 2dd-num-edit-idxs ((drawing 2dd-drawing))
;;   "How many edit idx points are there for this DRAWING.  It may be zero")
;; (cl-defgeneric 2dd-edit-idx-point ((drawing 2dd-drawing) (idx integer))
;;   "Get the 2dg-point location of the given edit IDX in DRAWING.

;; May return nil or error if idx is invalid.")
;; (cl-defgeneric 2dd-edit-idx-points ((drawing 2dd-drawing))
;;   "Get an ordered list of all the edit-idx points for this DRAWING.")
;; (cl-defgeneric 2dd-build-move-edited ((drawing 2dd-drawing) (move-vector 2dg-point) (viewport 2dd-viewport))
;;   ;; TODO - remove viewport from this, then from all methods.
;;   "Build a drawing based off moving DRAWING by MOVE-VECTOR.

;; This should only build a new drawing and return it (if possible)
;; and should not mutate anything.")
;; (cl-defgeneric 2dd-build-hint ((drawing 2dd-drawing) (parent-canvas 2dd-inner-canvas))
;;   "Given a DRAWING and PARENT-CANVAS generate a drawing 'hint'.

;; A drawing 'hint' is something that captures the intent of the
;; drawing but not the exact pixels.  Something like box-on-the-left
;; instead of an exact set of pixels/segments.  It may or may not be
;; relative to the parent-canvas.")
;; (cl-defgeneric 2dd-build-simplified ((drawing 2dd-drawing) (viewport 2dd-viewport))
;;   "Attempt to build a simplified DRAWING as seen by human eyes in VIEWPORT.

;; VIEWPORT is used to establish how agressive the simplification can be.")
;; (cl-defgeneric 2dd-get-inner-canvas ((drawing 2dd-drawing))
;;   "Return the inner canvas of DRAWING which may be nil.")
;; (cl-defmethod 2dd-get-inner-canvas ((drawing 2dd-drawing))
;;   "By default, drawings will have no inner canvas."
;;   nil)

;; (cl-defmethod 2dd-num-edit-idxs ((drawing 2dd-drawing))
;;   "Non-editable drawings always have zero edit indices."
;;   0)
;; (cl-defmethod 2dd-edit-idx-point ((drawing 2dd-drawing) (idx integer))
;;   "Non-editable drawings always error when being asked for an edit idx point."
;;   (error "Non-editable drawings do not have edit idxs"))
;; (cl-defmethod 2dd-edit-idx-points ((drawing 2dd-drawing))
;;   "Non-editable drawings do not have any points."
;;   nil)

;; (defclass 2dd-editable-drawing (2dd-drawing)
;;   ((_edit-idx :initarg :edit-idx
;;               :reader 2dd-edit-idx
;;               :writer 2dd-set-edit-idx
;;               :initform nil
;;               :type (or null integer)
;;               :documentation "Currently selected edit idx of the
;;               drawing, if any.  May be nil.  Edit idxs start at
;;               zero and count up."))
;;   :abstract t
;;   :documentation "A drawing which can have its shape edited.")
;; ;; (cl-defgeneric 2dd-build-edited-drawing ((drawing 2dd-editable-drawing) edit-idx (move-vector 2dg-point) (viewport 2dd-viewport))
;; ;;   "Derive an edited drawing from DRAWING, EDIT-IDX (nillable) and MOVE-VECTOR for VIEWPORT.

;; ;; This should only build a new drawing and return it (if possible)
;; ;; and should not mutate anything.  Note: EDIT-IDX can be nil
;; ;; meaning move all the edit-idxs (i.e. just move the whole
;; ;; thing)."
;; ;;   (if edit-idx
;; ;;       (2dd-build-idx-edited drawing edit-idx move-vector viewport)
;; ;;     (2dd-build-move-edited drawing move-vector viewport)))
;; (cl-defgeneric 2dd-build-idx-edited ((drawing 2dd-editable-drawing) (edit-idx integer) (move-vector 2dg-point) (viewport 2dd-viewport))
;;   "Build a drawing based off moving EDIT-IDX of DRAWING by MOVE-VECTOR.

;; This should only build a new drawing and return it (if possible)
;; and should not mutate anything.")

;; (provide '2dd-drawing)
;; ;;; 2dd-drawing.el ends here
