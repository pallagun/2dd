;;; 2dd-plotter --- 2dd plotting functions -*- lexical-binding: t -*-

;;; Commentary:
;; plot... things?

;;; Code:
(require '2dg)
(require '2dd-rect)
(require '2dd-division-rect)
(require '2dd-link)
(require '2dd-point)
(require '2dd-util)

;; "rectangles and nodes first, then arrows approach."
;; for plotting, you should supply a tree's root node
;; 1) go through all rect and point renderabel items and divide them out.
;; 2) handle the arrows.

;; I don't think I need hints, think I only need an invalid flag.
;; no need - inserting a new element by mouse -
;; no need - editing/moving an element by mouse or kbd
;; no need - deleting an element
;; no need - moving/shrinking/growing a parent element. - can apply same shrink to children.
;; no need - programatic addition of a node - can simply invalidate all siblings.
;;
;; will need a valid/invalid mechanism.
;; will need to retain the last valid drawing for resizing all children.
;; - I think I should get that for free since that's what the plotter will be fed.
;;
;; I think I need a hint (or some type of parent-relative coordinates) because without that
;; the plotting could get out of hand.
(cl-defgeneric 2dd-plot ((drawing 2dd-drawing) (canvas 2dd-canvas) child-fn preserve-drawing-p-fn &optional settings)
  ;; TODO - preserve-drawing-p-fn should really be replot-drawing-p-fn.  I'm having to negate it everywhere.
  "Plot DRAWING on CANVAS.")
(cl-defmethod 2dd-plot ((root-drawing 2dd-drawing) (canvas 2dd-canvas) child-fn preserve-drawing-p-fn &optional settings)
  "(Re)Plot drawings for all elements on a CANVAS.

Drawings are modified in place.

DRAWINGS should be a list of 2dd-drawing elements.

CANVAS is the area allowed for drawings to be plotted in.

CHILD-FN should produce a list of all child drawings of a given
parent drawing.  It will be called as: (funcall CHILD-FN
ROOT-DRAWING).

PRESERVE-DRAWING-P-FN should produce a non-nil value when a
drawing should be preserved and a nil value when a drawing should
be replotted.  It will be called as (funcall
PRESERVE-DRAWING-P-FN drawing).

SETTINGS is an optional plist of settings including the keys:
':method - 'simple-grid is default

simple-grid keys:
':sibling-margin-vertical
':sibling-margin-horizontal
':inner-padding-horizontal
':inner-padding-vertical

Padding and margin are used as they are in <html> styles.  Margin
is external to the element, Padding is internal."
  (unless (functionp child-fn)
    (error "Child-fn must be a function"))
  (unless (functionp preserve-drawing-p-fn)
    (error "Preserve-drawing-p-fn must be a function"))
  (let ((method (or (plist-get settings :method)
                    'simple-grid)))
    (cond ((eq method 'simple-grid)
           (2dd--plot-simple-grid root-drawing canvas child-fn preserve-drawing-p-fn settings))
          (t (error "Unknown drawing method: %s" method)))))

(defun 2dd---simple-grid-dimensions-by-num-children (num-children)
  "Given a NUM-CHILDREN return the dimensions of a grid which can hold them.

It returns a plist of the form (:columns <INT> :rows <INT>)
It will be able to hold at least that many children, possibly more."
  (let ((num-columns (ceiling (sqrt num-children))))
    (list :columns num-columns
          :rows (ceiling (/ (float num-children) (float num-columns))))))
(defun 2dd--plot-simple-grid (drawing canvas child-fn preserve-drawing-p-fn settings &optional force-replot parent-inner-canvas)
  "When force-replot is true it's because some parent drawing got replotted.

When plotting in simple grid mode:
- Rectangles will consume all of their allowable space.
- Points will consume the same space as a rectangle would.

This function will stop decending to child drawings when it
encounters a 2dd-link type drawing."
  (unless (2dd-link-class-p drawing)
    (let ((horizontal-pad (or (plist-get settings :inner-padding-horizontal) 0.0))
          (vertical-pad (or (plist-get settings :inner-padding-vertical) 0.0)))
      (cl-flet ((inner-pad (lambda (rect)
                             (if (not (or horizontal-pad vertical-pad))
                                 rect     ;no padding, just return rect as it is.
                               (with-slots (x-min x-max y-min y-max) rect
                                 (2dg-rect :x-min (+ x-min horizontal-pad)
                                           :x-max (- x-max horizontal-pad)
                                           :y-min (+ y-min vertical-pad)
                                           :y-max (- y-max vertical-pad)))))))

        ;; TODO - if the padding has changed: must replot too.  Or I guess
        ;; not replot.  the past plotting settings could be sticky and
        ;; only updated when a drawing is forced to replot.  That would
        ;; require, eventally, some type of update-settings-only behavior

        (let* ((child-drawings (funcall child-fn drawing))
               (has-geometry (2dd-geometry drawing))
               (must-replot (or force-replot
                                (not has-geometry)
                                (not (funcall preserve-drawing-p-fn drawing))))
               (prev-inner-canvas (and has-geometry (2dd-get-inner-canvas drawing))))

          (when must-replot
            (2dd-set-from drawing canvas parent-inner-canvas)
            (2dd-set-padding drawing horizontal-pad vertical-pad))

          (when child-drawings
            (if (or (not prev-inner-canvas)
                    (some (2dd-complement #'2dd-geometry)
                          child-drawings)
                    (some (lambda (child)
                            (not (funcall preserve-drawing-p-fn child)))
                          child-drawings))
                ;; Parent is new or at least one child must be
                ;; replotted, therefore all children must be replotted.
                (let* ((grid-dimensions (2dd---simple-grid-dimensions-by-num-children
                                         (length child-drawings)))
                       (inner-canvas (2dd-get-inner-canvas drawing))
                       (is-division-rect (2dd-division-rect-class-p drawing))
                       ;; TODO - I think the above + below lines
                       ;; indicate 2dd-split-grid should accept the
                       ;; drawing as an argument to be generalized on.
                       (grid-cells (2dd-split-grid inner-canvas
                                                   (plist-get grid-dimensions :rows)
                                                   (plist-get grid-dimensions :columns)
                                                   (if is-division-rect nil (plist-get settings :sibling-margin-horizontal))
                                                   (if is-division-rect nil (plist-get settings :sibling-margin-vertical)))))
                  ;; for each child, replot it in the grid-cell with the same idx
                  (when is-division-rect
                    (2dd-set-divisions-absolute drawing grid-cells))
                  (cl-loop for child-drawing in child-drawings
                           for division-idx from 0 to (1- (length child-drawings))
                           for grid-cell in grid-cells
                           do (progn
                                (2dd--plot-simple-grid child-drawing
                                                       (if is-division-rect
                                                           (2dd--build-relative-division-rect-reference drawing division-idx)
                                                         grid-cell)
                                                       child-fn
                                                       preserve-drawing-p-fn
                                                       settings
                                                       t
                                                       inner-canvas)
                                ;; (when (2dd-with-parent-relative-location-class-p child-drawing)
                                ;;   (2dd-set-relative-geometry child-drawing
                                ;;                              (2dg-relative-coordinates inner-canvas
                                ;;                                                        (2dd-geometry child-drawing))))
                                )))
              ;; every child drawing exists and all of them should be
              ;; preserved.  The parent has a previous inner canvas, use
              ;; that to carry over relative layout
              (let ((new-inner-canvas (2dd-get-inner-canvas drawing)))
                (mapc (lambda (child)
                        (let* ((relative-coord (2dg-relative-coordinates prev-inner-canvas
                                                                         (2dd-geometry child)))
                               (new-absolute-coord (2dg-absolute-coordinates new-inner-canvas
                                                                             relative-coord)))
                          (2dd--plot-simple-grid child
                                                 new-absolute-coord
                                                 child-fn
                                                 preserve-drawing-p-fn
                                                 settings
                                                 t
                                                 new-inner-canvas)))
                      child-drawings)))))))))

(cl-defgeneric 2dd-update-plot ((drawing 2dd-drawing) new-geometry child-fn &optional parent-drawing sibling-drawings)
  "Update DRAWING to have NEW-GEOMETRY.

This function should be called when a user desires that DRAWING
changes to have the geometry of NEW-GEOMETRY.  Returns non-nil on
sucessful update, nil on failure.

CHILD-FN should produce a list of all child drawings of a given
parent drawing.  It will be called as: (funcall CHILD-FN
ROOT-DRAWING).

PARENT-DRAWING and SIBLING-DRAWINGS are used to ensure any
drawing constraints are not invalidated by the NEW-GEOMETRY.
PARENT-DRAWING will be used to ensure that NEW-GEOMETRY is
entirely contained if the drawing is set to have such a
constraint.  SIBLING-DRAWINGS will be used to ensure that
NEW-GEOMETRY does not have any collision if the drawing is set to
have such a constraint.

This function wil alter all child drawings of DRAWING as needed
to maintain relative positions and enforce desired constraints.")
(cl-defmethod 2dd-update-plot ((drawing 2dd-drawing) new-geometry child-fn &optional parent-drawing sibling-drawings)
  "Update DRAWING to have NEW-GEOMETRY.

This function should be called when a user desires that DRAWING
changes to have the geometry of NEW-GEOMETRY.  Returns non-nil on
sucessful update, nil on failure.

CHILD-FN should produce a list of all child drawings of a given
parent drawing.  It will be called as: (funcall CHILD-FN
ROOT-DRAWING).

PARENT-DRAWING and SIBLING-DRAWINGS are used to ensure any
drawing constraints are not invalidated by the NEW-GEOMETRY.
PARENT-DRAWING will be used to ensure that NEW-GEOMETRY is
entirely contained if the drawing is set to have such a
constraint.  SIBLING-DRAWINGS will be used to ensure that
NEW-GEOMETRY does not have any collision if the drawing is set to
have such a constraint.

This function will alter all child drawings of DRAWING as needed
to maintain relative positions and enforce desired constraints."
  (if (2dd--validate-containment (2dd-get-constraint drawing)
                                 parent-drawing
                                 sibling-drawings
                                 new-geometry)
      ;; new geometry is validated - allow the update
      (2dd--set-geometry-and-plot-update drawing
                                         new-geometry
                                         child-fn
                                         (when parent-drawing
                                           (2dd-get-inner-canvas parent-drawing)))))

;; TODO - update plot has to be hung on the drawings,
;; (defun 2dd--update-plot-worker (drawing new-geometry child-fn parent-canvas &optional force-set)
;;   "Update child drawings after their parent is modified."
;;   (let ((child-drawings (funcall child-fn drawing))
;;         (prev-inner-canvas (2dd-get-inner-canvas drawing)))

;;     ;; Update this drawing, do it via relative parent change if possible.
;;     ;; otherwise resort to absolute change.
;;     (if force-set
;;         ;; force set is true, simply set it.
;;         (2dd-set-from drawing new-geometry parent-canvas)
;;       ;; not forcing, try to handle via parent change and fall back to setting.
;;       (unless (2dd-handle-parent-change drawing parent-canvas)
;;         (2dd-set-from drawing new-geometry parent-canvas)))

;;     (if child-drawings
;;         (let ((all-ok (lambda (update-success-list)
;;                         ;; TODO - find out why I can't (apply 'and (list-of-t-or-nil))
;;                         (eval (cons 'and update-success-list))))
;;               (new-inner-canvas (2dd-get-inner-canvas drawing)))
;;           (if (2dd-division-rect-class-p drawing)
;;               ;; division rect - all child drawings are controlled by the division rect
;;               (let ((divisions (2dd-get-divisions-absolute drawing)))
;;                 (cl-loop with results = nil
;;                          for child in child-drawings
;;                          for division in divisions
;;                          do (push (2dd--update-plot-worker child
;;                                                            division
;;                                                            child-fn
;;                                                            new-inner-canvas
;;                                                            t)
;;                                   results)
;;                          finally return (funcall all-ok results)))
;;             ;; normal rectangle child, use relative positions
;;             ;; report success as the (and of all your child successes).
;;             (funcall all-ok
;;                      (mapcar (lambda (child)
;;                                (when (not (2dd-link-class-p child))
;;                                  (let* ((relative-coord (or (2dd-get-relative-geometry child)
;;                                                             (2dg-relative-coordinates prev-inner-canvas
;;                                                                                       (2dd-geometry child))))
;;                                         (new-absolute-coord (2dg-absolute-coordinates new-inner-canvas
;;                                                                                       relative-coord)))
;;                                    (2dd--update-plot-worker child
;;                                                             new-absolute-coord
;;                                                             child-fn
;;                                                             new-inner-canvas))))
;;                              child-drawings))))
;;       ;; When there are no children, report success.
;;       t)))

(cl-defgeneric 2dd-validate-constraints ((drawing 2dd-drawing) (parent 2dd-drawing) (siblings list))
  "Validate that DRAWING satisfies all constrants relative to PARENT and SIBLINGS.

PARENT must be the parent of DRAWING and must exist.  SIBLINGS
must be a list of 2dd-drawing objects and may be empty.")
(cl-defmethod 2dd-validate-constraints ((drawing 2dd-drawing) (parent 2dd-drawing) (siblings list))
  "Validate that DRAWING satisfies all constrants relative to PARENT and SIBLINGS.

PARENT must be the parent of DRAWING and must exist.  SIBLINGS
must be a list of 2dd-drawing objects and may be empty."
  (2dd--validate-containment (2dd-get-constraint drawing)
                             parent
                             siblings
                             (2dd-geometry drawing)))

(defun 2dd--validate-containment (constraint parent-drawing sibling-drawings changed-geometry)
  "Return non-nil if CHANGED-GEOMETRY is inside of PARENT-DRAWING's inner canvas and does not collide with any SIBLING-DRAWINGS.

PARENT-DRAWING may be nil, sibling-drawings may be nil."
  ;; TODO - this should pay attention to the drawing containment flags but currently is not.
  ;; It should have a 'contained flag for checking containment within parent and an 'exclusive flag for checking non-intersection with siblings.
  (cond ((eq constraint 'captive+exclusive)
         (and (2dd--validate-parent-containment parent-drawing changed-geometry)
              (2dd--validate-exclusive sibling-drawings changed-geometry)))
        ((eq constraint 'captive)
         (2dd--validate-parent-containment parent-drawing changed-geometry))
        ((eq constraint 'exclusive)
         (2dd--validate-exclusive sibling-drawings changed-geometry))
        (t
         t)))
(defun 2dd--validate-parent-containment (parent-drawing child-geometry)
  "Return non-nil if CHILD-GEOMETRY is contained within PARENT-DRAWING, nil otherwise."
  (if parent-drawing
      ;; You have a parent drawing, check for containment
      (let ((parent-inner-canvas (2dd-get-inner-canvas parent-drawing))
            ;; child geometry may come as a normal rectangle or (in
            ;; the case of a division-rect as the first element of a
            ;; list.
            (child-outer-shell (cond ((and (recordp child-geometry)
                                           (2dg-rect-class-p child-geometry))
                                      child-geometry)
                                     ((2dg-rect-class-p (first child-geometry))
                                      (first child-geometry))
                                     (t (error "Unable to determine outer shell from geometry")))))
        (if (and parent-inner-canvas
                 (2dg-contains parent-inner-canvas child-outer-shell))
            t
          nil))
    ;; No parent drawing provided, unable to validate constraint, assume failure
    nil))
(defun 2dd--validate-exclusive (sibling-drawings changed-geometry)
  "Return non-nil if CHANGED-GEOMETRY does not intersect (in any way) with sibling-drawings, nil otherwise.

Note: for division rects CHANGED-GEOMETRY will be a list where
the first element contains the outer shell."
  (let ((shell (if (listp changed-geometry)
                   (first changed-geometry)
                 changed-geometry)))
    (cl-loop for sibling-drawing in sibling-drawings
             for sibling-geometry = (2dd-geometry sibling-drawing)
             when (2dg-has-intersection shell sibling-geometry 'strict)
             do (cl-return nil)
             finally return t)))


(provide '2dd-plotter)
;;; scxml-draw.el ends here
