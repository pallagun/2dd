;;; 2dd-plotter --- 2dd plotting functions -*- lexical-binding: t -*-

;;; Commentary:
;; plot... things?

;;; Code:
(require '2dg)
(require '2dd-rect)
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
(defun 2dd--plot-simple-grid (drawing canvas child-fn preserve-drawing-p-fn settings &optional force-replot)
  "When force-replot is true it's because some parent drawing got replotted.

When plotting in simple grid mode:
- Rectangles will consume all of their allowable space.
- Points will consume the same space as a rectangle would."
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
          (2dd-set-from drawing canvas)
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
                     (grid-cells (2dd-split-grid inner-canvas
                                                 (plist-get grid-dimensions :rows)
                                                 (plist-get grid-dimensions :columns)
                                                 (plist-get settings :sibling-margin-horizontal)
                                                 (plist-get settings :sibling-margin-vertical))))
                ;; for each child, replot it in the grid-cell with the same idx
                (cl-loop for child-drawing in child-drawings
                         for grid-cell in grid-cells
                         do (2dd--plot-simple-grid child-drawing grid-cell child-fn preserve-drawing-p-fn settings t)))
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
                                               t)))
                    child-drawings))))
        ;; - no children, no problem.
        ;; - have children at least one is must-replot
        ;; - have children none need replot.

        ;; OLD below.



        ;; (if (and (2dd-geometry drawing)
        ;;        (or force-replot
        ;;            (not (funcall preserve-drawing-p-fn drawing))))
        ;;   ;; forced to redraw but does not currently have existing drawing geometry.
        ;;   (let ((child-drawings (funcall child-fn drawing)))
        ;;     (if (null child-drawings)
        ;;         ;; no child drawings.
        ;;         (progn
        ;;           (2dd-set-from drawing canvas)
        ;;           (2dd-set-padding drawing horizontal-pad vertical-pad))
        ;;       ;; child drawings exist
        ;;       (let* ((inner-canvas (2dd-get-inner-canvas drawing))
        ;;              (children-and-relative-coords (mapcar
        ;;                                             (lambda (child)
        ;;                                               (cons child
        ;;                                                     (2dg-relative-coordinates inner-canvas
        ;;                                                                               (2dd-geometry child))))
        ;;                                             child-drawings)))
        ;;         (2dd-set-from drawing canvas)
        ;;         (2dd-set-padding drawing horizontal-pad vertical-pad)
        ;;         (let ((new-inner-canvas (2dd-get-inner-canvas drawing)))
        ;;           (mapc (lambda (child-and-relative-coords)
        ;;                   (let ((child (car child-and-relative-coords))
        ;;                         (relative-coords (cdr child-and-relative-coords)))
        ;;                     (2dd--plot-simple-grid drawing
        ;;                                           (2dg-absolute-coordinates new-inner-canvas relative-coords)
        ;;                                           child-fn
        ;;                                           preserve-drawing-p-fn
        ;;                                           settings
        ;;                                           t)))
        ;;                 children-and-relative-coords)))))

        ;; ;; Not being forced to redraw
        ;; (unless (2dd-geometry drawing)
        ;;   ;; This drawing does not exist it must be plotted.
        ;;   (2dd-set-from drawing canvas)
        ;;   (2dd-set-padding drawing horizontal-pad vertical-pad))

        ;; ;; Handle child drawings
        ;; (let* ((child-drawings (funcall child-fn drawing))
        ;;        (any-child-without-drawing (some (2dd-complement #'2dd-geometry)
        ;;                                          child-drawings)))
        ;;   (if any-child-without-drawing
        ;;       ;; When any child is missing a drawing the whole set of siblings
        ;;       ;; must be replotted.  because this is simple grid plotting we
        ;;       ;; will create a simple grid and place each drawing sequentialy
        ;;       ;; in a grid cell
        ;;       (let* ((grid-dimensions (2dd---simple-grid-dimensions-by-num-children
        ;;                                (length child-drawings)))
        ;;              (inner-canvas (2dd-get-inner-canvas drawing))
        ;;              (grid-cells (2dd-split-grid inner-canvas
        ;;                                          (plist-get grid-dimensions :rows)
        ;;                                          (plist-get grid-dimensions :columns)
        ;;                                          (plist-get settings :sibling-margin-horizontal)
        ;;                                          (plist-get settings :sibling-margin-vertical))))
        ;;         ;; for each child, replot it in the grid-cell with the same idx
        ;;         (cl-loop for child-drawing in child-drawings
        ;;                  for grid-cell in grid-cells
        ;;                  do (2dd--plot-simple-grid child-drawing grid-cell child-fn preserve-drawing-p-fn settings t)))
        ;;     ;; all children have drawings (which I assume are valid) recurse.
        ;;     (cl-loop for child-drawing in child-drawings
        ;;              do (2dd--plot-simple-grid child-drawing (2dd-geometry child-drawing) child-fn preserve-drawing-p-fn settings)))))
        ))))

(cl-defgeneric 2dd-update-plot ((drawing 2dd-drawing) new-geometry child-fn)
  "Update DRAWING to have NEW-GEOMETRY.")
(cl-defmethod 2dd-update-plot ((drawing 2dd-drawing) new-geometry child-fn)
  "Update DRAWING to have NEW-GEOMETRY.

This function will edit all child (or contained) drawings of
DRAWING to take on the same relative position to DRAWING as they
had before."
  (let ((child-drawings (funcall child-fn drawing))
        (prev-inner-canvas (2dd-get-inner-canvas drawing)))
    (2dd-set-from drawing new-geometry)

    (let ((new-inner-canvas (2dd-get-inner-canvas drawing)))
      (mapc (lambda (child)
              (let* ((relative-coord (2dg-relative-coordinates prev-inner-canvas
                                                               (2dd-geometry child)))
                     (new-absolute-coord (2dg-absolute-coordinates new-inner-canvas
                                                                   relative-coord)))
                (2dd-update-plot child
                                 new-absolute-coord
                                 child-fn)))
            child-drawings))))

(cl-defgeneric 2dd-validate-containment ((parent 2dd-drawing) (sibling-drawings list) changed-geometry)
  "Return non-nil if CHANGED-GEOMETRY is inside of PARENT's inner canvas and does not collide with any SIBLING-DRAWINGS.")
(cl-defmethod 2dd-validate-containment ((parent 2dd-drawing) (sibling-drawings list) changed-geometry)
  "Return non-nil if CHANGED-GEOMETRY is inside of PARENT's inner canvas and does not collide with any SIBLING-DRAWINGS."
  (let ((parent-inner-canvas (2dd-get-inner-canvas parent)))
    (if (not (2dg-contains parent-inner-canvas changed-geometry))
        ;; not contained in the parent, invalid
        nil
      ;; contained in parent, evaluate any collisions with siblings
      (cl-loop for sibling-drawing in sibling-drawings
               for sibling-geometry = (2dd-geometry sibling-drawing)
               when (2dg-has-intersection changed-geometry sibling-drawings 'strict)
                do (cl-return nil)
               finally return t))))




(provide '2dd-plotter)
;;; scxml-draw.el ends here
