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



(defun 2dd-plot (root-drawing canvas child-fn preserve-drawing-p-fn &optional method)
  "(Re)Plot drawings for all elements on a CANVAS.

Drawings are modified in place.

ROOT-DRAWING is a single root drawing which may contain children.

CANVAS is the area allowed for drawings to be plotted in.

CHILD-FN should produce a list of all child drawings of a given
parent drawing.  It will be called as: (funcall CHILD-FN
ROOT-DRAWING).

PRESERVE-DRAWING-P-FN should produce a non-nil value when a
drawing should be preserved and a nil value when a drawing should
be replotted.  It will be called as (funcall
PRESERVE-DRAWING-P-FN drawing).

METHOD is the optional plotting method to use."
  (cond ((or (null method) (eq method 'simple-grid)) ;default
         (2dd-plot-simple-grid root-drawing canvas child-fn))
        (t (error "Unknown drawing method: %s" method))))


(defun 2dd---simple-grid-dimensions-by-num-children (num-children)
  "Given a NUM-CHILDREN return the dimensions of a grid which can hold them.

It returns a plist of the form (:columns <INT> :rows <INT>)
It will be able to hold at least that many children, possibly more."
  (let ((num-columns (ceiling (sqrt num-child-nodes))))
    (list :columns num-columns
          :rows (ceiling (/ (float num-child-nodes) (float num-columns))))))
(defun 2dd-plot-simple-grid (drawing canvas child-fn preserve-drawing-p-fn &optional force-replot)
  "When force-replot is true it's because some parent drawing got replotted.

When plotting in simple grid mode:
- Rectangles will consume all of their allowable space.
- Points will consume the same space as a rectangle would."
  (if (or force-replot (not (funcall preserve-drawing-p-fn drawing)))
      ;; forced to redraw
      (let ((child-drawings (funcall child-fn drawing)))
        (if (not child-drawings)
            ;; no children: simply replot the drawing
            (2dd-set-from drawing canvas))
        ;; Has children, handle their replotting as well.
        (let* ((inner-canvas (2dd-get-inner-canvas drawing))
               (children-and-relative-coords (mapcar
                                              (lambda (child)
                                                (cons child
                                                      (2dg-relative-coordinates inner-canvas
                                                                                child)))
                                              child-drawings)))
          ;; now resize
          (2dd-set-from drawing canvas)
          (let ((new-inner-canvas (2dd-get-inner-canvas drawing)))
            (mapc (lambda (child-and-relative-coords)
                    (let ((child (car child-and-relative-coords))
                          (relative-coords (cdr child-and-relative-coords)))
                      (2dd-plot-simple-grid drawing
                                            (2dg-absolute-coordinates new-inner-canvas relative-coords)
                                            child-fn
                                            preserve-drawing-p-fn
                                            t)))
                  children-and-relative-coords))))
    ;; Not being forced to redraw
    (unless (2dd-geometry drawing)
      ;; This drawing does not exist it must be plotted.
      (2dd-set-from drawing canvas))

    (let* ((inner-canvas (2dd-get-inner-canvas drawing))
           (child-drawings (funcall child-fn drawing))
           (any-child-without-drawing (some (2dd-complement #'2dd-drawing)
                                            child-drawings)))
      (if any-child-without-drawing
          ;; When any child is missing a drawing the whole set of siblings
          ;; must be replotted.  because this is simple grid plotting we
          ;; will create a simple grid and place each drawing sequentialy
          ;; in a grid cell
          (let* ((grid-dimensions (2dd---simple-grid-dimensions-by-num-children
                                   (length child-drawings)))
                 (grid-cells (2dd-split-grid inner-canvas
                                             (plist-get grid-dimensions :rows)
                                             (plist-get grid-dimensions :columns))))
            ;; for each child, replot it in the grid-cell with the same idx
            HERE!!!



      )
  (mapc (lambda (child-drawing)
          (2dd-plot-simple-grid child-drawing (2dd-geometry child-drawing) child-fn preserve-drawing-p-fn))
        (funcall child-fn drawing)))


;; (cl-defmethod scxml---get-canvas-divisions ((rectangle scxml-drawing-divided-rect) (num-child-nodes integer))
;;   ;; TODO - this should be moved out into the drawing file.

;;   (mapcar (lambda (division)
;;             (let ((sub-rect (cdr division)))
;;               (with-slots (x-min x-max y-min y-max) sub-rect
;;                 (scxml-inner-canvas :x-min x-min
;;                                     :y-min y-min
;;                                     :x-max x-max
;;                                     :y-max y-max
;;                                     :drawing rectangle))))
;;           (scxml-get-divisions rectangle)))

;; (cl-defmethod scxml--plot-node ((element scxml-drawable-element) (canvas scxml-canvas))
;;   "Plot rectangular elements (and any child elements), phase 1 of plotting."
;;   (when (not (or (scxml---is-renderable-as-node element)
;;                  (object-of-class-p element 'scxml-scxml)))
;;     (error "Wat?  shouldn't be calling this with thtat")) ;TODO - remove this check at some point?
;;   (scxml--drawing-logger "scxml--plot-node: type:%s"
;;                           (scxml-xml-element-name element))
;;   (scxml--drawing-logger "scxml--plot-node: canvas: %s" (scxml-print canvas))
;;   (let ((child-nodes (seq-filter 'scxml---is-renderable-as-node (scxml-children element)))
;;         (node (scxml--update-drawing element canvas)))
;;     (scxml--drawing-logger "scxml--plot-node: has-hint: %s" (when (scxml--hint element) t))
;;     (scxml--drawing-logger "\tDrawing: %s" (scxml-print node))
;;     (when child-nodes
;;       (let ((divided-canvases (scxml---get-canvas-divisions node
;;                                                             (length child-nodes))))
;;         (unless divided-canvases
;;           (error "why is this null????"))
;;         ;; (cond ((scxml-parallel-p element)
;;         ;;        (error "Error????"))
;;         ;;       ('t                         ; scxml-state and scxml-scxml
;;         ;;        (let* ((num-child-nodes (length child-nodes))
;;         ;;               (num-columns (ceiling (sqrt num-child-nodes)))
;;         ;;               (num-rows (ceiling (/ num-child-nodes num-columns)))
;;         ;;               (divided-canvases (scxml--split-canvas (scxml-get-inner-canvas node)
;;         ;;                                                      num-rows
;;         ;;                                                      num-columns
;;         ;;                                                      10.0
;;         ;;                                                      4.0)))
;;         ;; TODO: remove this when-error check once you can trust scmxl---get-canvas-divisions.
;;         (cl-loop for child in child-nodes
;;                  while child
;;                  for sub-canvas in divided-canvases
;;                  do (scxml--plot-node child sub-canvas))))))
;; (cl-defmethod scxml--plot-links ((start scxml-element) (canvas scxml-canvas))
;;   "Plot all links for the start element on canvas"

;;   (cl-flet ((make-connector-for
;;              (transition drawing destination-drawing)
;;              (if (null drawing)
;;                  ;; this connector won't be connected, make a dangling connector.
;;                  (scxml-drawing-connector-dangling :point) ;; (2dg-centroid (scxml-element-drawing (scxml-parent transition))))

;;                (let* ((target-point (2dg-centroid (or destination-drawing (scxml-element-drawing (scxml-parent transition)))))

;;                       (edge-enumerator (scxml-leaving-segment-collision-edge drawing target-point)))
;;                  (cond ((object-of-class-p drawing 'scxml-drawing-rect)
;;                         (scxml-drawing-connector-rect :node drawing :edge edge-enumerator))
;;                        ((scxml-drawing-point-p drawing)
;;                         (scxml-drawing-connector-point :node drawing :exit-direction edge-enumerator))
;;                        ('t
;;                         (error "Unknown drawing type, unable to make connector"))))))
;;             (set-dangling-connector-positions
;;              ;; this function will set the location of a dangling connector for target connections only.
;;              (arrow-drawing)
;;              (let ((target-connector (scxml-arrow-target arrow-drawing)))
;;                (when (object-of-class-p target-connector 'scxml-drawing-connector-unconnected)
;;                  (let* ((source-connector (scxml-arrow-source arrow-drawing))
;;                         (exit-direction (scxml-from-node-direction source-connector)))
;;                    (when (null (scxml-dangling-point target-connector))
;;                      ;; no point is set, must set it.
;;                      (let ((exit-vector (2dg-vector-from-direction exit-direction)))
;;                        (scxml-set-to-node-direction target-connector exit-direction)
;;                        (scxml-set-point target-connector (2dg-add
;;                                                           (scxml-connection-point source-connector)
;;                                                           (2dg-scaled exit-vector 2.0))))))))))
;;     (let* ((all-transitions (scxml-collect start (lambda (e) (object-of-class-p e 'scxml-transition))))
;;            (need-drawings (seq-filter (lambda (transition)
;;                                         (or (not (scxml-element-drawing transition))
;;                                             (scxml--drawing-invalid? transition)))
;;                                       all-transitions))
;;            ;; (by-drawing-and-edge nil)
;;            (by-state-and-edge nil)
;;            ;; (by-state (make-hash-table))
;;            )

;;       ;; this mapc will build all arrows but will _not_ fill in the path.
;;       ;; and the connector parametrics won't be set for automatic arrows
;;       ;; (hinted arrows will still use their hinted connector parametrics)
;;       (mapc (lambda (transition)
;;               (cl-flet ((make-arrow
;;                          (source-connector target-connector locked)
;;                          (scxml-arrow :source source-connector
;;                                       :target target-connector
;;                                       :parent transition
;;                                       :highlight (scxml--highlight transition)
;;                                       :edit-idx (scxml--edit-idx transition)
;;                                       :locked locked))
;;                         ;; (collect-unparameterized-connectors2
;;                         ;;  (connector transition drawing)
;;                         ;;  ;; If this connector is connected to a rectangle it must have an
;;                         ;;  ;; edge and a parametric for that edge set.  Parametrics are set
;;                         ;;  ;; later on so I'll need to collect all unparameterized connectors
;;                         ;;  ;; for later assignment.  I'm collecting them by state(element)
;;                         ;;  ;; and edge
;;                         ;;  (when (scxml-drawing-connector-rect-p connector)
;;                         ;;    (let* ((edge (scxml-node-edge connector))
;;                         ;;           (key (cons drawing edge))
;;                         ;;           (cell (assoc key by-state-and-edge)))
;;                         ;;      (if cell
;;                         ;;          (setcdr cell (cons transition (cdr cell)))
;;                         ;;        (setq by-drawing-and-edge
;;                         ;;              (cons (cons key (list transition)) by-drawing-and-edge))))))
;;                         (collect-unparameterized-connectors
;;                          (connector transition element)
;;                          ;; If this connector is connected to a rectangle it must have an
;;                          ;; edge and a parametric for that edge set.  Parametrics are set
;;                          ;; later on so I'll need to collect all unparameterized connectors
;;                          ;; for later assignment.  I'm collecting them by state(element)
;;                          ;; and edge
;;                          (when (scxml-drawing-connector-rect-p connector)
;;                            (let* ((edge (scxml-from-node-direction connector))
;;                                   (key (cons element edge))
;;                                   (cell (assoc key by-state-and-edge)))
;;                              (if cell
;;                                  (setcdr cell (cons transition (cdr cell)))
;;                                (setq by-state-and-edge
;;                                      (cons (cons key (list transition)) by-state-and-edge)))))))
;;                 ;; group transitions by the scxml-states they touch.
;;                 (let ((source (scxml-source transition))
;;                       (target (scxml-target transition)))
;;                   (let ((source-drawing (scxml-element-drawing source))
;;                         (target-drawing (and target (scxml-element-drawing target)))
;;                         (hint (scxml--hint transition)))
;;                     (if hint
;;                         ;; when there's a hint just set up all parts of the connectors
;;                         (oset transition
;;                               drawing
;;                               (make-arrow (scxml-build-arrow-connector (scxml-source hint) source-drawing)
;;                                           (scxml-build-arrow-connector (scxml-target hint) target-drawing)
;;                                           't))
;;                       ;; when there's no hint do not set parametric part of the connectors,
;;                       ;; that'll get handled in a subsequent foreach (played by a maphash)
;;                       ;; place a partially filled drawing here.
;;                       ;; still needs connector parametrics and a path
;;                       (let ((source-connector (make-connector-for transition source-drawing target-drawing))
;;                             (target-connector (make-connector-for transition target-drawing source-drawing)))
;;                         (oset transition
;;                               drawing
;;                               (make-arrow source-connector target-connector 'nil))
;;                         ;; (collect-unparameterized-connectors2 source-connector transition source-drawing)
;;                         ;; (collect-unparameterized-connectors2 target-connector transition target-drawing)
;;                         (collect-unparameterized-connectors source-connector transition source) ;; source-edge)
;;                         (collect-unparameterized-connectors target-connector transition target) ;; target-edge)g
;;                         ))))))
;;             need-drawings)
;;       ;; now set the connector parametrics for unhinted transitions that you collected previously
;;       (mapc (lambda (cell)
;;               (let ((state (caar cell))
;;                     ;; (edge (cdar cell))
;;                     (transitions (cdr cell)))
;;                 (let ((num-transitions (length transitions))
;;                       (idx 0.0))
;;                   ;; set each transitions connector sequetially PER EDGE and in proportion
;;                   ;; this feels like it's leading to an optimization problem.
;;                   (mapc (lambda (transition)
;;                           (incf idx 1.0)
;;                           (let* ((drawing (scxml-element-drawing transition))
;;                                  (connector (if (eq (scxml-source transition) state)
;;                                                 (scxml-arrow-source drawing)
;;                                               (scxml-arrow-target drawing))))
;;                             (oset connector
;;                                   parametric
;;                                   (/ idx (+ 1.0 num-transitions)))))
;;                         transitions))))
;;             by-state-and-edge)
;;       ;; now you should be able to flesh out the paths
;;       ;; and then mark them as valid drawings
;;       (mapc (lambda (transition)
;;               (let ((hint (scxml--hint transition)))
;;                 (set-dangling-connector-positions (oref transition drawing))
;;                 (if hint
;;                     ;; handle hinted paths
;;                     (scxml--set-path-from-hint (oref transition drawing) hint)
;;                   ;; handle automatic paths
;;                   (scxml--arrow-set-default-path (oref transition drawing)))
;;                 (scxml--set-drawing-invalid transition 'nil)))
;;             need-drawings))))
;; (cl-defmethod scxml-plot ((diagram scxml-diagram))
;;   "Plot out DIAGRAM (ensure all drawable elements have valid drawings"
;;   (let ((start-node (float-time))
;;         (start-link 'nil)
;;         (end-time 'nil))
;;     (with-slots (canvas root (element display-element)) diagram
;;       (scxml--plot-node element canvas)
;;       (setq start-link (float-time))
;;       (scxml--plot-links element canvas)
;;       (setq end-time (float-time))
;;       (let ((node-time (- start-link start-node))
;;             (link-time (- end-time start-link)))
;;         (scxml--drawing-logger "scxml-plot (node: %.5f s, link: %.5f s, total: %.5f)"
;;                                 node-time
;;                                 link-time
;;                                 (+ node-time link-time)))))
;;   diagram)
;; (cl-defmethod scxml-draw ((diagram scxml-diagram))
;;   "Render a DIAGRAM in some buffer"
;;   (scxml-plot diagram)
;;   (let ((start-time (float-time)))
;;     (with-slots (canvas root buffer viewport (element display-element)) diagram
;;       (switch-to-buffer buffer)
;;       ;; TODO - don't need both of these, use the scxml--diagram.
;;       (setq-local scxml-draw--diagram diagram)
;;       (setq-local scxml--diagram diagram)
;;       (let ((scratch (scxml--get-scratch viewport)))
;;         ;; erase the buffer
;;         (goto-char (point-min))
;;         (delete-char (- (point-max) (point-min)) nil)

;;         ;; draw states, finals, parallels
;;         (scxml-visit root
;;                      (lambda (e)
;;                        (let ((drawing (scxml-element-drawing e)))
;;                          (scxml--drawing-logger "scxml-draw element:%s\n\tdrawing: %s\n"
;;                                                  (when e (scxml-print e))
;;                                                  (when drawing (scxml-print drawing)))
;;                          (progn
;;                            (when (scxml-drawing-divided-rect-class-p drawing);; (object-of-class-p drawing 'scxml-drawing-divided-rect)
;;                              (scxml--scratch-dividers scratch
;;                                                       viewport
;;                                                       (scxml-dividers drawing)))
;;                            (scxml--scratch-rect scratch
;;                                                 viewport
;;                                                 drawing
;;                                                 ;; if it's a noshell-rect don't draw the actual rectangle
;;                                                 (scxml-drawing-noshell-rect-p drawing)))))
;;                      'scxml---is-renderable-as-rect)

;;         ;; draw non-highlighted transitons first so they aren't obscured by other
;;         ;; non-highlighted transitions
;;         (scxml-visit root
;;                      (lambda (e)
;;                        (scxml--scratch-arrow scratch
;;                                              viewport
;;                                              (scxml-element-drawing e)))
;;                      (lambda (e) (and (object-of-class-p e 'scxml-transition)
;;                                       (not (scxml--highlight e)))))
;;         (scxml-visit root
;;                      (lambda (e)
;;                        (scxml--scratch-point-label scratch
;;                                                    viewport
;;                                                    (scxml-element-drawing e)))
;;                      (lambda (e) (object-of-class-p e 'scxml-initial)))
;;         (scxml-visit root
;;                      (lambda (e)
;;                        (scxml--scratch-arrow scratch
;;                                              viewport
;;                                              (scxml-element-drawing e)))
;;                      (lambda (e) (and (object-of-class-p e 'scxml-transition)
;;                                       (scxml--highlight e))))

;;         (scxml--scratch-write scratch))

;;       (scxml--drawing-logger "scxml-draw %.5f ms" (- (float-time) start-time))
;;       diagram)))

(provide '2dd-plotter)
;;; scxml-draw.el ends here
