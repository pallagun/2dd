;; 2dd-link.el --- A drawing for paths with connectors -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require '2dg)
(require '2dd-viewport)
(require '2dd-link-connector)

(defclass 2dd-link (2dd-editable-drawing)
  ((_source-connector :reader 2dd-get-source-connector
                      :writer 2dd-set-source-connector
                      :type 2dd-link-connector)
   (_target-connector :reader 2dd-get-target-connector
                      :writer 2dd-set-target-connector
                      :type 2dd-link-connector)
   (_inner-path :reader 2dd-get-inner-path
                :initform nil
                :type (or null 2dg-cardinal-path)
                :documentation "These are the points that make up
the _middle_ of the path. The full path uses the start and end
points from the connectors.  So the full path would
be (append (source-point) path (target-point)).")
   (_edit-history :initform nil
                  :type list
                  :documentation "This stores a number of tokens
indicating what types of edits have been made to the link.  These
will be used to change current edit behavior.  The tokens may be
one of: 'source 'target or 'inner-path.  These indicate that a
user has edited one of these components at some point and any
automatic plotting should not alter them." ))
  :documentation "A drawing for a path, optionally connecting
other drawings.  2dd-link objects are not able to serve as parent
drawings for other drawings.")

(defsubst 2dd-link-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-link)."
  (object-of-class-p any '2dd-link))

(cl-defmethod 2dd-set-geometry ((link 2dd-link) (geo-plist list))
  "Set the geometry for LINK to be GEO-PLIST.

GEO-PLIST must be a property list containing :source, :target
and :inner-point properties.

Additionally, if GEO-PLIST contains an :edit-history property
that will be merged into LINKs edit history as well.

No checking is done before geometry is changed."
  (let ((source (plist-get geo-plist :source))
        (target (plist-get geo-plist :target))
        (inner-points (plist-get geo-plist :inner-points))
        (new-edit-idx))
    (unless (and source target)
      (error "2dd-set-geometry (2dd-link) must supply source and target information"))

    (with-slots (_source-connector _target-connector _inner-path) link

      (assert (let ((test-start (2dd--link-connector-fake-point _source-connector
                                                                source))
                    (test-end (2dd--link-connector-fake-point _target-connector
                                                              target)))
                (2dg-is-cardinal-pts-list-p (append (list test-start)
                                                    inner-points
                                                    (list test-end))))
              t "Tried to set a link geometry that was not a cardinal path")
      (let ((edit-idx (2dd-get-edit-idx link)))
        (when edit-idx
          ;; Edit idx mode is enabled (have to add 2 here to handle
          ;; the source + target points).
          (let ((prev-length (+ 2 (2dg-num-points _inner-path)))
                (new-length (+ 2 (length inner-points))))
            (unless (eq prev-length new-length)
              ;; The number of points just changed.  This means you
              ;; might have some strange jumps.  Correct that.
              (cond ((eq edit-idx (1- prev-length))
                     ;; You were on the last idx, you remain there.
                     (setq new-edit-idx (1- new-length)))
                    ((> edit-idx 0)
                     ;; You were somewhere in the middle, find the
                     ;; closest point and go with that.
                     ;; TODO - implement.
                     (setq new-edit-idx (min (1- new-length)
                                             edit-idx))))))))
      (2dd--set-location _source-connector source)
      (2dd--set-location _target-connector target)
      (oset link _inner-path (2dg-cardinal-path :points inner-points))
      ;; (2dd-set-inner-path link inner-points)
      (when new-edit-idx
        (2dd-set-edit-idx link new-edit-idx))
      (oset link _edit-history (union (oref link _edit-history)
                                      (plist-get geo-plist :edit-history)))
      )))

(cl-defmethod 2dd-needs-replot ((link 2dd-link))
  "Return non-nil if LINK needs to be replotted."
  (or (not (2dd-has-location (oref link _target-connector)))
      (not (2dd-has-location (oref link _source-connector)))))

(cl-defmethod make-instance ((class (subclass 2dd-link)) &rest slots)
  "Ensure _source and _target are not unbound."
  (let ((instance (cl-call-next-method)))
    (oset instance _source-connector (2dd-link-connector))
    (oset instance _target-connector (2dd-link-connector))
    instance))
(defsubst 2dd-link-class-p (any)
  "Same as (object-of-class-p ANY '2dd-link)."
  (object-of-class-p any '2dd-link))
(cl-defmethod 2dd-pprint ((link 2dd-link))
  "Pretty print LINK."
  (with-slots (_source-connector _target-connector _inner-path _edit-history) link
    (format "link:([%s]->%s->[%s],hist:%s)"
            (if _source-connector
                (2dd-pprint _source-connector)
              "NONE")
            (if _inner-path
                (2dg-pprint _inner-path)
              "NONE")
            (if _target-connector
                (2dd-pprint _target-connector)
              "NONE")
            _edit-history)))
(cl-defmethod 2dd-get-source ((link 2dd-link))
  "Return the source drawing (if one exists) of LINK."
  (2dd-get-connectee (oref link _source-connector)))
(cl-defgeneric 2dd-set-source ((link 2dd-link) source-drawing)
  "Set the source of LINK to be SOURCE-DRAWING.")
(cl-defmethod 2dd-set-source ((link 2dd-link) source-drawing)
  "Set the source of LINK to be SOURCE-DRAWING."
  (unless (or (null source-drawing)
              (2dd-drawing-class-p source-drawing))
    (error "Unable to set source for a link drawing to anything other than nil or another drawing"))
  ;; TODO - this will need to shuffle the inner path.
  (2dd-set-connectee (oref link _source-connector) source-drawing))
(cl-defmethod 2dd-get-target ((link 2dd-link))
  "Return the target drawing (if one exists) of LINK."
  (2dd-get-connectee (oref link _target-connector)))
(cl-defgeneric 2dd-set-target ((link 2dd-link) target-drawing)
  "Set the target of LINK to be TARGET-DRAWING.")
(cl-defmethod 2dd-set-target ((link 2dd-link) target-drawing)
  "Set the target of LINK to be TARGET-DRAWING.

This function may invalidate the drawing and require the link to
be replotted."
  (unless (or (null target-drawing)
              (2dd-drawing-class-p target-drawing))
    (error "Unable to set target for a link drawing to anything other than nil or another drawing"))
  (2dd-set-connectee (oref link _target-connector) target-drawing))
(cl-defgeneric 2dd-set-inner-path ((link 2dd-link) path)
  "Set the path of LINK to PATH.")
(cl-defmethod 2dd-set-inner-path ((link 2dd-link) (inner-pts list))
  "Set LINK's inner path to be INNER-PTS."
  (2dd-set-inner-path link (2dg-cardinal-path :points inner-pts)))
(cl-defmethod 2dd-set-inner-path ((link 2dd-link) (inner-path 2dg-cardinal-path))
  "Set LINK's inner path to be INNER-PATH."
  (let ((source-pt (2dd-connection-point (oref link _source-connector))))
    (unless (2dg-cardinal-displacement-p source-pt (2dg-start inner-path))
      (error "Inner path must have a cardinal displacement to the source connector")))
  (let ((target-pt (2dd-connection-point (oref link _target-connector))))
    (unless (2dg-cardinal-displacement-p target-pt (2dg-end inner-path))
      (error "Inner path must have a cardinal displacement to the target connector")))
  (oset link _inner-path inner-path))
(cl-defgeneric 2dd-clear-inner-path ((link 2dd-link))
  "Set the inner path of LINK to be nil."
  (oset link _inner-path nil))
(cl-defmethod 2dd-serialize-geometry ((link 2dd-link) &optional additional-info)
  "Serialize LINK to a string.

Returns a stringified list in the form:
(:source <SOURCE-CONECTOR> :target <TARGET-CONNECTOR> :inner-path
<INNER-PATH>)."
  (with-slots (_source-connector _target-connector _inner-path _edit-history) link
    (prin1-to-string
     (list :source (2dd-serialize-geometry _source-connector)
           :target (2dd-serialize-geometry _target-connector)
           :path (2dd-all-link-points link)
           :edit-history _edit-history))))
(cl-defgeneric 2dd-all-link-points ((link 2dd-link) &optional offset)
  "Get the full path of LINK with optional start/end OFFSET from ends.

Return is an ordered list of 2dg-point objects going from start
to end of the LINK.")
(cl-defmethod 2dd-all-link-points ((link 2dd-link) &optional offset)
  "Get the full path of LINK with optional start/end OFFSET from ends.

Return is an ordered list of 2dg-point objects going from start
to end of the LINK.

NOTE: when you give a non-nil offset all bets are off, this
function just 'trys real hard' at that point.  Don't use non-nil
offset for anything except presentation to human eyeballs.

When offset is non-nil path stretching will be done to ensure the
path returned is always cardinal."
  ;; TODO: I'm using clone below because I'm not sure if I'll be
  ;; messing with the connector parameters or not.  establish if I
  ;; need clone and if there should be some convention for a
  ;; function's return.
  (with-slots (_source-connector _target-connector _inner-path) link
    (let ((raw-start (2dd-connection-point _source-connector))
          (raw-end (2dd-connection-point _target-connector)))
      ;; Don't return any points if you don't have any connectors.
      (if (and raw-start raw-end)
          (let ((inner-path-pts (when _inner-path (2dg-points _inner-path))))
            (if (null offset)
                ;; No offset, return the exact path.
                (append (cons raw-start inner-path-pts)
                        (list raw-end))
              ;; offset desired, get as close as possible.
              (let ((start (2dd-connection-point _source-connector offset))
                    (end (2dd-connection-point _target-connector offset)))
                ;; TODO: - a check that should be removed.  check things.
                ;; (unless (scxml-is-cardinal-path? (append (cons start-original path-pts)
                ;;                                          (list end-original)))
                ;;   (error "Not a cardinal path?!"))
                ;; stitch in path stretch routes here.
                (if inner-path-pts
                    ;; do I need to shape up the beginning?
                    (let ((path-start (first inner-path-pts))
                          (path-end (car (last inner-path-pts))))
                      (if (2dg-cardinal-displacement-p start path-start)
                          (setq inner-path-pts (cons start inner-path-pts))
                        (let ((start-path (2dg---path-stretch
                                           (list raw-start path-start)
                                           start
                                           path-start)))
                          (mapc (lambda (pt) (push pt inner-path-pts))
                                (nreverse start-path))))
                      (if (2dg-cardinal-displacement-p path-end end)
                          (append inner-path-pts (list end))
                        (append inner-path-pts
                                (2dg---path-stretch
                                 (list path-end raw-end)
                                 path-end
                                 end))))
                  ;; no inner points.  stretch the originals if needed
                  (if (2dg-cardinal-displacement-p start end)
                      (list start end)
                    (2dg---path-stretch (list raw-start raw-end)
                                        start
                                        end))))))
        ;; No source/target connectors - no points.
        nil))))
(cl-defgeneric 2dd-get-full-path ((link 2dd-link))
  "Return all the link points of LINK as a 2dg-path.

No offsets are allowed.")
(cl-defmethod 2dd-get-full-path ((link 2dd-link))
  "Return all the link points of LINK as a 2dg-path.

No offsets are allowed."
  (2dg-cardinal-path :points (2dd-all-link-points link)))
(cl-defmethod 2dd-simplify ((link 2dd-link) &optional slack-allowance)
  "Attempt to simplify the LINK.

This will modify the LINK's inner path in place."
  (let ((full-path (2dd-all-link-points link)))
    (if (null slack-allowance)
        ;; no slack allowance, use a simple simplify call.
        (oset link _inner-path
              (2dg-cardinal-path :points
                                 (2dg-truncate (2dg-simplified full-path) 1 1)))
      (let ((slack-x)
            (slack-y))
        (cond ((numberp slack-allowance)
               (setq slack-x slack-allowance)
               (setq slack-y slack-allowance))
              ((2dg-point-p slack-allowance)
               (setq slack-x (2dg-x slack-allowance))
               (setq slack-y (2dg-y slack-allowance)))
              (t
               (error "2dd-simplify: slack-allowance must be a number or 2dg-point")))
        (let ((new-inner-points (2dg-truncate
                                 (2dg-simplified
                                  (2dg-slack-simplified full-path slack-x slack-y))
                                 1 1)))
          ;; TODO - another paranoia check, this should be an assert.
          (assert (2dg-is-cardinal-pts-list-p
                   (append (list (2dd-connection-point (oref link _source-connector)))
                           new-inner-points
                           (list (2dd-connection-point (oref link _target-connector)))))
                  t
                  "2dd-simplify for a link returned non-cardinal geometry")
          (oset link _inner-path (2dg-cardinal-path :points new-inner-points)))))))
(defun 2dd--link-build-replot-inner-path (source-pt source-direction target-pt target-direction &optional min-segment-distance min-terminal-segment-distance)
  "Return a valid inner-path.

If source-direction is nil it is assumed to be unconnected and
any direction is ok.  Fi target-direction is nil it is assumed to
be unconnected and any direction is ok."
  (let ((min-segment-distance (or min-segment-distance
                                  1.0))
        (min-terminal-segment-distance (or min-terminal-segment-distance
                                           1.0)))
    (unless (and source-direction target-direction)
      (let* ((displacement (2dg-subtract target-pt source-pt))
             (direction (2dg-coarse-direction displacement)))
        (unless source-direction
          (setq source-direction direction))
        (unless target-direction
          (setq target-direction direction))))

    (2dg-build-path-cardinal source-pt
                             target-pt
                             (2dg-vector-from-direction source-direction)
                             (2dg-vector-from-direction target-direction)
                             min-segment-distance
                             (if source-direction
                                 min-terminal-segment-distance
                               nil)
                             (if target-direction
                                 min-terminal-segment-distance))))

(cl-defmethod 2dd-replot-inner-path ((link 2dd-link) &optional min-segment-distance min-terminal-segment-distance)
  "Replot the inner path of this link based on the cardinal-path plotter."
  ;; TODO - refactor this to use 2dd--link-build-replot-inner-path.
  (let ((min-segment-distance (or min-segment-distance
                                  1.0))
        (min-terminal-segment-distance (or min-terminal-segment-distance
                                           1.0)))
    (with-slots (_source-connector _target-connector) link
      (let* ((source-connected (2dd-get-connectee _source-connector))
             (target-connected (2dd-get-connectee _target-connector))
             (path (2dg-build-path-cardinal
                    (2dd-connection-point _source-connector)
                    (2dd-connection-point _target-connector)
                    (2dg-vector-from-direction
                     (2dd-from-connectee-direction _source-connector))
                    (2dg-vector-from-direction
                     (2dd-to-connectee-direction _target-connector))
                    min-segment-distance
                    (if source-connected
                        min-terminal-segment-distance
                      nil)
                    (if target-connected
                        min-terminal-segment-distance
                      nil)))
             (last-pt-idx (1- (2dg-num-points path))))
        (unless source-connected
          ;; source is unconnected, set the terminal direction based off the path.
          (2dd--set-link-connector-edge _source-connector
                                        (2dg-coarse-direction
                                         (2dg-subtract (2dg-nth path 1)
                                                       (2dg-nth path 0)))))
        (unless target-connected
          ;; target is unconnected, set the terminal direction based off the path.
          (2dd--set-link-connector-edge _target-connector
                                        (2dg-coarse-direction
                                         (2dg-subtract (2dg-nth path (1- last-pt-idx))
                                                       (2dg-nth path last-pt-idx)))))
        (2dd-set-inner-path link (2dg-truncate (2dg-simplify path) 1 1))
        ;; if there's edit history for the inner-path clear it.
        (oset link _edit-history (set-difference (oref link _edit-history)
                                                 '(inner-path)))
        ))))

(cl-defmethod 2dd-render ((link 2dd-link) scratch x-transformer y-transformer viewport &rest args)
  "Render LINK to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

If ARGS is used the first argument must be a plist containing
style information for the drawing.  Accepted plist keys are:

:connector-offset (a 2dg-point describing connector arrow head offsets)
:outline-style (defaults to no style)
:edit-idx-style (defaults to no style)

Overridable method for ecah drawing to render itself."

  (let* ((style-plist (first args))
         (connector-offset (plist-get style-plist :connector-offset))
         (outline-style (plist-get style-plist :outline-style))
         (link-start (plist-get style-plist :link-start))
         (link-end (plist-get style-plist :link-end))
         ;; (connector-offset (2dd-get-point-scaling viewport))
         (points (2dd-all-link-points link connector-offset)))
    (when points
      (let ((first-pt-x (funcall x-transformer (2dg-x (first points))))
            (first-pt-y (funcall y-transformer (2dg-y (first points)))))
        (when link-start
          (error "2dd-render for links needs to implement link start markers"))
        (let ((last-pt-x first-pt-x)
              (last-pt-y first-pt-y)
              (double-pt-x)
              (double-pt-y))
          (cl-loop for pt in (rest points)
                   do (let ((pt-x (funcall x-transformer (2dg-x pt)))
                            (pt-y (funcall y-transformer (2dg-y pt))))
                        (2dd---scratch-line scratch
                                            last-pt-x
                                            last-pt-y
                                            pt-x
                                            pt-y
                                            nil
                                            outline-style)
                        (setf double-pt-x last-pt-x
                              double-pt-y last-pt-y)
                        (setf last-pt-x pt-x
                              last-pt-y pt-y)))
          (when link-end
            (2dd---scratch-set scratch
                               last-pt-x
                               last-pt-y
                               (if (eq link-end 'arrow)
                                   (case (2dd-to-connectee-direction
                                          (oref link _target-connector))
                                     ('up 2dd---arrow-up)
                                     ('down 2dd---arrow-down)
                                     ('left 2dd---arrow-left)
                                     ('right 2dd---arrow-right)
                                     (_ 2dd---arrow-any))
                                 (error "Unknown end link style: %s" link-end))
                               (or (plist-get style-plist :end-style)
                                   outline-style)))
          (when (2dd-get-edit-idx link)
            (cl-loop with edit-idx-style = (plist-get style-plist :edit-idx-style)
                     for point in (2dd-edit-idx-points link)
                     for x-scratch = (funcall x-transformer (2dg-x point))
                     for y-scratch = (funcall y-transformer (2dg-y point))
                     do (2dd---scratch-set scratch
                                           x-scratch
                                           y-scratch
                                           2dd---arrow-any
                                           edit-idx-style)))
          )))))
(cl-defgeneric 2dd--update-plot ((link 2dd-link) (old-parent-canvas 2dd-canvas) (new-parent-canvas 2dd-canvas) child-fn)
  "Update LINK based on a parent drawing or connected drawing changes.

Function will return non-nil when changes are applied and nil
otherwise.

This function should be used when one or both of the connected
drawnings have been changed.

OLD-PARENT-CANVAS is not used but is passed along.

NEW-PARENT-CANVAS is not used but is passed along.

CHILD-FN should produce a list of all child drawings of a given
 drawing.  It will be called as: (funcall CHILD-FN ROOT-DRAWING)."

  ;; if either source or target have changed, update the link.
  ;;
  ;; TODO - if one connector is connected and the other is floating,
  ;; move them both.

  (with-slots (_source-connector _target-connector _inner-path) link
    (let ((last-source-pt (oref _source-connector last-point))
          (source-pt (2dd-connection-point _source-connector))
          (last-target-pt (oref _target-connector last-point))
          (target-pt (2dd-connection-point _target-connector)))
      (when (or (not (2dg-almost-equal last-source-pt source-pt))
                (not (2dg-almost-equal last-target-pt target-pt)))

        ;; If one connector is not connected to anything, move them both.
        (let ((source-connected (2dd-has-connection _source-connector))
              (target-connected (2dd-has-connection _target-connector)))
          (cond ((and source-connected (not target-connected))
                 ;; source is connected, target is not.  force target
                 ;; to mirror source's move.
                 (let ((target-location
                        (2dd--move-location _target-connector
                                            (2dg-add last-target-pt
                                                     (2dg-subtract source-pt
                                                                   last-source-pt)))))
                   (setq target-pt (plist-get target-location :absolute-coord))))
                ((and target-connected (not source-connected))
                 ;; target is connected, source is not, force source
                 ;; to mirror target's move
                 (let ((source-location
                        (2dd--move-location _source-connector
                                            (2dg-add last-source-pt
                                                     (2dg-subtract target-pt
                                                                   last-target-pt)))))
                   (setq source-pt (plist-get source-location :absolute-coord))))))

        (let* ((last-full-pts (append (cons last-source-pt
                                            (2dg-points _inner-path))
                                      (list last-target-pt)))
               (new-full-pts (2dg-stretch last-full-pts source-pt target-pt))
               (new-inner-pts (2dg--path-list-truncate new-full-pts 1 1)))
          (2dd-set-inner-path link (2dg-cardinal-path :points new-inner-pts))))
      (oset _source-connector last-point source-pt)
      (oset _target-connector last-point target-pt)))
  ;; cascade to children.
  (cl-loop for child in (funcall child-fn link)
           do (2dd--update-plot child nil nil child-fn)))
;; (defun 2dd--link-build-edit-geometry-between (source-pt source-connected target-pt target-connected &optional source-edge target-edge)
;;   "Return a new link geometry based on source and target parameters.

;; This function will entirely replot the inner path and return a
;; geometry-location for a link.

;; SOURCE-EDGE is only used if SOURCE-CONNECTED is non-nil.
;; TARGET-EDGE is only used if TARGET-CONNECTED is non nil.

;; This is basically a replot, but only for the inner path."
;;   (let ((path
;;          (2dd--link-build-replot-inner-path source-pt
;;                                             (when source-connected
;;                                               source-edge)
;;                                             target-pt
;;                                             (when target-connected
;;                                               target-edge))
;;   (let ((source-pt (or source-absolute-coord
;;                        (plist-get source-connector-location :absolute-coord)))
;;         (target-pt (or target-absolute-coord
;;                        (plist-get source-connector-location :absolute-coord)))
;;         (source-edge (if (plist-get source-connector-location :relative-coord)
;;                          (plist-get source-connector-location :edge)
;;                        nil))
;;         (target-edge (if (plist-get target-connector-location :relative-coord)
;;                          (plist-get target-connector-location :edge)
;;                        nil)))

;;   ;; get the inner path first
;;   HERE - fix this.
;;   ;; :source (2dd-serialize-geometry
;;   ;;          (oref link _target-connector))
;;   ;; :inner-points (append (butlast current-pts 2)
;;   ;;                       (cdr (2dg-points new-path-piece)))
;;   ;; :target new-target-location)))



;;   )
(defsubst 2dd--build-link-set-unconnected-edges (link-geometry)
  ;; if the target is not relative, adjust its edge to best fit the path leading to it.
  link-geometry
  )
(defsubst 2dd--build-link-replot-inner-path (source-geometry target-geometry edit-history)
  ;; Given a link geometry, rebuild the entire inner path.
  (let ((source-pt (plist-get source-geometry :absolute-coord))
        (target-pt (plist-get target-geometry :absolute-coord))
        (source-edge (plist-get source-geometry :edge))
        (target-edge (plist-get target-geometry :edge))
        ;; TODO - this is not ideal.  Here I'm saying if you have a
        ;; relative coord you must be saying that because you're
        ;; connected to something and if you are connected to
        ;; something then your edge/direction must be important so
        ;; I'll keep it as is.  Fix that.
        (source-requires-direction (plist-get source-geometry :relative-coord))
        (target-requires-direction (plist-get target-geometry :relative-coord)))
    (let* ((displacement (2dg-subtract target-pt source-pt))
           (coarse-direction (2dg-coarse-direction displacement))
           (rough-path (2dg-build-path-cardinal source-pt
                                                target-pt
                                                (2dg-vector-from-direction
                                                 (if source-requires-direction
                                                     source-edge
                                                   coarse-direction))
                                                (2dg-vector-from-direction
                                                 (if target-requires-direction
                                                     (2dg-reverse target-edge)
                                                   coarse-direction))
                                                0.5
                                                (if source-requires-direction
                                                    1.0
                                                  nil)
                                                (if target-requires-direction
                                                    1.0
                                                  nil)))
           (path (2dg-simplify rough-path)))
      (unless source-requires-direction
        ;; reset the direction from the path information for the source.
        ;; If there is no valid direction, default to up
        (plist-put source-geometry
                   :edge
                   (2dg-coarse-direction
                    (or (2dg-start-vector path)
                        (2dg-point :x 1.0 :y 0.0)))))
      (unless target-requires-direction
        ;; reset the direction from the path information for the target.
        ;; if there is no valid direction, default to down.
        (plist-put target-geometry
                   :edge
                   (2dg-coarse-direction
                    (2dg-scaled
                     (or (2dg-end-vector path)
                         (2dg-point :x 1.0 :y 0.0))
                     -1.0))))
      (list :source source-geometry
            :target target-geometry
            :inner-points (2dg-truncate (2dg-points path) 1 1)
            :edit-history edit-history))))
(cl-defgeneric 2dd-build-idx-edited-geometry ((link 2dd-link) (edit-idx integer) (move-vector 2dg-point))
  "Return new geometry based off moving EDIT-IDX of LINK by MOVE-VECTOR.

This should only build a new geometry and return it (if possible)
and should not mutate anything.  When the edit is not possible
this function will return nil.

This function will allow 'closest matches' for the start and ends
of the link.  This means the function request may ask for
geometry with a start point moved 2 units to the right but
recieve a success response and see the start point has only been
moved 1.5 units to the right.  This is expected.

The number of edit-idxs may change.

Constraints: (TODO - respect containment?)
- any inner point may move anywhere
- source and target connectors obey their own constraints
- source and target connectors may not change edges if they are
  locked to that edge or locked."

  (let* (
         ;; This should really be a constant or something variable
         ;; based on drawings or viewport.  This is the minimum
         ;; distance that a link must stay perpendicular to its
         ;; connector's edge.
         (min-perpendicular-distance 1.0)
         (num-edit-idxs (2dd-num-edit-idxs link))
         (current-pts (2dd-edit-idx-points link))
         (current-start (first current-pts))
         (current-end (car (last current-pts)))
         (is-start-idx (eq edit-idx 0))
         (is-end-idx (eq edit-idx (1- num-edit-idxs)))
         (new-source-location)
         (new-target-location)
         (edit-history (oref link _edit-history)))

    (block build-geometry
      ;; move some part of the inner-path
      ;; cases:
      ;; - no change is needed to a terminal point.
      ;; -- Just tack them back on and you've changed your inner path.
      ;; - a terminal point has moved.
      ;; -- The terminal point is locked.
      ;; --- Reject the build request.
      ;; -- The terminal point is unlocked.
      ;; --- do literally anything, who cares.
      ;; --- (for now, just build a cardinal path between them.)
      ;; (when is-start-idx
      ;;   ;; the starting point was moved, this may not be possible but
      ;;   ;; start points are allowed to move as close as they can even
      ;;   ;; if they can't move to the exact requested location.
      ;;   (setq new-source-location (2dd--move-location (oref link _source-connector)
      ;;                                                 (2dg-add current-start move-vector)
      ;;                                                 t
      ;;                                                 t))
      ;;   (if new-source-location
      ;;       ;; Able to get a new source location, adapt move vector to
      ;;       ;; match it as it may not.
      ;;       (let ((new-start (plist-get new-source-location :absolute-coord)))
      ;;         (setq move-vector (2dg-subtract new-start current-start)))
      ;;     ;; Unable to get a decent source location, abort.
      ;;     (return-from build-geometry nil)))
      ;; (when is-end-idx
      ;;   (setq new-target-location (2dd--move-location (oref link _target-connector)
      ;;                                                 (2dg-add current-end move-vector)
      ;;                                                 t
      ;;                                                 t))
      ;;   (if new-target-location
      ;;       ;; Able to get a new source location, adapt move vector to
      ;;       ;; match it as it may not.
      ;;       (let ((new-end (plist-get new-target-location :absolute-coord)))
      ;;         (setq move-vector (2dg-subtract new-send current-end)))
      ;;     ;; Unable to get a decent source location, abort.
      ;;     (return-from build-geometry nil)))


      ;; (let* ((nudged-pts (2dg-nudge-path current-pts edit-idx move-vector))
      ;;        (nudged-start (first nudged-pts))
      ;;        (nudged-end (car (last nudged-pts)))
      ;;        (source-location (2dd-serialize-geometry
      ;;                          (oref link _source-connector)))
      ;;        (target-location (2dd-serialize-geometry
      ;;                          (oref link _target-connector))))
      ;;   (unless (2dg-almost-equal nudged-start current-start)
      ;;     ;; The start point has moved, if it's unconnected
      ;;     ;; that's fine.  If it's connected it may only move
      ;;     ;; along the same edge, no edge changes are allowed.
      ;;     (let* ((new-source-location (2dd--move-location
      ;;                                  (oref link _source-connector)
      ;;                                  nudged-start))
      ;;            (source-edge (2dd--link-connector-location-edge
      ;;                          source-location))
      ;;            (new-source-edge (2dd--link-connector-location-edge
      ;;                              new-source-location)))

      ;;       (when (and (not (eq source-edge new-source-edge))
      ;;                  (not is-start-idx))
      ;;         ;; Edges may not change when editing an inner-point
      ;;         (return-from build-geometry nil))
      ;;       ;; move is ok
      ;;       (setq source-location new-source-location)))
      ;;   (unless (2dg-almost-equal nudged-end current-end)
      ;;     ;; The end point has moved, do something aboeut that.
      ;;     (let* ((new-target-location (2dd--move-location
      ;;                                  (oref link _target-connector)
      ;;                                  nudged-end))
      ;;            (target-edge (2dd--link-connector-location-edge
      ;;                          target-location))
      ;;            (new-target-edge (2dd--link-connector-location-edge
      ;;                              new-target-location)))

      ;;       (when (and (not (eq target-edge new-target-edge))
      ;;                  (not is-end-idx))
      ;;         ;; Edges may not change when editing an inner-point
      ;;         (return-from build-geometry nil))
      ;;       ;; move is ok
      ;;       (setq target-location new-target-location)))
      ;;   (list :source source-location
      ;;         :inner-points (2dg-truncate nudged-pts 1 1)
      ;;         :target target-location))
      (cond ((eq edit-idx 0)
             ;; Move the source connector
             (let* ((source-connector (oref link _source-connector))
                    (new-source-location (2dd--move-location source-connector
                                                             (2dg-add current-start
                                                                      move-vector)
                                                             t
                                                             t)))
               (cond
                ((null new-source-location)
                 ;; Unable to produce a new source location, abort
                 nil)
                ((not (memq 'inner-path edit-history))
                 ;; this link has no inner-path settings, the inner
                 ;; path can simply be replotted.
                 (2dd--build-link-replot-inner-path new-source-location
                                                    (2dd-serialize-geometry
                                                     (oref link _target-connector) t)
                                                    (union '(source) edit-history)))
                (t
                 ;; Otherwise, go.
                 (let ((new-edge (plist-get new-source-location :edge))
                       (old-edge (2dd-from-connectee-direction source-connector))
                       (new-start (plist-get new-source-location :absolute-coord)))
                   (if (not (eq old-edge new-edge))
                       ;; edges have changed, insert a path segment to
                       ;; handle that.  I'm just going to create a new path
                       ;; and stitch it in.
                       (let* ((next-point (second current-pts))
                              (min-segment-distance 1.0) ;for now.
                              (next-next-point (third current-pts))
                              (next-next-point-is-terminal (eq 3
                                                               (length current-pts)))
                              (next-dir (if next-next-point
                                            (2dg-subtract next-next-point next-point)
                                          (2dg-subtract next-point current-start)))
                              (new-path-piece (2dg-build-path-cardinal new-start
                                                                       next-point
                                                                       (2dg-vector-from-direction new-edge)
                                                                       next-dir
                                                                       0.5
                                                                       min-perpendicular-distance
                                                                       nil)))
                         (if next-next-point
                             ;; There was another point, this
                             ;; operation may have introduced a
                             ;; redundant edit idx or otherwise done
                             ;; weird things to the path, simplify the
                             ;; newly created area
                             (let* ((new-pts (2dg-points new-path-piece))
                                     ;; (new-starting-pt (seq-take new-pts 2))
                                    (new-questionable-pts (append (cdr new-pts)
                                                                  (list next-next-point)))
                                    (slack (+ (2dg-box-magnitude move-vector)
                                              min-perpendicular-distance))
                                    (simplified (2dg-simplified
                                                 (2dg-slack-simplified new-questionable-pts
                                                                       slack
                                                                       slack)))
                                    ;; (larger-new-piece (2dg-simplified (2dg-points new-path-piece)
                                    ;;                                   (list next-next-point)))
                                    (new-inner-points (append simplified
                                                              (nthcdr 3 current-pts)))
                                    )
                               (2dd--build-link-set-unconnected-edges
                                (list :edit-history (union '(source) edit-history)
                                      :source new-source-location
                                      :inner-points (if next-next-point-is-terminal
                                                        (nbutlast new-inner-points)
                                                      new-inner-points);; (append simplified
                                      ;; (nthcdr 3 current-pts))
                                      ;; :inner-points (append (cdr larger-new-piece)
                                      ;;                       (nthcdr 3 current-pts))
                                      :target (2dd-serialize-geometry
                                               (oref link _target-connector)))))
                           ;; there was no next-next point so use what you have.
                           (2dd--build-link-set-unconnected-edges
                            (list :edit-history (union '(source) edit-history)
                                  :source new-source-location
                                  :inner-points (append (cdr (2dg-points new-path-piece))
                                                        (nthcdr 2 current-pts))
                                  :target (2dd-serialize-geometry
                                           (oref link _target-connector))))))
                     ;; Edges did not change, this is a normal path
                     ;; change where no additional points will be
                     ;; added/removed and no path construction is
                     ;; needed.
                     (let* (;; (bad-new-path (2dg-stretch current-pts
                            ;;                            new-start)
                            ;;                            current-end))
                            (new-path (2dg-nudge-path current-pts
                                                      0
                                                      (2dg-subtract new-start current-start)))
                            (new-inner-pts (2dg--path-list-truncate new-path 1 1)))
                       (2dd--build-link-set-unconnected-edges
                        (list :edit-history (union '(source) edit-history)
                              :source new-source-location
                              :inner-points new-inner-pts
                              :target (2dd-serialize-geometry
                                       (oref link _target-connector)))))))))))
            ((eq edit-idx (1- num-edit-idxs))
             ;; move the target connector
             (let* ((target-connector (oref link _target-connector))
                    (new-target-location (2dd--move-location target-connector
                                                             (2dg-add current-end
                                                                      move-vector)
                                                             t
                                                             t)))
               (cond
                ((null new-target-location)
                 ;; Unable to move this connector as desired, abort
                 nil)
                ((not (memq 'inner-path edit-history))
                 ;; This link has no inner-path settings, the inner
                 ;; path can simply be replotted.
                 (2dd--build-link-replot-inner-path (2dd-serialize-geometry
                                                     (oref link _source-connector) t)
                                                    new-target-location
                                                    (union '(target) edit-history)))
                (t
                 ;; Else, do whatever was going to happen.
                 (let ((new-edge (plist-get new-target-location :edge))
                       (new-end (plist-get new-target-location :absolute-coord)))
                   (if (not (eq (2dd-from-connectee-direction target-connector)
                                new-edge))
                       ;; edges have changed, insert a path segment to
                       ;; handle that.  I'm just going to create a new path
                       ;; and stitch it in.
                       (let* ((2nd-to-last-point (car (last current-pts 2)))
                              (min-segment-distance 1.0) ;for now.
                              (3rd-to-last-point (if (> (length current-pts) 2)
                                                     (car (last current-pts 3))
                                                   nil))
                              (resume-dir (if 3rd-to-last-point
                                            (2dg-subtract 3rd-to-last-point 2nd-to-last-point)
                                          (2dg-subtract 2nd-to-last-point current-end)))
                              (new-path-piece (2dg-build-path-cardinal 2nd-to-last-point
                                                                       new-end
                                                                       resume-dir
                                                                       (2dg-vector-from-direction (2dg-reverse new-edge))
                                                                       0.5
                                                                       nil
                                                                       1.0)))
                         (if 2rd-to-last-point
                             ;; There was another point, this
                             ;; operation may have introduced a
                             ;; redundant edit idx.  Smooth the path
                             ;; to remove it.
                             (let ((larger-new-piece (2dg-simplified (2dg-points new-path-piece)
                                                                     (list next-next-point))))
                               (2dd--build-link-set-unconnected-edges
                                (list :edit-history (union '(target) edit-history)
                                      :source new-source-location
                                      :inner-points (append (butlast current-pts 3)
                                                            (nthcdr 2 (2dg-points new-path-piece)))
                                      :target (2dd-serialize-geometry
                                               (oref link _target-connector)))))
                               ;; there was no next-next point so use what you have.
                           (2dd--build-link-set-unconnected-edges
                            (list :edit-history (union '(target) edit-history)
                                  :source (2dd-serialize-geometry
                                           (oref link _target-connector))
                                  :inner-points (append (butlast current-pts 2)
                                                        (cdr (2dg-points new-path-piece)))
                                  :target new-target-location))))
                     ;; Edges did not change, this is a normal path
                     ;; change where no additional points will be
                     ;; added/removed and no path construction is
                     ;; needed.
                     (let* (;; (bad-new-path (2dg-stretch current-pts
                            ;;                            new-start)
                            ;;                            current-end))
                            (new-path (2dg-nudge-path current-pts
                                                      (1- num-edit-idxs)
                                                      (2dg-subtract new-end current-end)))
                            (new-inner-pts (2dg--path-list-truncate new-path 1 1)))
                       (2dd--build-link-set-unconnected-edges
                        (list :edit-history (union '(target) edit-history)
                              :source (2dd-serialize-geometry
                                       (oref link _source-connector) t)
                              :inner-points new-inner-pts
                              :target new-target-location)))))))))
            ((and (< 0 edit-idx) (< edit-idx num-edit-idxs))
             ;; move some part of the inner-path
             ;; cases:
             ;; - no change is needed to a terminal point.
             ;; -- Just tack them back on and you've changed your inner path.
             ;; - a terminal point has moved.
             ;; -- The terminal point is locked.
             ;; --- Reject the build request.
             ;; -- The terminal point is unlocked.
             ;; --- do literally anything, who cares.
             ;; --- (for now, just build a cardinal path between them.)
             (let* ((nudged-pts (2dg-nudge-path current-pts edit-idx move-vector))
                    (nudged-start (first nudged-pts))
                    (nudged-end (car (last nudged-pts)))
                    (source-location (2dd-serialize-geometry
                                      (oref link _source-connector)))
                    (target-location (2dd-serialize-geometry
                                      (oref link _target-connector))))
               (unless (2dg-almost-equal nudged-start current-start)
                 ;; The start point has moved, if it's unconnected
                 ;; that's fine.  If it's connected it may only move
                 ;; along the same edge, no edge changes are allowed.
                 (let* ((new-source-location (2dd--move-location
                                              (oref link _source-connector)
                                              nudged-start
                                              t))
                        (source-edge (2dd--link-connector-location-edge
                                      source-location))
                        (new-source-edge (2dd--link-connector-location-edge
                                          new-source-location)))

                   (unless (eq source-edge new-source-edge)
                     ;; Edges may not change when editing an inner-point
                     (return-from build-geometry nil))
                   ;; move is ok
                   (setq source-location new-source-location)))
               (unless (2dg-almost-equal nudged-end current-end)
                 ;; The end point has moved, do something aboeut that.
                 (let* ((new-target-location (2dd--move-location
                                              (oref link _target-connector)
                                              nudged-end
                                              t))
                        (target-edge (2dd--link-connector-location-edge
                                      target-location))
                        (new-target-edge (2dd--link-connector-location-edge
                                          new-target-location)))

                   (unless (eq target-edge new-target-edge)
                     ;; Edges may not change when editing an inner-point
                     (return-from build-geometry nil))
                   ;; move is ok
                   (setq target-location new-target-location)))
               (2dd--build-link-set-unconnected-edges
                (list :edit-history (union '(inner-path) edit-history)
                      :source source-location
                      :inner-points (2dg-truncate nudged-pts 1 1)
                      :target target-location))))
            (t
             (error "2dd-build-idx-edited-geometry: invalid edit idx: %s must be in [0, %d]"
                    edit-idx
                    (1- num-edit-idxs))))
  )))
(cl-defgeneric 2dd--set-geometry-and-update-plot ((link 2dd-link) (new-geometry list) child-fn &optional parent-canvas)
  "Update LINK to have NEW-GEOMETRY and cascade any needed updates to child drawings.

NEW-GEOMETRY should be specified as a plist containing :source, :target and :inner-points keys.

CHILD-FN should produce a list of all child drawings of a given
 drawing.  It will be called as: (funcall CHILD-FN ROOT-DRAWING).

When PARENT-CANVAS is supplied it will not be used but will be
passed on to any child drawings.

Note: this function assumes that constraints are already
validated."
    (2dd-set-geometry link new-geometry)
    (2dd--update-plot-all (funcall child-fn link)
                          parent-canvas
                          parent-canvas
                          child-fn))

(cl-defgeneric 2dd-edit-idx-points ((link 2dd-link))
  "Return an ordered list of edit-idx points for LINK."
  (2dd-all-link-points link))
(cl-defmethod 2dd-edit-idx-point ((link 2dd-link) (idx integer))
  (nth idx (2dd-all-link-points link)))
(cl-defmethod 2dd-num-edit-idxs ((link 2dd-link))
  (+ 2 (2dg-num-points (oref link _inner-path))))



(provide '2dd-link)
;;; 2dd-link.el ends here
