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
   (_inner-path :initarg :inner-path
                :reader 2dd-get-inner-path
                :initform nil
                :type (or null 2dg-cardinal-path)
                :documentation "These are the points that make up
the _middle_ of the path. The full path uses the start and end
points from the connectors.  So the full path would
be (append (source-point) path (target-point))."))
  :documentation "A drawing for a path, optionally connecting
other drawings.  2dd-link objects are not able to serve as parent
drawings for other drawings.")

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
  (with-slots (_source-connector _target-connector _inner-path) link
    (format "link:([%s]->%s->[%s])"
            (if _source-connector
                (2dd-pprint _source-connector)
              "NONE")
            (if _inner-path
                (2dg-pprint _inner-path)
              "NONE")
            (if _target-connector
                (2dd-pprint _target-connector)
              "NONE"))))

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
  (2dd-set-connectee (oref link _source-connector) source-drawing))
(cl-defmethod 2dd-get-target ((link 2dd-link))
  "Return the target drawing (if one exists) of LINK."
  (2dd-get-connectee (oref link _target-connector)))
(cl-defgeneric 2dd-set-target ((link 2dd-link) target-drawing)
  "Set the target of LINK to be TARGET-DRAWING.")
(cl-defmethod 2dd-set-target ((link 2dd-link) target-drawing)
  "Set the target of LINK to be TARGET-DRAWING."
  (unless (or (null target-drawing)
              (2dd-drawing-class-p target-drawing))
    (error "Unable to set target for a link drawing to anything other than nil or another drawing"))
  (2dd-set-connectee (oref link _target-connector) target-drawing))
(cl-defgeneric 2dd-set-inner-path ((link 2dd-link) path)
  "Set the path of LINK to PATH.")
(cl-defmethod 2dd-set-inner-path ((link 2dd-link) (inner-path 2dg-cardinal-path))
  "Set LINK's inner path to be INNER-PATH."
  ;; TODO - check here to ensure cardinal directions between connectors???
  (oset link _inner-path inner-path))
(cl-defgeneric 2dd-clear-inner-path ((link 2dd-link))
  "Set the inner path of LINK to be nil."
  (oset link _inner-path nil))
(cl-defmethod 2dd-serialize-geometry ((link 2dd-link) &optional additional-info)
  "Serialize LINK to a string.

Returns a stringified list in the form:
(:source <SOURCE-CONECTOR> :target <TARGET-CONNECTOR> :inner-path
<INNER-PATH>)."
  (with-slots (_source-connector _target-connector _inner-path) link
    (prin1-to-string
     (list :source (2dd-serialize-geometry _source-connector)
           :target (2dd-serialize-geometry _target-connector)
           :path (2dd-all-link-points link)))))
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
                (nconc (cons raw-start inner-path-pts)
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
                                 (list path-end rawend)
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

(cl-defmethod 2dd-render ((link 2dd-link) scratch x-transformer y-transformer viewport &rest args)
  "Render LINK to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

If ARGS is used the first argument must be a plist containing
style information for the drawing.  Accepted plist keys are:

:connector-offset (a 2dg-point describing connector arrow head offsets)
:outline-style (defaults to no style)
:edit-idx-style (defaults to no style)

Overridable method for ecah drawing to render itself."

  (let* ((style-list (first args))
         (connector-offset (2dd-get-point-scaling viewport))
         (points (2dd-all-link-points link connector-offset)))
    (when points
      (let ((last-pt-x (funcall x-transformer (2dg-x (first points))))
            (last-pt-y (funcall y-transformer (2dg-y (first points))))
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
                                          nil)
                      (setf double-pt-x last-pt-x
                            double-pt-y last-pt-y)
                      (setf last-pt-x pt-x
                            last-pt-y pt-y)))
        ))))

(provide '2dd-link)
;;; 2dd-link.el ends here
