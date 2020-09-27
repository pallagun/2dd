;;; 2dd-link-connector -- connector for links to other drawings -*- lexical-binding: t -*-

;;; Commentary:
;; A connector is a drawing object that's used to indicate two (or
;; one) other drawing object(s) that are connected and how they are
;; connected.

(require '2dg)
(require '2dd-rect)

;;; Code:
(defclass 2dd-link-connector ()
  ((connectee :initarg :connectee
              ;; TODO - set-connectee should first ensure there is no
              ;; connectee reference in the location.
              :initform nil
              :reader 2dd-get-connectee
              :writer 2dd-set-connectee
              :type (or null 2dd-drawing)
              :documentation "What drawing this connector
              connects to if it connects to one at all.")
   (location :initarg :location
             :initform nil
             :type list
             :documentation "Where this connector is.")
   ;; TODO rename this without the underscore in front, that's weird.
   ;; It happened for debugging reasons but is now no longer needed.
   (_last-point :initform nil
                :type (or null 2dg-point)
                :reader 2dd-get-last-point
                :writer 2dd-set-last-point
                :documentation "Wherever the actual connection
               point was last.  This is used to track how a
               connection point has changed when there is a
               change."))
  :documentation "Describes a connection point, possibly to another drawing.")

(cl-defmethod make-instance ((class (subclass 2dd-link-connector)) &rest slots)
  "Ensure all location parameters are properly set if supplied."
  ;; TODO - should this be an :after method?  It's not really doing
  ;; any validation just ensuring the lambdas are setup.
  (let ((connector (cl-call-next-method)))
    (let ((location (plist-get slots :location))
          (relative-coord)
          (edge)
          (absolute-coord)
          (connectee (plist-get slots :connectee)))
      (when location
        (setf relative-coord (plist-get location :relative-coord)
              edge (plist-get location :edge)
              absolute-coord (plist-get location :absolute-coord)))
      (cond ((and connectee
                  edge
                  relative-coord
                  (2dd-rect-class-p connectee))
             ;; connecting to a rect drawing.
             (plist-put (oref connector location)
                       :lambda (2dd--link-connector-build-rect-relative-lambda
                                edge
                                relative-coord)))
            ((and connectee
                  edge
                  (not relative-coord)
                  (2dd-point-class-p connectee))
             ;; connecting to a point drawing
             (plist-put (oref connector location)
                        :lambda (2dd--link-connector-build-point-relative-lambda)))))
    connector))

(cl-defmethod 2dd-pprint ((connector 2dd-link-connector))
  "Pretty print CONNECTOR."
  (with-slots (connectee location _last-point) connector
    (format "cn:to:%s, by[%s], last[%s]"
            (if connectee
                (2dd-pprint connectee)
              "UNCONNECTED")
            (2dd-serialize-geometry connector)
            (if _last-point
                (2dg-pprint _last-point)
              nil))))

(cl-defmethod 2dd-set-connectee :before ((this 2dd-link-connector) value)
  ;; If you're going to be setting the connectee to something new I'll
  ;; need to clear out location info
  (let ((current-connectee (oref this connectee)))
    (unless (eq current-connectee value)
      (oset this location nil))))

(defun 2dd--link-connector-fake-point (connector fake-location)
  "Return the connection point of CONNECTOR if it had FAKE-LOCATION."
  (let ((connectee (oref connector connectee))
        (edge (plist-get fake-location :edge))
        (relative-coord (plist-get fake-location :relative-coord)))
    (if (and edge relative-coord)
        ;; use the connectee edge and relative coord.
        (2dd--rect-edge-point connectee
                              edge
                              relative-coord)
      ;; Use the aboslute coord if you have it.
      (plist-get fake-location :absolute-coord))))

(cl-defgeneric 2dd-connection-point ((connector 2dd-link-connector) &optional offset)
  "Get the location of this CONNECTOR.

Optionally offset by OFFSET to render for human eyes."
  (with-slots (connectee location) connector
    (let ((coord-lambda (plist-get location :lambda)))
      (if coord-lambda
          (let ((raw-point (funcall coord-lambda connectee)))
            (if offset
                (let* ((offset-direction (plist-get location :edge))
                       (offset-vector (2dg-vector-from-direction offset-direction))
                       (offset (2dg-scaled offset-vector offset)))
                  (2dg-add raw-point offset))
              raw-point))
        ;; no lambda, maybe there is an absolute coord.
        (let ((absolute-coord (plist-get location :absolute-coord)))
          (if absolute-coord
              absolute-coord
            ;; No connection point right now.
            nil))))))
(cl-defgeneric 2dd-to-connectee-direction ((connector 2dd-link-connector))
  "Get the direction from the CONNECTOR to the connected drawing.

This will return one of: 'up, 'down, 'left or 'right."
  (let ((edge (plist-get (oref connector location) :edge)))
    (2dg-reverse edge)))
(cl-defgeneric 2dd-from-connectee-direction ((connector 2dd-link-connector))
  "Get the direction from the connected drawing to CONNECTOR.

This will return one of: 'up, 'down, 'left or 'right.

This should be the exact opposite of 2dd-to-connectee-direction"
  (let ((edge (plist-get (oref connector location) :edge)))
    edge))

(cl-defgeneric 2dd-has-location ((connector 2dd-link-connector))
  "Return t if CONNECTOR has a set location, nil otherwise.")
(cl-defmethod 2dd-has-location ((connector 2dd-link-connector))
  "Return t if CONNECTOR has a set location, nil otherwise."
  (and (oref connector location) t))

(defsubst 2dd--link-connector-location-edge (location)
  "Return the edge enumerator from this location if it exists.

Return nil otherwise."
  (plist-get location :edge))
(cl-defmethod 2dd-get-edge ((connector 2dd-link-connector))
  "Return the edge enumerator of this location if it's being used.

Will return nil when this connector is not using edge relative
positioning."
  (2dd--link-connector-location-edge (oref connector location)))
(defsubst 2dd--set-link-connector-edge (connector edge)
  "Set the edge of this CONNECTOR to be EDGE."
  (plist-put (oref connector location) :edge edge))

(cl-defgeneric 2dd-has-connection ((connector 2dd-link-connector))
  "Return t if CONNECTOR has a connection to another drawing, nil otherwise.")
(cl-defmethod 2dd-has-connection ((connector 2dd-link-connector))
  "Return t if CONNECTOR has a connection to another drawing, nil otherwise."
  (and (oref connector connectee) t))

(cl-defgeneric 2dd-disconnect ((connector 2dd-link-connector))
  "Disconnect the CONNECTOR from its connectee and leave the connector where it is.")
  
(cl-defmethod 2dd-disconnect ((connector 2dd-link-connector))
  "Disconnect the LINK from the target drawing and leave the connector where it is."
  (let ((current-location (2dd-connection-point connector)))
      (2dd-set-connectee connector nil)
      (2dd--set-location connector `(:absolute-coord ,current-location))))

(defsubst 2dd--copy-plist-remove (plist-to-copy removal-keys)
  "Return a copy of PLIST-TO-COPY with REMOVAL-KEYS missing."
  (cl-loop for elt in plist-to-copy
           with skip-next = nil
           with accumulator = nil
           do (if (memq elt removal-keys)
                  (setq skip-next t)
                (if skip-next
                    (setq skip-next nil)
                  (setq accumulator (cons elt accumulator))))
           finally return (nreverse accumulator)))
(cl-defmethod 2dd-serialize-geometry ((connector 2dd-link-connector) &optional additional-info)
  "Serialize the parameters of CONNECTOR to a list.

When additional-info is non-nil the absolute coordinate of the
connector will be added to the serialized geometry even if the
connector is relative."
  (let ((geometry (2dd--copy-plist-remove (oref connector location) '(:lambda))))
    (if additional-info
        (cons :absolute-coord
              (cons (2dd-connection-point connector 0) geometry))
      geometry)))

(defsubst 2dd--link-connector-build-rect-relative-lambda (edge relative-coord)
  "Return a lambda for producing a point from a connectee rectangle based on relative geometry."
  (lambda (rect-drawing)
    (2dd--rect-edge-point rect-drawing
                          edge
                          relative-coord)))
(defsubst 2dd--link-connector-build-point-relative-lambda ()
  "Return a lambda for producing a point from a connectee point based on relative geometry (which there is none of since it's a point)."
  (lambda (point-drawing)
    ;; Because I don't want a reference to the drawing point, I want
    ;; the value of the drawing's point.  TODO: is there some better
    ;; way to do this?
    (clone
     (2dd-geometry point-drawing))))
(cl-defgeneric 2dd-clear-location ((connector 2dd-link-connector))
  "Clear out CONNECTOR's location information.

This should only be done before triggering a replot.")
(cl-defmethod 2dd-clear-location ((connector 2dd-link-connector))
  "Clear out CONNECTOR's location information."
  (oset connector location nil)
  (oset connector _last-point nil))

(cl-defgeneric 2dd--set-location ((connector 2dd-link-connector) location)
  "Set the CONNECTOR's location to be LOCATION.")
(cl-defmethod 2dd--set-location ((connector 2dd-link-connector) (location list))
  "Set the CONNECTOR's location to be LOCATION.

This function will prefer relative locations if both relative and
absolute locations are specified.

Location should be a plist and may containt the following keys:

:edge (used for rectangle connections, may not be used with
to-connectee-direction)

:relative-coord (used for connections along a segment,
e.g. rectangle edge)

:absolute-coord (used for a connector that has no connectee and
is simply floating).  Note - when provided with an edge and
relative coord the absolute-coord will be ignored.
"
  ;; track the last connection point
  (let ((connectee (oref connector connectee))
        (edge (plist-get location :edge))
        (absolute-coord (plist-get location :absolute-coord))
        ;; default the relative-coord to the middle when not found.
        (relative-coord (plist-get location :relative-coord)))

    (unless (or edge absolute-coord)
      (error "Edge or absolute coordinate required to form connection."))
    (when (and relative-coord
               (not (2dd-rect-class-p connectee))
               (not (2dd-point-class-p connectee)))
      (error "Unable to set relative-coordinates for connection to a non-rectangle"))

    (oset connector
          location
          (cond ((and edge (2dd-point-class-p connectee))
                 (list :lambda (2dd--link-connector-build-point-relative-lambda)
                       :edge edge))

                ((and edge relative-coord (2dd-rect-class-p connectee))
                 ;; connect to connectee's edge
                 (list :lambda (2dd--link-connector-build-rect-relative-lambda
                                edge relative-coord)
                       :edge edge
                       :relative-coord relative-coord))
                (absolute-coord
                 ;; Connect to nothing at an arbitrary point
                 (let ((edge (or edge 'up)))
                   (list :absolute-coord absolute-coord
                         :edge edge)))
                (t
                 (error "2dd--set-location: Invalid location list: %s" location ))))
    (oset connector _last-point (2dd-connection-point connector))))

(cl-defgeneric 2dd--move-location ((connector 2dd-link-connector) location &optional test-only allow-closest-match)
  "Try to move CONNECTOR to the new LOCATION.

Return the new location when complete or nil if the move is not
possible.  The location will be a plist containing valid location
data (for calls to 2dd--set-location) and will always have an
:absolute-coord key even if relative which will contain the
resulting location after the move.

When TEST-ONLY is non-nil the connector wil not be moved but all
calculations for the movement will be executed.

When ALLOW-CLOSEST-MATCH is non-nil the move operation will
succeed even if it can not reach LOCATION exactly.  The move will
get as close as possible to LOCATION given connection
constraints.")
(cl-defmethod 2dd--move-location ((connector 2dd-link-connector) (absolute-coord 2dg-point) &optional test-only allow-closest-match)
  "Move CONNECTOR to ABSOLUTE-COORD."
  ;; if this is an absolute-coord location already, simply move it.
  (with-slots (location connectee) connector
    (if (2dd-point-class-p connectee)
        ;; If you're connected to a point, you're not moving.
        nil
      ;; Otherwise you're connected to nothing or a rect, maybe you can move.
      (let ((current-absolute-coord (plist-get location :absolute-coord)))
        (if current-absolute-coord
            ;; location already has an absolute coordinate, simply update it.
            (if test-only
                (let ((output (2dd--copy-plist-remove location '(:lambda))))
                  (plist-put output :absolute-coord absolute-coord)
                  output)
              ;; not a test, actually do it.
              (plist-put location :absolute-coord absolute-coord)
              (2dd--copy-plist-remove location '(:lambda)))
          ;; location does not have an absolute coordinate, move while
          ;; obeying constraints.  Currently only able to connect to rectangles.
          (let* ((current-edge (2dd-from-connectee-direction connector))
                 (connectee-rect (2dd-geometry connectee))
                 (best-location (2dd--get-best-rect-location connectee-rect
                                                             absolute-coord
                                                             current-edge)))
            (if (and (not allow-closest-match)
                     (not (2dg-almost-equal (plist-get best-location :absolute-coord)
                                            absolute-coord)))
                ;; Not allowing closest matches and this new location does
                ;; not match.  This is a failure
                nil
              ;; closest matches allowed or this is actually a perfect match.
              (if test-only
                  ;; simply return the location
                  best-location
                ;; set the location and return it.
                (2dd--set-location connector best-location)
                best-location))))))))

(defun 2dd--get-best-rect-location (rect desired-point current-connector-edge)
  (cl-labels ((sort-predicate
               (a-location b-location)
               ;; First part of the -location lists is the distance.
               (let ((dist-a (first a-location))
                     (dist-b (first b-location)))
                 (if (2dg-almost-equal dist-a dist-b)
                     ;; return whichever candidate causes an edge jump
                     (if (not (eq (second a-location) current-connector-edge))
                         ;; Well, A moved, so that's good enough.
                         a-location
                       b-location)
                   ;; The distances are different, use that to determine the winner.
                   (< dist-a dist-b))))
              (pick-best
               (seq predicate)
               (cl-loop with best = (first seq)
                        for other in (rest seq)
                        when (funcall predicate other best)
                        do (setq best other)
                        finally return best)))
    (let* ((candidates
            (mapcar (lambda (edge)
                      (let* ((edge-segment (2dg-edge rect edge))
                             (edge-parametric (2dg-get-closest-parametric edge-segment
                                                                          desired-point
                                                                          t))
                             (edge-point (2dg-absolute-coordinates edge-segment
                                                                   edge-parametric))
                             (distance-sq (2dg-distance-sq edge-point desired-point)))
                        (list distance-sq edge edge-parametric edge-point)))
                    '(up down left right)))
           (best-candidate (pick-best candidates #'sort-predicate)))
      (list :edge (second best-candidate)
            :relative-coord (third best-candidate)
            :absolute-coord (fourth best-candidate)))))


(provide '2dd-link-connector)
;;; 2dd-link-connector.el ends here
