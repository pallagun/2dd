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
              :reader 2dd-get-connectee
              :writer 2dd-set-connectee
              :type (or null 2dd-drawing)
              :documentation "What drawing this connector
              connects to if it connects to one at all.")
   (location :initarg :location
             :initform nil
             :type list
             :documentation "Where this connector is."))
  :documentation "Describes a connection point, possibly to another drawing.")
(cl-defgeneric 2dd-connection-point ((connector 2dd-link-connector) &optional offset)
  "Get the location of this CONNECTOR.

Optionally offset by OFFSET to render for human eyes."
  (with-slots (connectee location) connector
    (let* ((coord-lambda (plist-get location :lambda))
           (raw-point (funcall coord-lambda connectee)))
      (if offset
          (let* ((offset-direction (plist-get location :edge))
                 (offset-vector (2dg-vector-from-direction offset-direction))
                 (offset (2dg-scaled offset-vector offset)))
            (2dg-add raw-point offset))
        raw-point))))
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
  "Return t if CONNECTOR has a set location, nil otherwise."
  (and (oref connector location) t))
(cl-defmethod 2dd-pprint ((connector 2dd-link-connector))
  "Pretty print CONNECTOR."
  (with-slots (connectee) connector
    (format "cn:to:%s"
            (if connectee
                (2dd-pprint connectee)
              "UNCONNECTED"))))
(cl-defmethod 2dd-serialize-geometry ((connector 2dd-link-connector))
  "Serialize the parameters of CONNECTOR to a list."
  ;; remove the lambda before returning
  (cl-loop for elt in (oref connector location)
           with skip-next = nil
           with accumulator = nil
           do (if (eq elt :lambda)
                  (setq skip-next t)
                (if skip-next
                    (setq skip-next nil)
                  (setq accumulator (cons elt accumulator))))
           finally return (nreverse accumulator)))

(cl-defgeneric 2dd-set-location ((connector 2dd-link-connector) location)
  "Set the CONNECTOR's location to be LOCATION.")
(cl-defmethod 2dd-set-location ((connector 2dd-link-connector) (location list))
  "Set the CONNECTOR's location to be LOCATION.

Location should be a plist and may containt the following keys:

:edge (used for rectangle connections, may not be used with
to-connectee-direction)

:relative-coord (used for connections along a segment,
e.g. rectangle edge)

"
  (let ((connectee (oref connector connectee))
        (edge (plist-get location :edge))
        ;; default the relative-coord to the middle when not found.
        (relative-coord (plist-get location :relative-coord)))

    (unless edge
      (error "Edge required to form connection."))
    (when (and relative-coord (not (2dd-rect-class-p connectee)))
      (error "Unable to set relative-coordinates for connection to a non-rectangle"))

    (let ((relative-coord (or relative-coord 0.5)))
      (oset connector
            location
            (list :lambda (lambda (rect-drawing)
                            (2dd--rect-edge-point rect-drawing edge relative-coord))
                  :edge edge
                  :relative-coord relative-coord)))))

(provide '2dd-link-connector)
;;; 2dd-link-connector.el ends here
