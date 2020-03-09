;;; 2dd-link-connector -- connector for links to other drawings -*- lexical-binding: t -*-

;;; Commentary:
;; A connector is a drawing object that's used to indicate two (or
;; one) other drawing object(s) that are connected and how they are
;; connected.

(require '2dg)

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
             :documentation "Where this connector is."))
  :documentation "Describes a connection point, possibly to another drawing.")
(cl-defgeneric 2dd-connection-point ((connector 2dd-link-connector) &optional offset)
  "Get the location of this CONNECTOR.

Optionally offset by OFFSET to render for human eyes."
  nil)
(cl-defgeneric 2dd-to-connectee-direction ((connector 2dd-link-connector))
  "Get the direction from the CONNECTOR to the connected drawing.

This will return one of: 'up, 'down, 'left or 'right."
  nil)
(cl-defgeneric 2dd-from-connectee-direction ((connector 2dd-link-connector))
  "Get the direction from the connected drawing to CONNECTOR.

This will return one of: 'up, 'down, 'left or 'right.

This should be the exact opposite of 2dd-to-connectee-direction"
  (2dg-reverse (2dd-to-connectee-direction connector)))
(cl-defmethod 2dd-pprint ((connector 2dd-link-connector))
  "Pretty print CONNECTOR."
  (with-slots (connectee) connector
    (format "cn:to:%s"
            (if connectee
                (2dd-pprint connectee)
              "UNCONNECTED"))))
(cl-defmethod 2dd-serialize-geometry ((connector 2dd-link-connector))
  "Serialize the parameters of CONNECTOR to a list."
  (list :location (oref connector location)))

(provide '2dd-link-connector)
;;; 2dd-link-connector.el ends here
