;;; 2dd-point.el --- A "point" drawing -*- lexical-binding: t -*-

;;; Commentary:
;; Draws a "point" - actually a configurable single letter label.

;;; Code:
(require '2dg)
(require '2dd-drawing)

(defclass 2dd-point (2dd-drawing 2dd-with-label)
  ()
  :documentation "Represents point based drawing.  Basically a
  single character label floating at a point.")
(cl-defmethod 2dd-set-geometry :before ((this 2dd-point) value)
  "Restrict THIS to have a VALUE which is of type 2dg-point."
  (unless (2dg-point-p value)
    (error "2dd-point must use a 2dg-point as their geometry")))

(cl-defmethod 2dd-pprint ((pt 2dd-point))
  "Pretty print PT."
  (format "dr:point(%s:%s)"
          (2dd-label pt)
          (2dg-pprint (oref pt _geometry))))
(cl-defmethod cl-print-object ((object 2dd-point) stream)
  "This seems to be used only for edebug sessions."
  (princ (2dd-pprint object) stream))

(cl-defmethod 2dd-build-move-edited ((pt 2dd-point) (move-vector 2dg-point) (viewport 2dd-viewport))
  "Given a PT drawing and a MOVE-VECTOR, apply the movement.

TODO - VIEWPORT is passed in here but not actually needed, remove it."
  (let ((new-pt (clone pt)))
    (oset new-pt _geometry (2dg-point :x (+ (2dg-x pt) (2dg-x move-vector))
                                      :y (+ (2dg-y pt) (2dg-y move-vector))))
    new-pt))

(cl-defmethod 2dd--leaving-segment-collision-edge ((source 2dd-point) (dest 2dg-point))
  "If you leave SOURCE headed towards DEST, which edge do you hit?

Returned as one of 4 symbols: 'up, 'down, 'left, 'right."
  (2dg-coarse-direction (2dg-subtract dest (oref source _geometry))))

(provide '2dd-point)
;;; 2dd-point.el ends here
