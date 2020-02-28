;;; 2dd-rect-container.el --- rectangular container for drawing -*- lexical-binding: t -*-

;;; Commentary:
;; A rectangular 'drawing' that has no renderable features.
;;
;; The purpose is to serve as a container for one or more child drawins.
;;
;; Padding is always 0.

;;; Code:
(require '2dg)
(require '2dd-drawing)

(defclass 2dd-rect-container (2dd-with-inner-canvas 2dd-drawing)
  ()
  :documentation "Represents an invisible rectangular container for other drawings.")
(cl-defmethod 2dd-set-geometry :before ((this 2dd-rect-container) value)
  "Restrict THIS to have a VALUE which is of type 2dg-rect."
  (unless (2dg-rect-p value)
    (error "2dd-rect must use a 2dg-rect as their geometry")))
(cl-defmethod 2dd-pprint ((rect 2dd-rect-container))
  "Pretty print RECT."
  (format "dr:container(%s:%s)"
          (2dg-pprint (2dd-geometry rect))))
(cl-defmethod 2dd-has-inner-canvas-p ((rect 2dd-rect-container))
  "Given a rectangle RECT, produce its inner canvas."
  t)
(cl-defmethod 2dd-get-inner-canvas ((rect 2dd-rect-container))
  "Return the inner canvas of RECT."
  (with-slots (x-min x-max y-min y-max) (2dd-geometry rect)
    (2dd-inner-canvas :parent rect
                      :x-min x-min
                      :x-max x-max
                      :y-min y-min
                      :y-max y-max)))
(cl-defmethod 2dd-set-from ((drawing-rect 2dd-rect-container) (source-rect 2dg-rect) &optional parent-canvas)
  "Set the x/y min/max coordinates of DRAWING-RECT to match SOURCE-RECT."
  (with-slots ((rect _geometry)) drawing-rect
    (if (null rect)
        ;; missing a rect entirely, create one.
        (setf rect (clone source-rect))
      ;; rect exists, just set it.
      (oset rect x-min (oref source-rect x-min))
      (oset rect x-max (oref source-rect x-max))
      (oset rect y-min (oref source-rect y-min))
      (oset rect y-max (oref source-rect y-max)))))
(cl-defmethod 2dd-set-padding ((drawing 2dd-rect-container) (padding-horizontal float) (padding-vertical float))
  "Setting padding on a container is a no-op, it is ignored"
  nil)
(cl-defmethod 2dd-set-padding-horizontal ((drawing 2dd-rect-container) value)
  "Setting padding on a container is a no-op, it is ignored"
  nil)
(cl-defmethod 2dd-set-padding-vertical ((drawing 2dd-rect-container) value)
  "Setting padding on a container is a no-op, it is ignored"
  nil)
(cl-defmethod 2dd-get-inner-canvas ((rect 2dd-rect-container))
  "Return the inner canvas of RECT."
  (let ((rectg (2dd-geometry rect)))
    (2dd-inner-canvas :parent rect
                      :x-min (2dg-x-min rectg)
                      :x-max (2dg-x-max rectg)
                      :y-min (2dg-y-min rectg)
                      :y-max (2dg-y-max rectg))))



(provide '2dd-rect-container)
;;; 2dd-rect-container.el ends here
