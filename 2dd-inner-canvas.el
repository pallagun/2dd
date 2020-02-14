;;; 2dd-inner-canvas.el --- 2dd inner-canvas objects -*- lexical-binding: t -*-

;;; Commentary:
;; An inner canvas is a rectangular section in space wher a drawing is
;; painted or rendered.  An inner canvas belongs to another drawing
;; (it has a parent) and the canvas exists inside that parent drawing.

;;; Code:
(require '2dg)
(require '2dd-drawing)
(require '2dd-canvas)

(defclass 2dd-inner-canvas (2dd-canvas)
  ((_parent :initarg :parent
            :accessor 2dd-parent
            :type 2dd-drawing))
  :documentation "A canvas _inside_ some drawable element.")
(cl-defmethod 2dd-pprint ((canvas 2dd-inner-canvas))
  (format "inner-canvas(%s)" (cl-call-next-method)))
(cl-defmethod cl-print-object ((object 2dd-inner-canvas) stream)
  "This seems to be used only for edebug sessions."
  (princ (2dd-pprint object) stream))

(cl-defmethod 2dd--relative ((canvas 2dd-inner-canvas) x-size y-size x-offset y-offset)
  "Return a canvas relative to the CANVAS.

The X-SIZE and Y-SIZE of the relative canvas can be specified as
well as the X-OFFSET and Y-OFFSET."
  (with-slots (_parent x-min x-max y-min y-max) parent-canvas
    (2dd-canvas :parent _parent
                :x-min (+ x-min x-offset)
                :y-min (+ y-min y-offset)
                :x-max (+ x-min x-offset x-size)
                :y-max (+ y-min y-offset y-size))))

(provide '2dd-inner-canvas)
;;; 2dd-inner-canvas.el ends here
