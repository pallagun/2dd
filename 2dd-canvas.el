;;; 2dd-canvas.el --- 2dd canvas object -*- lexical-binding: t -*-

;;; Commentary:
;; A canvas is a rectangular section in space where a drawing is
;; painted/rendered.

;;; Code:
(require '2dg)

(defvar 2dd-default-canvas-width 100.0
  "Default width of the main canvas.")
(defvar 2dd-default-canvas-height 40.0
  "Default height of the main canvas.")

(defclass 2dd-canvas (2dg-rect)
  ()
  :documentation "A container for a canvas, a place where scxml things can be drawn")

(cl-defmethod 2dd-pprint ((canvas 2dd-canvas))
  (format "canvas(%s)" (cl-call-next-method)))
(cl-defmethod cl-print-object ((object 2dd-canvas) stream)
  "This seems to be used only for edebug sessions."
  (princ (2dd-pprint object) stream))

(cl-defmethod 2dd--relative ((canvas 2dd-canvas) x-size y-size x-offset y-offset)
  "Return a canvas relative to the CANVAS.

The X-SIZE and Y-SIZE of the relative canvas can be specified as
well as the X-OFFSET and Y-OFFSET."
  (with-slots (x-min x-max y-min y-max) parent-canvas
    (2dd-canvas :x-min (+ x-min x-offset)
                :y-min (+ y-min y-offset)
                :x-max (+ x-min x-offset x-size)
                :y-max (+ y-min y-offset y-size))))
(defun 2dd--build-default-canvas ()
  "Generate a default canvas."
  (2dd-canvas :x-min 0.0
              :y-min 0.0
              :x-max 2dd-default-canvas-width
              :y-max 2dd-default-canvas-height))

(cl-defgeneric 2dd-split-grid ((canvas 2dd-canvas) (rows integer) (columns integer) &optional horizontal-spacing vertical-spacing)
  "Return a list of canvas representing a gridded division of CANVAS.

CANVAS will be devided into a grid having ROWS rows and COLUMNS columns.")
(cl-defmethod 2dd-split-grid ((canvas 2dd-canvas) (rows integer) (columns integer) &optional horizontal-spacing vertical-spacing)
  "Return a list of canvas representing a gridded division of CANVAS.

CANVAS will be devided into a grid having ROWS rows and COLUMNS columns."
  (let ((x-spacing (or horizontal-spacing 10.0))
        (y-spacing (or vertical-spacing 4.0))
        (num-rows (float rows))
        (num-columns (float columns)))
    ;; Go from top left to bottom right - english language reading order.
    ;; But I'm going to be pushing them - so opposite that.
    (cl-loop for row-idx from 0 to (1- num-rows)
             with cells = 'nil
             with cell-x-size = (/ (- (2dg-width canvas) (* (1- num-columns) x-spacing))
                                   num-columns)
             with cell-y-size = (/ (- (2dg-height canvas) (* (1- num-rows) y-spacing))
                                   num-rows)
             with cell-x-offset = (+ cell-x-size x-spacing)
             with cell-y-offset = (+ cell-y-size y-spacing)
             do (cl-loop for column-idx from (1- num-columns) downto 0
                         do (push
                             (2dd--relative canvas
                                            cell-x-size
                                            cell-y-size
                                            (* column-idx cell-x-offset)
                                            (* row-idx cell-y-offset))
                             cells))
             finally return cells)))

(provide '2dd-canvas)
;;; 2dd-canvas.el ends here
