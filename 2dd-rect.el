;;; 2dd-rect.el --- rectangular drawing -*- lexical-binding: t -*-

;;; Commentary:
;; A drawable rectangle has 8 edit indexs.  One for each corner and
;; one at the midpoint of each segment.
;;
;; Edit indices are numbered as:
;; 6 - 5 - 4
;; |       |
;; 7       3
;; |       |
;; 0 - 1 - 2

;;; Code:
(require '2dg)
(require '2dd-drawing)
(require '2dd-scratch-render)

(defclass 2dd-rect (2dd-with-inner-canvas 2dd-editable-drawing 2dd-with-label)
  ()
  :documentation "Represents a rectangle which can be drawn on a
canvas.  It has an inner-canvas and a label")
(defsubst 2dd-rect-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-rect)"
  (object-of-class-p any '2dd-rect))
(cl-defmethod 2dd-set-geometry :before ((this 2dd-rect) value)
  "Restrict THIS to have a VALUE which is of type 2dg-rect."
  (unless (2dg-rect-p value)
    (error "2dd-rect must use a 2dg-rect as their geometry")))
(cl-defmethod 2dd-pprint ((rect 2dd-rect))
  "Pretty print RECT."
  (format "dr:rect(%s:%s)"
          (2dd-label rect)
          (2dg-pprint (oref rect _geometry))))
(cl-defmethod 2dd-render ((rect 2dd-rect) scratch x-transformer y-transformer)
  "Render RECT to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

Overridable method for ecah drawing to render itself."
  (let ((rectg (2dd-geometry rect))
        (label (2dd-get-label rect)))
    (let ((x-min (funcall x-transformer (2dg-x-min rectg)))
          (x-max (funcall x-transformer (2dg-x-max rectg)))
          (y-min (funcall y-transformer (2dg-y-min rectg)))
          (y-max (funcall y-transformer (2dg-y-max rectg))))
      (2dd---scratch-line-vert scratch x-min y-min y-max 2dd---vertical)
      (2dd---scratch-line-vert scratch x-max y-min y-max 2dd---vertical)
      (2dd---scratch-line-hori scratch x-min x-max y-min 2dd---horizontal)
      (2dd---scratch-line-hori scratch x-min x-max y-max 2dd---horizontal)
      ;; if there is a label, place it in the top left *pixel*
      (when label
        (let ((label-length (length label)))
          (when (> label-length 0)
            (let ((max-display-length (- (- x-max x-min) 2)))
              (2dd---scratch-label scratch
                                   (1+ x-min)
                                   (1- y-max)
                                   (if (> label-length max-display-length)
                                       (substring label 0 max-display-length)
                                     label)))))))))

(defsubst 2dd--clone-2dg-rect (any-rect)
  "Create a clone of ANY-RECT returning a 2dg-rect."
  (2dg-rect :x-min (2dg-x-min any-rect)
            :x-max (2dg-x-max any-rect)
            :y-min (2dg-y-min any-rect)
            :y-max (2dg-y-max any-rect)))
(cl-defmethod 2dd-num-edit-idxs ((rect 2dd-rect))
  "How many edit idx points are there for this RECT.

There are always 8."
  8)
(cl-defmethod 2dd-edit-idx-points ((rect 2dd-rect))
  "Get the locations of the edit idxs for RECT as an ordered list.

Points start at the bottom left and go counter clock wise."
  (with-slots (x-min x-max y-min y-max) rect
    (list (2dg-point :x x-min :y y-min)
          (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-min)
          (2dg-point :x x-max :y y-min)
          (2dg-point :x x-max :y (/ (+ y-max y-min) 2.0))
          (2dg-point :x x-max :y y-max)
          (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-max)
          (2dg-point :x x-min :y y-max)
          (2dg-point :x x-min :y (/ (+ y-max y-min) 2.0)))))
(cl-defmethod 2dd-edit-idx-point ((rect 2dd-rect) (idx integer))
  ;; TODO - figure out how to deduplicate this with points.
  "Get the location of the given edit idx BL is zero, go CCW from there."
  (with-slots (x-min x-max y-min y-max) rect
    (case idx
     ;; BL
     (0 (2dg-point :x x-min :y y-min))
     ;; Bottom
     (1 (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-min))
     ;; BR
     (2 (2dg-point :x x-max :y y-min))
     ;; R
     (3 (2dg-point :x x-max :y (/ (+ y-max y-min) 2.0)))
     ;; TR
     (4 (2dg-point :x x-max :y y-max))
     ;; T
     (5 (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-max))
     ;; TL
     (6 (2dg-point :x x-min :y y-max))
     ;; L
     (7 (2dg-point :x x-min :y (/ (+ y-max y-min) 2.0)))
     ;; err
     (otherwise (error "Invalid edit-mode idx: %s" idx)))))

(cl-defmethod 2dd-build-move-edited ((rect 2dd-rect) (move-vector 2dg-point) (viewport 2dd-viewport))
  "Given a RECT, and a MOVE-DIRECTION, move in one pixel in that direction."
  (let ((moved (clone rect)))
    (oset moved _geometry (2dg-add (oref rect _geometry) move-vector))
    moved))
(cl-defmethod 2dd-build-idx-edited ((rect 2dd-rect) (edit-idx integer) (move-vector 2dg-point) (viewport 2dd-viewport))
  "Build a rectangle drawing based off RECT having EDIT-IDX moved by MOVE-VECTOR."
  (let ((horizontal-pts)
        (vertical-pts)
        (rectg (oref rect _geometry)))
    (let ((pts (2dg-bounding-pts rectg)))
      (cond ((equal 0 edit-idx)           ;bottom left
             (setq horizontal-pts (list (first pts) (fourth pts)))
             (setq vertical-pts (list (first pts) (second pts))))
            ((equal 1 edit-idx)           ;bottom edge
             (setq vertical-pts (list (first pts) (second pts))))
            ((equal 2 edit-idx)           ;bottom right
             (setq horizontal-pts (list (second pts) (third pts)))
             (setq vertical-pts (list (first pts) (second pts))))
            ((equal 3 edit-idx)           ;right edge
             (setq horizontal-pts (list (second pts) (third pts))))
            ((equal 4 edit-idx)           ;top right
             (setq horizontal-pts (list (second pts) (third pts)))
             (setq vertical-pts (list (third pts) (fourth pts))))
            ((equal 5 edit-idx)           ;top
             (setq vertical-pts (list (third pts) (fourth pts))))
            ((equal 6 edit-idx)           ;top left
             (setq horizontal-pts (list (first pts) (fourth pts)))
             (setq vertical-pts (list (third pts) (fourth pts))))
            ((equal 7 edit-idx)           ;left edge
             (setq horizontal-pts (list (first pts) (fourth pts))))
            ('t
             (error "Invalid edit-idx for 2dd-rect: %s" edit-idx)))
      (with-slots (x y) move-vector
        (when (and horizontal-pts (not (equal x 0.0)))
          (cl-loop for pt in horizontal-pts
                   do (oset pt x (+ (2dg-x pt) x))))
        (when (and vertical-pts (not (equal y 0.0)))
          (cl-loop for pt in vertical-pts
                   do (oset pt y (+ (2dg-y pt) y)))))
      (let ((moved (clone rect)))
        (oset moved _geometry (2dg-rect :y-min (2dg-y (first pts))
                                        :y-max (2dg-y (third pts))
                                        :x-min (2dg-x (first pts))
                                        :x-max (2dg-x (second pts))))
        moved))))

(cl-defmethod 2dd-has-inner-canvas-p ((rect 2dd-rect))
  "Given a rectangle RECT, produce its inner canvas."
  t)
(cl-defmethod 2dd-get-inner-canvas ((rect 2dd-rect))
  "Return the inner canvas of RECT."
  (let ((rectg (2dd-geometry rect))
        (pad-x (2dd-padding-horizontal rect))
        (pad-y (2dd-padding-vertical rect)))
    (2dd-inner-canvas :parent rect
                      :x-min (+ (2dg-x-min rectg) pad-x)
                      :x-max (- (2dg-x-max rectg) pad-x)
                      :y-min (+ (2dg-y-min rectg) pad-y)
                      :y-max (- (2dg-y-max rectg) pad-y))))
(cl-defmethod 2dd-set-from ((drawing-rect 2dd-rect) (source-rect 2dg-rect))
  "Set the x/y min/max coordinates of DRAWING-RECT to match SOURCE-RECT."
  (with-slots ((rect _geometry)) drawing-rect
    (if (null rect)
        ;; missing a rect entirely, create one.
        (setf rect (2dd--clone-2dg-rect source-rect))
      ;; rect exists, just set it.
      (oset rect x-min (oref source-rect x-min))
      (oset rect x-max (oref source-rect x-max))
      (oset rect y-min (oref source-rect y-min))
      (oset rect y-max (oref source-rect y-max)))))
(cl-defmethod 2dd-leaving-segment-collision-edge ((drawing-rect 2dd-rect) (pt 2dg-point))
  "If you leave centroid of RECT headed towards PT, which edge do you hit?

Returned as one of 4 symbols: 'up, 'down, 'left, 'right."
  (let* ((rect (oref drawing-rect _geometry))
         (centroid (2dg-centroid rect))
         (path (2dg-segment :start centroid :end pt))
         (char-vector (2dg-characteristic-vector path))
         (to-tl (2dg-segment :start centroid :end (2dg-TL rect)))
         (to-tr (2dg-segment :start centroid :end (2dg-TR rect)))
         (cross-tl (2dg-cross-prod char-vector (2dg-characteristic-vector to-tl)))
         (cross-tr (2dg-cross-prod char-vector (2dg-characteristic-vector to-tr))))
    (cond ((and (>= cross-tl 0.0) (>= cross-tr 0.0))
           'right)
          ((and (<= cross-tl 0.0) (<= cross-tr 0.0))
           'left)
          ((and (>= cross-tl 0.0) (<= cross-tr 0.0))
           'up)
          ((and (<= cross-tl 0.0) (>= cross-tr 0.0))
           'down)
          ('t (error "Impossible?")))))

(provide '2dd-rect)
;;; 2dd-rect.el ends here
