;;; 2dd-division-rect.el --- rectangular drawing with internal divisions -*- lexical-binding: t -*-

;;; Commentary:
;; A rectangle drawing which manages and constrains child drawings.
;; Constraints:
;; - May have zero divisions.
;; - When there is >= 1 division:
;;   - Exactly one child drawing is placed in each division.
;;   - All space in the division-rect must be claimed by exactly one division.
;;   - A child drawing must consume the entire space of its assigned division

;;; Code:
(require '2dg)
(require '2dd-drawing)
(require '2dd-scratch-render)
(require '2dd-rect)

(defclass 2dd-division-rect (2dd-rect)
  ((_divisons :initform nil
              :reader 2dd-get-divisions
              :writer 2dd-set-divisions
              :type list
              :documentation "An ordered list of subdivisons"))
  :documentation "Represents a container rectangle which can be
drawn on a canvas.  It has an inner-canvas and a label.  The
container rectangle has a number of inner rectangles which can be
arranged to desired shapes though they are constrained to consume
all the inner space of the container rectangle.")
(defsubst 2dd-division-rect-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-rect)"
  (object-of-class-p any '2dd-division-rect))
(cl-defmethod 2dd-set-geometry :before ((this 2dd-division-rect) value)
  "Restrict THIS to have a VALUE which is of type 2dg-rect."
  (error "Currently unable to set geometry for a division-rect"))
(cl-defmethod 2dd-pprint ((rect 2dd-division-rect))
  "Pretty print RECT."
  (with-slots ((geo _geometry) (relative-geo _relative-geometry)) rect
    (format "dr:drect(%s:g->%s:rg->%s,div->%s)"
            (2dd-get-label rect)
            (if geo (2dg-pprint geo) 'nil)
            (if relative-geo (2dg-pprint relative-geo) nil)
            "??Divisions??")))
(cl-defmethod 2dd-render ((rect 2dd-division-rect) scratch x-transformer y-transformer &rest style-plist)
  "Render RECT to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

Overridable method for ecah drawing to render itself."
  (error "Can't currently render a divison rect"))
(cl-defmethod 2dd-serialize-geometry ((rect 2dd-division-rect))
  "Serialize RECT to a string.

Returns a stringified list in one of two forms:
(:relative <RELATIVE-GEOMETRY> :divisions <DIVISIONS>) or (:absolute
<ABSOLUTE-GEOMETRY> :divisions <DIVISIONS>).

<DIVISIONS> is a list of all divison rectangles in
<RELATIVE-GEOMETRY> form."
  (nconc (cl-call-next-method)
         :divisions 'I-should-put-divisions-here))


(defsubst 2dd--outside-unit (number)
  (or (> number 1.0)
      (< number 0.0)))
(defun 2dd--split-on-overlaps (primary other)
  "Separate OTHER (a segment) into pices that do and do not intersect PRIMARY (a segment).

This function does not care about the direction of primary or
other, only the intersections.  An intersection where both
segments are going in the same direction is equivalent to an
intersection where they are going in opposite directions.
Additionally the return of this function may not represent the
direction of the inputs (with regards to other).

Returns in the form of (overlapping-other . (list
remains-of-other))"
  (let ((other-start (2dg-start other))
        (other-end (2dg-end other)))
    (let ((other-rel-start (2dg-get-closest-parametric primary other-start))
          (other-rel-end (2dg-get-closest-parametric primary other-end)))
      (assert (not (and (> other-rel-start 1.0) (> other-rel-end 1.0)))
              t
              "There is no overlap, the other segment is entirely after the primary")
      (assert (not (and (< other-rel-start 0.0) (< other-rel-end 0.0)))
              t
              "There is no overlap, the other segment is entirely before the primary")
      (if (> other-rel-start other-rel-end)
          ;; Segments are going in opposite directions
          (2dd--split-on-overlaps primary (2dg-flipped other))
        ;; segments are going in the same direction.
        (if (< other-rel-start 0.0)
            ;; the other segment starts 'before' the primary
            (if (< 1.0 other-rel-end)
                ;; the other segment starts before and ends after primary.
                ;; this other completely overlaps segment and extends infront of and after it.
                ;; return the primary and the two starting and ending segments.
                (cons primary
                      (list (2dg-segment :start other-start :end (2dg-start primary))
                            (2dg-segment :start (2dg-end primary) :end other-end)))
              ;; other segment starts before primary but ends in primary.
              (cons (2dg-segment :start (2dg-start primary) :end other-end)
                    (list (2dg-segment :start other-start :end (2dg-start primary)))))
          ;; the other segment starts in the primary.
          (if (< 1.0 other-rel-end)
              ;; the other segment starts in the primary and ends after the primary
              (cons (2dg-segment :start other-start :end (2dg-end primary))
                    (list (2dg-segment :start (2dg-end primary) :end other-end)))
            ;; the other segment starts in the primary and ends in the primary.
            ;; the overlap is the entire other segment, there are no remains.
            (cons other nil)))))))
(defun 2dd--subtract-overlaps (segment segment-collection)
  "Subtract any overlaps from SEGMENT in SEGMENT-COLLECTION.

Returns the symbol 'bad-overlap if segment is not 100% overlapped
exactly once by some portion of the segments in
segment-collection.  This indicates that some part of segment was
not overlapped or that some part of segment was overlapped twice.

If all of segment is overlapped exactly once by some portion of
segment-collection it will return the non-overlapping portion of
segment-collection as a list."
  ;; run through segments, for each segment, find any other segment
  ;; that shares some portion and remove it.  Split up segments
  ;; where needed and collect multiple segments where needed
  (let ((segment-start (2dg-start segment))
        (segment-unit-vec (2dg-unit-vector segment))
        (segment-length (2dg-length segment))
        (non-intersecting-segments)
        (intersecting-segments))
    (cl-loop for other in segment-collection
             if (2dg-pierced-p segment other)
             do (let* ((split-result (2dd--split-on-overlaps segment other))
                       (intersecting-other (car split-result))
                       (non-intersecting-other (cdr split-result)))
                  (push intersecting-other intersecting-segments)
                  (setq non-intersecting-segments (nconc non-intersecting-segments
                                                         non-intersecting-other)))
             else
             do (push other non-intersecting-segments))
    ;; determine if segment was overlapped fully, partially or doubly.
    (if (null intersecting-segments)
        ;; it's not possible for this to be overlapped, there are no intersection candidates.
        'bad-overlap
      (let ((intersecting-spans
             (mapcar (lambda (intersector)
                       (let ((int-start (/ (2dg-dot-prod segment-unit-vec
                                                         (2dg-subtract (2dg-start intersector)
                                                                       segment-start))
                                           segment-length))
                             (int-end (/ (2dg-dot-prod segment-unit-vec
                                                       (2dg-subtract (2dg-end intersector)
                                                                     segment-start))
                                         segment-length)))
                         (2dg-build-span-ordered int-start int-end)))
                     intersecting-segments)))
        (setq intersecting-spans (sort intersecting-spans
                                       (lambda (a b)
                                         (< (2dg-start a) (2dg-start b)))))
        ;; scan the spans and ensure they do not overlap or leave a gap.
        (let ((intersection-start (2dg-start (first intersecting-spans)))
              (intersection-end (2dg-end (first intersecting-spans)))
              (gap-or-overlap-detected))
          (cl-loop for span in (rest intersecting-spans)
                   when (not (2dg-almost-equal intersection-end (2dg-start span)))
                   do (progn (setq gap-or-overlap-detected t)
                             (cl-return))
                   do (setq intersection-end (2dg-end span)))
          ;; if no gap and start/end are ok - you're set.
          (if (and (not gap-or-overlap-detected)
                   (2dg-almost-equal intersection-start 0.0)
                   (2dg-almost-equal intersection-end 1.0))
              ;; success, no double coverage, no gap, return the remaining segments.
              non-intersecting-segments
            ;; failure, gap or overlap or insufficient coverage.
            'bad-overlap))))))

(cl-defmethod 2dd-set-divisions :before ((this 2dd-division-rect) value)
  "Ensure that divisions in VALUE do not violate any 2dd-division-rect constraints.

Value must be an ordered list of 2dg-rect objects."
  ;; I think the algorithm I'm going to use here is to break down all
  ;; the rectangles into segments.  Then I'll state that for all space
  ;; to be consumed each line segment must have exactly one other line
  ;; segment which overlaps it for any length of the segment.
  ;; Meaning, I should never have a line segment that isn't butted up
  ;; against some other line segment.  If I *do* have a line segment
  ;; without something touching it that means there is at least *some*
  ;; empty space someplace.
  ;;
  ;; When I'm done the only line segments I should have remaining
  ;; should overlay exactly on the rectangles outline.  Since I'm
  ;; working in relative coordinates this will mean the unit square.

  ;; assert that value must be empty, or a list of 2dg-rect objects
  (assert (or (null value)
              (and (mapcar (lambda (x) (object-of-class-p x '2dg-rect)) value)))
          t
          "Divisions must be empty or set to a list of 2dg-rect objects.")
  (when value
    (let ((all-division-segments (apply #'nconc (mapcar #'2dg-segments value)))
          (unit-square-segments (2dg-segments (2dg-rect :x-min 0.0 :x-max 1.0
                                                        :y-min 0.0 :y-max 1.0))))
      (let ((all-segments (nconc unit-square-segments all-division-segments))
            (iteration-count-down 1000))
        (while (and all-segments (> iteration-count-down 0))
          (decf iteration-count-down)
          (let ((remaining-segments (2dd--subtract-overlaps (first all-segments)
                                                            (rest all-segments))))
            (if (eq remaining-segments 'bad-overlap)
                (error "Detected a bad overlap near segments at %s" (first all-segments))
              (setq all-segments remaining-segments))))
        (unless (> iteration-count-down 0)
          (error "Unable to validate divisions with maximum iterations.")
        ;; if you made it here, this is valid.
        )))))

(cl-defgeneric 2dd-get-num-divisions ((rect 2dd-division-rect))
  "Get the number of divisions in RECT."
  (length (2dd-get-divisions rect)))

(cl-defmethod 2dd-num-edit-idxs ((rect 2dd-division-rect))
  "How many edit idx points are there for this RECT.

There are always 8."
  (error "Currently can't tell you how many edit-idxs a division-rect has"))
(cl-defmethod 2dd-edit-idx-points ((rect 2dd-division-rect))
  "Get the locations of the edit idxs for RECT as an ordered list.

Points start at the bottom left and go counter clock wise."
  (error "Currently can't tell you where edit-idxs are"))
(cl-defmethod 2dd-edit-idx-point ((rect 2dd-division-rect) (idx integer))
  (error "Currently can't tell you where edit-idxs are"))

(cl-defmethod 2dd-build-idx-edited-geometry ((rect 2dd-division-rect) (edit-idx integer) (move-vector 2dg-point))
  (error "Currently can't build idx-edited geometry"))

(cl-defmethod 2dd-has-inner-canvas-p ((rect 2dd-division-rect))
  "Given a rectangle RECT, produce its inner canvas."
  nil)
(cl-defmethod 2dd-get-inner-canvas ((rect 2dd-division-rect))
  "Return the inner canvas of RECT.
Note: a division rect has NO padding and NO interdivision padding"
  (let ((rectg (2dd-geometry rect)))
    (2dd-inner-canvas :parent rect
                      :x-min (2dg-x-min rectg)
                      :x-max (2dg-x-max rectg)
                      :y-min (2dg-y-min rectg)
                      :y-max (2dg-y-max rectg))))
(cl-defmethod 2dd-set-from ((drawing-rect 2dd-division-rect) (source-rect 2dg-rect) &optional parent-canvas)
  "Set the x/y min/max coordinates of DRAWING-RECT to match SOURCE-RECT.

When PARENT-CANVAS is supplied the relative coordinate will also
be set.  If PARENT-CANVAS is not suppled any relative coordinates
will be cleared."
  (error "Can't 2dd-set-from for division rects now."))
(cl-defmethod 2dd-handle-parent-change ((drawing 2dd-division-rect) (new-parent-canvas 2dd-canvas))
  "Update DRAWING from stored relative coordinates to NEW-PARENT-CANVAS.

Returns non-nil if the drawing was updated, nil if it was not
able to be updated."
  (error "Can't 2dd-handle-parent-change for 2dd-division-rect right now."))

(provide '2dd-division-rect)
;;; 2dd-division-rect.el ends here
