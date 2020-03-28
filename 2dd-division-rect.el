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

(defclass 2dd-division-rect (2dd-with-divided-inner-canvas 2dd-rect)
  ((_divisons :initform nil
              :reader 2dd-get-divisions
              :writer 2dd-set-divisions
              :type list
              :documentation "An ordered list of subdivisons.
              Subdivisions are stored as relative divisions of a
              unit square.")
   (_division-constraint :initform 'exclusive+no-diff
                         :reader 2dd-get-division-constraint
                         :type symbol
                         :documentation "The division constraints
                         for this rectangle.  It may be one of:
                         'exclusive+no-diff"))
  :documentation "Represents a container rectangle which can be
drawn on a canvas.  It has an inner-canvas and a label.  The
container rectangle has a number of inner rectangles which can be
arranged to desired shapes though they are constrained to consume
all the inner space of the container rectangle.

Note: the geometry slot for a division-rect is a cons cell where
the cdr is a normal rectangle and the cdr is a description of the
divisions inside the main rectangle.")
(defsubst 2dd-division-rect-class-p (any)
  "Equivalent of (object-of-class-p ANY '2dd-rect)"
  (object-of-class-p any '2dd-division-rect))
(cl-defmethod 2dd-set-geometry ((this 2dd-division-rect) value)
  ;; for now this just forwards to 2dd-rect
  (cl-call-next-method))
(cl-defmethod 2dd-pprint ((rect 2dd-division-rect))
  "Pretty print RECT."
  (with-slots ((geo _geometry) (relative-geo _relative-geometry)) rect
    (format "dr:drect(%s:g->%s:rg->%s,div->%s)"
            (2dd-get-label rect)
            (if geo (2dg-pprint geo) 'nil)
            (if relative-geo (2dg-pprint relative-geo) nil)
            "??Divisions??")))
(defun 2dd--build-relative-division-rect-reference (division-rect idx)
  "Return a list describing IDX-th division within DIVISION-RECT.

This list may be used by other drawings which are locked to the
division."
  (list :lambda (lambda ()
                  (2dd-get-division division-rect idx))
        :description (list :division-idx idx)))

(defun 2dd--division-rect-get-renderable-separators (rect)
  "From RECT, determine all division separators which must be drawn."
  (let ((all-segments (apply #'nconc (mapcar #'2dg-segments
                                             (2dd-get-divisions rect))))
        (drawable-segments)
        (iteration-count-down 1000))
      (while (and all-segments (> iteration-count-down 0))
        (decf iteration-count-down)
        (let ((split-results (2dd--split-overlap-and-non (first all-segments)
                                                         (rest all-segments))))
          (setq drawable-segments (nconc drawable-segments (car split-results)))
          (setq all-segments (cdr split-results))))
      (unless (> iteration-count-down 0)
        (error "Unable to determine what separators to render."))
      drawable-segments))
(cl-defmethod 2dd-render ((rect 2dd-division-rect) scratch x-transformer y-transformer viewport &rest args)
  "Render RECT to SCRATCH buffer using X-TRANSFORMER and Y-TRANSFORMER.

If ARGS is used the first argument must be a plist containing
style information for the drawing.  For acceptable style keys
please see 2dd-render for 2dd-rect.

Overridable method for ecah drawing to render itself."
  ;; draw parallel separators first, then call the normal rectangle
  ;; renderer
  (let* ((outer-rect (2dd-geometry rect))
         (style-plist (first args))
         (absolute-segments
          (mapcar (lambda (seg)
                    (2dg-absolute-coordinates outer-rect seg))
                  (2dd--division-rect-get-renderable-separators rect))))
    (mapcar (lambda (segment)
              (let ((start (2dg-start segment))
                    (end (2dg-end segment)))
                (let ((x-start (funcall x-transformer (2dg-x start)))
                      (y-start (funcall y-transformer (2dg-y start)))
                      (x-end (funcall x-transformer (2dg-x end)))
                      (y-end (funcall y-transformer (2dg-y end))))
                  (2dd---scratch-line scratch
                                      x-start
                                      y-start
                                      x-end
                                      y-end
                                      2dd---divider
                                      (plist-get style-plist :outline-style)))))
            absolute-segments)
    ;; Update label offset style to go up as is needed for parallel
    ;; elements then render the outer rectangle normally.
    (let ((style-plist (first args)))
      (cl-call-next-method rect
                           scratch
                           x-transformer
                           y-transformer
                           viewport
                           (cons :label-y-offset (cons 1 style-plist))))
    ;; now, if this drawing is in edit-idx mode, render any edit-idx
    ;; points over idx 7
    (when (2dd-get-edit-idx rect)
      (cl-loop with edit-idx-style = (plist-get style-plist :edit-idx-style)
               for segment in absolute-segments
               for point = (2dg-centroid segment)
               with marker-char = ?X
               for x-scratch = (funcall x-transformer (2dg-x point))
               for y-scratch = (funcall y-transformer (2dg-y point))
               do (2dd---scratch-set scratch
                                     x-scratch
                                     y-scratch
                                     marker-char
                                     edit-idx-style)))
    ))

(cl-defmethod 2dd-serialize-geometry ((rect 2dd-division-rect) &optional additional-info)
  "Serialize RECT to a string.

Returns a stringified list in one of two forms:
(:relative <RELATIVE-GEOMETRY> :divisions <DIVISIONS>) or (:absolute
<ABSOLUTE-GEOMETRY> :divisions <DIVISIONS>).

<DIVISIONS> is a list of all divison rectangles in
<RELATIVE-GEOMETRY> form."
  (let ((division-info `(:divisions ,(2dd-get-divisions rect))))
    (if additional-info
        (cl-call-next-method rect (nconc division-info additional-info))
      (cl-call-next-method rect division-info))))

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
(defun 2dd--split-overlap-and-non (segment segment-collection)
  "Given SEGMENT and SEGMENT-COLLECTION return overlapping and non-overlapping segments.

Any segment in SEGMENT-COLLECTION which overlaps any part of
SEGMENT will be broken into the overlapping and non-overlapping
segments.


Return is in the form of (cons LIST-OF-OVERLAPPING-SEGMENTS
LIST-OF-NON-OVERLAPPING-SEGMENTS).  LIST-OF-OVERLAPPING-SEGMENTS
will not contain duplicates."
  (let ((non-intersecting-segments)
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
    (cons intersecting-segments non-intersecting-segments)))
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
    (let ((split-results (2dd--split-overlap-and-non segment segment-collection)))
      (setq intersecting-segments (car split-results))
      (setq non-intersecting-segments (cdr split-results)))
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
(defun 2dd--validate-exclusive+no-diff (division-rectangles)
  "Validate that DIVISION-RECTANGLES contains mutually exclusive rectangles which consume an entire unit square.

DIVISION-RECTANGLES should be a list of 2dg-rect objects.

If DIVISION-RECTANGLES fails validation an error will be thrown."
  (let ((all-division-segments (apply #'nconc (mapcar #'2dg-segments division-rectangles)))
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
        ))))

(cl-defmethod 2dd-set-divisions-absolute ((rect 2dd-division-rect) (divisions list))
  "Set divisions of RECT to be DIVISIONS which are in absolute coordinates."
  (let ((outer-rect (2dd-geometry rect)))
    (2dd-set-divisions rect
                       (mapcar (lambda (division)
                                 (2dg-relative-coordinates outer-rect division))
                               divisions))))
(cl-defmethod 2dd-get-divisions-absolute ((rect 2dd-division-rect))
  "Get the divisions of this division rect in absolute coordinates."
  (let ((outer-shell (2dd-geometry rect)))
        (mapcar (lambda (division)
                  (2dg-absolute-coordinates outer-shell division))
                (2dd-get-divisions rect))))
(cl-defmethod 2dd-get-division ((rect 2dd-division-rect) (division-idx integer))
  "Return the division (in relative coordinates) at division-idx."
  (nth division-idx (2dd-get-divisions rect)))
(cl-defmethod 2dd-set-divisions :before ((this 2dd-division-rect) value)
  "Ensure that divisionps in VALUE do not violate any 2dd-division-rect constraints.

Value must be an ordered list of 2dg-rect objects."
  ;; assert that value must be empty, or a list of 2dg-rect objects
  (assert (or (null value)
              (and (mapcar (lambda (x) (object-of-class-p x '2dg-rect)) value)))
          t
          "Divisions must be empty or set to a list of 2dg-rect objects.")
  (when value
    (case (2dd-get-division-constraint this)
      ('exclusive+no-diff (2dd--validate-exclusive+no-diff value))
      (t (error "Unknown division constraint type: %s"
                (2dd-get-division-constraint this))))))
(cl-defgeneric 2dd-get-num-divisions ((rect 2dd-division-rect))
  "Get the number of divisions in RECT."
  (length (2dd-get-divisions rect)))
(cl-defmethod 2dd-num-edit-idxs ((rect 2dd-division-rect))
  "How many edit idx points are there for this RECT.

There are always 8."
  (let ((separators (2dd--division-rect-get-renderable-separators rect)))
    (+ (cl-call-next-method) (length separators))))
(cl-defmethod 2dd-edit-idx-points ((rect 2dd-division-rect))
  "Get the locations of the edit idxs for RECT as an ordered list.

Points start at the bottom left and go counter clock wise."
  (let ((separators (2dd--division-rect-get-renderable-separators rect))
        (outer-rect (2dd-geometry rect)))
    (nconc (cl-call-next-method)
           (mapcar (lambda (seg)
                     (2dg-absolute-coordinates
                      outer-rect
                      (2dg-centroid seg)))
                   separators))))
(cl-defmethod 2dd-edit-idx-point ((rect 2dd-division-rect) (idx integer))
  (if (< idx 8)
      (cl-call-next-method)
    (let ((separators (2dd--division-rect-get-renderable-separators rect))
          (separator-idx (- idx 8)))
      (2dg-absolute-coordinates (2dd-geometry rect)
                                (2dg-centroid (nth separator-idx separators))))))

(cl-defmethod 2dd-build-idx-edited-geometry ((rect 2dd-division-rect) (edit-idx integer) (move-vector 2dg-point))
  "Build new geometry for RECT based off EDIT-IDX being moved by MOVE-VECTOR.

For a division rect this may return one of two things: a single
2dg-rect or a list where the first element is a 2dg-rect and the
second element is a list of divisions."
  (if (< edit-idx 8)
      (cl-call-next-method)
    ;; identify any divisions which touch this segment and move the accordingly.
    (list (2dd--clone-2dg-rect (2dd-geometry rect))
          (2dd--division-rect-build-idx-edited-separator-geometry
           rect
           (- edit-idx 8)
           move-vector))))

(defun 2dd--division-rect-build-idx-edited-separator-geometry (rect separator-idx move-vector)
  "Build updated separator geometry based off SEPARATOR-IDX moving by MOVE-VECTOR."
  (let* ((all-divisions (2dd-get-divisions rect))
         (all-separators (2dd--division-rect-get-renderable-separators rect))
         (separator (nth separator-idx all-separators))
         ;; what direction does the separator run in, really just
         ;; using this to get the axis, horizontal or vertical.
         (separator-direction (2dg-coarse-direction separator))
         (separator-unit-vec (2dg-vector-from-direction separator-direction))
         ;; zero out any part of move-vector parallel to
         ;; separator-direction.  Only allowed to move separators in a
         ;; direction perpendicular to their orientation.
         (filtered-move (2dg-subtract move-vector
                                      (2dg-scaled separator-unit-vec
                                                  (2dg-dot-prod separator-unit-vec
                                                                move-vector))))
         ;; Now translate that movement to unit square relative
         ;; movement.  I'll do this by cheating and using the
         ;; relative-coordinates of the bottom left of the outer
         ;; rectangle as an anchor.
         (outer-rect (2dd-geometry rect))
         (relative-move (2dg-relative-coordinates outer-rect
                                                  (2dg-add (2dg-BL outer-rect)
                                                           move-vector))))
    ;; Now I need to move separator by relative-move and ensure that
    ;; all divisions stay as proper rectangular divisions.

    (cl-flet* ((which-edge
                (rect segment)
                (let ((start (2dg-start segment))
                      (end (2dg-end segment)))
                  (if (2dg-almost-equal (2dg-y start) (2dg-y end))
                      ;; this is a horizontal segment
                      (if (2dg-almost-equal (2dg-y start) (2dg-y-min rect))
                          ;; it's the bottom segment
                          'down
                        ;; the only other horizontal segment is the top
                        'up)
                    ;; this is a vertical segment
                    (if (2dg-almost-equal (2dg-x start) (2dg-x-min rect))
                        ;; it's the left segment
                        'left
                      ;; the only other vertical segment is the right
                      'right))))
               (build-modified-rect
                (start-rect segment move-vector)
                (let ((edge (which-edge start-rect segment))
                      (x-min (2dg-x-min start-rect))
                      (x-max (2dg-x-max start-rect))
                      (y-min (2dg-y-min start-rect))
                      (y-max (2dg-y-max start-rect)))
                  (case edge
                    ('left (2dg-rect :x-min (+ x-min (2dg-x move-vector))
                                     :x-max x-max
                                     :y-min y-min
                                     :y-max y-max))
                    ('right (2dg-rect :x-min x-min
                                      :x-max (+ x-max (2dg-x move-vector))
                                      :y-min y-min
                                      :y-max y-max))
                    ('down (2dg-rect :x-min x-min
                                     :x-max x-max
                                     :y-min (+ y-min (2dg-y move-vector))
                                     :y-max y-max))
                    ('up (2dg-rect :x-min x-min
                                   :x-max x-max
                                   :y-min y-min
                                   :y-max (+ y-max (2dg-y move-vector))))
                    (_ (error "invalid edge enumeration"))))))
      ;; 1) identify which divisions would be altered.
      ;; 2) for each division
      ;; 3) if it is unaltered
      ;; 3.1) copy it over to the output list
      ;; 4) else ;; it will be altered
      ;; 4.1) identify which segment/edge of the division would change.
      ;; 4.2) apply the relative-move to that edge
      ;; 4.3) ensure the changed division is still valid (non-zero, non-negative area)
      ;; 4.4) place it on the output list

      (let ((changing-divisions
             (seq-filter (lambda (division)
                           (let ((split-results
                                  (2dd--split-overlap-and-non separator
                                                              (2dg-segments division))))
                             ;; car of split results wil be non-nil if
                             ;; there was overlap
                             (car split-results)))
                         all-divisions)))
        (cl-loop with output-divisions = nil
                 for division in all-divisions
                 do (if (memq division changing-divisions)
                        (push (build-modified-rect division separator relative-move)
                              output-divisions)
                      (push (clone division) output-divisions))
                 finally return (nreverse output-divisions))))))
(cl-defmethod 2dd-has-inner-canvas-p ((rect 2dd-division-rect))
  "Given a rectangle RECT, produce its inner canvas."
  t)
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
  ;; for now this just forwards to 2dd-rect
  (cl-call-next-method))
(cl-defmethod 2dd-set-from ((drawing-rect 2dd-division-rect) (source-list list) &optional parent-canvas)
  "Set the geometry of DRAWING-RECT from SOURCE-LIST.

When PARENT-CANVAS is supplied the relative coordinate will also
be set.  If PARENT-CANVAS is not suppled any relative coordinates
will be cleared."
  (let ((outer-shell (first source-list))
        (divisions (second source-list)))
    ;; set the divisions first
    (2dd-set-divisions drawing-rect divisions)

    (2dd-set-from drawing-rect outer-shell parent-canvas)))

(cl-defmethod 2dd--set-geometry-and-update-plot ((rect 2dd-division-rect) (geometry list) child-fn &optional parent-canvas)
  "Update RECT to have GEOMETRY.

GEOMETRY will be in the form of a list where the first element is
a 2dg-rect describing the outer shell.  The second element of
GEOMETRY is a list containing ordered divisions.

Returns a list of entries describing all drawings which were
updated."
  ;; first, set divisions, then set outershell to get cascading
  (let ((outer-shell (first geometry))
        (divisions (second geometry)))
    (2dd-set-divisions rect divisions)
    (2dd--set-geometry-and-update-plot rect outer-shell child-fn parent-canvas)))


(cl-defmethod 2dd--set-geometry-and-update-plot ((rect 2dd-division-rect) (rectg 2dg-rect) child-fn &optional parent-canvas)
  "Update RECT to have RECTG outer-shell geometry, cascade child drawings as needed.

"
  (let ((old-inner-canvas (2dd-get-inner-canvas rect)))
    (2dd-set-from rect rectg parent-canvas)
    (let ((children (funcall child-fn rect))
          (divisions (2dd-get-divisions-absolute rect))
          (new-inner-canvas (2dd-get-inner-canvas rect))
          (success t))
      (cl-loop for child in children
               for division in divisions
               do (setq success
                        (and success
                             (2dd--set-geometry-and-plot-update child
                                                                division
                                                                child-fn)))
               finally return success))))

(provide '2dd-division-rect)
;;; 2dd-division-rect.el ends here
