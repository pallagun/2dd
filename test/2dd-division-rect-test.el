(require 'ert)


(defsubst get-test-rect (x-min x-max y-min y-max)
  "Wrapper around 2dg-rect for ease of creation in testing."
  (2dg-rect :x-min (float x-min)
            :x-max (float x-max)
            :y-min (float y-min)
            :y-max (float y-max)))
(defun almost-equal-seg (a b)
  "return true if a and b are almost equal even if one needs to be flipped to be almost equal."
  (or (2dg-almost-equal a b)
      (2dg-almost-equal a (2dg-flipped b))))
(defun almost-equal-seg-idx (a list-of-segments)
  "Find the segment in LIST-OF-SEGMENTS that is almost-equal-seg to A and return its index."
  (cl-loop for i from 0 to (length list-of-segments)
           for other-seg in list-of-segments
           when (almost-equal-seg a other-seg)
           do (cl-return i)
           finally return nil))
(defun almost-equal-segs (a-segments b-segments)
  "Return true if all segments in a and b are almost-equal to each other regardless of order."
  (let* ((all-idxs (mapcar (lambda (a-segment)
                             (almost-equal-seg-idx a-segment b-segments))
                           a-segments))
         (unique-idxs (seq-uniq all-idxs #'eq)))
    (and (eq (length unique-idxs) (length a-segments))
         (eq (length a-segments) (length b-segments)))))

(ert-deftest 2dd--split-on-overlaps-test ()
  (let ((primary (2dg-segment- 0 0 1 1)))
    ;; exact overlaps
    (let* ((other (2dg-segment- 0 0 1 1))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap primary))
      (should (null extra)))
    (let* ((other (2dg-flipped (2dg-segment- 0 0 1 1)))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap primary))
      (should (null extra)))
    ;; over-full overlaps
    (let* ((other (2dg-segment- 2 2 -1 -1))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap primary))
      (should (eq (length extra) 2))
      (let ((expected-a (2dg-segment- 1 1 2 2))
            (expected-b (2dg-segment- -1 -1 0 0)))
        (should (or (almost-equal-seg (first extra) expected-a)
                    (almost-equal-seg (second extra) expected-a)))
        (should (or (almost-equal-seg (first extra) expected-b)
                    (almost-equal-seg (second extra) expected-b)))))
    (let* ((other (2dg-flipped (2dg-segment- 2 2 -1 -1)))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap primary))
      (should (eq (length extra) 2))
      (let ((expected-a (2dg-segment- 1 1 2 2))
            (expected-b (2dg-segment- -1 -1 0 0)))
        (should (or (almost-equal-seg (first extra) expected-a)
                    (almost-equal-seg (second extra) expected-a)))
        (should (or (almost-equal-seg (first extra) expected-b)
                    (almost-equal-seg (second extra) expected-b)))))
    ;; non-full overlap covering start point
    (let* ((other (2dg-segment- -1 -1 0.5 0.5))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap (2dg-segment- 0 0 0.5 0.5)))
      (should (eq (length extra) 1))
      (should (almost-equal-seg (first extra) (2dg-segment- -1 -1 0 0))))
    (let* ((other (2dg-flipped (2dg-segment- -1 -1 0.5 0.5)))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap (2dg-segment- 0 0 0.5 0.5)))
      (should (eq (length extra) 1))
      (should (almost-equal-seg (first extra) (2dg-segment- -1 -1 0 0))))
    ;; non-full overlap covering end-point
    (let* ((other (2dg-segment- 0.5 0.5 1.5 1.5))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap (2dg-segment- 0.5 0.5 1 1)))
      (should (eq (length extra) 1))
      (should (almost-equal-seg (first extra) (2dg-segment- 1 1 1.5 1.5))))
    (let* ((other (2dg-flipped (2dg-segment- 0.5 0.5 1.5 1.5)))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap (2dg-segment- 0.5 0.5 1 1)))
      (should (eq (length extra) 1))
      (should (almost-equal-seg (first extra) (2dg-segment- 1 1 1.5 1.5))))
    ;; non-full overlap not touching either terminal point
    (let* ((other (2dg-segment- 0.5 0.5 0.75 0.75))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap other))
      (should (null extra)))
    (let* ((other (2dg-flipped (2dg-segment- 0.5 0.5 0.75 0.75)))
           (split-result (2dd--split-on-overlaps primary other))
           (overlap (car split-result))
           (extra (cdr split-result)))
      (should (almost-equal-seg overlap other))
      (should (null extra)))))
(ert-deftest 2dd--subtract-overlaps-test ()
  ;; Subtract the bottom edge from a square.
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0 0 1 0)
                                             square-segments)))
    (should (almost-equal-segs subtracted
                               (list (2dg-segment- 1 0 1 1)
                                     (2dg-segment- 1 1 0 1)
                                     (2dg-segment- 0 1 0 0)))))
  ;; Subtract only the left side of the bottom edge from a square
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0 0 0.5 0)
                                             square-segments)))
    (should (almost-equal-segs subtracted
                               (list (2dg-segment- 1 0 1 1)
                                     (2dg-segment- 1 1 0 1)
                                     (2dg-segment- 0 1 0 0)
                                     (2dg-segment- 0.5 0 1 0)))))
  ;; Subtract only the left side of the bottom edge from a square
  ;; with overlap.  Should fail as there is a part of the primary
  ;; segment that is not overlapped at all.
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (subtracted (2dd--subtract-overlaps (2dg-segment- -0.5 0 0.5 0)
                                             square-segments)))
    (should (eq subtracted 'bad-overlap)))
  ;; Subtract only the right side of the bottom edge from a square
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0.5 0 1 0)
                                             square-segments)))
    (should (almost-equal-segs subtracted
                               (list (2dg-segment- 1 0 1 1)
                                     (2dg-segment- 1 1 0 1)
                                     (2dg-segment- 0 1 0 0)
                                     (2dg-segment- 0 0 0.5 0)))))
  ;; Subtract only the right side of the bottom edge from a square
  ;; with overlap.  Should fail as there is a part of the primary
  ;; segment that is not overlapped at all.
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0.5 0 1.5 0)
                                             square-segments)))
    (should (eq subtracted 'bad-overlap)))
  ;; Subtract only a center portion of the bottom edge.
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0.3 0 0.7 0)
                                             square-segments)))
    (should (almost-equal-segs subtracted
                               (list (2dg-segment- 1 0 1 1)
                                     (2dg-segment- 1 1 0 1)
                                     (2dg-segment- 0 1 0 0)
                                     (2dg-segment- 0 0 0.3 0)
                                     (2dg-segment- 0.7 0 1.0 0)))))
  ;; Attempt to subtract a doubled segment on the entire bottom
  ;; edge.
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (extra-segments (list (2dg-segment- 0 0 1 0)))
         (all-segments (append square-segments extra-segments))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0 0 1 0)
                                             all-segments)))
    (should (eq subtracted 'bad-overlap)))
  ;; Attempt to subtract a double segment on the left of the bottom
  ;; edge
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (extra-segments (list (2dg-segment- 0 0 0.5 0)))
         (all-segments (append square-segments extra-segments))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0 0 1 0)
                                             all-segments)))
    (should (eq subtracted 'bad-overlap)))
  ;; Attempt to subtract a double segment on hanging past the left
  ;; of the bottom edge
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (extra-segments (list (2dg-segment- -0.5 0 0.5 0)))
         (all-segments (append square-segments extra-segments))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0 0 1 0)
                                             all-segments)))
    (should (eq subtracted 'bad-overlap)))
  ;; Attempt to subtract a double segment in the middle of the
  ;; bottom edge
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (extra-segments (list (2dg-segment- 0.2 0 0.8 0)))
         (all-segments (append square-segments extra-segments))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 0 0 1 0)
                                             all-segments)))
    (should (eq subtracted 'bad-overlap)))
  ;; Attempt to subtract something that can't be subtracted because
  ;; there are no intersections
  (let* ((square-segments (2dg-segments (2dg-rect- 0 1 0 1)))
         (subtracted (2dd--subtract-overlaps (2dg-segment- 2 0 2 1)
                                             square-segments)))
    (should (eq subtracted 'bad-overlap))))

(ert-deftest 2dd-division-rect-set-divisions-test ()
  "Must validate divisions before assigning them."
  (let ((div-rect (2dd-division-rect)))
    ;; should be able to set it to no divisions
    (2dd-set-divisions div-rect nil)
    ;; should be able to set it to a single division that covers the
    ;; whole area.
    (2dd-set-divisions div-rect (list (2dg-rect- 0 1 0 1)))
    (should (eq (2dd-get-num-divisions div-rect) 1))
    (should (2dg-almost-equal (first (2dd-get-divisions div-rect))
                              (2dg-rect- 0 1 0 1)))
    ;; Should not be able to set it to a single division if that
    ;; single division is not exactly a unit square with the bottom
    ;; left point at the origin.

    ;; too big
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 1 0 2))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 1 -1 2))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- -1 1 -1 2))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- -1 2 -1 2))))

    ;; too small
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 1 0 0.5))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0.5 1 0 1))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0.5 0.75 0 1))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0.5 0.75 0.2 0.8))))

    ;; should be able to set it to two divisions if they both take up
    ;; all the space of a unit square.

    ;; left and right.
    (2dd-set-divisions div-rect (list (2dg-rect- 0 0.4 0 1)
                                      (2dg-rect- 0.4 1 0 1)))
    (should (eq (2dd-get-num-divisions div-rect) 2))
    (should (2dg-almost-equal (first (2dd-get-divisions div-rect))
                              (2dg-rect- 0 0.4 0 1)))
    (should (2dg-almost-equal (second (2dd-get-divisions div-rect))
                              (2dg-rect- 0.4 1 0 1)))
    ;; top and bottom.
    (2dd-set-divisions div-rect (list (2dg-rect- 0 1 0 0.8)
                                      (2dg-rect- 0 1 0.8 1)))
    (should (eq (2dd-get-num-divisions div-rect) 2))
    (should (2dg-almost-equal (first (2dd-get-divisions div-rect))
                              (2dg-rect- 0 1 0 0.8)))
    (should (2dg-almost-equal (second (2dd-get-divisions div-rect))
                              (2dg-rect- 0 1 0.8 1)))

    ;; should fail if there are two divisions and any gap is left.
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 0.4 0 1)
                                                    (2dg-rect- 0.5 1 0 1))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 1 0.8 1)
                                                    (2dg-rect- 0 1 0 0.7))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 1 0.1 0.5)
                                                    (2dg-rect- 0 1 0.5 1))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 0.5 0 1)
                                                    (2dg-rect- 0.5 0.9 0 1))))

    ;; should fail if there are two divisions and there is any overlap
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 1 0 0.6)
                                                    (2dg-rect- 0 1 0.4 1))))
    (should-error (2dd-set-divisions div-rect (list (2dg-rect- 0 0.5 0 1)
                                                    (2dg-rect- 0.3 1 0 1))))

    ;; should allow a nice grid.
    (let* ((bl (2dg-rect- 0 0.5 0 0.5))
           (br (2dg-rect- 0.5 1 0 0.5))
           (tr (2dg-rect- 0.5 1 0.5 1))
           (tl (2dg-rect- 0 0.5 0.5 1))
           (divisions (list bl br tr tl)))
      (2dd-set-divisions div-rect divisions)
      (should (eq (2dd-get-num-divisions div-rect) 4))
      (should (2dg-almost-equal bl (first (2dd-get-divisions div-rect))))
      (should (2dg-almost-equal br (second (2dd-get-divisions div-rect))))
      (should (2dg-almost-equal tr (third (2dd-get-divisions div-rect))))
      (should (2dg-almost-equal tl (fourth (2dd-get-divisions div-rect)))))))


(provide '2dd-division-rect-test)
