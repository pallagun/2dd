(require 'ert)
(require '2dd-link)
(require '2dd-rect)

(defun pts-almost-equal (A B)
  (2dg-almost-equal
   (if (2dg-path-p A)
       A
     (2dg-path :points A))
   (if (2dg-path-p B)
       B
     (2dg-path :points B))))

(ert-deftest 2dd-link-build-idx-editied-geometry-001 ()
  "Source is on a rect, target is unconnected, move the target.."
  ;; This should setup a source drawing square [0,10],[0,10] and a
  ;; path going to an unconnected target at 5,10 -> 5,15 -> 10,15.
  (let* ((source-drawing (let ((rect (2dd-rect)))
                           (2dd-set-geometry rect
                                             (2dg-rect- 0 10 0 10))
                           rect))
         (source-connector (2dd-link-connector :connectee source-drawing
                                               :location `(:edge up
                                                                 :relative-coord 0.5)))
         (target-connector (2dd-link-connector :location `(:edge left
                                                                 :absolute-coord ,(2dg-point- 10 15))))
         (inner-path (2dg-cardinal-path :points (list (2dg-point- 5 15))))
         (link (2dd-link)))
    (2dd-set-source-connector link source-connector)
    (should (2dg-almost-equal (2dd-connection-point source-connector)
                              (2dg-point- 5 10)))
    (2dd-set-target-connector link target-connector)
    (should (2dg-almost-equal (2dd-connection-point target-connector)
                              (2dg-point- 10 15)))
    (2dd-set-inner-path link inner-path)
    (should (2dg-almost-equal (2dd-get-full-path link)
                              (2dg-path :points (list (2dg-point- 5 10)
                                                      (2dg-point- 5 15)
                                                      (2dg-point- 10 15)))))

    ;; first, move the terminal connector to the right
    (let* ((location (2dd-build-idx-edited-geometry link 2 (2dg-point- 1 0)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the target moved.
      (should (equal edit-history '(target)))
      ;; it should know that the target is in the desired spot.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 11 15)))
      ;; It should also not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should not have changed.
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 5 15))))
      ;; the source connector should not have moved.
      (should (equal (plist-get source :relative-coord) 0.5))
      (should (equal (plist-get source :edge) 'up)))
    ;; Now move it to the left
    (let* ((location (2dd-build-idx-edited-geometry link 2 (2dg-point- -1 0)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the target moved.
      (should (equal edit-history '(target)))
      ;; it should know that the target is in the desired spot.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 9 15)))
      ;; It should also not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should not have changed.
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 5 15))))
      ;; the source connector should not have moved.
      (should (equal (plist-get source :relative-coord) 0.5))
      (should (equal (plist-get source :edge) 'up)))
    ;; Now move it up (the inner point should track)
    (let* ((location (2dd-build-idx-edited-geometry link 2 (2dg-point- 0 1)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the target moved.
      (should (equal edit-history '(target)))
      ;; it should know that the target is in the desired spot.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 10 16)))
      ;; It should not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should have moved up to handle the new target.
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 5 16))))
      ;; the source connector should not have moved.
      (should (equal (plist-get source :relative-coord) 0.5))
      (should (equal (plist-get source :edge) 'up)))
    ;; Now move it down (the inner point should track)
    (let* ((location (2dd-build-idx-edited-geometry link 2 (2dg-point- 0 -1)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the target moved.
      (should (equal edit-history '(target)))
      ;; it should know that the target is in the desired spot.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 10 14)))
      ;; It should not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should have moved down to handle the new target.
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 5 14))))
      ;; the source connector should not have moved.
      (should (equal (plist-get source :relative-coord) 0.5))
      (should (equal (plist-get source :edge) 'up)))))

(ert-deftest 2dd-link-build-idx-editied-geometry-002 ()
  "Source is on a rect, target is unconnected, move the source without edge changes."
  ;; This should setup a source drawing square [0,10],[0,10] and a
  ;; path going to an unconnected target at 5,10 -> 5,15 -> 10,15.
  (let* ((source-drawing (let ((rect (2dd-rect)))
                           (2dd-set-geometry rect
                                             (2dg-rect- 0 10 0 10))
                           rect))
         (source-connector (2dd-link-connector :connectee source-drawing
                                               :location `(:edge up
                                                                 :relative-coord 0.5)))
         (target-connector (2dd-link-connector :location `(:edge left
                                                                 :absolute-coord ,(2dg-point- 10 15))))
         (inner-path (2dg-cardinal-path :points (list (2dg-point- 5 15))))
         (link (2dd-link)))
    (2dd-set-source-connector link source-connector)
    (should (2dg-almost-equal (2dd-connection-point source-connector)
                              (2dg-point- 5 10)))
    (2dd-set-target-connector link target-connector)
    (should (2dg-almost-equal (2dd-connection-point target-connector)
                              (2dg-point- 10 15)))
    (2dd-set-inner-path link inner-path)
    (should (2dg-almost-equal (2dd-get-full-path link)
                              (2dg-path :points (list (2dg-point- 5 10)
                                                      (2dg-point- 5 15)
                                                      (2dg-point- 10 15)))))

    ;; first, move the source connector to the right
    (let* ((location (2dd-build-idx-edited-geometry link 0 (2dg-point- 1 0)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the source has moved.
      (should (equal edit-history '(source)))
      ;; the target should not have moved.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 10 15)))
      ;; It should also not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should have changed to handle the new source
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 6 15))))
      ;; the source connector should have moved.  Confusingly, this
      ;; segment is going to the left. so this looks wrong but is
      ;; correct.
      (should (equal (plist-get source :relative-coord) 0.4))
      ;; the edge should still be up
      (should (equal (plist-get source :edge) 'up))
      ;; The absolute coordinate should be included in the output
      (should (equal (plist-get source :absolute-coord)
                     (2dg-point- 6 10))))
    ;; second, move the source connector to the left
    (let* ((location (2dd-build-idx-edited-geometry link 0 (2dg-point- -1 0)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the source has moved.
      (should (equal edit-history '(source)))
      ;; the target should not have moved.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 10 15)))
      ;; It should also not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should have changed to handle the new source
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 4 15))))
      ;; the source connector should have moved left.  Because this
      ;; segment goes left, this appears wrong but is correct.
      (should (equal (plist-get source :relative-coord) 0.6))
      ;; the edge should still be up
      (should (equal (plist-get source :edge) 'up))
      ;; The absolute coordinate should be included in the output
      (should (equal (plist-get source :absolute-coord)
                     (2dg-point- 4 10))))

    ;; Now move it up (should simply return the current information,
    ;; this edit is not possible but not a failure.
    (let* ((location (2dd-build-idx-edited-geometry link 0 (2dg-point- 0 1)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the source has moved.
      (should (equal edit-history '(source)))
      ;; the target should not have moved.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 10 15)))
      ;; It should also not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should have changed to handle the new source
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 5 15))))
      ;; the source connector should have moved left.  Because this
      ;; segment goes left, this appears wrong but is correct.
      (should (equal (plist-get source :relative-coord) 0.5))
      ;; the edge should still be up
      (should (equal (plist-get source :edge) 'up))
      ;; The absolute coordinate should be included in the output
      (should (equal (plist-get source :absolute-coord)
                     (2dg-point- 5 10))))
    ;; Now move it down, again, not possible should simply return the
    ;; current geometry.
    (let* ((location (2dd-build-idx-edited-geometry link 0 (2dg-point- 0 -1)))
           (edit-history (plist-get location :edit-history))
           (source (plist-get location :source))
           (target (plist-get location :target))
           (inner-points (plist-get location :inner-points)))

      ;; It should know that the source has moved.
      (should (equal edit-history '(source)))
      ;; the target should not have moved.
      (should (2dg-almost-equal (plist-get target :absolute-coord)
                                (2dg-point- 10 15)))
      ;; It should also not have changed the target direction.
      (should (eq (plist-get target :edge)
                  'left))
      ;; it should not have switched the target to relative coords
      (should-not (plist-get target :relative-coord))
      ;; the inner path should have changed to handle the new source
      (should (pts-almost-equal inner-points
                                (list (2dg-point- 5 15))))
      ;; the source connector should have moved left.  Because this
      ;; segment goes left, this appears wrong but is correct.
      (should (equal (plist-get source :relative-coord) 0.5))
      ;; the edge should still be up
      (should (equal (plist-get source :edge) 'up))
      ;; The absolute coordinate should be included in the output
      (should (equal (plist-get source :absolute-coord)
                     (2dg-point- 5 10))))))


(provide '2dd-link-test)
