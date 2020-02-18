(require 'ert)
(require '2dd-rect)

(ert-deftest 2dd-rect-scxml-build-move-edited-test ()
  (let ((rect (2dd-rect :label "test"
                        :edit-idx 5
                        :geometry (2dg-rect :x-min 1.0
                                            :x-max 10.0
                                            :y-min 2.0
                                            :y-max 20.0)
                        :containment 'captive))
        (move-delta (2dg-point- 3 4)))
    (let ((moved (2dd-build-move-edited rect move-delta (2dd-viewport))))
      ;; Ensure that edit idx and label were cloned too.
      (should (eql (2dd-edit-idx moved) 5))
      (should (equal (2dd-label moved) "test"))
      (should (eq (2dd-containment moved) 'captive))

      ;; now change them to ensure they can be changed.
      (2dd-set-label moved "changed")
      (2dd-set-edit-idx moved 6)
      (2dd-set-containment moved 'free)
      ;; ensure the original rect has not changed.
      (let ((rectg (2dd-geometry rect)))
        (should (eql (2dg-x-min rectg) 1.0))
        (should (eql (2dg-x-max rectg) 10.0))
        (should (eql (2dg-y-min rectg) 2.0))
        (should (eql (2dg-y-max rectg) 20.0)))

      (should (eq (2dd-edit-idx rect) 5))
      (should (equal (2dd-label rect) "test"))
      (should (eq (2dd-containment rect) 'captive))

      ;; ensure the moved rect is correct.
      (let ((movedg (2dd-geometry moved)))
        (should (eql (2dg-x-min movedg) 4.0))
        (should (eql (2dg-x-max movedg) 13.0))
        (should (eql (2dg-y-min movedg) 6.0))
        (should (eql (2dg-y-max movedg) 24.0)))
      (should (eq (2dd-edit-idx moved) 6))
      (should (equal (2dd-label moved) "changed"))
      (should (eq (2dd-containment moved) 'free)))))

(ert-deftest 2dd-rect-scxml-build-idx-edited-test ()
  (let ((rect (2dd-rect :label "test"
                        :edit-idx 5
                        :geometry (2dg-rect :x-min 1.0
                                            :x-max 10.0
                                            :y-min 2.0
                                            :y-max 20.0)))
        (move-delta (2dg-point- 3 4))
        (edit-idx 0))
    (let ((moved (2dd-build-idx-edited rect edit-idx move-delta (2dd-viewport))))
      ;; Ensure that edit idx and label were cloned too.
      (should (eql (2dd-edit-idx moved) 5))
      (should (equal (2dd-label moved) "test"))

      ;; now change them to ensure they can be changed.
      (2dd-set-label moved "changed")
      (2dd-set-edit-idx moved 6)
      ;; ensure the original rect has not changed.
      (let ((rectg (2dd-geometry rect)))
        (should (eql (2dg-x-min rectg) 1.0))
        (should (eql (2dg-x-max rectg) 10.0))
        (should (eql (2dg-y-min rectg) 2.0))
        (should (eql (2dg-y-max rectg) 20.0)))
      (should (eq (2dd-edit-idx rect) 5))
      (should (equal (2dd-label rect) "test"))

      ;; ensure the moved rect is correct.
      (let ((movedg (2dd-geometry moved)))
        (should (eql (2dg-x-min movedg) 4.0))
        (should (eql (2dg-x-max movedg) 10.0))
        (should (eql (2dg-y-min movedg) 6.0))
        (should (eql (2dg-y-max movedg) 20.0)))
      (should (eq (2dd-edit-idx moved) 6))
      (should (equal (2dd-label moved) "changed")))))

(provide '2dd-rect-test)
