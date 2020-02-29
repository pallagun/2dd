(require 'ert)
(require '2dd-rect)

(ert-deftest 2dd-rect-scxml-build-move-edited-geometry-test ()
  (let ((rect (2dd-rect :label "test"
                        :edit-idx 5
                        :geometry (2dg-rect :x-min 1.0
                                            :x-max 10.0
                                            :y-min 2.0
                                            :y-max 20.0)
                        :constraint 'captive))
        (move-delta (2dg-point- 3 4)))
    (let ((moved-geometry (2dd-build-move-edited-geometry rect move-delta)))
      ;; ensure the original rect has not changed.
      (let ((rectg (2dd-geometry rect)))
        (should (eql (2dg-x-min rectg) 1.0))
        (should (eql (2dg-x-max rectg) 10.0))
        (should (eql (2dg-y-min rectg) 2.0))
        (should (eql (2dg-y-max rectg) 20.0)))

      (should (eq (2dd-get-edit-idx rect) 5))
      (should (equal (2dd-get-label rect) "test"))
      (should (eq (2dd-get-constraint rect) 'captive))

      ;; ensure the moved rect is correct.
      (should (eql (2dg-x-min moved-geometry) 4.0))
      (should (eql (2dg-x-max moved-geometry) 13.0))
      (should (eql (2dg-y-min moved-geometry) 6.0))
      (should (eql (2dg-y-max moved-geometry) 24.0)))))

(ert-deftest 2dd-rect-scxml-build-idx-edited-geometry-test ()
  (let ((rect (2dd-rect :label "test"
                        :edit-idx 5
                        :geometry (2dg-rect :x-min 1.0
                                            :x-max 10.0
                                            :y-min 2.0
                                            :y-max 20.0)))
        (move-delta (2dg-point- 3 4))
        (edit-idx 0))
    (let ((moved-geometry (2dd-build-idx-edited-geometry rect
                                                         edit-idx
                                                         move-delta)))

      ;; ensure the original rect has not changed.
      (let ((rectg (2dd-geometry rect)))
        (should (eql (2dg-x-min rectg) 1.0))
        (should (eql (2dg-x-max rectg) 10.0))
        (should (eql (2dg-y-min rectg) 2.0))
        (should (eql (2dg-y-max rectg) 20.0)))
      (should (eq (2dd-get-edit-idx rect) 5))
      (should (equal (2dd-get-label rect) "test"))

      ;; ensure the moved rect is correct.
      (should (eql (2dg-x-min moved-geometry) 4.0))
      (should (eql (2dg-x-max moved-geometry) 10.0))
      (should (eql (2dg-y-min moved-geometry) 6.0))
      (should (eql (2dg-y-max moved-geometry) 20.0)))))

(provide '2dd-rect-test)
