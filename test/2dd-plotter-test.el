(require 'ert)
(require '2dd-plotter)

(ert-deftest 2dd-plotter-simple-grid-test-001 ()
  "A single rectangle, should take up all the canvas space."
  (let ((rectd (2dd-rect :label "something"))
        (canvas (2dd-canvas- 0 100 0 100)))
    (2dd-plot rectd canvas (lambda (_) nil) (lambda (_) t) '(:method simple-grid))
    (should (2dg-almost-equal (2dd-geometry rectd)
                              canvas))))
(ert-deftest 2dd-plotter-simple-grid-test-002 ()
  "Two rectangles, should take up all the space evenly."
  (let ((rect-a (2dd-rect :label "A"))
        (rect-b (2dd-rect :label "B"))
        (container (2dd-rect-container))
        (canvas (2dd-canvas- 0 100 0 100)))
    (2dd-plot container
              canvas
              (lambda (parent)
                (if (eq parent container)
                    (list rect-a rect-b)
                  nil))
              (lambda (t) nil)
              '(:method simple-grid))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry container)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 50.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 50.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry rect-b)))))
(ert-deftest 2dd-plotter-simple-grid-test-003 ()
  "Three rectangles, should take up a 3/4ths of a 4x4 grid."
  (let ((rect-a (2dd-rect :label "A"))
        (rect-b (2dd-rect :label "B"))
        (rect-c (2dd-rect :label "C"))
        (container (2dd-rect-container))
        (canvas (2dd-canvas- 0 100 0 100)))
    (2dd-plot container
              canvas
              (lambda (parent)
                (if (eq parent container)
                    (list rect-a rect-b rect-c)
                  nil))
              (lambda (_) t)
              '(:method simple-grid))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry container)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 50.0
                                        :y-min 50.0
                                        :y-max 100.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 50.0
                                        :x-max 100.0
                                        :y-min 50.0
                                        :y-max 100.0)
                              (2dd-geometry rect-b)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 50.0
                                        :y-min 0.0
                                        :y-max 50.0)
                              (2dd-geometry rect-c)))))
(ert-deftest 2dd-plotter-simple-grid-test-004 ()
  "Three rectangles, With some sibling margins."
  (let ((rect-a (2dd-rect :label "A"))
        (rect-b (2dd-rect :label "B"))
        (rect-c (2dd-rect :label "C"))
        (container (2dd-rect-container))
        (canvas (2dd-canvas- 0 100 0 100))
        (settings (list :method 'simple-grid
                        :sibling-margin-horizontal 12
                        :sibling-margin-vertical 14
                        :something-else 'doesnt-matter)))
    (2dd-plot container
              canvas
              (lambda (parent)
                (if (eq parent container)
                    (list rect-a rect-b rect-c)
                  nil))
              (lambda (_) t)
              settings)
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry container)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 44.0
                                        :y-min 57.0
                                        :y-max 100.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 56.0
                                        :x-max 100.0
                                        :y-min 57.0
                                        :y-max 100.0)
                              (2dd-geometry rect-b)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 44.0
                                        :y-min 0.0
                                        :y-max 43.0)
                              (2dd-geometry rect-c)))))
(ert-deftest 2dd-plotter-simple-grid-test-005 ()
  "Two rectangles inside another rectangle"
  (let ((rect-a (2dd-rect :label "A"))
        (rect-b (2dd-rect :label "B"))
        (parent (2dd-rect :label "Parent"))
        (canvas (2dd-canvas- 0 100 0 100))
        (settings '(:method simple-grid)))
    (2dd-plot parent
              canvas
              (lambda (x)
                (if (eq x parent)
                    (list rect-a rect-b)
                  nil))
              (lambda (_) t)
              settings)
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry parent)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 50.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 50.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry rect-b)))))
(ert-deftest 2dd-plotter-simple-grid-test-006 ()
  "Two rectangles inside another rectangle with padding."
  (let ((rect-a (2dd-rect :label "A"))
        (rect-b (2dd-rect :label "B"))
        (parent (2dd-rect :label "Parent"))
        (canvas (2dd-canvas- 0 100 0 100))
        (settings (list :method 'simple-grid
                        :inner-padding-horizontal 5
                        :inner-padding-vertical 6)))
    (2dd-plot parent
              canvas
              (lambda (x)
                (if (eq x parent)
                    (list rect-a rect-b)
                  nil))
              (lambda (_) t)
              settings)
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry parent)))
    (should (2dg-almost-equal (2dg-rect :x-min 5.0
                                        :x-max 50.0
                                        :y-min 6.0
                                        :y-max 94.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 50.0
                                        :x-max 95.0
                                        :y-min 6.0
                                        :y-max 94.0)
                              (2dd-geometry rect-b)))))

(ert-deftest 2dd-plotter-simple-grid-test-007 ()
  "A drawing should not be modified if it's not requested.
A drawing should be set if it's not set.
Parent: set
A,B: not set."
  (let ((rect-a (2dd-rect :label "A"))
        (rect-b (2dd-rect :label "B"))
        (parent (2dd-rect :label "Parent"
                          :geometry (2dg-rect :x-min 1.0
                                              :x-max 99.0
                                              :y-min 2.0
                                              :y-max 98.0)))
        (canvas (2dd-canvas- 0 100 0 100))
        (settings (list :method 'simple-grid)))
    (2dd-plot parent
              canvas
              (lambda (x)
                (if (eq x parent)
                    (list rect-a rect-b)
                  nil))
              (lambda (_) t)
              settings)
    (should (2dg-almost-equal (2dg-rect :x-min 1.0
                                        :x-max 99.0
                                        :y-min 2.0
                                        :y-max 98.0)
                              (2dd-geometry parent)))
    (should (2dg-almost-equal (2dg-rect :x-min 1.0
                                        :x-max 50.0
                                        :y-min 2.0
                                        :y-max 98.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 50.0
                                        :x-max 99.0
                                        :y-min 2.0
                                        :y-max 98.0)
                              (2dd-geometry rect-b)))))
(ert-deftest 2dd-plotter-simple-grid-test-008 ()
  "A drawing should not be modified if it's not requested.  All drawings set."
  (let ((rect-a (2dd-rect :label "A"
                          :geometry (2dg-rect :x-min 10.0
                                              :x-max 20.0
                                              :y-min 10.0
                                              :y-max 20.0)))
        (rect-b (2dd-rect :label "B"
                          :geometry (2dg-rect :x-min 30.0
                                              :x-max 40.0
                                              :y-min 10.0
                                              :y-max 20.0)))
        (parent (2dd-rect :label "Parent"
                          :geometry (2dg-rect :x-min 1.0
                                              :x-max 99.0
                                              :y-min 2.0
                                              :y-max 98.0)))
        (canvas (2dd-canvas- 0 100 0 100))
        (settings (list :method 'simple-grid)))
    (2dd-plot parent
              canvas
              (lambda (x)
                (if (eq x parent)
                    (list rect-a rect-b)
                  nil))
              (lambda (_) t)
              settings)
    (should (2dg-almost-equal (2dg-rect :x-min 1.0
                                        :x-max 99.0
                                        :y-min 2.0
                                        :y-max 98.0)
                              (2dd-geometry parent)))
    (should (2dg-almost-equal (2dg-rect :x-min 10.0
                                        :x-max 20.0
                                        :y-min 10.0
                                        :y-max 20.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 30.0
                                        :x-max 40.0
                                        :y-min 10.0
                                        :y-max 20.0)
                              (2dd-geometry rect-b)))))

(ert-deftest 2dd-plotter-simple-grid-test-008 ()
  "A drawing should not be modified if it is requested."
  (let ((rect-a (2dd-rect :label "A"
                          :geometry (2dg-rect :x-min 10.0
                                              :x-max 30.0
                                              :y-min 10.0
                                              :y-max 20.0)))
        (rect-b (2dd-rect :label "B"
                          :geometry (2dg-rect :x-min 10.0
                                              :x-max 30.0
                                              :y-min 20.0
                                              :y-max 30.0)))
        (parent (2dd-rect :label "Parent"
                          :geometry (2dg-rect :x-min 10.0
                                              :x-max 30.0
                                              :y-min 10.0
                                              :y-max 30.0)))
        (canvas (2dd-canvas- 0 100 0 100))
        (settings (list :method 'simple-grid)))
    (2dd-plot parent
              canvas
              (lambda (x)
                (if (eq x parent)
                    (list rect-a rect-b)
                  nil))
              (lambda (x) (or (eq x rect-a)
                              (eq x rect-b))) ;only preserve A and B, not parent.
              settings)
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 100.0)
                              (2dd-geometry parent)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 0.0
                                        :y-max 50.0)
                              (2dd-geometry rect-a)))
    (should (2dg-almost-equal (2dg-rect :x-min 0.0
                                        :x-max 100.0
                                        :y-min 50.0
                                        :y-max 100.0)
                              (2dd-geometry rect-b)))))


(provide '2dd-plotter-test)
