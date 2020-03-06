(require 'ert)
(require '2dd-diagram)

(defun 2dd--lists-equal (a b)
  (if (eq (length a) (length b))
      (cl-loop for a-elt in a
               for b-elt in b
               when (not (eq a-elt b-elt))
               return nil
               finally return t)
    nil))

(ert-deftest 2dd-diagram-test-001 ()
  "Simple diagram, one shape"
  (let ((diagram (2dd-diagram :canvas (2dd-canvas- 0 10 0 10)
                              :root (2dd-rect :geometry (2dg-rect :x-min 5.0
                                                                  :x-max 9.0
                                                                  :y-min 2.0
                                                                  :y-max 8.0)))))
    ;; setup a default viewport.
    (2dd-set-viewport diagram (2dd-build-viewport (2dd-get-canvas diagram)))
    (let ((output (with-temp-buffer
                    (2dd-render-all diagram (lambda (_) nil))
                    (buffer-string)))
          (expected (mapconcat 'identity '("           "
                                           "           "
                                           "     +---+ "
                                           "     |   | "
                                           "     |   | "
                                           "     |   | "
                                           "     |   | "
                                           "     |   | "
                                           "     +---+ "
                                           "           "
                                           "           "
                                           "") "\n")))
      (should (equal output expected)))))
(ert-deftest 2dd-diagram-test-002 ()
  "Diagram, three shapes two of which are nested."
  (let* ((child-a (2dd-rect :geometry (2dg-rect :x-min 2.0
                                                :x-max 4.0
                                                :y-min 2.0
                                                :y-max 4.0)))
         (child-b (2dd-rect :geometry (2dg-rect :x-min 6.0
                                                :x-max 9.0
                                                :y-min 6.0
                                                :y-max 9.0)))
         (parent (2dd-rect :geometry (2dg-rect :x-min 1.0
                                               :x-max 10.0
                                               :y-min 1.0
                                               :y-max 10.0)))
         (diagram (2dd-diagram :canvas (2dd-canvas- 0 10 0 10) :root parent)))
    ;; setup a default viewport.
    (2dd-set-viewport diagram (2dd-build-viewport (2dd-get-canvas diagram)))
    ;; setup a child-fn
    (setq child-fn (lambda (any)
                     (if (eq any parent)
                         `(,child-a ,child-b)
                       nil)))

    (let ((output (with-temp-buffer
                    (2dd-render-all diagram child-fn)
                    (buffer-string)))
          (expected (mapconcat 'identity '(" +--------+"
                                           " |    +--+|"
                                           " |    |  ||"
                                           " |    |  ||"
                                           " |    +--+|"
                                           " |        |"
                                           " |+-+     |"
                                           " || |     |"
                                           " |+-+     |"
                                           " +--------+"
                                           "           "
                                           "") "\n")))
      (should (equal output expected)))

    ;; nothing is in the bottom left of the bottom left pixel
    (should (2dd--lists-equal (2dd-find-drawing-selection diagram
                                                          (2dg-rect :x-min 0.0
                                                                    :x-max 0.5
                                                                    :y-min 0.0
                                                                    :y-max 0.5)
                                                          child-fn)
                              nil))
    ;; in the top left of the parent box should be only the parent.
    (should (2dd--lists-equal (2dd-find-drawing-selection diagram
                                                          (2dg-rect :x-min 3.0
                                                                    :x-max 4.5
                                                                    :y-min 7.0
                                                                    :y-max 8.5)
                                                          child-fn)
                              (list parent)))

    ;; in the bottom left is child A
    (should (2dd--lists-equal (2dd-find-drawing-selection diagram
                                                          (2dg-rect :x-min 2.0
                                                                    :x-max 3.0
                                                                    :y-min 3.0
                                                                    :y-max 4.0)
                                                          child-fn)
                              (list child-a parent)))

    ;; in the top right is child B
    (should (2dd--lists-equal (2dd-find-drawing-selection diagram
                                                          (2dg-rect :x-min 5.0
                                                                    :x-max 6.5
                                                                    :y-min 5.0
                                                                    :y-max 6.5)
                                                          child-fn)
                              (list child-b parent)))

    ;; if you select everything it should return the first and deepest
    ;; leaf node drawing
    (should (2dd--lists-equal (2dd-find-drawing-selection diagram
                                                          (2dg-rect :x-min 0.5
                                                                    :x-max 9.0
                                                                    :y-min 0.5
                                                                    :y-max 10.5)
                                                          child-fn)
                              (list child-a parent)))))

(provide '2dd-diagram-test)
