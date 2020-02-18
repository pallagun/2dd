(require 'ert)
(require '2dd)

(ert-deftest 2dd-full-test-001-single-rect ()
  "Draw one rectangle"
  (let* ((canvas (2dd-canvas :x-min 0.0 :x-max 10.0
                             :y-min 0.0 :y-max 10.0))
         (viewport (2dd-build-viewport canvas))
         (rect (2dd-rect :geometry (2dg-rect :x-min 2.0 :x-max 8.0
                                             :y-min 2.0 :y-max 8.0)))
         (sbuffer (2dd--get-scratch viewport)))
    (2dd--scratch-rect-outline sbuffer viewport (2dd-geometry rect))
    (let ((output (with-temp-buffer
                    (2dd--scratch-write sbuffer)
                    (buffer-string)))
          (expected (mapconcat 'identity
                               '("           "
                                 "           "
                                 "  +-----+  "
                                 "  |     |  "
                                 "  |     |  "
                                 "  |     |  "
                                 "  |     |  "
                                 "  |     |  "
                                 "  +-----+  "
                                 "           "
                                 "           "
                                 "")
                               "\n")))
      (should (equal output expected)))))

(provide '2dd-full-tests)
