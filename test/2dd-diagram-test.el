(require 'ert)
(require '2dd-diagram)

(ert-deftest 2dd-diagram-test-001 ()
  "Simple diagram, one shape"
  (let ((diagram (2dd-diagram :canvas (2dd-canvas- 0 10 0 10)
                              :root (2dd-rect :label "A"
                                              :geometry (2dg-rect :x-min 5.0
                                                                  :x-max 9.0
                                                                  :y-min 2.0
                                                                  :y-max 8.0)))))
    ;; setup a default viewport.
    (2dd-set-viewport diagram (2dd-build-viewport (2dd-get-canvas diagram)))
    (let ((output (with-temp-buffer
                    (2dd-render diagram (lambda (_) nil))
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


(provide '2dd-diagram-test)
