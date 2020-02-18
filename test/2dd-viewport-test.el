;; -*- lexical-binding: t; -*-

(require 'ert)
(require '2dd-viewport)

;; (ert-deftest 2dg-pixel-point-pixel-roundtrips ()
;;   "Make sure a pixel, translated to a drawing coordinate and back, is the same"
;;   (let ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
;;                                   :x-min 0.0 :x-max 101.0
;;   test1#s(2dd-viewport 0.0 0.0 41.0 101.0 #s(2dg-point 1.0 1.0))
;;   test2#s(2dg-point 22.5 40.0))))))

(ert-deftest 2dd-get-pixel-from-viewport-point ()
  ;; this test is really bad, this has to get fixed.
  (let* ((canvas (2dd-canvas :x-min 0.0 :x-max 4.0
                               :y-min 0.0 :y-max 4.0))
         (viewport (2dd-build-viewport canvas))
         (height (2dd-required-pixel-height viewport))
         (bottom-px-offset (1- height)))
    ;; We'll need to be one pixel taller than the canvas to properly draw all of the canvas
    (should (eq height 5))
    (mapc (lambda (pt)
            (should (2dg-equal
                     (2dd-get-pixel viewport pt)
                     (2dg-pixel :x 0 :y bottom-px-offset))))
          (list (2dg-point :x 0.0 :y 0.0)
                (2dg-point :x 0.1 :y 0.0)
                (2dg-point :x 0.1 :y 0.1)
                (2dg-point :x 0.0 :y 0.1)
                (2dg-point :x 0.9 :y 0.0)
                (2dg-point :x 0.9 :y 0.1)
                (2dg-point :x 0.0 :y 0.9)
                (2dg-point :x 0.1 :y 0.9)
                (2dg-point :x 0.9 :y 0.9)))
    (should (2dg-equal
             (2dd-get-pixel viewport (2dg-point :x 0.0 :y 0.1))
             (2dg-pixel :x 0 :y bottom-px-offset)))
    (should (2dg-equal
             (2dd-get-pixel viewport (2dg-point :x 1.0 :y 0.0))
             (2dg-pixel :x 1 :y bottom-px-offset)))
    (should (2dg-equal
             (2dd-get-pixel viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-pixel :x 1 :y (1- bottom-px-offset))))
    ;; out of bounds coordinates should still be calculable (is that a word?)
    (should (2dg-equal
             (2dd-get-pixel viewport (2dg-point :x -1.0 :y -1.0))
             (2dg-pixel :x -1 :y (1+ bottom-px-offset))))
    ;; (should (2dg-equal
    ;;          (2dd-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
    ;;          (2dg-pixel :x 12 :y -2)))
    )
  ;; Same should work for offset canvases
  (let* ((canvas (2dd-canvas :x-min 0.0 :x-max 10.0
                               :y-min 0.0 :y-max 10.0))
         (viewport (2dd-build-viewport canvas))
         (focus (2dg-rect :x-min 2.0 :x-max 10.0
                            :y-min 2.0 :y-max 10.0)))
    (2dd-set-domain viewport focus)
    (let* ((height (2dd-required-pixel-height viewport))
           (bottom-px-offset (1- height)))
      (should (2dg-equal
               (2dd-get-pixel viewport (2dg-point :x 2.0 :y 2.0))
               (2dg-pixel :x 0 :y bottom-px-offset)))
      (should (2dg-equal
               (2dd-get-pixel viewport (2dg-point :x 3.0 :y 2.0))
               (2dg-pixel :x 1 :y bottom-px-offset)))
      (should (2dg-equal
               (2dd-get-pixel viewport (2dg-point :x 3.0 :y 3.0))
               (2dg-pixel :x 1 :y (1- bottom-px-offset))))
      ;; out of bounds coordinates should still be calculable (is that a word?)
      (should (2dg-equal
               (2dd-get-pixel viewport (2dg-point :x 1.0 :y 1.0))
               (2dg-pixel :x -1 :y (1+ bottom-px-offset))))
      (should (2dg-equal
               (2dd-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
               (2dg-pixel :x 10 :y -2)))))
  ;; Same should work for offset canvases zoomed in.
  ;; (let* ((canvas (2dd-canvas :x-min 0.0 :x-max 10.0
  ;;                              :y-min 0.0 :y-max 10.0))
  ;;        (viewport (2dd-build-viewport canvas))
  ;;        (focus (2dg-rect :x-min 2.0 :x-max 10.0
  ;;                           :y-min 2.0 :y-max 10.0)))
  ;;   (2dd-set-domain viewport focus)
  ;;   (2dd-zoom viewport 2.0)

  ;;   (let* ((height (2dd-required-pixel-height viewport))
  ;;          (bottom-px-offset (1- height)))
  ;;        (should (2dg-equal
  ;;                 (2dd-get-pixel viewport (2dg-point :x 2.0 :y 2.0))
  ;;                 (2dg-pixel :x 0 :y bottom-px-offset)))
  ;;        (should (2dg-equal
  ;;                 (2dd-get-pixel viewport (2dg-point :x 3.0 :y 2.0))
  ;;                 (2dg-pixel :x 2 :y bottom-px-offset)))
  ;;        (should (2dg-equal
  ;;                 (2dd-get-pixel viewport (2dg-point :x 3.0 :y 3.0))
  ;;                 (2dg-pixel :x 2 :y (- bottom-px-offset 2))))
  ;;        (should (2dg-equal
  ;;                 (2dd-get-pixel viewport (2dg-point :x 1.0 :y 1.0))
  ;;                 (2dg-pixel :x -2 :y (+ 2 bottom-px-offset))))
  ;;        (should (2dg-equal
  ;;                 (2dd-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
  ;;                 (2dg-pixel :x 20 :y -4)))))
  ;; Same should work for offset canvases zoomed out.
  ;; (let* ((viewport (2dd-viewport :scaling (2dg-point :x 0.5 :y 0.5)
  ;;                                  :x-min 2.0
  ;;                                  :x-max 10.0
  ;;                                  :y-min 2.0
  ;;                                  :y-max 10.0))
  ;;        (height (2dd-required-pixel-height viewport))
  ;;        (bottom-px-offset (1- height)))
  ;;   (should (2dg-equal
  ;;            (2dd-get-pixel viewport (2dg-point :x 2.0 :y 2.0))
  ;;            (2dg-pixel :x 0 :y bottom-px-offset)))
  ;;   (should (2dg-equal
  ;;            (2dd-get-pixel viewport (2dg-point :x 3.0 :y 2.0))
  ;;            (2dg-pixel :x 0 :y bottom-px-offset)))
  ;;   (should (2dg-equal
  ;;            (2dd-get-pixel viewport (2dg-point :x 3.0 :y 3.0))
  ;;            (2dg-pixel :x 0 :y bottom-px-offset)))
  ;;   (should (2dg-equal
  ;;            (2dd-get-pixel viewport (2dg-point :x 4.0 :y 4.0))
  ;;            (2dg-pixel :x 1 :y (1- bottom-px-offset))))
  ;;   (should (2dg-equal
  ;;            (2dd-get-pixel viewport (2dg-point :x 0.99 :y 0.99))
  ;;            (2dg-pixel :x -1 :y (+ 1 bottom-px-offset))))
  ;;   (should (2dg-equal
  ;;            (2dd-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
  ;;            (2dg-pixel :x 5 :y -1))))
  )

(ert-deftest 2dd-get-scratch-coord-from-viewport-drawing-pt ()
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 0.0 :y 0.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 1.0 :y 0.0))
             (2dg-point :x 1.0 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x 1.0 :y 1.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x -1.0 :y -1.0))
             (2dg-point :x -1.0 :y -1.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 12.0 :y 12.0))
             (2dg-point :x 12.0 :y 12.0))))
  ;; Same should work for offset canvases
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 2.0 :y 2.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 3.0 :y 2.0))
             (2dg-point :x 1.0 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 3.0 :y 3.0))
             (2dg-point :x 1.0 :y 1.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x -1.0 :y -1.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 12.0 :y 12.0))
             (2dg-point :x 10.0 :y 10.0))))
  ;; Same should work for offset canvases zoomed in.
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 2.0 :y 2.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 3.0 :y 2.0))
             (2dg-point :x 2.0 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 3.0 :y 3.0))
             (2dg-point :x 2.0 :y 2.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x -2.0 :y -2.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 12.0 :y 12.0))
             (2dg-point :x 20.0 :y 20.0))))
  ;; Same should work for offset canvases zoomed out.
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 2.0 :y 2.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 3.0 :y 2.0))
             (2dg-point :x 0.5 :y 0.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 3.0 :y 3.0))
             (2dg-point :x 0.5 :y 0.5)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 4.0 :y 4.0))
             (2dg-point :x 1.0 :y 1.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x -0.5 :y -0.5)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-point :x 11.0 :y 11.0))
             (2dg-point :x 4.5 :y 4.5)))))
(ert-deftest 2dd-get-coord-centroid-from-viewport ()
  "should fire out rectangles"
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 0 :y 0))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 1 :y 1))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 1 :y 1))))))
  ;; offset a bit.
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 0 :y 0))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 1 :y 1))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 1 :y 1))))))
  ;; offset a bit and zoom in
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 0 :y 0))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 1 :y 1))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 1 :y 1))))))
  ;; offset a bit and zoom out
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 0 :y 0))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (2dd-get-coord-centroid viewport (2dg-pixel :x 1 :y 1))
             (2dg-centroid (2dd-get-coord viewport (2dg-pixel :x 1 :y 1)))))))
(ert-deftest 2dd-get-coord-from-viewport ()
  "should fire out rectangles"
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-rect :x-min 0.0 :x-max 1.0
                         :y-min 9.0 :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-rect :x-min 1.0 :x-max 2.0
                         :y-min 8.0 :y-max 9.0))))

  ;; offset a bit.
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-rect :x-min 2.0 :x-max 3.0
                         :y-min 9.0 :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-rect :x-min 3.0 :x-max 4.0
                         :y-min 8.0 :y-max 9.0))))
  ;; offset a bit and zoom in
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-rect :x-min 2.0 :x-max 2.5
                         :y-min 9.5 :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-rect :x-min 2.5 :x-max 3.0
                         :y-min 9.0 :y-max 9.5))))
  ;; offset a bit and zoom out
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-rect :x-min 2.0 :x-max 4.0
                         :y-min 8.0 :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-rect :x-min 4.0 :x-max 6.0
                         :y-min 6.0 :y-max 8.0)))))
(ert-deftest 2dd-get-scratch-coord-from-viewport-pixel ()
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 9.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 8.0))))
  ;; offset a bit.
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 7.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 6.0))))
  ;; offset a bit and zoom in
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 15.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 14.0)))
  ;; offset a bit and zoom out
  (let* ((viewport (2dd-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 3.0)))
    (should (2dg-almost-equal
             (2dd-get-scratch-coord viewport (2dg-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 2.0))))))
(ert-deftest 2dd-get-scratch-transformers-match ()
  (let ((viewports (list (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                         :x-min 0.0
                                         :x-max 10.0
                                         :y-min 0.0
                                         :y-max 10.0)
                         (2dd-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                         :x-min 2.0
                                         :x-max 10.0
                                         :y-min 2.0
                                         :y-max 10.0)
                         (2dd-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                         :x-min 2.0
                                         :x-max 10.0
                                         :y-min 2.0
                                         :y-max 10.0)
                         (2dd-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                         :x-min 2.0
                                         :x-max 10.0
                                         :y-min 2.0
                                         :y-max 10.0))))
    (cl-loop for viewport in viewports
             for transformers = (2dd-get-scratch-transformers viewport)
             for transform-x = (car transformers)
             for transform-y = (cdr transformers)
             do (cl-loop for x from -2.0 to 2.0 by 0.1
                         do (cl-loop for y from -2.0 to 2.0 by 0.1
                                     for drawing-point = (2dg-point :x x :y y)
                                     for transformers-scratch = (2dg-point :x (funcall transform-x x)
                                                                             :y (funcall transform-y y))
                                     for one-shot-scratch = (2dd-get-scratch-coord viewport drawing-point)
                                     do (should (2dg-almost-equal transformers-scratch
                                                                    one-shot-scratch)))))))



(provide '2dd-viewport-test)
