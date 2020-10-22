(require 'ert)
(require '2dd-rect)
(require '2dd-canvas)

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

(ert-deftest 2dd-rect-serialization ()
  "serialize and unserialize a rect"
  (let ((rectg (2dg-rect- 2 10 5 15))
        (relative-rectg (2dg-rect- 0.5 0.75 0.33 0.66))
        (rect (2dd-rect)))
    (2dd-set-geometry rect rectg)

    ;; this should be an absolute rectangle
    (let ((geom-string (2dd-serialize-geometry rect)))
      (let* ((geom-list-readcell (read-from-string geom-string))
             (geom-list-length (cdr geom-list-readcell))
             (geom-list (car geom-list-readcell)))
        ;; there should be only one expression.
        (should (eq geom-list-length (length geom-string)))
        ;; it should be a plist with one attribute
        (should (eq 2 (length geom-list)))
        (let ((geom (plist-get geom-list :absolute)))
          (should (2dg-rect-p geom))
          (should (not (eq rectg geom)))
          (should (2dg-almost-equal rectg geom)))))

    ;; It may be a relatively specified rectangle too and it should
    ;; then omit the absolute geometry
    (2dd-set-relative-geometry rect relative-rectg)
    (let ((geom-string (2dd-serialize-geometry rect)))
      (let* ((geom-list-readcell (read-from-string geom-string))
             (geom-list-length (cdr geom-list-readcell))
             (geom-list (car geom-list-readcell)))
        ;; there should be only one expression.
        (should (eq geom-list-length (length geom-string)))
        ;; it should be a plist with one attribute
        (should (eq 2 (length geom-list)))
        (let ((geom (plist-get geom-list :relative)))
          (should (2dg-rect-p geom))
          (should (not (eq rectg geom)))
          (should (2dg-almost-equal relative-rectg geom)))))

    ;; And a rect can also be the child of a division rect in which
    ;; case it'll have it's geometry assigned with a division idx.
    ;;... I think
    ;; TODO: test that...
    ))

(ert-deftest 2dd-rect-set-from-stringified ()
  "unserialization and setter tests"
  
  (let* ((rectg (2dg-rect- 1 2 3 4))
         (rectg-string (prin1-to-string rectg))
         (stringified-absolute-geometry (format "(:absolute %s)" rectg-string)))

    ;; This should set the absolute geometry from a 2dg-rect
    (let ((rect (2dd-rect)))

      ;; without a canvas the relative geometry should be null
      (2dd-set-from rect rectg)
      (let ((geom (2dd-geometry rect))
            (relative-geom (2dd-get-relative-geometry rect)))
        (should (2dg-almost-equal rectg geom))
        (should (null relative-geom)))

      ;; with a canvas the relative geometry should be set.
      (let ((canvas (2dd-canvas- 0 10 0 10)))
        (2dd-set-from rect rectg canvas)
        (let ((geom (2dd-geometry rect))
              (relative-geom (2dd-get-relative-geometry rect)))
          (should (2dg-almost-equal rectg geom))
          (should (2dg-almost-equal (2dg-rect- 0.1 0.2 0.3 0.4)
                                    relative-geom)))))

    ;; This should set the absolute geometry but not relative
    (let ((rect (2dd-rect)))
      (2dd-set-from rect stringified-absolute-geometry)
      (let ((geom (2dd-geometry rect))
            (relative-geom (2dd-get-relative-geometry rect)))
        (should (2dg-almost-equal rectg geom))
        (should (null relative-geom))))
    
    ;; This should set the absolute and relative geometry
    ;; This should set the absolute geometry but not relative
    (let ((rect (2dd-rect))
          (canvas (2dd-canvas- 0 10 0 10)))
      (2dd-set-from rect stringified-absolute-geometry canvas)
      (let ((geom (2dd-geometry rect))
            (relative-geom (2dd-get-relative-geometry rect)))
        (should (2dg-almost-equal rectg geom))
        (should (2dg-almost-equal (2dg-rect- 0.1 0.2 0.3 0.4)
                                  relative-geom))))

    ;; This should not be able to handle anything else, even a parent
    ;; relative division.
    (let* ((canvas (2dd-canvas- 0 10 0 10))
           (division-idx 4)
           (stringified-relative-geometry
            (format "(:relative (:division-idx %d))" division-idx)))
      (should-error (2dd-set-from rect stringified-relative-geometry canvas)))

  ;; I should be able to set the relative geometry too.
  (let* ((rectg (2dg-rect- 0.3 0.4 0.5 0.6))
         (rectg-string (prin1-to-string rectg))
         (stringified-relative-geometry (format "(:relative %s)" rectg-string))
         (canvas (2dd-canvas- 0 10 0 10)))
    
    ;; It should fail without a parent canvas
    (should-error (2dd-set-from (2dd-rect) stringified-relative-geometry))
    (let ((rect (2dd-rect)))
      (2dd-set-from rect stringified-relative-geometry canvas)
      (let ((geom (2dd-geometry rect))
            (relative-geom (2dd-get-relative-geometry rect)))
        (should (2dg-almost-equal (2dg-rect- 3 4 5 6) geom))
        (should (2dg-almost-equal rectg
                                  relative-geom)))))))

(ert-deftest 2dd-rect-set-from-list ()
  "Setting rect geometry by list."
  ;; right now the only for is with a relative geometry specifier and a lambda
  (let* ((canvas (2dd-canvas- 0 10 0 10))
         (geo-list (list :relative '(:division-idx 0)
                         :lambda (lambda () (2dg-rect- 0.1 0.9 0.1 0.9)))))
    (let ((rect (2dd-rect)))
      (2dd-set-from rect geo-list canvas)
      (let ((geom (2dd-geometry rect))
            (relative-geom (2dd-get-relative-geometry rect))
            (relative-raw (2dd--get-raw-rect-relative-geometry rect)))
        (should (2dg-almost-equal (2dg-rect- 1 9 1 9) geom))
        (should (2dg-almost-equal (2dg-rect- 0.1 0.9 0.1 0.9) relative-geom))
        (should (equal geo-list relative-raw))))))

(ert-deftest 2dd-rect-set-from-rectg ()
  "Setting rect geometry by absolute rectangle"

  (let ((canvas (2dd-canvas- 0 10 0 10))
        (rectg (2dg-rect- 2 6 2 6)))
    (let ((rect (2dd-rect)))
      (2dd-set-from rect rectg)
      (should (2dg-almost-equal (2dg-rect- 2 6 2 6) (2dd-geometry rect)))
      (should (null (2dd-get-relative-geometry rect))))
    (let ((rect (2dd-rect)))
      (2dd-set-from rect rectg canvas)
      (should (2dg-almost-equal (2dg-rect- 2 6 2 6) (2dd-geometry rect)))
      (should (2dg-almost-equal (2dg-rect- 0.2 0.6 0.2 0.6)
                                (2dd-get-relative-geometry rect))))))


(provide '2dd-rect-test)
