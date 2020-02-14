(require 'ert)

(require '2dd-scratch-render)


(ert-deftest 2dd-scratch-buffer-line-vert-test ()
  (let ((sbuffer (2dd---scratch-buffer 10 10)))
    (2dd---scratch-line-vert sbuffer 2 2 8 2dd---vertical)))


(provide '2dd-scratch-render-test)
