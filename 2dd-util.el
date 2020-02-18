;;; 2dd-util --- utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Collecting bin for utility functions.

;;; Code:
(defun 2dd-complement (fn)
  "Generate a Common Lisp style complement to FN."
  (lambda (&rest args)
    (not (apply fn args))))


(provide '2dd-util)
;;; 2dd-util.el ends here
