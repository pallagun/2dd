;;; 2dd-util --- utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Collecting bin for utility functions.

;;; Code:
(defun 2dd-complement (fn)
  "Generate a Common Lisp style complement to FN."
  (lambda (&rest args)
    (not (apply fn args))))


(defun 2dd-repeat-compressor (start-node child-fn)
  "Given a START-NODE and a CHILD-FN return a list containing all nodes traversed in depth-first searching.

This function will traverse all nodes and when one node is found to have two parents it will only be returned once (the first time)"
  (cl-loop with compressed = nil
           for node in (2dd--depth-first-traversal start-node child-fn)
           unless (memq node compressed)
           do (push node compressed)
           finally return (nreverse compressed)))
(defun 2dd--depth-first-traversal (start-node child-fn)
  "Return an ordered list of nodes for depth first traversal."
  (let ((children (funcall child-fn start-node)))
    (if children
        (apply #'nconc (cons (list start-node)
                             (mapcar (lambda (child)
                                       (2dd--depth-first-traversal child child-fn))
                                     children)))
      (list start-node))))


(provide '2dd-util)
;;; 2dd-util.el ends here
