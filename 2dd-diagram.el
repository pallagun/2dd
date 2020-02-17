;;; 2dd-diagram.el --- diagram container -*- lexical-binding: t -*-

;;; Commentary:
;; A diagram holds all pieces of data needed render and interact with
;; a collection of drawings

;;; Code:

(defclass 2dd-diagram ()
  ((_canvas :initarg :canvas
            :reader 2dd-get-canvas
            :writer 2dd-set-canvas
            :type 2dd-canvas)
   (_viewport :initarg :viewport
              :reader 2dd-get-viewport
              :writer 2dd-set-viewport
              :type 2dd-viewport)
   (_root :initarg :root
          :reader 2dd-get-root
          :writer 2dd-set-root
          :type 2dd-drawing)
   :documentation "Contains everything you'll need to render this drawing"))
(cl-defgeneric 2dd-find-element-selection ((diagram 2dd-diagram) (selection-rect 2dg-rect) child-fn)
  "Find the drawing in DIAGRAM inside the SELECTION-RECT.

CHILD-FN should return all children of a given drawing.")
(cl-defmethod 2dd-find-element-selection ((diagram 2dd-diagram) (selection-rect 2dg-rect) child-fn)
  "Find the element in DIAGRAM inside the SELECTION-RECT.

CHILD-FN should return all children of a given drawing.

Probably used to resolve mouse clicks or point selections.

Selection preference order:
- Point drawings
- link drawings
- Rectange drawings
- The root element"
  (let ((start-drawing (oref diagram _root)))
    (or ;; (2dd---find-point selection-rect start-drawing)
        ;; (2dd---find-link selection-rect start-drawing)
        (2dd---find-other selection-rect start-drawing)
        start-drawing)))
(defun 2dd---find-other (selection-rect search-drawing child-fn)
  "Get first non-link/non-point element in the SELECTION-RECT.
Start searching at SEARCH-PARENT.  When nothing is found return
search-parent."
  (block 2dd---find-selection
    THIS - get this working HERE.
    (mapc (lambda (child)
            (let ((drawing (scxml-element-drawing child)))
              (when (and drawing
                         (2dg-has-intersection selection-rect drawing 'stacked))
                (return-from scxml---find-selection
                  (scxml---find-other selection-rect child)))))
          (funcall child-fn search-drawing))
    search-parent))
;; (defun scxml---find-transition (selection-rect search-parent)
;;   "Get first transition element in SELECTION-RECT.

;; Start searching at SEARCH-PARENT.  When nothing is found return
;; nil."
;;   (block scxml---find-selection
;;     (scxml-visit search-parent
;;                  (lambda (element)
;;                    (let ((drawing (scxml-element-drawing element)))
;;                      (when (and drawing
;;                                 (2dg-has-intersection selection-rect drawing 'stacked))
;;                        (return-from scxml---find-selection element))))
;;                  (lambda (element)
;;                    (object-of-class-p element 'scxml-transition)))
;;     nil))
;; (defun scxml---find-initial (selection-rect search-parent)
;;   "Get the first initial element in SELECTION-RECT.
;; Start searching at SEARCH-PARENT.  When nothing is found return
;; nil."
;;   (block scxml---find-selection
;;     (scxml-visit search-parent
;;                  (lambda (element)
;;                    (let ((drawing (scxml-element-drawing element)))
;;                      (when (and drawing
;;                                 (2dg-has-intersection selection-rect drawing 'stacked)
;;                                 (return-from scxml---find-selection element)))))
;;                  (lambda (element)
;;                    (object-of-class-p element 'scxml-initial)))
;;     nil))

(provide 'scxml-diagram)
