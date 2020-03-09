;;; 2dd-diagram.el --- diagram container -*- lexical-binding: t -*-

;;; Commentary:
;; A diagram holds a root canvas, root drawing and viewport.

;;; Code:

(require '2dg)
(require '2dd-canvas)
(require '2dd-drawing)

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
          :type 2dd-drawing))
  :documentation "Contains everything you'll need to render this drawing")
(cl-defgeneric 2dd-find-drawing-selection ((diagram 2dd-diagram) (selection-rect 2dg-rect) child-fn)
  "Find the drawing in DIAGRAM inside the SELECTION-RECT.

CHILD-FN should return all children of a given drawing.")
(cl-defmethod 2dd-find-drawing-selection ((diagram 2dd-diagram) (selection-rect 2dg-rect) child-fn)
  "Find the elements in DIAGRAM inside the SELECTION-RECT.

This function will return a list of elements from most specific
to least specific.

CHILD-FN should return all children of a given drawing.

Probably used to resolve mouse clicks or point selections.

Selection preference order:
- Point drawings
- link drawings
- Rectange drawings
- The root element"
  ;; TODO - this function does not current take containment constraints into account, it should.
  ;; it only assumes that all drawings are fully contained.
  (let ((start-drawing (oref diagram _root)))
    (nconc
     ;; (2dd---find-point selection-rect start-drawing)
     ;; (2dd---find-link selection-rect start-drawing)
     (nreverse (2dd---find-other selection-rect start-drawing child-fn)))))

(cl-defgeneric 2dd-is-drawing-in-selection ((drawing 2dd-drawing) (selection-rect 2dg-rect))
  "This function returns non-nil if SELECTION-RECT would select DRAWING.")
(cl-defmethod 2dd-is-drawing-in-selection ((drawing 2dd-drawing) (selection-rect 2dg-rect))
  "This function returns non-nil if SELECTION-RECT would select DRAWING."
  (2dd---other-touches-selection selection-rect drawing))
(defsubst 2dd---other-touches-selection (selection-rect drawing)
  "Return non-nil if DRAWING touches or is inside of SELECTION-RECT."
  ;; TODO - it might be good if this function returned a list from
  ;; most specific to least specific selection.
  (let ((geom (2dd-geometry drawing)))
    (and geom
         (2dg-has-intersection selection-rect geom 'stacked))))
(defun 2dd---find-other (selection-rect start-drawing child-fn)
  "Get a list of all non-link/non-point element in the SELECTION-RECT.

Elements in selection-rect are returned in order of least
specific to most specific (parents at start, children at the
end).

Start searching at START-DRAWING.  When nothing is found return nil"
  (if (2dd---other-touches-selection selection-rect start-drawing)
      ;; inside this drawing, see if any children are inside.
      (cons start-drawing
            (cl-loop for child in (funcall child-fn start-drawing)
                     for child-result = (2dd---find-other selection-rect child child-fn)
                     when child-result
                       return child-result
                     finally return nil))
    nil))

(cl-defgeneric 2dd-render-all ((diagram 2dd-diagram) child-fn)
  "Render DIAGRAM to a string.

CHILD-FN should provide child drawings when called with a parent
drawing.")
(cl-defmethod 2dd-render-all ((diagram 2dd-diagram) child-fn)
  "Render this DIAGRAM to a string."
  (with-slots (_root _viewport _canvas) diagram
    (let ((scratch (2dd--get-scratch _viewport))
          (transformers (2dd-get-scratch-int-transformers _viewport)))
      (2dd---render-worker scratch
                           _root
                           _canvas
                           _viewport
                           child-fn
                           (car transformers)
                           (cdr transformers))
      (2dd--scratch-write scratch))))

(defun 2dd---render-worker (scratch drawing canvas viewport child-fn x-transformer y-transformer)
  "Draw everything recursively."
  (2dd-render drawing scratch x-transformer y-transformer viewport)
  ;; now draw children.
  (mapc (lambda (child)
          (2dd---render-worker scratch
                               child
                               canvas
                               viewport
                               child-fn
                               x-transformer
                               y-transformer))
        (funcall child-fn drawing)))

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

(provide '2dd-diagram)
