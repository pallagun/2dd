;;; 2dd-scratch-render.el --- Rendering to a scratch array  -*- lexical-binding: t -*-

;;; Commentary:
;; Scratch coordinates are *not* pixel coordinates
;; Pixel coordinates are integers with origin top left.
;; scratch cordinates are integer-ish floats with origin bottom left.
;;  - and i'll even let you use floats as scratch coordinates
;;    cause I'm gonna round 'em for you.
;;
;; A scratch buffer is a 2d vector which is Y major

;;; Code:
(require '2dg)
(require '2dd-viewport)

(defconst 2dd---vertical ?|)
(defconst 2dd---horizontal ?-)
(defconst 2dd---cross ?+)
(defconst 2dd---arrow-any ?X)
(defconst 2dd---arrow-up ?^)
(defconst 2dd---arrow-down ?v)
(defconst 2dd---arrow-left ?<)
(defconst 2dd---arrow-right ?>)
(defconst 2dd---divider ?.)

(defun 2dd---scratch-debug (scratch &optional x y)
  "Human visible debug message."
  (let* ((y-size (length scratch))
         (x-size (length (elt scratch 0)))
         (prefix (format "Scratch dim[%s, %s]" x-size y-size)))
    (if (and x y)
        (format "%s coord[%s, %s]" prefix x y)
      prefix)))
(defun 2dd---scratch-coord-valid-p (scratch x y &optional build-message)
  "return nil if it's a valid coordinate, return a debug string if it's not."
  (let ((y-size (length scratch))
        (x-size (length (elt scratch 0))))
    (when (and (<= 0 x (1- x-size))
               (<= 0 y (1- y-size)))
      t)))
(defsubst 2dd---scratch-size-x (scratch)
  "Return the X dimension of this SCRATCH buffer."
  (length (elt scratch 0)))
(defsubst 2dd---scratch-size-y (scratch)
  "Return the Y dimension of this SCRATCH buffer."
  (length scratch))

(defun 2dd---scratch-buffer-factory (x-size y-size)
  "Given an X-SIZE and Y-SIZE build a new scratch buffer."
  (let ((scratch (make-vector y-size 'nil)))
    (cl-loop for y-idx from 0 to (1- y-size)
             do (setf (elt scratch y-idx) (make-vector x-size 'nil))
             finally return scratch)))
(defsubst 2dd---scratch-elt (scratch x y)
  "Get the element from SCRATCH at X and Y coordinates."
  (elt (elt scratch y) x))
(defun 2dd---scratch-label (scratch x y string &optional style)
  "Write a label STRING to SCRATCH starting at X Y with STYLE."
  ;; TODO - this (when is the check, remove to get back to normal
  ;; TODO - It's been long enough that I don't remember what I was talking about...
  (when (and string
             (<= 0 y (1- (2dd---scratch-size-y scratch))))
    (let ((x-pos x)
          (x-max (1- (2dd---scratch-size-x scratch))))
      (mapc (lambda (char)
              ;; Here's the old unchecked version
              ;; (2dd---scratch-set scratch (incf x-pos) y char style)
              (let ((x x-pos))
                (when (<= 0 x x-max)
                  (2dd---scratch-set scratch x y char style)))
              (incf x-pos))
            string))))
(defsubst 2dd---scratch-set (scratch x y char &optional style)
  "Set scratch-pixel in SCRATCH at X Y to be CHAR optionally with STYLE."
  ;; TODO - should I use aset here?
  ;; (aset (elt scratch y) x (cons char style)) ????
  (setf (elt (elt scratch y) x)
        (cons char style)))
(defun 2dd---scratch-overlay (scratch x y char &optional style)
  "Overlay CHAR onto SCRATCH at X Y with optional STYLE."
  (let ((extant (car (elt (elt scratch y) x))))
    (setf (elt (elt scratch y) x)
          (cons (cond
                 ;; if it's empty just put it there
                 ;; if it's an arrow or an X it's top priority
                 ((or (eq extant 'nil)
                      (eq char 2dd---arrow-any)
                      (eq char 2dd---arrow-up)
                      (eq char 2dd---arrow-down)
                      (eq char 2dd---arrow-left)
                      (eq char 2dd---arrow-right))
                  char)
                 ;; two perpendicular lines .
                 ((or (and (or (eq extant 2dd---vertical)
                               (eq extant 2dd---cross))
                           (eq char 2dd---horizontal))
                      (and (or (eq extant 2dd---horizontal)
                               (eq extant 2dd---cross))
                           (eq char 2dd---vertical)))
                  2dd---cross)
                 ;; I guess just slap it on there?
                 ('t char))
                style))))
(defun 2dd---scratch-line (scratch x-start y-start x-end y-end &optional char style)
  "Place a line from X-START/Y-START to X-END/Y-END on SCRATCH with optional STYLE."
  (cond ((equal x-start x-end)
         ;; Vertical!
         (if (<= y-start y-end)
             (2dd---scratch-line-vert scratch
                                        x-start
                                        y-start
                                        y-end
                                        (or char 2dd---vertical)
                                        style)
           (2dd---scratch-line-vert scratch
                                      x-start
                                      y-end
                                      y-start
                                      (or char 2dd---vertical)
                                      style)))
        ((equal y-start y-end)
         ;; horizontal
         (if (<= x-start x-end)
             (2dd---scratch-line-hori scratch
                                        x-start
                                        x-end
                                        y-start
                                        (or char 2dd---horizontal)
                                        style)
           (2dd---scratch-line-hori scratch
                                      x-end
                                      x-start
                                      y-start
                                      (or char 2dd---horizontal)
                                      style)))
        ('t
         (error "Non cardinal direction line in 2dd---scratch-line (%s, %s) -> (%s, %s)"
                x-start y-start x-end y-end))))
(defun 2dd---scratch-line-vert (scratch x y-min y-max char &optional style)
  "Place a vertical line at X (from Y-MIN to Y-MAX) on SCRATCH with optional STYLE."
  (if (<= 0 x (1- (2dd---scratch-size-x scratch)))
      (let ((y-start (max 0 y-min))
            (y-end (min y-max (1- (2dd---scratch-size-y scratch)))))
        (cl-loop for y from y-start to y-end
                 do (2dd---scratch-overlay scratch x y char style))))
  ;; TODO - do I need a check?
  ;; Here's the old unchecked version
  ;; (cl-loop for y from y-min to y-max
  ;;          do (2dd---scratch-overlay scratch x y char style))
  )
(defun 2dd---scratch-line-hori (scratch x-min x-max y char &optional style)
  "Place a horizontal line at Y (from X-MIN to X-MAX) on SCRATCH with optional STYLE."
  (if (<= 0 y (1- (2dd---scratch-size-y scratch)))
      (let ((x-start (max 0 x-min))
            (x-end (min x-max (1- (2dd---scratch-size-x scratch)))))
        (cl-loop for x from x-start to x-end
                 do (2dd---scratch-overlay scratch x y char style))))
  ;; TODO - do I need the check?
  ;; here's the old uncheckd version
  ;; (cl-loop for x from x-min to x-max
  ;;          do (2dd---scratch-overlay scratch x y char style))
  )

(defconst 2dd---scratch-rect-edit-point-bytes
  (list 2dd---arrow-any
        2dd---arrow-down
        2dd---arrow-any
        2dd---arrow-right
        2dd---arrow-any
        2dd---arrow-up
        2dd---arrow-any
        2dd---arrow-left
        2dd---arrow-any)
  "If you're rendering edit idxs for a rect, this is the order you'll need to do that in.")
(defun 2dd---scratch-fit-string (string max-chars)
  "Given a STRING, shorten it so it is at most MAX-CHARS"
  (if (<= max-chars 0)
    nil
    (let ((strlen (length string)))
      (if (<= strlen max-chars)
          string
        (substring string 0 max-chars)))))

(defun 2dd--get-scratch (viewport)
  "Given a VIEWPORT - fire out a scratch."
  (2dd---scratch-buffer-factory (2dd-required-pixel-width viewport)
                                (2dd-required-pixel-height viewport)))
;; (defun 2dd--scratch-dividers (scratch viewport divider-list)
;;   "Place DIVIDER-LIST of 2dg-segments on to SCRATCH for VIEWPORT."
;;   (let* ((transformers (2dd-get-scratch-int-transformers viewport))
;;          (x-transformer (car transformers))
;;          (y-transformer (cdr transformers)))
;;     (mapc (lambda (divider-segment)
;;             (with-slots (start end) divider-segment
;;               (2dd---scratch-line scratch
;;                                   (funcall x-transformer (2dg-x start))
;;                                   (funcall y-transformer (2dg-y start))
;;                                   (funcall x-transformer (2dg-x end))
;;                                   (funcall y-transformer (2dg-y end))
;;                                   2dd---divider)))
;;           divider-list)))
;; (defun 2dd--scratch-rect-outline (scratch viewport rect &optional style)
;;   "Place RECT (a 2dd-rect) on to SCRATCH for VIEWPORT, optionally with STYLE.

;; This function will only draw the shell of the rectangle."
;;   ;; (2dd--drawing-logger "scratch rendering id: %s" (scxml-name rect))
;;   (let* ((transformers (2dd-get-scratch-int-transformers viewport))
;;          (x-transformer (car transformers))
;;          (y-transformer (cdr transformers)))
;;     (2dd--scratch-rect-outline-tr scratch rect x-transformer y-transformer style)))


;; (defun 2dd--scratch-point-label (scratch viewport pt-label)
;;   "Place PT-LABEL (an scxml-drawing-point) on to SCRATCH for VIEWPORT.

;; Right now this only properly works with single character labels."
;;   ;; (2dd--drawing-logger "scratch rendering id: %s" (scxml-print pt-label))
;;   ;; (2dd--drawing-logger "\tLabel (should not length === 1): %s" (scxml-label pt-label))
;;   (let ((style (if (scxml-drawing-highlight pt-label)
;;                    'scxml-highlight
;;                  nil))
;;         (transformers (scxml-get-scratch-int-transformers viewport)))
;;     (let ((x-transformer (car transformers))
;;           (y-transformer (cdr transformers)))
;;       (2dd---scratch-label scratch
;;                              (funcall x-transformer (2dg-x pt-label))
;;                              (funcall y-transformer (2dg-y pt-label))
;;                              (scxml-label pt-label)
;;                              style))))
;; (defun 2dd--scratch-arrow (scratch viewport arrow)
;;   "Place ARROW (an scxml-drawing-arrow) on to SCRATCH for VIEWPORT with optional STYLE."
;;   (let ((points (2dd--full-path arrow (scxml-get-point-scaling viewport)))
;;         (line-style (if (scxml-drawing-highlight arrow)
;;                         'scxml-highlight
;;                       'scxml-arrow))
;;         (head-style (if (scxml-drawing-highlight arrow)
;;                         'scxml-highlight
;;                       'scxml-arrow-head))
;;         (transformers (scxml-get-scratch-int-transformers viewport)))
;;     (let ((x-transformer (car transformers))
;;           (y-transformer (cdr transformers)))
;;       (let ((last-pt-x (funcall x-transformer (2dg-x (first points))))
;;             (last-pt-y (funcall y-transformer (2dg-y (first points))))
;;             (double-pt-x 'nil)
;;             (double-pt-y 'nil))
;;         (cl-loop for pt in (cdr points)
;;                  do (let ((pt-x (funcall x-transformer (2dg-x pt)))
;;                           (pt-y (funcall y-transformer (2dg-y pt))))
;;                       (2dd---scratch-line scratch
;;                                             last-pt-x
;;                                             last-pt-y
;;                                             pt-x
;;                                             pt-y
;;                                             nil
;;                                             line-style)
;;                       (setf double-pt-x last-pt-x
;;                             double-pt-y last-pt-y)
;;                       (setf last-pt-x pt-x
;;                             last-pt-y pt-y)))
;;         ;; now do the arrow.
;;         (let ((terminal-direction (scxml-to-node-direction
;;                                    (scxml-arrow-target arrow))))
;;           (when (2dd---scratch-coord-valid-p scratch last-pt-x last-pt-y)
;;             (2dd---scratch-set scratch
;;                                  last-pt-x
;;                                  last-pt-y
;;                                  (cond
;;                                   ((eq terminal-direction 'up)
;;                                    2dd---arrow-up)
;;                                   ((eq terminal-direction 'down)
;;                                    2dd---arrow-down)
;;                                   ((eq terminal-direction 'left)
;;                                    2dd---arrow-left)
;;                                   ((eq terminal-direction 'right)
;;                                    2dd---arrow-right)
;;                                   (t
;;                                    (error "Unknown terminal direction: %s"
;;                                           terminal-direction)))
;;                                  head-style)))
;;         (when (and nil (2dd---scratch-coord-valid-p scratch last-pt-x last-pt-y))
;;           ;; old head rendering code
;;           (if (equal last-pt-x double-pt-x)
;;               ;; x is equal, this is a vertical line.
;;               (2dd---scratch-set scratch
;;                                    last-pt-x
;;                                    last-pt-y
;;                                    (if (>= last-pt-y double-pt-y)
;;                                        2dd---arrow-up
;;                                      2dd---arrow-down)
;;                                    head-style)
;;             ;; y is equal (probably), this is a horizontal line.
;;             (2dd---scratch-set scratch
;;                                  last-pt-x
;;                                  last-pt-y
;;                                  (if (>= last-pt-x double-pt-x)
;;                                      2dd---arrow-right
;;                                    2dd---arrow-left)
;;                                  head-style)))
;;         ;; now handle the edit-points if they're supposed to be visible
;;         (when (scxml-drawing-edit-idx arrow)
;;           (cl-loop for point in (scxml-edit-idx-points arrow)
;;                    for x = (funcall x-transformer (2dg-x point))
;;                    for y = (funcall y-transformer (2dg-y point))
;;                    when (2dd---scratch-coord-valid-p scratch x y)
;;                    do (2dd---scratch-set scratch
;;                                            x
;;                                            y
;;                                            2dd---arrow-any
;;                                            'scxml-drawing-edit)))
;;         ))))
(defun 2dd--scratch-write (scratch)
  "Write out SCRATCH contents to current point."
  (let ((size-y (length scratch))
        (size-x (length (elt scratch 0)))
        (default-char (get-byte 0 " ")))
    (cl-loop for y from (1- size-y) downto 0
             for y-row = (elt scratch y)
             do (cl-loop for x from 0 to (1- size-x)
                         for pixel-data = (elt y-row x)
                         do (insert (or (car pixel-data) default-char))
                         do (when (cdr pixel-data)
                              (let ((pos (point)))
                                (put-text-property (1- pos)
                                                   pos
                                                   'face
                                                   (cdr pixel-data))))
                         finally (insert "\n")))))

(provide '2dd-scratch-render)
;;; 2dd-scratch-render.el ends here
