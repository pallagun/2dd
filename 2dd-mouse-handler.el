;;; 2dd-mouse-handler.el --- 2dd mouse handling -*- lexical-binding: t -*-

;;; Commentary:
;; Mouse interaction in 2dd is handled by a single function.  This
;; function is resposible for real-time mouse dragging updates,
;; clicks, double clicks as well as providing hooks for mouse actions

;;; Code:
(require '2dg)

(defvar-local 2dd-mouse-preempts 'nil
  "This variable holds an alist of the current mouse preempts.

A mouse preempt is a function called only once when a mouse-event
of the specified type is clicked.  Preempts are used to
facilitate GUI actions such as 'the arrow will end at the next
drawing selected'.  In that case a preempt for (probably)
'down-mouse-1 would be set to a function which completes an
arrow's destination.  The mouse handler will call the function
when it detects a 'mouse-down-1 event before calling the normal
'mouse-down-1 event handler.  Once the mouse handler calls the
preempt function it will be cleared from the preempt list so it
is only called once.

This variable is buffer local.")
(defvar-local 2dd-mouse-last-clicked-pixel 'nil
  "This variable is updated each time a 'mouse-down' type event
  is receieved.")
(defvar-local 2dd-mouse-hooks 'nil
  "This holds an alist of all mouse hooks for the mouse handler.

A mouse hook is a cons cell of the form (MOUSE-ACTION . FN).  MOUSE-ACTION must be one of the 2dd-valid-mouse-hooks and FN should be a callable.  FN will be invoked as: (funcall FN CURRENT-SELECTION SELECTION-DELTA TOTAL-SELECTION-DELTA).

Valid MOUSE-ACTION types for mouse button 1 are:

- 'mouse-1 : invoked when the mouse-1 button is released and
there is no mouse movement (no dragging happened).

- 'double-mouse-1 : invoked when the mouse-1 button is released
  after a double click.

- 'drag-increment-mouse-1 : invoked when the mouse-1 button is depressed and it has moved at least one whole character 'pixel' cell.

Mouse button 2 and 3 have the same 3 valid actions with the expected names.

- 'error : A special mouse hook, invoked when the mouse handler encounters an error.  This function will be called as (funcall FN ERROR-MESSAGE)")
(defconst 2dd-valid-mouse-preempts
  '(down-mouse-1)
  "These are all the valid mouse preempt types known to mouse handler.")

(defun 2dd-mouse-set-preempt (preempt-type fn)
  "Set the mouse preempt for action PREEMPT-TYPE to be FN."
  (let ((existing (assq preempt-type 2dd-mouse-preempts)))
    (if existing
        (setcdr existing fn)
      (setq 2dd-mouse-preempts (cons `(,preempt-type . ,fn) 2dd-mouse-preempts)))))
(defsubst 2dd-mouse-clear-preempt (preempt-type)
  "Remove the PREEMPT-TYPE preempt from the current mouse preempts."
  (setq 2dd-mouse-preempts (assq-delete-all preempt-type 2dd-mouse-preempts)))
(defsubst 2dd-mouse-clear-all-preempts ()
  "Remove all elements from the current mouse preempts."
  (setq 2dd-mouse-preempts nil))
(defsubst 2dd-mouse-get-preempt (preempt-type)
  "Return the function for PREEMPT-TYPE if there is any."
  (let ((cell (assq preempt-type 2dd-mouse-preempts)))
    (if cell
        (cdr cell)
      nil)))
(defsubst 2dd--mouse-run-hook (type &rest args)
  "This function will run mouse hook of TYPE with ARGS.

If the hook exists this function will return t.  If the hook does
not exist and therefore nothing was run this function will return
nil."
  (let ((hook (assq type 2dd-mouse-hooks)))
    (if hook
        (progn
          (apply (cdr hook) args)
          t)
      nil)))
(defun 2dd--mouse-ignore-motion-send-error (bubble-up-error)
  "This function will absorb any mouse movement and signal an error afterwards.

It will"
  (let ((event))
    (track-mouse
      (while (and (setq event (read-event))
                  (mouse-movement-p event))
        nil))
    (unless (2dd--run-mouse-hook 'error bubble-up-error)
      (error "Error from 2dd-mouse-handler: %s" bubble-up-error))))

(defun 2dd-mouse-handler (event)
  "Handle all mouse events of concern."
  ;; TODO - Not sure if I'm approaching mouse handling properly.
  (interactive "e")
  ;; (message "mouse initial event: %s" event)
  (let ((bubble-up-error)
        (current-window (first (second event))))
    (cl-flet ((pixel-from-event
               (event)
               ;; If you're not in the current window (where the event was started), do not produce a pixel.
               (when (eq (first (second event)) current-window)
                 (let ((col-row-cell (first (nthcdr 6 (second event)))))
                   ;; TODO - this can probably be made into a cons here.
                   (2dg-pixel :x (car col-row-cell)
                              :y (cdr col-row-cell))))))
      (let ((event-type (car event))
            (drag-handler-type))
        ;; mouse down handlers
        ;; (message "(%s event-type %s)" (gensym) event-type)

        ;; This when condition both evaluates if the mouse event is
        ;; acceptable to start drag monitoring as well as set the
        ;; proper drag handler type.  It's not my best work...
        (when (or (when (or (eq event-type 'down-mouse-1)
                            (eq event-type 'double-down-mouse-1))
                    (setq drag-handler-type 'drag-increment-mouse-1))
                  (when (or (eq event-type 'down-mouse-2)
                            (eq event-type 'double-down-mouse-2))
                    (setq drag-handler-type 'drag-increment-mouse-2))
                  (when (or (eq event-type 'down-mouse-3)
                            (eq event-type 'double-down-mouse-3))
                    (setq drag-handler-type 'drag-increment-mouse-3)))
          ;; (message "%s down" scxml-test-counter)
          (mouse-set-point event)
          (let* ((start-pixel (pixel-from-event event))
                 (last-pixel start-pixel)
                 (is-drag)
                 (event-count 0))
            (setq 2dd-mouse-last-clicked-pixel start-pixel)

            ;; Preempt down-mouse-1
            (when (eq event-type 'down-mouse-1)
              (let ((down-mouse-1-preempt (2dd-mouse-get-preempt 'down-mouse-1)))
                (when down-mouse-1-preempt
                  ;; if a mouse-down event causes an error and aborts this
                  ;; function the error will be displayed.  At that point
                  ;; when the mouse is released another event will be
                  ;; generated (a mouse click event, which I'm starting to
                  ;; understand is a bit more of a mouse-up event).  That
                  ;; mouse-click(mouse-up) event is another event and will
                  ;; clear the error displayed in the minibuffer.
                  ;; However, I wish to show the error message from the
                  ;; mouse-down event in the minibuffer after the mouse is
                  ;; released.  Therefore I will catch any errors from the
                  ;; mouse-down event here, allow the (track-mouse) form
                  ;; below to catch all mouse movement and even the
                  ;; mouse-click (mouse-up) event later.  After all mouse
                  ;; interactions I will then display the error.
                  ;;
                  ;; So
                  ;; - mouse down
                  ;; -  error generated -> capture error
                  ;; - enter (track-mouse) form which takes no actions and simply absorbs mouse events
                  ;; -  when that (track-mouse) form detects a click (mouse-up) exit.
                  ;; - Execute no functionality but finally signal the error captured.
                  (condition-case caught-error
                      (progn
                        (funcall down-mouse-1-preempt start-pixel)
                        (2dd-mouse-clear-preempt 'down-mouse-1))
                    (error (progn
                             (2dd-mouse-clear-all-preempts)
                             (setq bubble-up-error (second caught-error)))))
                  (setq event-count 1))))

            (2dd--mouse-run-hook event-type start-pixel nil nil)

            (if bubble-up-error
                ;; there was an error, eat up all mouse motion and send the error.
                (2dd--mouse-ignore-motion-send-error bubble-up-error)
              ;; No error, track the mouse motion if there is any.
              (track-mouse
                ;; real mouse track-mouse form - no errors have yet happened.
                (while (and (setq event (read-event))
                            (mouse-movement-p event))
                  (setq is-drag t)
                  ;; (message "mouse track event: %s" event)
                  ;; ok, you've started moving....
                  (incf event-count)
                  ;; (message "event count: %s" event-count)
                  (let* ((current-pixel (pixel-from-event event)))
                    ;; Only process when the 'pixel' changes.  That's the smallest unit of distance a user can change something by
                    (when (and current-pixel ;pixel must be valid and exist (it won't exist if you leave the window)
                               (not (equal current-pixel last-pixel)))
                      (let* ((current-delta (2dg-subtract current-pixel last-pixel))
                             (total-delta (2dg-subtract current-pixel start-pixel)))

                        ;; call some drag handler
                        (2dd--mouse-run-hook drag-handler-type
                                             current-pixel
                                             current-delta
                                             total-delta))
                      (setq last-pixel current-pixel)))))
              (setq event-type (car event))
              (setq 2dd-mouse-last-clicked-pixel (pixel-from-event event))
              ;; (message "mouse post-drag event: %s" event)
              ;; this will either be a mouse-1,2 or 3 event.  It will
              ;; not be a drag event so I won't be sending any drag
              ;; delta information.
              (2dd--mouse-run-hook (car event) 2dd-mouse-last-clicked-pixel nil nil)
              )))))))

(provide '2dd-mouse-handler)
;;; 2dd-mouse-handler.el ends here
