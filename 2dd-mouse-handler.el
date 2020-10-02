;;; 2dd-mouse-handler.el --- 2dd mouse handling -*- lexical-binding: t -*-

;;; Commentary:
;; Mouse interaction in 2dd is handled by a single function.  This
;; function is resposible for real-time mouse dragging updates,
;; clicks, double clicks as well as providing hooks for mouse actions

;;; Commentary:
;; Currently the mouse handler only works with a 3 button mouse that
;; has a single mouse wheel.  It will handle mouse-1,2 & 3 fully (with
;; dragging) and mouse-4 and 5 for clicks only (4 and 5 seem to be
;; mouse wheel movements).

;; It also has an 'override' system where certain mouse actions can
;; have their next event 'overridden' with different functionality.
;; This is indented to be used for things such as default mouse
;; behavior being one thing, but during a more complex user
;; interaction (e.g link these 3 things together) the typical handler
;; can be ignored and the override handler is called.
;;
;; An override handler is cleared out after it is called making it a
;; one time override.  When the override handler is cleared the normal
;; mouse handler resumes

;;; Code:
(require '2dg)

;; TODO - I'm not even sure if I need this....
(defvar-local 2dd-mouse-enabled 't
  "Enable flag for the mouse handler (should be set to 't or 'nil).

This flag can be toggled whenever is needed to disable the mouse
handler entirely.

Things which may cause you to disable the mouse handler:
- popup menus which work on mouse events.
- Anything else that needs mouse input and you don't also want
the mouse-handler working with that input.")
(defvar-local 2dd-mouse-overrides 'nil
  "This variable holds an alist of the current mouse overrides.

A mouse override is a function called only once when a
mouse-event of the specified type is clicked.  Overrides are used
to facilitate GUI actions such as 'the arrow will end at the next
drawing selected'.  In that case a override for (probably)
'down-mouse-1 would be set to a function which completes an
arrow's destination.  The mouse handler will call the function
when it detects a 'mouse-down-1 event AND WILL NOT CALL THE
NORMAL HANDLER.  Once the mouse handler calls the override
function it will be cleared from the override list so it is only
called once.

Note: behavior is for a single mouse event.  A mouse click-drag
event is multiple events so overridding mouse-down-X will only
override the first event.  The subsequent mouse-drag-X events
will still be handled normally (unless they also have an
override).

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
(defconst 2dd-valid-mouse-overrides
  '(down-mouse-1)
  "These are all the valid mouse preempt types known to mouse handler.")

(defsubst 2dd-mouse-set-enable (flag)
  "Enable (if FLAG is non-nil) or disable (if FLAG is nil) the mouse handler."
  (setq-local 2dd-mouse-enabled flag))

(defun 2dd-mouse-set-override (type fn)
  "Set the mouse override for mouse event TYPE to be FN."
  (let ((existing (assq type 2dd-mouse-overrides)))
    (if existing
        (setcdr existing fn)
      (setq 2dd-mouse-overrides
            (cons `(,type . ,fn) 2dd-mouse-overrides)))))
(defsubst 2dd-mouse-clear-override (type)
  "Remove the mouse override for TYPE from the current overrides."
  (setq 2dd-mouse-overrides (assq-delete-all type 2dd-mouse-overrides)))
(defsubst 2dd-mouse-clear-all-overrides ()
  "Remove all elements from the current mouse overrides."
  (setq 2dd-mouse-overrides nil))
(defun 2dd--mouse-run-hook-get-error (type &rest args)
  "This function will run mouse hook (or override) of TYPE with ARGS.

Any errors from the hook/override will be caught and returned as
a string.  If there is any error detected, all mouse overrides
will be cleared."
  ;; (message "mouse hook: %s, %s" type args) TODO - this condition
  ;; case hides a good deal of information about the error, address
  ;; that or make a toggle to capture or not capture errors.
  ;; TODO - possibly I should be catching signals and not errors.
  (let ((error-message))
    ;; (condition-case caught-error
        (let ((override-hook (assq type 2dd-mouse-overrides)))
          (if override-hook
              ;; Override hook only
              (progn
                (apply (cdr override-hook) args)
                (2dd-mouse-clear-override type))
            ;; normal hook if found
            (let ((hook (assq type 2dd-mouse-hooks)))
              (when hook
                (apply (cdr hook) args)))))
        ;; (error (progn
        ;;        (2dd-mouse-clear-all-overrides)
        ;;        (setq error-message (second caught-error)))))
    error-message))
(defsubst 2dd--mouse-handle-error (error-string)
  "Handle this mouse error"
  (let ((err-handler (assq 'error 2dd-mouse-hooks)))
    (if err-handler
        (funcall (cdr err-handler) error-string)
      (error "Error from 2dd-mouse-handler: %s" error-string))))
(defun 2dd--mouse-ignore-motion-send-error (bubble-up-error)
  "This function will absorb any mouse movement and signal an error afterwards.

It will"
  (let ((event))
    (track-mouse
      (while (and (setq event (read-event))
                  (mouse-movement-p event))
        nil))
    (2dd--mouse-handle-error bubble-up-error)))

(defsubst 2dd-mouse-handler (event)
  "Main entry point for mouse handling."
  (interactive "e")
  (when 2dd-mouse-enabled
    (2dd--mouse-handler-enabled event)))
(defun 2dd--mouse-handler-enabled (event)
  "Handle all mouse events of concern."
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
            (drag-handler-type)
            (start-pixel (pixel-from-event event)))
        ;; mouse down handlers
        ;; (message "(%s event-type %s)" (gensym) event-type)
        (setq 2dd-mouse-last-clicked-pixel start-pixel)
        (mouse-set-point event)

        ;; This condition evaluates if the mouse event is acceptable
        ;; to start drag monitoring and sets the drag handler type.
        (cond ((or (eq event-type 'down-mouse-1)
                   (eq event-type 'double-down-mouse-1))
               (setq drag-handler-type 'drag-increment-mouse-1))
              ((or (eq event-type 'down-mouse-2)
                   (eq event-type 'double-down-mouse-2))
               (setq drag-handler-type 'drag-increment-mouse-2))
              ((or (eq event-type 'down-mouse-3)
                  (eq event-type 'double-down-mouse-3))
               (setq drag-handler-type 'drag-increment-mouse-3)))

        ;; TODO - this needs a refactor at this point.
        (if (null drag-handler-type)
            ;; This mouse event is not one monitored for dragging
            ;; (e.g. mouse-wheel-up and mouse-wheel-down).  Simply
            ;; call the hook and finish.
            (let ((bubble-up-error (2dd--mouse-run-hook-get-error event-type
                                                                  start-pixel
                                                                  nil
                                                                  nil)))
              (when bubble-up-error
                (2dd--mouse-handle-error bubble-up-error)))

          ;; This mouse event supports dragging, begin dragging code.
          ;; (message "%s down" scxml-test-counter)
          (let* ((last-pixel start-pixel)
                 (is-drag)
                 (event-count 0))
            ;; Preempt down-mouse-1 - TODO - relocate this.
            (setq bubble-up-error (2dd--mouse-run-hook-get-error event-type
                                                                 start-pixel
                                                                 nil
                                                                 nil))

            (if bubble-up-error
                ;; there was an error, eat up all mouse motion and send the error.
                (2dd--mouse-ignore-motion-send-error bubble-up-error)
              ;; No error, track the mouse motion if there is any.
              (track-mouse
                ;; real mouse track-mouse form - no errors have yet happened.
                (while (and (null bubble-up-error)
                            (setq event (read-event))
                            (mouse-movement-p event))
                  (setq is-drag t)
                  ;;(message "mouse track event: %s" event)
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
                        (setq bubble-up-error (2dd--mouse-run-hook-get-error drag-handler-type
                                                                             current-pixel
                                                                             current-delta
                                                                             total-delta)))
                      (setq last-pixel current-pixel)))))
              (if bubble-up-error
                  (2dd--mouse-ignore-motion-send-error bubble-up-error)
                (setq event-type (car event))
                (setq 2dd-mouse-last-clicked-pixel (pixel-from-event event))
                ;;(message "mouse post-drag event: %s" event)
                ;; this will either be a mouse-1,2 or 3 event.  It will
                ;; not be a drag event so I won't be sending any drag
                ;; delta information.
                (setq bubble-up-error (2dd--mouse-run-hook-get-error (car event) 2dd-mouse-last-clicked-pixel nil nil))
                (when bubble-up-error
                  (2dd--mouse-ignore-motion-send-error bubble-up-error))
              ))))))))

(provide '2dd-mouse-handler)
;;; 2dd-mouse-handler.el ends here
