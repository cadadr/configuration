;;; gk-mac.el --- generic utility macros             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Various general purpose macros.

;;; Code:

(defmacro when-fbound (proc &rest args)
  "Run proc if bound.

\(when-fbound PROC ARGS...)"
  `(when (fboundp (quote ,proc))
     (,proc ,@args)))

(defmacro gk-interactively (&rest body)
  "Wrap the BODY in an interactive lambda form.

Return the lambda.  It has as its sole argument a catch-all ‘_’."
  `(lambda (&rest _)
     ,(if (stringp (car body))
          (pop body)
        "Not documented.")
     (interactive)
     ,@body))


(defmacro gk-with-new-frame (parameters &rest body)
  "Create a new frame and run BODY in it.

PARAMETERS is passed to ‘make-frame’.

The new frame is bound to the lexically scoped variable
‘new-frame’ inside BODY.

The newly created frame is centred and the mouse pointer is put
at the centre of the newly created frame.  This only happens when
‘display-graphic-p’ is truthy."
  (declare (indent defun))
  (let ((frame (gensym)))
    `(let ((,frame (make-frame ,parameters)))
       (raise-frame ,frame)
       (select-frame-set-input-focus ,frame)
       (select-window (frame-first-window ,frame))
       (when (display-graphic-p)
         ;; Center frame
         (set-frame-position
          ,frame
          (/ (- (x-display-pixel-width) (window-pixel-width)) 2)
          ;; XXX(2020-09-15): for some reason this works better than
          ;; dividing by 2 on my Linux Mint 20 with Cinnamon.
          (floor (/ (- (x-display-pixel-height) (window-pixel-height)) 2.5)))
         ;; Move mouse into the new frame
         (set-mouse-absolute-pixel-position
          (/ (x-display-pixel-width) 2)
          (/ (x-display-pixel-height) 2)))
       (let ((new-frame ,frame)) ,@body))))


(defmacro setc (variable value)
  "Exactly like setq, but handles custom."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))



(provide 'gk-mac)
;;; gk-mac.el ends here
