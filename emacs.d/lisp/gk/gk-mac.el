;;; gk-mac.el --- utility lisp macros



;;; Code:

(defmacro when-fbound (proc &rest args)
  "Run proc if bound.
\(when-fbound PROC ARGS...)"
  `(when (fboundp (quote ,proc))
     (,proc ,@args)))

(defmacro gk-interactively (&rest body)
  "Wrap the BODY in an interactive lambda form.
Return the lambda."
  `(lambda nil (interactive) ,@body))

(defmacro gk-with-new-frame (parameters &rest body)
  "Create a new frame and run BODY in it.
PARAMETERS are passed into ‘make-frame’."
  (declare (indent defun))
  (let ((frame (gensym)))
    `(let ((,frame (make-frame ,parameters)))
       (raise-frame ,frame)
       (select-frame-set-input-focus ,frame)
       (progn ,@body))))

(defmacro setc (variable value)
  "Exactly like setq, but handles custom."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))



;;; Footer:

(provide 'gk-mac)
;;; gk-mac.el ends here
