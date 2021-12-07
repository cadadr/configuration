;;; gk-recomp.el --- (re)compilation related functions, customisations and commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; This bit of code helps with recompilation.  Various files external to
;; the configuration tree are loaded during the initialisation process.
;; Here we define a function called =gk-load= which makes note of each
;; file it loads in the variable =gk-loaded-files=, which is then used by
;; =gk-recompile= to determine which files need to be compiled to boost
;; the load speed next time.  This way, =gk-recompile= does not need a
;; manually curated list of files to be compiled, like it did up until
;; now.

;;; Code:

(defvar gk-loaded-files nil)

(defun gk-load (&rest args)
  "Identical to ‘load’, but makes note of files.

This function passes its arguments untouched to ‘load’, but
conses the car of ARGS to ‘gk-loaded-files’.  The contents of
that variable is then to be used to byte compile all the files
explicitly loaded in this config without manually listing their
names."
  (when (apply #'load args)
    (pushnew (expand-file-name (car args)) gk-loaded-files)))


(defun gk-recompile (&optional force)
  "Recompile my configuration.

If FORCE is non-nil, force compilation, i.e. compile even if
up-to-date."
  (interactive "p")
  (if (member 'native-compile features)
      (let ((files (cons gk-elisp-site-dir
                         (mapcar ($ (concat $1 ".el"))
                                 (remove-if-not #'file-exists-p gk-loaded-files)))))
        (native-compile-async files t))
    (mapcar ($ (byte-recompile-file $1 (> force 1) 0))
            (remove-if-not #'file-exists-p gk-loaded-files))
    (byte-recompile-directory (locate-user-emacs-file "lisp/site") 0 (> force 4))))

(defvar gk-load-test-file
  (expand-file-name (locate-user-emacs-file "etc/load-test.el")))

(defvar gk-load-test-output-buffer-name
  "*Startup File Test*")

(defvar gk-load-test-process-name
  "*Startup Test Process*")

(defun gk-test-init ()
  (interactive)
  (compile (mapconcat
            #'identity
            (list gk-emacs-executable "-Q" "--batch" "-l" gk-load-test-file)
            " ")))



(provide 'gk-recomp)
;;; gk-recomp.el ends here
