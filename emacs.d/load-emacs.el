;;; .emacs -- load emacs configuration.
(setf user-emacs-directory (expand-file-name "~/co/emacs.d/"))
(load (locate-user-emacs-file "init.el"))
