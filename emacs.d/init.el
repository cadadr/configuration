;; init.el -- Emacs initialisation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl-lib)

;;; Loadpath:
(progn
  ;; Remove Emacs' Org mode for it to not mix with the local install.
  (setf load-path (cl-remove-if (lambda (p) (string-match "/org$" p)) load-path))

  ;; Sanitise.
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

  ;; Add custom paths.
  (add-to-list 'load-path (expand-file-name  "~/co/elisp"))
  (dolist (p '("gk" "site" "lisp" "themes" "ext"))
    (add-to-list 'load-path
                 (expand-file-name
                  (locate-user-emacs-file p))))

  ;; Add packages from ~/.emacs.d/packages.
  (let ((dirs (directory-files (locate-user-emacs-file "packages") t
                               directory-files-no-dot-files-regexp)))
    (dolist (dir dirs)
      (add-to-list 'load-path dir)))

  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list (locate-user-emacs-file "docs")))

  ;; Custom themes:
  (add-to-list 'custom-theme-load-path
               (expand-file-name (locate-user-emacs-file "themes")))
  (add-to-list 'custom-theme-load-path
               (expand-file-name  "~/co/elisp"))
  (message "Load paths are set up."))

;;; Load gk:
;; Load the source file if it's newer than its byte-compiled
;; counterpart.
(setf load-prefer-newer t)

(require 'org)
(let ((init-file (locate-user-emacs-file "gk.org")))
  (with-current-buffer
      (find-file-noselect init-file)
    (org-babel-tangle nil nil "elisp")
    (org-babel-load-file init-file)))


;;; Footer:
(provide 'init)
;;; init.el ends here

;;; Auto-generated stuff:

(put 'scroll-left 'disabled nil)
