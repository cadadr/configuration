;; init.el -- Emacs initialisation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl-lib)

;;; Loadpath:
(progn
  ;; Sanitise.
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

  ;; Add custom paths.
  (add-to-list 'load-path (expand-file-name  "~/co/elisp"))
  (dolist (p '("site" "lisp" "themes" "ext"))
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
(let* ((init-org (locate-user-emacs-file "gk.org"))
       (init-org-modtime
        (time-to-seconds
         (file-attribute-modification-time (file-attributes init-org))))
       (init-org-buf (find-file-noselect init-org))
       (init-el (locate-user-emacs-file "gk.el"))
       (init-el-modtime
        (time-to-seconds
         (file-attribute-modification-time (file-attributes init-el)))))
  (when (or
	 (not (file-exists-p init-el))
	 (> init-org-modtime init-el-modtime))
    (with-current-buffer init-org-buf
      (org-babel-tangle nil nil "elisp")))
  (load (file-name-sans-extension init-el)))


;;; Footer:
(provide 'init)
;;; init.el ends here

;;; Auto-generated stuff:

(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)
