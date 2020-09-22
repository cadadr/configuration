;;; loadpaths.el --- setup loadpaths

;;; Commentary:
;; This file is meant to be ‘load’ed by init.el.  Do not ‘require’ it.



;;; Code:

;; Prefer newer files when loading:
(setq load-prefer-newer t)

(require 'cl-lib)

(defun gk-directory-files (directory &optional include-dotfiles relative-names)
  "Saner version of ‘directory-files’.
Exclude dot-files, don't sort, and return full paths by default."
  (directory-files
   directory
   (not include-dotfiles)
   directory-files-no-dot-files-regexp
   t))

(defvar gk-elisp-site-dir
  (locate-user-emacs-file "lisp/site")
  "Directory where 3rd party Elisp is contained.")

(defvar gk-elisp-gk-dir
  (locate-user-emacs-file "lisp/gk")
  "Directory where my config libraries are contained.")

;; System paths.
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(let ((default-directory "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (expand-file-name ".guix-profile/share/emacs/site-lisp"
                                          (getenv "HOME")))

;; Add custom paths.
(add-to-list 'load-path (expand-file-name  "~/co/elisp"))
(dolist (p '("lisp" "themes" "ext"))
  (add-to-list 'load-path
               (expand-file-name
                (locate-user-emacs-file p))))

(defun gk-update-user-site-paths ()
  "Update ‘load-path’ with new subdirs from ‘gk-elisp-site-dir’."
  (interactive)
  (let ((dirs (cl-remove-if-not
               #'file-directory-p
               (gk-directory-files gk-elisp-site-dir))))
    (dolist (dir dirs)
      (unless (equal load-path
                     (cl-pushnew dir load-path
                                 :test #'string-equal))
        (message "Added %s to ‘load-path’" dir)))))

(gk-update-user-site-paths)

(add-to-list 'load-path gk-elisp-gk-dir)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list
   'Info-directory-list
   (expand-file-name "docs" gk-elisp-site-dir)))

;; Custom themes:
(add-to-list 'custom-theme-load-path
             (expand-file-name (locate-user-emacs-file "themes")))
(add-to-list 'custom-theme-load-path
             (expand-file-name (locate-user-emacs-file "vendored-lisp/themes")))
(add-to-list 'custom-theme-load-path
             (expand-file-name  "~/co/elisp"))
(message "Load paths are set up.")
