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
  ;; XXX(2021-04-16): this will fail on systems that didn’t load
  ;; dotfiles/profile.sh
  (expand-file-name "lisp/emacs" (getenv "MYLIB"))
  "Directory where 3rd party Elisp is contained.")

(defvar gk-elisp-gk-dir
  (locate-user-emacs-file "gk")
  "Directory where my config libraries are contained.")

(defvar gk-pub-elisp-dir
  (expand-file-name
   (car
    (cl-remove-if-not
     (lambda (f) (file-exists-p f))
     (list
      "~/Sources/elisp"
      "~/elisp"))))
  "Directory where my public Elisp programs are.")

(unless (file-exists-p gk-pub-elisp-dir)
  (user-error "Göktuğ’s Elisp Packages directory does not exist; edit ‘gk-pub-elisp-dir’ or clone https://codeberg.org/cadadr/elisp"))

;; System paths.
(when (file-exists-p "/usr/share/emacs/site-lisp/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
  (let ((default-directory "/usr/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

(when-let* ((guix-site-lisp-path
             (expand-file-name ".guix-profile/share/emacs/site-lisp" (getenv "HOME"))))
  (add-to-list 'load-path guix-site-lisp-path))

;; Add custom paths.
(add-to-list 'load-path gk-pub-elisp-dir)
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
   (expand-file-name "docs" gk-elisp-site-dir))
  (add-to-list
   'Info-directory-list
   (expand-file-name "docs/gcl-clhs" gk-elisp-site-dir)))

;; Custom themes:
(add-to-list 'custom-theme-load-path
             (expand-file-name (locate-user-emacs-file "themes")))
(add-to-list 'custom-theme-load-path
             (expand-file-name (locate-user-emacs-file "vendored-lisp/themes")))
(add-to-list 'custom-theme-load-path
             gk-pub-elisp-dir)
(message "Load paths are set up.")
