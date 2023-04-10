;;; gk-doc.el --- document viewing / procecessing (doc/odt/pdf/man...)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023  Göktuğ Kayaalp

;;; Commentary:

;; Customisations for various modes that handle ‘rich’---for some
;; definition of ‘rich’---document formats.

;;; Code:

(require 'deft)
(require 'doc-view)
(require 'man)
(require 'pdf-tools)

(require 'gk-fd)

;;;; Docview:

(defun gk-doc-view-open-externally ()
  "Open current document externally."
  (interactive)
  (browse-url-xdg-open (concat "file://" (buffer-file-name))))

(define-key doc-view-mode-map [?&] #'gk-doc-view-open-externally)



;;;; PDF-tools:

;; This needs to be set first, before loading the libraries and
;; running ‘pdf-tools-install’ or ‘pdf-tools-install-noverify’.
(setf
 pdf-info-epdfinfo-program
 (gk-executable-ensure
  (expand-file-name "pdf-tools/server/epdfinfoo" gk-elisp-site-dir)))

(when (and pdf-info-epdfinfo-program
           (file-exists-p pdf-info-epdfinfo-program))
  (require 'gk-pdftools))



;;;; Man pages:

(setf
 ;; Make man page current buffer in current window of current frame.
 ;; Or: show the fucking man page where I want it.
 Man-notify-method 'pushy)



;;; Deft:

(setf
 ;; Finding the files to be searched.
 deft-directory gk-notes-directory
 ;; Search recursively from ‘deft-directory’.
 deft-recursive t
 ;; The ‘car’ of this list is the default extension when creating
 ;; files from Deft.
 deft-extensions '("org" "txt" "md" "markdown" "textile")
 ;; Destination for ‘C-c C-a’ in deft.
 deft-archive-directory "Attic/deft/"
 ;; Disable auto save.
 deft-auto-save-interval 0)

(defun gk-deft (&optional arg)
  "Run ‘deft’.

With no prefix arguments, just run ‘deft’; it’ll open in the
current window.

With one prefix argument, it’ll open in a new frame.

With two prefix arguments, it’ll open in the current frame and
will become the only window."
  (interactive "p")
  (cl-case arg
    (1 (deft))
    (4 (gk-with-new-frame nil (deft)))
    (16 (delete-other-windows) (deft))))



(provide 'gk-doc)
;;; gk-doc.el ends here
