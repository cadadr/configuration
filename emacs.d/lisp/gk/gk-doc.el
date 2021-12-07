;;; gk-doc.el --- document viewing / procecessing (doc/odt/pdf/man...)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Customisations for various modes that handle ‘rich’---for some
;; definition of ‘rich’---document formats.

;;; Code:

;;;; Docview:

(defun gk-doc-view-open-externally ()
  "Open current document externally."
  (interactive)
  (browse-url-xdg-open (concat "file://" (buffer-file-name))))

(define-key doc-view-mode-map [?&] #'gk-doc-view-open-externally)



;;;; PDF-tools:

;; TODO(2018-05-25): implement a smarter resizing addon where the
;; resize factor can vary

;; PDF tools is a sophisticated alternative to DocView for PDF files.

(setf
 pdf-info-epdfinfo-program
 (gk-executable-ensure
  (expand-file-name "pdf-tools/server/epdfinfo" gk-elisp-site-dir))
 pdf-tools-enabled-modes
 '(pdf-isearch-minor-mode
   pdf-links-minor-mode
   pdf-misc-minor-mode
   pdf-outline-minor-mode
   pdf-misc-size-indication-minor-mode
   pdf-misc-menu-bar-minor-mode
   pdf-sync-minor-mode
   pdf-misc-context-menu-minor-mode
   pdf-cache-prefetch-minor-mode
   pdf-annot-minor-mode)
 ;; Manually change the page.
 pdf-view-continuous nil
 ;; Resize more granularly.
 pdf-view-resize-factor 1.1)

(pdf-tools-install-noverify)

(define-key pdf-view-mode-map (kbd "M-w") #'pdf-view-kill-ring-save)
(define-key pdf-view-mode-map "q" #'bury-buffer)

(define-key pdf-view-mode-map (kbd "M-1")
  (gk-interactively
   (pdf-annot-add-highlight-markup-annotation
    (pdf-view-active-region t)
    "yellow")))

(define-key pdf-view-mode-map (kbd "M-2")
  (gk-interactively
   (pdf-annot-add-highlight-markup-annotation
    (pdf-view-active-region t)
    "medium spring green")))

(define-key pdf-view-mode-map (kbd "M-3")
  (gk-interactively
   (pdf-annot-add-highlight-markup-annotation
    (pdf-view-active-region t)
    "coral")))

(define-key pdf-view-mode-map (kbd "M-4")
  (gk-interactively
   (pdf-annot-add-highlight-markup-annotation
    (pdf-view-active-region t)
    "dark turquoise")))



;;;; Man pages:

(when (executable-find "man")
  (require 'man)
  (setf
   ;; Make man page current buffer in current window of current frame.
   ;; Or: show the fucking man page where I want it.
   Man-notify-method 'pushy))



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
