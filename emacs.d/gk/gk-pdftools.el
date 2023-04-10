;;; gk-pdftools.el --- pdf-tools configuration       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Göktuğ Kayaalp


;;; Commentary:

;; PDF-Tools is pretty finicky if it’s server executable is not
;; compiled. Even merely trying to load its modules causes errors. So
;; I isolate its configuration in this module and parameterise its
;; loading in ‘gk-doc’ on whether the server executable exists.

;; ‘pdf-info-epdfinfo-program’ is set in ‘gk-doc’.

;;; Code:



;;;; Setup:

;; TODO(2018-05-25): implement a smarter resizing addon where the
;; resize factor can vary

(require 'pdf-view)
(require 'pdf-annot)
(require 'pdf-cache)
(require 'pdf-isearch)
(require 'pdf-misc)
(require 'pdf-outline)
(require 'pdf-sync)

(setf
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
   pdf-annot-minor-mode))

(pdf-tools-install-noverify)

;;;; Configuration:

;; Add a way to get the currently shown annotation, to be able to
;; store link.

(defvar-local gk-pdf-annot-currently-shown-annotation
    nil
  "The annotation that is highlighted in the current buffer.")

(define-advice pdf-annot-show-annotation
    (:after (a &optional _ window) make-note-of-active-annot)
  "Make a buffer-local note of the shown annotation."
  (with-current-buffer (window-buffer window)
    (setq-local gk-pdf-annot-currently-shown-annotation a)))

(setf
 ;; Manually change the page.
 pdf-view-continuous nil
 ;; Resize more granularly.
 pdf-view-resize-factor 1.1
 ;; Show ‘contents’ instead of label in annotation list.
 pdf-annot-list-format
 '((page . 5) (type . 10) (contents . 30) (date . 24))
 ;; Make pdf-annot-list appear in a nicer fashion.
 pdf-annot-list-display-buffer-action
 '((display-buffer-reuse-window display-buffer-at-bottom)
   (inhibit-same-window . t)
   (side . bottom)
   (window-height . 10)))

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

(defun gk-pdf-annot-list-mode-hook ()
  (pdf-annot-list-follow-minor-mode +1))

(add-hook 'pdf-annot-list-mode-hook #'gk-pdf-annot-list-mode-hook)




(provide 'gk-pdftools)
;;; gk-pdftools.el ends here
