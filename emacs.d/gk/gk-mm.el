;;; gk-mm.el --- multimedia configurations           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Configuration for images, videos, music, etc.

;;; Code:

(require 'image-mode)

(require 'gk-mac)

;;;; Images:

;; Viewing and editing images.

(setf
 ;; Automatically fit images to window on both dimensions.
 image-auto-resize 'fit-window)



;;;;; Scaling images:

(define-obsolete-function-alias 'gk-fit-image-to-window
  'image-transform-fit-to-window "prior to 2021-12-07")

(define-key image-mode-map "=" #'gk-fit-image-to-window)

;;;;; Keybindings:

;; By default animated images in Emacs don't loop when played.  We set up
;; so that when animated with RET, they play once; and when animated with
;; SPC, they loop.  Also, when hit ‘q‘, kill the image buffer, don't bury
;; it.  This'd presumably save some memory.

(define-key image-mode-map [?q] 'kill-this-buffer)
(define-key image-mode-map [?\ ] (gk-interactively
                                   (let ((image-animate-loop t))
                                     (image-toggle-animation))))




(provide 'gk-mm)
;;; gk-mm.el ends here
