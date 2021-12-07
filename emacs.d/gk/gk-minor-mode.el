;;; gk-minor-mode.el --- meta minor mode for my config  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; The GK minor mode is at the heart of this configuration.  Almost
;; all keybindings, except unmapping some keys from the global map,
;; and except bindings in specific modes, should be done with this
;; minor modes keymap.  This minor mode is active everywhere, except
;; the Minibuffer and the Fundamental mode buffers.

;;; Code:

(defgroup GK nil
  "Group for my configuration."
  :group 'emacs
  :prefix "gk-")

(defvar gk-minor-mode-map
  (make-sparse-keymap)
  "Where to put all my bindings.")

(defvar gk-minor-mode-prefix-map
  (make-sparse-keymap)
  "Prefix map for my bindings.")

(fset 'gk-minor-mode-prefix-map gk-minor-mode-prefix-map)

(defvar gk-minor-mode-prefix "\C-c"
  "Keymap prefix for `gk-minor-mode'.")

(define-minor-mode gk-minor-mode
  "Global minor mode for customisations.

\\{gk-minor-mode-map}"
  nil "" gk-minor-mode-map
  (let ((map gk-minor-mode-map))
    (define-key map gk-minor-mode-prefix #'gk-minor-mode-prefix-map)))

(define-globalized-minor-mode global-gk-minor-mode gk-minor-mode
  gk-minor-mode)

;; Helper macros for binding keys with ‘gk-minor-mode’.
(defmacro gk-global-binding (&rest args)
  (declare (indent defun))
  `(define-key gk-minor-mode-map ,@args))

(defmacro gk-prefix-binding (&rest args)
  (declare (indent defun))
  `(define-key gk-minor-mode-prefix-map ,@args))



(provide 'gk-minor-mode)
;;; gk-minor-mode.el ends here
