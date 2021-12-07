;;; gk-global-mode-util.el --- help dealing with global modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; This module provides utilities for global modes, like turning them on
;; and off collectively with a single command, registering and
;; unregistering them, disabling default modes etc.

;; All the modes listed in =gk-global-modes= are toggled on with an
;; =after-init-hook=, so modifications to this variable that happen up
;; until the execution of the named hook will actually determine which
;; modes are turned on.

;; =gk-disabled-modes= is a list of modes to disable.

;; Each of this lists contain symbols, actually =*-mode= functions.  The
;; ones in the former will be called with =+1= as the argument, and ones
;; in the latter with =-1=.

;;; Code:

(defvar gk-global-modes nil "List of global modes to be enabled.")
(defvar gk-disabled-modes nil "List of disabled global modes.")

(defvar gk-toggle-global-modes nil)
(defun gk-toggle-global-modes ()
  "Enable or disable the modes listed in `gk-global-modes' at once."
  (interactive)
  (setf gk-toggle-global-modes (not gk-toggle-global-modes))
  (let (errors)
    ;; Enable global modes
    (dolist (mode gk-global-modes)
      (condition-case e
          (funcall mode (if gk-toggle-global-modes 1 -1))
        (error (push `(,mode ,e) errors))))
    ;; Disable modes in gk-disabled-modes
    (dolist (mode gk-disabled-modes)
      (condition-case e
          (funcall mode (if gk-toggle-global-modes -1 1))
        (error (push `(,mode ,e) errors))))
    (when errors
      (warn "Following errors occurred when activating global modes:\n%S"
            errors))))

(add-hook 'after-init-hook 'gk-toggle-global-modes)



(provide 'gk-global-mode-util)
;;; gk-global-mode-util.el ends here
