;;; gk-ad.el --- generic advices                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Nonspecific general purpose functions, designed to be advices
;; (viz. ‘add-function’).

;;; Code:

(defun gk-ad-stay-here (fun &rest args)
  "Stay in the current buffer when running FUN.

Pass ARGS to FUN."
  (save-window-excursion
    (apply fun args)))

(defun gk-protect-frame-focus (f &rest args)
  "Generic :around advice to reclaim frame focus.

Some interactions with the OS, e.g. sending a link to the browser
may result in Emacs losing focus. This is very rude of the
OS. This function is a generic :around-advice that runs the given
function and then reclaims focus after some time so the user can
continue interacting with Emacs."
  (let ((frame (selected-frame)))
    (apply f args)
    (sit-for .3)
    (x-focus-frame frame)))



(provide 'gk-ad)
;;; gk-ad.el ends here
