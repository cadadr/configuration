;;; sappho-config.el --- configuration specific to the sappho system  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Göktuğ Kayaalp



;;; Code:



;;;; Dock:

;; Detect docked/undocked status and update Emacs configuration
;; accordingly.

(defvar gk-host:sappho-docked-status nil
  "Record most recent dockedness status.")

(defun gk-host:sappho-docked-p ()
  "Return t if the device is attached to the dock."
  (not
   (null
    (cl-remove-if
     ($
      (zerop
       (string-to-number
        (gk-get-file-contents
         (expand-file-name "docked" $1)))))
     (directory-files "/sys/bus/platform/devices" t "dock")))))

(defun gk-host:sappho-dock-watcher ()
  (let ((maybe-new-status (gk-host:sappho-docked-p)))
    (unless (equal maybe-new-status gk-host:sappho-docked-status)
      (setf gk-host:sappho-docked-status maybe-new-status)
      ;; Docked status changed, respond to the new situation.
      (if gk-host:sappho-docked-status
          (gk-host:sappho-on-docked)
        (gk-host:sappho-on-undocked))
      (gk-setup-frame-looks))))

(defun gk-host:sappho-on-docked ()
  (message "sappho: dock detected! updating configuration")
  (setf gk-font-default-height
        (- gk-font-default-height 10)))

(defun gk-host:sappho-on-undocked ()
  (message "sappho: no dock detected, or undocked! updating configuration")
  (setf gk-font-default-height
        (+ gk-font-default-height 10)))

(defvar gk-host:sappho-dock-watch-timer
  (run-with-idle-timer 3 t #'gk-host:sappho-dock-watcher)
  "Timer that watches the docked status
cf. ‘gk-host:sappho-on-docked’.")



(provide 'sappho-config)
;;; sappho-config.el ends here
