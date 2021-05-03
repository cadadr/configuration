;;; exwm-init.el --- EXWM init                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>

;;; Code:

(require 'battery)
(require 'exwm)
(require 'exwm-config)
(require 'exwm-systemtray)

;; Set the initial workspace number.
(setq exwm-workspace-number 4)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

;; 's-N': Switch to certain workspace
(dolist (i (number-sequence 0 3))
  (exwm-input-set-key
   (kbd (format "s-%d" (1+ i)))
   (gk-interactively (exwm-workspace-switch i))))

;; Launch command
(exwm-input-set-key
 (kbd "s-r")
 (lambda (command)
   (interactive (list (read-shell-command "$ ")))
   (start-process-shell-command command nil command)))

;; Line-editing shortcuts
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ([?\M->] . [end])
        ([?\M-<] . [home])))

(setf exwm-systemtray-height 16)

;; Enable EXWM
(exwm-systemtray-enable)
(exwm-enable)

;; Configure Ido
(when ido-mode
  (add-hook 'exwm-init-hook
            #'exwm-config--fix/ido-buffer-window-other-frame))

;; Shortcuts
(defun www ()
  (interactive)
  (start-process-shell-command "Firefox" nil "gk-web-browser"))

(defun free ()
  (interactive)
  (shell-command "free -h"))

;; Monitors
(cl-pushnew 'display-time-mode gk-global-modes)
(cl-pushnew 'display-battery-mode gk-global-modes)

(provide 'exwm-init)
;;; exwm-init.el ends here
