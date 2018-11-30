;;; exwm-init.el --- EXWM init                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>

;;; Code:

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
(cl-loop for i from 1 to exwm-workspace-number
         do (exwm-input-set-key
             (kbd (format "s-%d" i))
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
        ([?\C-k] . [S-end delete])))

(setf exwm-systemtray-height 16)

;; Enable EXWM
(exwm-systemtray-enable)
(exwm-enable)

;; Configure Ido
(when ido-mode
  (add-hook 'exwm-init-hook
            #'exwm-config--fix/ido-buffer-window-other-frame))

(defun www ()
  (interactive)
  (start-process-shell-command "Firefox" nil "gk-web-browser"))

(provide 'exwm-init)
;;; exwm-init.el ends here
