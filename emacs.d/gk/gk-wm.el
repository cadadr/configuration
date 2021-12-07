;;; gk-wm.el --- utilities for window managers and desktop environments  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; These functions and commands are meant to help Emacs communicate
;; and/or manipulate the WM/DE it finds itself in.

;;; Code:

(defun gk-i3wm-get-current-workspace-id ()
  "Return focused workspace number and name as a cons cell."
 (let* ((workspaces
         (with-temp-buffer
           (call-process "i3-msg" nil t nil "-t" "get_workspaces")
           (goto-char (point-min))
           (json-parse-buffer)))
        (focused (seq-filter ($ (eq t (gethash "focused" $1)))
                             workspaces)))
   (unless (eq 1 (length focused))
     (error "Unreachable state: multiple focused workspaces"))
   (cons (gethash "num" (car focused))
         (gethash "name" (car focused)))))

(defun gk-preferred-colour-scheme ()
  "Find out the system’s preferred colour scheme.

Returns :light if the preferred colour scheme is light, :light if
no preference is set or can be determined, or :dark if the user
prefers dark themes."
  (cond
   ((and (string= (getenv "DESKTOP_SESSION") "cinnamon")
         (save-match-data
           (string-match
            "Dark"
            (shell-command-to-string
             "dconf read /org/cinnamon/desktop/interface/gtk-theme"))))
    :dark)
   ((and (string= (getenv "DESKTOP_SESSION") "pop")
         (save-match-data
           (string-match
            "dark"
            (shell-command-to-string
             "dconf read /org/gnome/desktop/interface/gtk-theme"))))
    :dark)
   ((and (string= (getenv "DESKTOP_SESSION") "xfce")
         (save-match-data
           (string-match
            "Dark"
            (shell-command-to-string
             "xfconf-query --channel xsettings --property /Net/ThemeName"))))
    :dark)
   ((string= (getenv "DESKTOP_SESSION") "i3wm")
    (intern (concat ":" (getenv "GK_COLOUR_SCHEME_PREFERENCE"))))
   (t
    :light)))



(provide 'gk-wm)
;;; gk-wm.el ends here
