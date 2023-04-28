;;; gk-wm.el --- utilities for window managers and desktop environments  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023  Göktuğ Kayaalp

;;; Commentary:

;; These functions and commands are meant to help Emacs communicate
;; and/or manipulate the WM/DE it finds itself in.

;;; Code:

(defvar gk-preferred-colour-scheme-override nil
  "Override the value ‘gk-preferred-colour-scheme’ returns.

‘gk-preferred-colour-scheme’ tries to predict the current colour
scheme from a variety of sources.  If this variable is non-nil,
it will return this variable’s value instead.

Useful values are ‘:light’, ‘:dark’, and ‘:no-preference’.")

(defun gk-override-preferred-colour-scheme (new-scheme)
  "Override the preferred colour scheme for this session.
NEW-SCHEME should be one of ‘:light’ or ‘:dark’.

Sets ‘gk-preferred-colour-scheme-override’ to NEW-SCHEME and runs
‘gk-update-frame-names’.

Interactively, prompt for the value of NEW-SCHEME."
  (interactive
   (list
    (let* ((opts `((,(format "Dark colour scheme (theme: %s)"
                             (plist-get gk-preferred-themes :dark))
                    :dark)
                   (,(format "Light colour scheme (theme: %s)"
                             (plist-get gk-preferred-themes :light))
                    :light)))
           (current-scheme (substring
                            (symbol-name (gk-preferred-colour-scheme)) 1))
           (question (format "New colour scheme (currently: %s): "
                             current-scheme))
           (answer (completing-read question opts nil t)))
      (car (alist-get answer opts nil nil #'string=)))))
  (unless (member new-scheme (list :dark :light))
    (user-error "New colour scheme should be one of: :dark, :light."))
  (setf gk-preferred-colour-scheme-override new-scheme)
  (gk-setup-frame-looks))

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
prefers dark themes.

If ‘gk-preferred-colour-scheme-override’ is non-nil, its value is
returned instead."
  (cond
   ((not (null gk-preferred-colour-scheme-override))
    gk-preferred-colour-scheme-override)
   ((and (eq system-type 'darwin)
         ;; Adapted from https://github.com/LionyxML/auto-dark-emacs.
         (string-equal
          "true"
          (string-trim
           (shell-command-to-string
            (concat "osascript -e 'tell application \"System Events\" "
                    "to tell appearance preferences to return dark mode'")))))
    :dark)
   ((and (getenv "DBUS_SESSION_BUS_ADDRESS")
         (eq 1 (caar (dbus-ignore-errors
              (dbus-call-method
               :session
               "org.freedesktop.portal.Desktop"
               "/org/freedesktop/portal/desktop"
               "org.freedesktop.portal.Settings" "Read"
               "org.freedesktop.appearance" "color-scheme")))))
    :dark)
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
