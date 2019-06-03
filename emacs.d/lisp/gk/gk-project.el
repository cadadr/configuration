;;; gk-project.el --- project frames

;;; Commentary:
;; This is a simple project setup based on ‘project.el’ that uses one
;; frame per project.



;;; Code:

(require 'f)
(require 'magit)
(require 'dollar)
(require 'project)

(require 'gk-mac)
(require 'gk-util)

(defvar gk-project-compile--hist nil)

(defvar gk-project-compile-default-command "make test"
  "Default command for ‘gk-project-compile’.")

(defun gk-project-compile (command)
  (interactive
   (list
    (read-shell-command
     "Run project compile command: "
     gk-project-compile-default-command
     gk-project-compile--hist)))
  (if-let* ((projbuf (get-buffer (assoca 'gk-project (frame-parameters)))))
      (with-current-buffer projbuf
        (compile command))
    (user-error "Not a project frame")))

(defvar gk-projects-directory "~/co"
  "Directory where software projects are located.")

(defun gk-open-project (path)
  "Open a project folder.

Dired buffer to the left, magit (or VC if not git) to the
right. Start a shell with name ‘*XXX shell*’ where XXX is the
basename of the PATH.

PATH is the path to the project."
  (interactive
   (list
    (f-slash
     (read-directory-name
      "Project to open: "
      (f-slash (expand-file-name gk-projects-directory))
      nil t))))
  (let* ((vcs
          (cond
           ((file-exists-p (expand-file-name ".git" path))
            #'magit-status)
           ((or (mapcar #'vc-backend (gk-directory-files path)))
            #'vc-dir)))
         (project-name (file-name-base
                        (replace-regexp-in-string "/+\\'" "" path)))
         (shell-name (format "*%s shell*" project-name)))
    (gk-with-new-frame `((fullscreen . maximized)
                         (gk-project . ,project-name)
                         (gk-project-dir . ,path)
                         (gk-project-shell . ,shell-name)
                         (gk-project-vcs . ,vcs))
      (delete-other-windows)
      (dired path)
      (split-window-sensibly)
      (other-window 1)
      (funcall vcs path)
      (save-window-excursion
        (let ((buf (get-buffer-create shell-name))
              (default-directory path))
          (unless (get-buffer-process buf)
            (shell buf)))))))

(defun gk-frame-parameters ()
  "Get my frame parameters."
  (cl-remove-if-not
   ($ (s-starts-with? "gk-" (symbol-name (car $1))))
   (frame-parameters)))

(defun gk-home ()
  "Take me to the home view."
  (interactive)
  ;; Close side windows off first because they can’t be the only
  ;; window.
  (when (window-with-parameter 'window-side)
    (window-toggle-side-windows))
  (delete-other-windows)
  (if (assoca 'gk-project-shell (frame-parameters))
      (let* ((fparam (frame-parameters))
             (vcs (assoca 'gk-project-vcs fparam))
             (dir (assoca 'gk-project-dir fparam)))
        (dired dir)
        (split-window-sensibly)
        (other-window 1)
        (funcall vcs dir))
    (find-file (gk-org-dir-file "start.org"))
    (split-window-sensibly)
    (other-window 1)
    (org-agenda nil "p")
    (other-window 1)
    (gk-flash-current-line)))



;;; Footer:

(provide 'gk-project)
;;; gk-project.el ends here
