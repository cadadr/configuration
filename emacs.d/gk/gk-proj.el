;;; gk-proj.el --- minimal project library           -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;;; Commentary:

;; This is a simple library to help working with «projects», similar
;; to ‘project.el’ and ‘projectile’.

;; ‘gk-proj’ is simpler tho, and probably lackluster for folks working
;; with big projects.

;; The idea here is we have one frame per project, the frame is
;; associated with some data about the project (like tye VCS type,
;; path, an associated ‘shell-mode’ buffer), and we have a few
;; functions that use such data, stored as frame properties, to help
;; us navigate the project.

;; A project is a directory tree, preferably with some sort of version
;; control.

;; We use ‘magit’ for git projects, and ‘vc’ for non-git projects.

;;; Code:

(require 'eshell)

(require 'gk-mac)
(require 'gk-minor-mode)


(defvar gk-projects-directory (expand-file-name "~/co")
  "Directory where software projects are located.")

(defvar gk-projects-use-eshell nil
  "Whether to use ‘eshell’ for project shells.

If nil, use ‘shell’ instead.")

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
  (if-let* ((projbuf (get-buffer (gk-assoca 'gk-project (frame-parameters)))))
      (with-current-buffer projbuf
        (compile command))
    (user-error "Not a project frame")))

(defun gk-create-project (name vcs parent-tree)
  "Create a new project.

NAME is the project name, and the project path is located in the
directory at PARENT-TREE + NAME.  PARENT-TREE defaults to
‘gk-projects-directory’.

If VCS is non-nil (and the name of a version control system
included in ‘vc-handled-backends’), a new repository with the
selected VCS is initialised under the new project directory.

The value of NAME is used directly in the project directory name,
so make sure it does not include unnecessary slashes or
problematic characters."
  (interactive (list (read-string "Project name (will be project path basename): ")
                     (vc-read-backend "VCS, empty for none: ")
                     (read-directory-name "Parent directory for project subtree: "
                                          (concat gk-projects-directory "/"))))
  (let ((project-tree (expand-file-name name parent-tree)))
    (condition-case e
        (make-directory project-tree)
      ('file-already-exists (message (apply #'format "%s: %s" (cdr e)))))
    (when vcs
     (let ((default-directory project-tree))
       (vc-create-repo vcs)))
    (gk-open-project project-tree)))

(defun gk-open-project (path &optional use-this-frame)
  "Open a project folder.

Dired buffer to the left, magit (or VC if not git) to the
right. Start a shell with name ‘*XXX shell*’ where XXX is the
basename of the PATH.

PATH is the path to the project.

If USE-THIS-FRAME is non-nil, or called interactively with a
non-zero prefix argument, use the current frame, instead of
creating a new one."
  (interactive
   (list
    (f-slash
     (read-directory-name
      (if current-prefix-arg
          "Project to open (*in _current_ frame*): "
        "Project to open (in new frame): ")
      (f-slash (expand-file-name "~"))
      nil t))
    (not (not current-prefix-arg))))
  (let* ((vcs
          (cond
           ((and
             (fboundp 'magit-status)
             (file-exists-p (expand-file-name ".git" path)))
            #'magit-status)
           ((or (mapcar #'vc-backend (gk-directory-files path)))
            #'vc-dir)))
         ;; This should be fairly duplicate-proof...
         (project-name (concat
                        (user-login-name)
                        "@"
                        (system-name)
                        ":"
                        ;; remove trailing slash(es)
                        (replace-regexp-in-string "/+\\'" "" path)))
         (shell-name (format "*%s shell*" project-name))
         (frame-params `((fullscreen . maximized)
                         (gk-project . ,project-name)
                         (gk-project-dir . ,path)
                         (gk-project-shell . ,shell-name)
                         (gk-project-vcs . ,vcs))))
    (cond (use-this-frame
           (pcase-dolist (`(,param . ,val) frame-params)
             (set-frame-parameter nil param val))
           (gk--open-project-1 vcs path shell-name))
          (t
           (gk-with-new-frame frame-params
             (gk--open-project-1 vcs path shell-name))))))


(defun gk--open-project-1 (vcs path shell-name)
  "Subroutine of ‘gk-open-project’."
  (delete-other-windows)
  (dired path)
  (split-window-sensibly)
  (other-window 1)
  (funcall vcs path))

(defun gk-frame-parameters ()
  "Get my frame parameters."
  (cl-remove-if-not
   ($ (s-starts-with? "gk-" (symbol-name (car $1))))
   (frame-parameters)))

;; Popup shell:
(defun gk--get-shell-for-frame (&optional arg-for-shell frame)
  "Get a shell for current frame, depending on whether it’s a project frame.

Subroutine for ‘gk-pop-shell’ and ‘gk-display-shell’."
  (save-window-excursion
    (let* ((prefix-arg arg-for-shell)
           (project-shell (frame-parameter frame 'gk-project-shell))
           (eshell-buffer-name (or project-shell
                                   eshell-buffer-name))
           (default-directory (or (frame-parameter frame 'gk-project-dir)
                                  default-directory)))
      (if gk-projects-use-eshell
          (eshell)
        (shell project-shell)))))

(defun gk-pop-shell (arg)
  "Pop a shell in a side window.

Pass arg to ‘shell’.  If already in a side window that displays a
shell, toggle the side window.

If there is a project shell associated to the frame, just show
that instead."
  (interactive "P")
  (if (and (gk-assoca 'window-side (window-parameters))
           (equal major-mode
                  (if gk-projects-use-eshell
                      'eshell-mode
                    'shell-mode)))
      (window-toggle-side-windows)
    (when-let* ((win (display-buffer-in-side-window
                      (gk--get-shell-for-frame arg)
                      '((side . bottom)))))
      (select-window win))))

;; Home view
(defun gk-home ()
  "Take me to the home view."
  (interactive)
  ;; Close side windows off first because they can’t be the only
  ;; window.
  (when (window-with-parameter 'window-side)
    (window-toggle-side-windows))
  (delete-other-windows)
  (if (gk-assoca 'gk-project-shell (frame-parameters))
      (let* ((fparam (frame-parameters))
             (vcs (gk-assoca 'gk-project-vcs fparam))
             (dir (gk-assoca 'gk-project-dir fparam)))
        (dired dir)
        (split-window-sensibly)
        (other-window 1)
        (funcall vcs dir))
    ;; If agenda is used, show that in ‘main’, otherwise show initial
    ;; buffer or scratch.
    (cond ((and (fboundp #'gk-org-agenda)
                gk-org-agenda-used-p)
           (gk-org-agenda))
          (initial-buffer-choice
           (ignore-errors (find-file initial-buffer-choice)))
          (t (switch-to-buffer "*scratch*")))
    (gk-flash-current-line)
    ;; If we can split the window, show ~/Desktop.
    (when (split-window-sensibly)
      (other-window 1)
      ;; If we can’t show ~/Desktop for some reason, delete the window
      ;; we created, as it’s now become useless.
      (unless (ignore-errors (dired "~/Desktop/"))
        (delete-window))
      (other-window 1))))



;;;; Keybindings:

(gk-prefix-binding "\C-f" #'project-find-file)
(gk-prefix-binding "\C-p" #'gk-open-project)
(gk-prefix-binding (kbd "C-+") #'gk-create-project)
(gk-prefix-binding [?\r] #'gk-project-compile)
(gk-prefix-binding "]" #'gk-pop-shell)
(gk-prefix-binding "\C-]" #'gk-display-shell)
(gk-prefix-binding "[" #'window-toggle-side-windows)
(gk-global-binding [home] 'gk-home)



(provide 'gk-proj)
;;; gk-proj.el ends here
