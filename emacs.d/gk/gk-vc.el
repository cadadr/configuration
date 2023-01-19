;;; gk-vc.el --- version control utilities and customisation  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023  Göktuğ Kayaalp

;;; Commentary:

;; Customisation for ‘magit’, ‘vc’, diffing, and other version control
;; related stuff.

;;; Code:

(require 'diff-mode)
(require 'ediff-diff)
(require 'ediff-wind)
(require 'log-edit)
(require 'magit-autorevert)
(require 'magit-commit)
(require 'magit-diff)
(require 'magit-todos)
(require 'vc)
(require 'vc-cvs)
(require 'vc-hg)
(require 'vc-hooks)

(require 'gk-mac)

(setf
 log-edit-diff-function 'vc-diff
 ;; STFU and follow them links.
 vc-follow-symlinks t
 ;; Do not write these out.
 vc-find-revision-no-save t)

(defun gk-log-edit-mode-hook ()
  "Hook for `log-edit-mode', used for vc checkins."
  (gk-text-editing-modes-hook))

(add-hook 'log-edit-mode-hook #'gk-log-edit-mode-hook)

(setf vc-cvs-diff-switches "-quNp"
      vc-rcs-diff-switches "-u")

(defun log-edit-insert-message-template ()
  "Disable log template.

Creates problems.
The default implementation of this function is in `log-edit.el'."
  (interactive))

(add-hook
 'vc-dir-mode-hook
 (defun gk-vc-dir-mode-hook ()
   (hl-line-mode +1)))



;;;; Diff:

;; Redefining this function to not let it try to find the current
;; defun from the hunk itself because it errors.
;;
;; This was initially done for ‘gk-git-commit-mode-hook’.
(defun gk-diff-current-defun ()
  "Find the name of function at point.

For use in `add-log-current-defun-function'."
  ;; Kill change-log-default-name so it gets recomputed each time, since
  ;; each hunk may belong to another file which may belong to another
  ;; directory and hence have a different ChangeLog file.
  (kill-local-variable 'change-log-default-name)
  (save-excursion
    (when (looking-at diff-hunk-header-re)
      (forward-line 1)
      (re-search-forward "^[^ ]" nil t))
    (pcase-let ((`(,buf ,_line-offset ,pos ,src ,dst ,switched)
                 (ignore-errors         ;Signals errors in place of prompting.
                   ;; Use `noprompt' since this is used in which-func-mode
                   ;; and such.
                   (diff-find-source-location nil nil 'noprompt))))
      (when buf
        (beginning-of-line)
        (with-current-buffer buf
          (goto-char (+ (car pos) (cdr src)))
          (add-log-current-defun))))))
(defalias 'diff-current-defun #'gk-diff-current-defun)

(defun gk-diff-mode-hook ()
  "Diffs."
  )

(setf
 vc-cvs-diff-switches "-uNp"
 vc-diff-switches "-uNp"
 diff-switches "-uNp"
 ;; Do not syntax-highligh diffs for source language.  Confusing.
 diff-font-lock-syntax nil)

(add-hook 'diff-mode-hook 'gk-diff-mode-hook)

;; Ediff:

(setf
 ;; Single frame setup.
 ediff-window-setup-function #'ediff-setup-windows-plain
 ediff-split-window-function #'split-window-horizontally
 ;; Use unified diffs for output.
 ediff-custom-diff-options "-uNp")



;;;; RCS:

(defun gk-rcs-maybe-unlock (file)
  "Check to see if FILE is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (when (stringp file)
    (let ((default-directory (file-name-directory file)))
      (when
          (and
           (fboundp 'vc-backend)
           (eq 'RCS (vc-backend file))
           (eq 'up-to-date (vc-rcs-state file))
           (y-or-n-p
            "File is controlled by RCS and not locked by you, lock?"))
        (with-current-buffer (find-file-noselect file)
          (vc-next-action nil))))))



;;;; Git:

(define-advice vc-git-push (:around (fn &rest args) always-prompt)
  "Always prompt for editing the push command."
  (funcall fn t))


(defun gk-git-commit-message-template ()
  "Subroutine of ‘gk-git-commit-mode-hook’."
  (catch 'dirty
    (let ((modified-re "^#	modified:")
          (new-re "^#	new file:")
          (renamed-re "^#	renamed:")
          (issue-re "^[+\\- ]\\*+ \\(TODO\\|DONE\\) ")
          current-defun filename addp onlyp issuep renamep)
      (save-excursion
        (with-current-buffer "COMMIT_EDITMSG"
          (goto-char (point-min))
          (unless (looking-at "^$")
            (goto-char (line-end-position))
            (throw 'dirty nil))
          (re-search-forward "^# Changes to be committed:" nil t)
          (forward-line)
          (beginning-of-line)
          (cond ((looking-at renamed-re)
                 (re-search-forward ": +" nil t)
                 (setf filename (cons (thing-at-point 'filename)
                                      (progn
                                        (re-search-forward " -> " nil t)
                                        (thing-at-point 'filename)))
                       renamep t))
                ((looking-at modified-re)
                 (re-search-forward ":   " nil t)
                 (setf filename (thing-at-point 'filename t)))
                ((looking-at new-re)
                 (re-search-forward ":   " nil t)
                 (setf filename (thing-at-point 'filename t)
                       addp t)))
          (setq onlyp (progn
                        (forward-line)
                        (beginning-of-line)
                        (looking-at "^#$")))
          (when (and onlyp (equal filename "Readme.org"))
            (goto-char (point-min))
            (when-let* ((pos (re-search-forward issue-re nil t)))
              (setq issuep (progn
                             (re-search-backward "\\*" nil t)
                             (buffer-substring (1+ (point))
                                               (line-end-position))))))
          ;; Try to set ‘current-defun’.
          (when onlyp
            (save-excursion
              (goto-char (point-min))
              ;; Error if not found, means verbose diffs
              ;; not enabled.
              (re-search-forward "^diff --git" nil t)
              (goto-char (line-beginning-position))
              (let ((str (buffer-substring (point) (point-max)))
                    (default-directory (expand-file-name "..")))
                (with-temp-buffer
                  (insert str)
                  (diff-mode)
                  (goto-char (point-min))
                  (re-search-forward "^@@ " nil t)
                  (re-search-forward "^[\\+\\-]" nil t)
                  (setq current-defun (diff-current-defun))))))))
      (if onlyp
          (cond
           (renamep
            (goto-char (point-min))
            (insert "Renamed ‘" (car filename) "’ as ‘" (cdr filename) "’"))
           ((and issuep (not addp))
            (goto-char (point-min))
            (insert ";" issuep))
           ((equal filename "TAGS")
            (goto-char (point-min))
            (insert "; Update TAGS"))
           ((equal filename ".gitignore")
            (goto-char (point-min))
            (insert "; Ignore ")
            ;; If just one addition, add the filename too.
            (save-excursion
              (re-search-forward (rx (and bol "+++")) nil t)
              (when (and (not (re-search-forward "^\\-" nil t))
                         (re-search-forward "^\\+" nil t)
                         (not (re-search-forward "^\\+" nil t)))
                (let ((str (buffer-substring-no-properties
                            (1+ (line-beginning-position))
                            (line-end-position))))
                  (goto-char (point-min))
                  (goto-char (line-end-position))
                  (insert str))))
            ;; If no additions, say ‘Don’t ignore’, if just one removal,
            ;; add it.
            (save-excursion
              (re-search-forward (rx (and bol "+++")) nil t)
              (when (and (save-excursion (re-search-forward "^\\-" nil t))
                         (not (re-search-forward "^\\+" nil t)))
                (save-excursion
                  (goto-char (1+ (point-min)))
                  (insert " Don’t")
                  (downcase-word 1))
                (when (and (re-search-forward "^\\-" nil t)
                           (not (re-search-forward "^\\-" nil t)))
                  (let
                      ((str (buffer-substring-no-properties
                             (1+ (line-beginning-position))
                             (line-end-position))))
                    (goto-char (point-min))
                    (goto-char (line-end-position))
                    (insert str)))))
            (goto-char (line-end-position)))
           ((equal filename "xdg-config/dconf/user.dump")
            (goto-char (point-min))
            (insert "; " filename))
           (filename
            (goto-char (point-min))
            (if addp
                (insert "Add " filename)
              ;; Here we’re manipulating the undo history as such:
              ;; 1) Insert an ‘insignificant change’ form.
              (insert "; " filename)
              ;; 2) Add an undo boundary here.  If there is a
              ;;   ‘current-defun’, two undos will get us back to the
              ;;   insignificant form, it not, one will suffice.
              (undo-boundary)
              ;; 3) Now delete that whole line, and...
              (delete-region (line-beginning-position) (line-end-position))
              ;;    amalgamate the undo boundaries so that we don’t
              ;;    need an extra undo to return to the insignificant
              ;;    change form.
              (undo-auto-amalgamate)
              ;; 4) Now insert the file name with a colon following it.
              (insert filename ": ")
              ;; 5) Add an undo boundary before trying to insert
              ;; ‘current-defun’.
              (undo-boundary)
              ;; 6) Try to insert ‘current-defun’.
              (when (and current-defun)
                (save-excursion
                  (backward-char 2)
                  (insert (format " (%s)" current-defun)))))))
        (when (and (equal filename "Readme.org")
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward (concat modified-re
                                                " +Readme.org_archive")
                                        nil t))
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "\\-\\*+ DONE" nil t))
                   (not
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "\\+\\*[\\+\\-] TODO" nil t))))
          (goto-char (point-min))
          (insert "; Archive DONE"))))))


(defun gk-git-commit-mode-hook ()
  "Set up git commit buffer."
  (when (string= (buffer-name) "COMMIT_EDITMSG")
    (gk-git-commit-message-template))
  ;; Just won’t work otherwise...
  (run-with-timer
   .1 nil
   ($
    (when (and (< (car (buffer-line-statistics)) 1000)
               (save-excursion (re-search-forward "^diff \\-\\-git" nil t)))
      (save-excursion
        (while (re-search-forward "^@@ " nil t)
          (message "refine!")
          (diff-refine-hunk)
          (forward-line 1)))))))

(defun gk-git-commit-deparen ()
  "Move stuff in parens to after colon.

If the headline looks like

    ‘file (something interesting): ’,

this will transform it such that it looks like

    ‘file: something interesting"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "(")
    (delete-char -2)
    (insert ": ")
    (goto-char (line-end-position))
    (re-search-backward ")")
    (delete-region (point) (line-end-position))))

(add-hook 'git-commit-mode-hook #'gk-git-commit-mode-hook)

(define-key git-commit-mode-map (kbd "C-c :") #'gk-git-commit-deparen)

(defun gk-copy-git-forge-url-as-kill (file &optional line-or-region)
  "Generate a Github/Gitlab url for FILE and copy it as kill.

If LINE-OR-REGION is non-nil or called with a prefix argument,
append the line number.  If the region is also active, append the
line numbers that match the beginning and the end of the region."
  (interactive (list (buffer-file-name)
                     (not (not current-prefix-arg))))
  (unless file (user-error "Buffer not visiting a file"))
  (when (buffer-modified-p)
    (user-error
     "Buffer modified, save and commit before using this function"))
  (when (save-window-excursion (vc-diff))
    (user-error "This file has uncommitted changes, commit first"))
  (revert-buffer)
  (when line-or-region
    (setq line-or-region
          (if (not (region-active-p))
              (line-number-at-pos (point))
            (cons (line-number-at-pos (region-beginning))
                  (line-number-at-pos (region-end))))))
  (if-let* ((dir (expand-file-name
                  (locate-dominating-file file ".git/config"))))
      (with-current-buffer (find-file-noselect
                            (expand-file-name ".git/config" dir))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (rx (and bol "[remote \"origin\"]")) nil t)
            (re-search-forward (rx (and bol "	url = ")))
            (if-let* ((str (gk--copy-git-forge-url-as-kill-1 dir file line-or-region)))
                (progn
                  (with-temp-buffer
                    (insert str)
                    (clipboard-kill-ring-save (point-min) (point-max))
                    (message str)
                    str))
              (error "Failed building Git forge url for %s" file)))))
    (user-error "Could not build a Git forge url")))

(defun gk--copy-git-forge-url-as-kill-1 (dir file line-or-region)
  "Subroutine of ‘gk-copy-github-url-as-kill’."
  (let
      ((line-str
        (cond
         ((and line-or-region (numberp line-or-region))
          (concat "#L" (number-to-string line-or-region)))
         ((and line-or-region (consp line-or-region))
          (concat "#L" (number-to-string (car line-or-region))
                  "-L" (number-to-string (cdr line-or-region))))
         (t ""))))
    (cond
     ((looking-at (rx "https://git" (or "hub" "lab") ".com"))
      (concat
       (buffer-substring-no-properties (point) (line-end-position))
       "tree/"
       (magit-get-current-branch)
       "/"
       (replace-regexp-in-string
        (concat "^" (regexp-quote dir)) "" file)
       line-str))
     ((looking-at (rx "git@git" (or "hub" "lab") ".com:"))
      (let ((str (buffer-substring-no-properties
                  (point) (line-end-position))))
        (save-match-data
          (when (string-match (rx
                               (and
                                string-start
                                "git@"
                                (submatch "git" (or "hub" "lab") ".com")
                                ":"
                                (submatch (1+ (not (any "/"))))
                                "/"
                                (submatch (1+ nonl))
                                ".git" string-end))
                              str)
            (concat "https://"
                    (match-string 1 str)
                    "/"
                    (match-string 2 str)
                    "/"
                    (match-string 3 str)
                    (if (string= (match-string 1 str) "github")
                        "blob"
                      "/tree/")
                    (vc-working-revision file)
                    "/"
                    (replace-regexp-in-string
                     (concat "^" (regexp-quote dir)) "" file)
                    line-str))))))))



;;;;; Magit:

(setf
 ;; No autorevert.
 magit-auto-revert-mode nil
 magit-auto-revert-immediately nil
 ;; Don’t pop up diff, commit --verbosely instead.
 magit-commit-show-diff nil
 magit-commit-arguments '("--verbose")
 ;; Exclude 3rd-party lisp from todos search.
 magit-todos-exclude-globs '("emacs.d/lisp/site/*" "*/patches/*")
 ;; Refine all displayed hunks.
 magit-diff-refine-hunk 'all)

;; (cl-pushnew 'magit-todos-mode gk-global-modes)


;; From: https://www.reddit.com/r/emacs/comments/gmkg4g/weekly_tipstricketc_thread/fr4gdm6/
(advice-add 'magit-whitespace-disallowed
            :around (gk-interactively (insert "-")))

(define-advice  magit-gitignore-read-pattern (:around (fn local) no-leading-slashes)
  "Same thing but simpler and don’t add leading slashes."
  (ignore fn)
  (let* ((default (magit-current-file))
         (choices
          (delete-dups
           (--mapcat
            (cons it
                  (-when-let (ext (file-name-extension it))
                    (list (concat "*." ext))))
            (magit-untracked-files)))))
    (unless (member default choices)
      (setq default nil))
    (magit-completing-read (concat "File or pattern to ignore"
                                   (and local " locally"))
                           choices nil nil nil nil default)))

;; Automatically ask for ssh-add when necessary.
(add-hook
 'magit-credential-hook
 ($ (save-window-excursion
      (shell-command "gk-ssh-add.bash" (generate-new-buffer "*ssh-add*")))))

;; Disable with-editor stuff.
(gk-deadvice 'server-switch-buffer)

(add-hook 'magit-mode-hook
          ($ (when (eq gk-gui-theme 'yoshi)
               (hl-line-mode +1))))



;;;; Mercurial:

(setf
 ;; Show the revision number in the mode line.
 vc-hg-symbolic-revision-styles '("{rev}/{branch}"))

;; Always prompt for editing the push command before pushing.
;; Requires prefix arg ortherwise.
(define-advice vc-hg-push (:around (fn &rest args) always-prompt)
  "Always prompt for editing the push command."
  (funcall fn t))



(provide 'gk-vc)
;;; gk-vc.el ends here
