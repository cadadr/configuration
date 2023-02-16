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

(setf
 git-link-use-single-line-number nil
 git-link-use-commit t)

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


(defun gk-git-do-auto-fill ()
  "Variant of ‘do-auto-fill’ for ‘git-commit-mode’ buffers.
Meant to be assigned locally to ‘auto-fill-function’."
  ;; Do not fill the first line, I don’t care about weird ass unix
  ;; nerd git commit message conventions.
  (if (zerop (current-line))
      nil
    (do-auto-fill)))

(define-advice git-commit-turn-on-auto-fill
    (:after (&rest _) set-gk-git-do-auto-fill)
  "Set my ‘auto-fill-function’ for ‘git-commit-mode’."
  (setq-local auto-fill-function #'gk-git-do-auto-fill))

(defun gk-git-commit-mode-hook ()
  "Set up git commit buffer."
  (when (string= (buffer-name) "COMMIT_EDITMSG")
    (gk-git-commit-message-template))
  ;; Interferes with our auto-fill setup.
  (yas-minor-mode -1)
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

;; Disable with-editor stuff.
(gk-deadvice 'server-switch-buffer)

(add-hook 'magit-mode-hook
          ($ (when (eq gk-gui-theme 'yoshi)
               (hl-line-mode +1))))



;;;;;; Magit credential management using pass(1):

;; Automate magit credential management using ‘gk-ssh-add.bash’, and
;; indirectly, ‘dmenu’ and ‘pass’.
;;
;; We keep track of what remotes are picked interactively, and use
;; that information to hint ‘gk-ssh-add.bash’ on which key to pick.
;; Failing that, user will need to pick the key manually.

(defvar gk-magit-last-picked-remote nil
  "Most recently selected remote using ‘magit-read-remote-branch’.
Either the remote branch identifier, or ‘upstream’, when pushing
upstream.")

(define-advice magit-read-remote-branch
    (:filter-return (ret) record-selection)
  "Record the selected remote in ‘gk-magit-last-picked-remote’."
  (setf gk-magit-last-picked-remote ret)
  ret)

(define-advice magit-push-current-to-upstream
    (:before (&rest _) record-upstream)
  "Record that we’re pushing to upstream in ‘gk-magit-last-picked-remote’."
  (setf gk-magit-last-picked-remote 'upstream))

(define-advice magit-pull-from-upstream
    (:before (&rest _) record-upstream)
  "Record that we’re pulling from upstream in ‘gk-magit-last-picked-remote’."
  (setf gk-magit-last-picked-remote 'upstream))

(define-advice magit-pull-from-pushremote
    (:before (&rest _) record-upstream)
  "Record that we’re pulling from current branch push remote in ‘gk-magit-last-picked-remote’."
  (setf gk-magit-last-picked-remote 'pushremote))

(define-advice magit-fetch-from-upstream
    (:before (&rest _) record-upstream)
  "Record that we’re fetching from upstream in ‘gk-magit-last-picked-remote’."
  (setf gk-magit-last-picked-remote 'upstream))

(define-advice magit-fetch-from-pushremote
    (:before (&rest _) record-upstream)
  "Record that we’re fetching from current branch push remote in ‘gk-magit-last-picked-remote’."
  (setf gk-magit-last-picked-remote 'pushremote))



(defun gk-magit--get-last-picked-remote-ref ()
  (cond ((eq gk-magit-last-picked-remote 'upstream)
         (magit-get-upstream-remote))
        ((eq gk-magit-last-picked-remote 'pushremote)
         (magit-get-push-remote))
        ((stringp gk-magit-last-picked-remote)
         (car (split-string gk-magit-last-picked-remote "/")))))

(defun gk-magit--get-last-picked-remote-domain ()
  (when-let* ((remote (gk-magit--get-last-picked-remote-ref))
              (remote-url (vc-git-repository-url default-directory remote)))
    (nth 1 (split-string remote-url "\[@:\]" t))))

(defun gk-magit-credential-hook ()
  (let* ((maybe-last-remote-domain
          (gk-magit--get-last-picked-remote-domain))
         (process-environment
          (cons (concat "GK_SSH_ADD_DOMAIN=" (or maybe-last-remote-domain ""))
                process-environment)))
    (save-window-excursion
     (shell-command "gk-ssh-add.bash" (generate-new-buffer "*ssh-add*")))))

(add-hook 'magit-credential-hook #'gk-magit-credential-hook)




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
