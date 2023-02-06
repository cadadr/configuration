;;; gk-dired.el --- dired customisations             -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2023  Göktuğ Kayaalp

;;; Commentary:

;; Pretty much what it says on the tin.  Dired is one of the more
;; heavily used modes in my setup, so it has a somewhat big config.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'dired-subtree)
(require 'dired-x)
(require 'ls-lisp)
(require 'wdired)

(require 'gk-mac)


(setf
 ;; Show ls switches in modeline
 dired-switches-in-mode-line 'as-is
 ;; Allow drag-n-dropping files.
 dired-mouse-drag-files t)



;;;; The hook:

(defun gk-dired-hook ()
  "Main hook for `dired-mode'."
  ;; C-x M-o -> toggle omitting
  ;; * O -> mark omitted
  (dired-omit-mode 1)
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook #'gk-dired-hook)



;;;; Utilities:

(defun gk-dired-copy-marked-file-paths-as-kill (&optional arg)
  "Copy the paths of marked files into the kill ring as one big string.

The string is space separated, ready for use in shell.

If ARG is non-nil, or one prefix arg is given, place each file
in single quotes.

If two prefix arguments are given, place each file in double
quotes.

If called with prefix arg 0 (zero), return a null-separated list
instead of space separated.

If called with a negative prefix arg, return a comma-separated
list.

If called with three prefix args, return a colon separated list."
  (interactive "p")
  (let ((str (mapconcat
              (case arg
                ((1 0 -1 64)  #'identity)
                ('4  ($ (concat "'" $1 "'")))
                ('16 ($ (concat "\"" $1 "\""))))
              (dired-get-marked-files)
              (case arg
                ((1 4 16 nil) " ")
                ('0 "\0")
                ('-1 ", ")
                ('64 ":")))))
    (with-temp-buffer
      (insert str)
      (clipboard-kill-ring-save (point-min) (point-max)))
    (message str)))


(defun gk-dired-update-default-directory-from-current-line (ret)
  "Set `default-directory' to dirname of entity under point.

Useful when using dired-subtree."
  (ignore ret)
  (ignore-errors
    (setq-local default-directory
                (file-name-directory (dired-get-file-for-visit)))))

(advice-add 'dired-previous-line :after #'gk-dired-update-default-directory-from-current-line)
(advice-add 'dired-next-line :after #'gk-dired-update-default-directory-from-current-line)


(defun gk-dired-find-file-other-frame ()
  "In Dired, visit this file or directory in another frame."
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))




;;;; Customisations:

(setf
 ;; Omit ., .., #*, *~, ~,v; and some other stuff.
 dired-omit-files
 (rx (or (or (and bol (or "." "#") (optional (1+ ".")))
             (and (or "~" ",v") eol))
         (and bol (or "__pycache__"))))
 dired-omit-extensions (cl-remove-if ($ (string= $1 ".mo"))
                                     dired-omit-extensions)
 ;; Show symlinks' targets: it's useful, and dired-subtree is stupid
 ;; otherwise.
 dired-hide-details-hide-symlink-targets nil)

(setf
 ls-lisp-dirs-first t
 ;; HACK(2021-09-25): if this is t, the sort is case sensitive...
 ls-lisp-use-string-collate nil)

(setf
 ;; Ask for confirmation
 wdired-confirm-overwrite t
 ;; Human readable size.
 dired-listing-switches "-alh")



;;;; Keymappings:

(define-key dired-mode-map (kbd "W") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "w") 'gk-dired-copy-marked-file-paths-as-kill)
(define-key dired-mode-map (kbd "f") #'gk-dired-find-file-other-frame)



;;;; Subtrees:

;; If there arent any open subtrees, behave like < and > do in normal
;; dired.
(define-advice dired-subtree-up
    (:override (&optional arg) prev-dir-if-no-subtree)
  "Jump up one directory."
  (interactive "p")
  (if-let* ((ov (dired-subtree--get-ov)))
      (progn (goto-char (overlay-start ov))
             (dired-previous-line 1))
    (dired-prev-dirline 1)))

(define-advice dired-subtree-down
    (:override (&optional arg) next-dir-if-no-subtree)
  "Jump up one directory."
  (interactive "p")
  (if-let* ((ov (dired-subtree--get-ov)))
      (progn (goto-char (overlay-start ov))
             (dired-previous-line 1))
    (dired-next-dirline 1)))

(define-key dired-mode-map "i" 'dired-subtree-toggle)
(define-key dired-mode-map "<" 'dired-subtree-up)
(define-key dired-mode-map ">" 'dired-subtree-down)

(setf dired-subtree-use-backgrounds nil)

(defun gk-dired-subtree-hook ()
  ;; Reset omissions when necessary.  Subtrees do not initially apply
  ;; omissions.
  (when dired-omit-mode
    (dired-omit-mode +1)))

(add-hook 'dired-subtree-after-insert-hook 'gk-dired-subtree-hook t)



(provide 'gk-dired)
;;; gk-dired.el ends here
