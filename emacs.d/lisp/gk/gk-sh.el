;;; gk-sh.el --- customisations for various shell modes of Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Emacs supports a bunch of shell modes, and this module configures
;; the ones I (may) use (sometimes).

;;; Code:

;;;; Some comint QoL:

;; Make comint great again!

;; DEATH TO ALL FUCKING COLOURS!
;; FUCK ALL COLOURS!
;; THANK GOD FOR THIS VARIABLE!
;; THANK YOU WHOMEVER ADDED THIS!
;; NOW COLOUR WHATEVER THE FUCK YOU WANT YOU BRAIN
;; DEAD DEVELOPERS OF COMMAND LINE UTILITIES WITH
;; COLOURFUL OUTPUT THAT CANNOT BE DISABLED!
;; FUCK YOU ALL.
(setf ansi-color-for-comint-mode 'filter)

(defun gk-toggle-comint-process-echoes ()
  "Toggle ‘comint-process-echoes’ variable."
  (interactive)
  (message
   (if (setq-local comint-process-echoes (not comint-process-echoes))
       "Turned *on* comint echo filter."
     "Turned *off* comint echo filter.")))

(define-key comint-mode-map "\C-c\C-e" 'gk-toggle-comint-process-echoes)
(define-key comint-mode-map (kbd "C-c DEL") 'comint-clear-buffer)

;;;; Shell mode:

;; Adapted from: https://www.emacswiki.org/emacs/ShellDirtrackByProcfs
(defun gk-procfs-dirtrack (str)
  (prog1 str
    (when (string-match comint-prompt-regexp str)
      (let ((directory (file-symlink-p
                        (format "/proc/%s/cwd"
                                (process-id
                                 (get-buffer-process
                                  (current-buffer)))))))
        (when (file-directory-p directory)
          (cd directory))))))

(define-minor-mode gk-procfs-dirtrack-mode
  "Track shell directory by inspecting procfs."
  nil nil nil
  (cond (gk-procfs-dirtrack-mode
         (when (bound-and-true-p shell-dirtrack-mode)
           (shell-dirtrack-mode 0))
         (when (bound-and-true-p dirtrack-mode)
           (dirtrack-mode 0))
         (add-hook 'comint-preoutput-filter-functions
                   'gk-procfs-dirtrack nil t))
        (t
         (remove-hook 'comint-preoutput-filter-functions
                      'gk-procfs-dirtrack t))))

(defun gk-display-shell (arg)
  "Pop a shell in a side window.

Pass arg to ‘shell’.  If already in a side window that displays a
shell, toggle the side window.

If there is a project shell associated to the frame, just show
that instead."
  (interactive "P")
  (display-buffer (gk--get-shell-for-frame arg)))

(defun gk-shell-mode-hook ()
  "Hook for `shell-mode'."
  ;; BSD /bin/sh echoes.
  (when (and (not (memq system-type '(gnu gnu/linux gnu/kfreebsd)))
             (string-match "/k?sh$" (getenv "SHELL")))
    (setq-local comint-process-echoes t))
  ;; Compilation shell minor mode activates certain parts of command
  ;; output as clickable links to parts of files (e.g. grep -Hn).
  (compilation-shell-minor-mode 1)
  ;; ‘shell-dirtrack-mode’ fails a lot.
  (shell-dirtrack-mode +1)
  ;; (gk-procfs-dirtrack-mode +1)
  )

(add-hook 'shell-mode-hook 'gk-shell-mode-hook)




;;;; Eshell:

(setf
 eshell-ls-initial-args
 (list "--group-directories-first" "-Fh"))

(dolist (key '(up down left right))
  (define-key eshell-hist-mode-map `[,key] nil))
(define-key eshell-hist-mode-map (kbd "M-p") #'eshell-previous-matching-input-from-input)
(define-key eshell-hist-mode-map (kbd "M-n") #'eshell-next-matching-input-from-input)



(provide 'gk-sh)
;;; gk-sh.el ends here
