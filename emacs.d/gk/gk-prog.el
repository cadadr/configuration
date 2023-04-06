;;; gk-prog.el --- configurations for programming modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023  Göktuğ Kayaalp

;;; Commentary:

;; Configurations for programming language major modes and related
;; tools.

;;; Code:

(require 'autoinsert)
(require 'cc-vars)
(require 'debug)
(require 'eglot)
(require 'eldoc)
(require 'eval-sexp-fu)
(require 'flymake)
(require 'flymake-python-pyflakes)
(require 'inf-lisp)
(require 'js)
(require 'paredit)
(require 'python)
(require 'pythonic)
(require 'ruby-mode)
(require 'savehist)
(require 'scheme)
(require 'yasnippet)

(require 'gk-global-mode-util)
(require 'gk-minor-mode)


(diminish 'highlight-parentheses-mode)
(diminish 'eldoc-mode)
(diminish 'paredit-mode "☮")
(diminish 'outline-minor-mode "*")

(defvar-local gk-algol-like nil
  "Whether current buffer is an algol-like programming language.

Set by ‘gk-algol-like-hook’, don’t manually set.")

(defun gk-algol-like-hook ()
  "Hook for Algol-like programming languages editing."
  (setq-local gk-algol-like t)
  (electric-pair-local-mode +1))

(defun gk-prog-mode-hook ()
  "Hook for all programming modes."
  (highlight-indent-guides-mode)
  (highlight-parentheses-mode +1)
  (paren-face-mode +1)
  (setq-local indent-tabs-mode nil)
  (git-gutter-mode +1))

(add-hook 'prog-mode-hook #'gk-prog-mode-hook)



;;;; Snippets:

(define-advice yas-maybe-load-snippet-buffer
    (:around (fn &rest args) no-jumping)
  "Do not move the point."
  (save-excursion
    (apply fn args)))

(setf yas-snippet-dirs
      (list
       (locate-user-emacs-file "etc/snippets")))

(pushnew 'yas-global-mode gk-global-modes)

(diminish 'yas-minor-mode)



;;;; Eglot / LSP:

(setf
 ;; Shut down if no buffers need it.
 eglot-autoshutdown t
 ;; Don’t mess up echo area with full documentation.
 eglot-put-doc-in-help-buffer t)



;;;; Flymake:

(setf
 ;; Don’t bother me unless I save.
 flymake-no-changes-timeout nil
 flymake-start-on-flymake-mode nil)



;;;; Lisps:



;;;;; Common:

;; Settings common to all Lisp modes.

(defun gk-lisp-mode-hook ()
  "Standard Lisp mode hook.

Usable for Repl buffers."
  (paredit-mode 1)
  (highlight-parentheses-mode 1)
  (setq indent-tabs-mode nil)
  (gk-lisp-editing-mode-hook))

(defun gk-lisp-editing-mode-hook ()
  "Specific hook for files visiting Lisp buffers."
  (gk-turn-on-outline-minor-mode ";;;;* " ":$" "C-'"))

(add-hook 'lisp-mode-hook 'gk-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'gk-lisp-editing-mode-hook)

;; Let gk-minor-mode do this.
(define-key paredit-mode-map "\M-;" nil)
;; ‘paredit-newline’ is wacky.
(define-key paredit-mode-map "\C-j" nil)

(setf
 eval-sexp-fu-flash-face 'region
 eldoc-idle-delay 0)



;;;;; Emacs Lisp:

(add-to-list 'gk-global-modes 'eros-mode)

(defun gk-emacs-lisp-mode-hook ()
  (imenu-add-to-menubar "Definitions"))

(add-hook 'emacs-lisp-mode-hook 'gk-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'gk-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'gk-lisp-mode-hook)

(defun gk-lisp-interaction-mode-hook ()
  (setq-local lexical-binding t))

(add-hook 'lisp-interaction-mode-hook 'gk-lisp-interaction-mode-hook)

;; Do not eval on C-j.
(define-key lisp-interaction-mode-map "\n" nil)

(defun gk-minibuf-eval-hook ()
  "Better editing for /M-:/."
  (when (eq this-command  'eval-expression)
    (paredit-mode 1)
    (highlight-parentheses-mode 1)))

(add-hook 'minibuffer-setup-hook #'gk-minibuf-eval-hook)
;; Elisp template.
(defvar gk-elisp-template-file (locate-user-emacs-file "lisp/elisp-template"))
(defvar gk-elisp-file-template nil)

(defun gk-elisp-load-template ()
  "Load the Elisp file template."
  (setq gk-elisp-file-template
        (with-current-buffer
            (find-file-noselect gk-elisp-template-file)
          (prog2
              (font-lock-defontify)
              (buffer-string)
            (kill-buffer (current-buffer))))))

(defun gk-elisp-file-from-template (name description keywords)
  "Create an Elisp file NAME from the template.

Template is the file named in the variable `gk-elisp-template-file'.
DESCRIPTION is the short description added to the first line.
KEYWORDS are the keywords for the file."
  (interactive
   (list (read-file-name "Lisp file name: "
                         (locate-user-emacs-file "lisp"))
         (read-string "File description: ")
         (read-string "Keywords (comma separated): ")))
  (let ((template (or gk-elisp-file-template (gk-elisp-load-template)))
        (name-sans-dir (file-name-nondirectory name))
        (str))
    (setq str
          (format template name-sans-dir description
                  (format-time-string
                   "%Y"
                   (current-time))  ; Copyright year
                  user-full-name    ; Copyright author
                  (format
                   "%s <%s>"        ; Package author
                   user-full-name user-mail-address)
                  keywords
                  (file-name-sans-extension ; Package name, provide form
                   name-sans-dir)
                  name-sans-dir))   ; .. ends here
    (find-file name)
    (insert str)))

(defun gk-elisp-add-require-for-symbol-at-point ()
  "Add a requirement for the feature that exports symbol at point.

On success, prints a message and returns the feature name (a
symbol)."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (file (symbol-file (intern symbol)))
         (feature (file-name-base file))
         (lastreq (save-excursion (re-search-backward "^(require '" nil t)))
         (inspos (or lastreq
                     (save-excursion (re-search-backward "^;;; Code:" nil t))))
         (form (concat "(require '" feature ")")))
    (when (or
           (null file)
           (string= (expand-file-name file)
                    (expand-file-name (buffer-file-name))))
      (user-error "Symbol ‘%S’ defined in current buffer" symbol))
    (push-mark nil t)
    (goto-char inspos)
    (end-of-line)
    (open-line 1)
    (forward-line)
    (insert form)
    (message
     (substitute-command-keys
      "Added ‘%S’; Hit \\[pop-mark] to go back to where you were")
     feature)
    feature))

;; Pretty-printing:
(define-key emacs-lisp-mode-map (kbd "C-c C-M-x") 'pp-macroexpand-expression)
(define-key emacs-lisp-mode-map (kbd "C-c C-x C-e") 'pp-eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-c C-x C-e") 'pp-macroexpand-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c !") #'ert-run-tests-interactively)



;;;;;; Debugger:

(setq
 ;; (expr ...) not expr(...)
 debugger-stack-frame-as-list t)

;; From https://www.reddit.com/r/emacs/comments/7htdzk/show_reddit_prettyprint_debugger_frames/

(defun gk-debugger-pp-frame ()
  (interactive)
  (let ((inhibit-read-only t)
        (frame (backtrace-frame (debugger-frame-number))))
    (set-buffer (pop-to-buffer "*BT: Frame*"))
    (destructuring-bind (special fn &rest args) frame
      (erase-buffer)
      (progn
        (insert "(" (pp-to-string fn))
        (dolist (arg args)
          (insert "\n" (pp-to-string arg)))
        (insert ")"))
      (goto-char (point-min))
      (indent-pp-sexp))))

(define-key debugger-mode-map "r" 'gk-debugger-pp-frame)



;;;;; Common Lisp:

(defvar gk-lisp-program (or (executable-find "ccl")
                            (executable-find "sbcl"))
  "Full path to the default Common Lisp implementation.")

(setf
 ;; Set default Lisp interpreter.
 inferior-lisp-program gk-lisp-program)



;;;;; Scheme:

(setf scheme-program-name "guile")

(add-hook 'scheme-mode-hook 'gk-lisp-mode-hook)
(add-hook 'inferior-scheme-mode-hook 'gk-lisp-mode-hook)



;;;; C family:

(setf
 ;; Default C style.
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "gnu")))

(add-hook 'c-mode-hook 'gk-algol-like-hook)



;;;; GDB/GUD:

(setf
 ;; Use the multi-window layout with source code, locals, output &c.
 gdb-many-windows t
 ;; Start up with the source file containing main()’s definition shown.
 gdb-show-main t)



;;;; Javascript:

(defun gk-javascript-hook ()
  "Standard JS hook."
  (highlight-parentheses-mode 1)
  (setq indent-tabs-mode nil
        js-indent-level 2))

(add-hook 'js-mode-hook 'gk-javascript-hook)
(add-hook 'js-mode-hook 'gk-algol-like-hook)

;; Skewer mode setup.
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)



;;;; CSS:

(defun gk-css-mode-hook ()
  (gk-turn-on-outline-minor-mode "^/\\*\\*\\*+" ": \\*/" "C-'"))

(add-hook 'css-mode-hook #'gk-css-mode-hook)



;;;; Ruby:

(defun gk-ruby-send-line ()
  "Send current-line to inferior Ruby."
  (interactive)
  (ruby-send-region (line-beginning-position) (line-end-position)))

(defun gk-ruby-send-toplevel ()
  "Send toplevel block to inferior Ruby."
  (interactive)
  (save-excursion
    (ruby-send-region
     (re-search-backward "^\\(%[qw]\\|class\\|def\\|if\\|begin\\|module\\)")
     (re-search-forward "^end"))))

(defvar gk-ri-history nil
  "The history list for ‘gk-ri’.")
(cl-pushnew 'gk-ri-history savehist-additional-variables)

(define-derived-mode gk-ri-mode view-mode "GKRi"
  "GK Ri mode is an adapation of ‘view-mode’ to ‘gk-ri’."
  :group 'GK
  (let ((map gk-ri-mode-map))
   (define-key map "r" #'gk-ri)
   (define-key map (kbd "C-c C-h") #'gk-ri)))

(defun gk-ri (what)
  "Interface to ri(1) documentation browser."
  (interactive
   (list
    (let* ((w (word-at-point))
           (p (format "Search in Ruby documentation (default: %s): " w)))
      (read-string p nil 'gk-ri-history w))))
  (if-let* ((bufnam (format "*ri %s*" what))
            (buf (get-buffer bufnam)))
      ;; If already exists, display the buffer.
      (display-buffer buf)
    (let ((buf (generate-new-buffer bufnam))
          (inhibit-read-only t))
      (with-current-buffer buf
        (erase-buffer)
        (call-process "ri" nil buf nil "-f" "ansi" what)
        ;; If ri reports no documentation is available, kill the buffer
        ;; and redirect the error to the user.  Else, treat the buffer
        ;; and present it.
        (if (save-match-data
              (goto-char (point-min))
              (looking-at "^Nothing known about"))
            (let ((m (string-trim (buffer-string))))
              (kill-this-buffer)
              (message m))
          (ansi-color-filter-region (goto-char (point-min)) (point-max))
          (gk-ri-mode)
          (gk-minor-mode)
          (display-buffer buf))))))

(define-key ruby-mode-map "\C-\M-x" 'ruby-send-definition)
(define-key ruby-mode-map "\C-x\C-e" 'ruby-send-last-sexp)
(define-key ruby-mode-map "\C-c\C-b" 'ruby-send-block)
(define-key ruby-mode-map "\C-c\C-r" 'ruby-send-region)
(define-key ruby-mode-map "\C-c\C-l" 'gk-ruby-send-line)
(define-key ruby-mode-map "\C-c\C-t" 'gk-ruby-send-toplevel)
(define-key ruby-mode-map "\C-c\C-h" 'gk-ri)

(defun gk-ruby-mode-hook ()
  (imenu-add-to-menubar "Definitions")
  (gk-turn-on-outline-minor-mode "###*" ":$" "C-'")
  (call-interactively 'eglot))

(defun gk-inf-ruby-mode-hook ()
  (setf truncate-lines nil word-wrap t))

(add-hook 'ruby-mode-hook 'gk-ruby-mode-hook)
(add-hook 'inf-ruby-mode-hook 'gk-inf-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'gk-algol-like-hook)



;;;; Perl:

(pushnew '(perl-mode "pls") eglot-server-programs)

(defalias 'perldoc 'cperl-perldoc)

(add-hook 'perl-mode-hook 'gk-algol-like-hook)

(defun gk-perl-mode-hook ()
  (eglot-ensure))

(add-hook 'perl-mode-hook #'gk-perl-mode-hook)

(pushnew
 `((perl-mode . "Perl source file")
   nil
   (when (y-or-n-p "Is this an executable script?")
     "#!/usr/bin/env perl\n")
   "# "
   (or (ignore-errors (file-name-nondirectory (buffer-file-name)))
       "<filename>")
   " --- "
   (let ((d (string-trim (read-string "Description: "))))
     (if (string-empty-p d)
         "..."
       d))
   "\n\n"
   ,(concat
     "use v5.36;\n"
     "\n"
     "use strict;\n"
     "use warnings;\n"
     "no warnings 'experimental::smartmatch';\n"
     "\n"
     "use feature 'signatures';\n"
     "\n"))
 auto-insert-alist)



;;;; Python:
(setf python-shell-interpreter "run-python.sh"
      ;; Use system python3 for anaconda.
      pythonic-interpreter "python3"
      ;; Please don't annoy me, and fuck you.
      python-indent-guess-indent-offset nil

      flymake-python-pyflakes-executable "flake8")

(defvar gk-python-version
  (when (gk-executable-ensure pythonic-interpreter)
    (shell-command (format "%s --version" pythonic-interpreter))
    (with-current-buffer "*Shell Command Output*"
      (-interleave
       (list :major :minor :patch)
       (mapcar #'string-to-number
               (split-string
                (cadr (split-string (buffer-substring-no-properties (point-min) (point-max))))
                "\\.")))))
  "A plist containing the version information for ‘pythonic-interpreter’.")

;; Lookup Python symbols in Python Info pages.
(pydoc-info-add-help (list (format "python%d.%d"
                                   (plist-get gk-python-version :major)
                                   (plist-get gk-python-version :minor))))

(defun gk-python-mode-hook ()
  (eglot-ensure))

(add-hook 'python-mode-hook 'gk-algol-like-hook)
(add-hook 'python-mode-hook 'gk-python-mode-hook)

(defun gk-python-send-statement ()
  "Send statement under point to inferior Python."
  (interactive)
  (python-shell-send-string-no-output
   (buffer-substring
    (save-excursion (python-nav-beginning-of-statement) (point))
    (save-excursion (python-nav-end-of-statement) (point)))))

(define-key python-mode-map "\C-c\C-l" #'gk-python-send-statement)
(define-key python-mode-map "\C-c\C-h" #'python-eldoc-at-point)



;;;; Makefiles:

(defun gk-makefile-hook ()
  "Generic hook for makefile modes."
  (gk-turn-on-outline-minor-mode "####* " ":$" "C-'")
  (setq-local indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'gk-makefile-hook)



;;;; Shell scripts:

(add-to-list
 'auto-insert-alist
 '((sh-mode . "Shell script template")
   nil
   (let* ((filename (or
                     (ignore-errors
                       (file-name-nondirectory
                        (buffer-file-name)))
                     "<filename>"))
          (bashp (string= "bash" (file-name-extension filename))))
     (concat
      (if bashp
          "#!/usr/bin/env bash"
        "#!/bin/sh")
      "\n# "
      filename
      " --- "
      (let
          ((d
	    (string-trim
	     (read-string "Description: "))))
        (if
	    (string-empty-p d)
	    "..." d))
      "\n\n"
      ;; Adapted from: http://redsymbol.net/articles/unofficial-bash-strict-mode/
      (if bashp
          "# bash strict mode\nset -euo pipefail"
        "# POSIX strict-ish mode, beware eager pipelines!\nset -eu")
      "\nIFS=$'\\n\\t'\n\n"))))

(defun gk-shell-script-hook ()
  "Generic hook for shell script modes."
  (gk-turn-on-outline-minor-mode "####* " ":$" "C-'"))

(add-hook 'sh-mode-hook 'gk-shell-script-hook)



;;;; Lua:

(defun gk-lua-mode-hook ()
  (imenu-add-to-menubar "Definitions"))

(add-hook 'lua-mode-hook #'gk-lua-mode-hook)



;;;; Dart:

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))



;;;; Compilation:

;; Send desktop notification when a compilation is finished.
;; Adapted from https://www.reddit.com/r/emacs/comments/cw1eky/ey8tisj/
(add-to-list 'compilation-finish-functions
             ($ [_ status]
                (gk-send-desktop-notification "Compilation finished" status)))



;;;; Keybindings:

(gk-prefix-binding "d" 'xref-find-definitions)
(gk-prefix-binding "k" 'recompile)



(provide 'gk-prog)
;;; gk-prog.el ends here
