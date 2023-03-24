;;; init.el --- Göktuğ's Emacs configuration. -*- lexical-binding: t; coding: utf-8 -*-

;; This file is the initialisation file for my personal setup.  It's
;; split into sections using the form feed characters and outline-mode
;; section headers.

;; I encourage anybody to take all the bits they'd like from this file
;; (or any other file in this whole configuration tree, for that
;; matter), but I strongly *discourage* it's use as a whole package of
;; software, as it's tailored for my workflow and my general computing
;; environment, so it probably won't work for you.  Still, you can
;; totally do that if you want, as this whole tree contains all the
;; required scripts and configuration files to instantiante a working
;; environment nearly identical to mine.



;;; Prelude:

(defconst gk-emacs-executable
  (executable-find "emacs"))

;; Report load time.
(defvar gk-emacs-initialisation-started-time nil
  "Time at which ‘user-init-file’ started loading.")
(setq gk-emacs-initialisation-started-time (current-time))

(defvar gk-emacs-initialisation-completed-time nil
  "Time at which ‘user-init-file’ finished loading.")

(unless noninteractive
  (add-hook
   'after-init-hook
   (lambda ()
     (setq
      gk-emacs-initialisation-completed-time
      (current-time)
      initial-scratch-message
      (format-time-string
       (concat
        ";;\n"
        ";; Welcome to Emacs, with cadadr’s mutila^Wcustomisations!\n"
        ";; Initialisation completed, took %-S.%3N seconds.\n"
        ";;\n\n\n")
       (time-subtract
        gk-emacs-initialisation-completed-time
        gk-emacs-initialisation-started-time)))
     (run-with-timer
      1.5 nil
      (lambda ()
        (unless (get-buffer-window "*scratch*")
          (message
           ;; remove the trailing newline
           (substring initial-scratch-message
                      0 (- (length initial-scratch-message) 3)))))))))

(when (version< emacs-version "30.0")
  (error "This configuration requires a recent build of Emacs ‘master’ branch"))

;; Use elisp directory listing program.  This needs to be set before
;; loading ls-lisp.el.
(defvar ls-lisp-use-insert-directory-program nil)



(load (locate-user-emacs-file "loadpaths"))



;; Load all requirements up before running initialisation code.  This
;; slows down initialisation and increases initial memory use, but
;; otherwise running commands may take a longer time when features
;; need to be loaded for running them.

;; This list should be sorted alphabetically.

(eval-when-compile (require 'cl))
(require 'ace-jump-mode)
(require 'anaconda-mode)
(require 'ansi-color)
(require 'apropos)
(require 'auth-source)
(require 'autoinsert)
(require 'bbdb)
(require 'bbdb-vcard)
(require 'bookmark)
(require 'boxquote)
(require 'browse-url)
(require 'calendar)
(require 'cc-mode)
(require 'comint)
(require 'compile)
(require 'copyright)
(require 'dart-mode)
(require 'dash)
(require 'debug)
(require 'deft)
(require 'desktop)
(require 'diff)
(require 'diminish)
(require 'dired)
(require 'dired-narrow)
(require 'dired-subtree)
(require 'dired-x)
(require 'doc-view)
(require 'doifetch)
(require 'dollar)
(require 'ebib)
(require 'eglot)
(require 'eldoc)
(require 'elfeed)
(require 'elpher)
(require 'epa)
(require 'epa-mail)
(require 'epg)
(require 'eros)
(require 'eshell)
(require 'em-hist)
(require 'ess-r-mode)
(require 'etags)
(require 'eval-sexp-fu)
(require 'eww)
(require 'f)
(require 'face-remap) ; buffer-face-mode
(require 'ffap)
(require 'files)
(require 'flymake-python-pyflakes)
(require 'flyspell)
(require 'forecast)
(require 'gemini-mode)
(require 'geoclue)
(require 'git-commit)
(require 'git-gutter)
(require 'git-gutter-fringe)
(require 'git-link)
(require 'gk-greek)
(require 'gk-unilat)
(require 'goto-addr)
(require 'goto-last-change)
(require 'haskell-mode)
(require 'highlight-indent-guides)
(require 'highlight-parentheses)
(require 'hl-line)
(require 'ido)
(require 'ido-vertical-mode)
(require 'image)
(require 'image-dired)
(require 'imenu)
(require 'inf-lisp)
(require 'inf-ruby)
(require 'info-look)
(require 'ispell)
(require 'js)
(require 'log-edit)
(require 'lorem-ipsum)
(require 'lua-mode)
(require 'ls-lisp)
(require 'magit)
(require 'magit-todos)
(require 'mail-source)
(require 'mairix)
(require 'markdown-mode)
(require 'message)
(require 'mm-url)
(require 'multiple-cursors)
(require 'netrc)
(require 'nnfolder)
(require 'nsm)
(require 'org)
(require 'org-attach-screenshot)
(require 'org-capture)
(require 'org-checklist)
(require 'org-ebib)
(require 'org-eldoc)
(require 'org-habit)
(require 'org-id)
(require 'org-inlinetask)
(require 'org-mobile)
(require 'org-num)
(require 'org-protocol)
(require 'org-tempo)                    ; <s, <q &c
(require 'org-variable-pitch)
(require 'org-zotxt)
(require 'outline)
(require 'ox)
(require 'ox-beamer)
(require 'ox-hugo)
(require 'ox-latex)
(require 'ox-odt)
(require 'ox-org)
(require 'ox-publish)
(require 'paredit)
(require 'paren-face)
(require 'parse-time)
(require 'pass-listing)
(require 'perl-mode)
(require 'persistent-scratch)
(require 'pip-requirements)
(require 'pixel-scroll)
(require 'pp)
(require 'project)
(require 'pydoc-info)
(require 'python)
(require 'pythonic)
(require 'quail)
(require 'rect)
(require 'rmail)
(require 'rmailsum)
(require 'ruby-mode)
(require 'rx)
(require 's)
(require 'saveplace)
(require 'savehist)
(require 'scheme)
(require 'sendmail)
(require 'seq)
(require 'shell)
(require 'shr)
(require 'simple)
(require 'skewer-mode)
(require 'skewer-css)
(require 'skewer-html)
(require 'smex)
(require 'smtpmail)
(require 'so-long)
(require 'subr-x)
(require 'switch-window)
(require 'textile-mode)
(require 'thingatpt)
(require 'thinks)
(require 'time)
(require 'tramp)
(require 'tramp-cache)
(require 'turkish)
(require 'undo-tree)
(require 'uniquify)
(require 'url)
(require 'valign)
(require 'vc)
(require 'vc-cvs)
(require 'vc-rcs)
(require 'wdired)
(require 'which-key)
(require 'whitespace)
(require 'whole-line-or-region)
(require 'windmove)
(require 'winner)
(require 'zencoding-mode)
(require 'yasnippet)



;;; Load ‘gk-’ libraries:

(require 'gk-fd)
(require 'gk-ad)
(require 'gk-misc)
(require 'gk-cmds)
(require 'gk-recomp)
(require 'gk-eless)
(require 'gk-diff-regions)
(require 'gk-hexcolour)
(require 'gk-mac)
(require 'gk-global-mode-util)
(require 'gk-minor-mode)
(require 'gk-proj)
(require 'gk-wm)
(require 'gk-txt)
(require 'gk-doc)
(require 'gk-sh)
(require 'gk-dired)
(require 'gk-im)
(require 'gk-vc)
(require 'gk-prog)
(require 'gk-pim)
(require 'gk-org)
(require 'gk-ebib-latex)
(require 'gk-mm)
(require 'gk-ui)
(when (gk-gui-p)
  (require 'gk-gui))
(require 'gk-url)



;;; General advices:

(define-advice what-cursor-position
    (:around (fn detail)
             tell-me-the-name-of-the-char-w/o-going-into-details)
  "Extend the basic output of ‘what-cursor-position’ with character name."
  (if detail
      (funcall fn detail)
    (message
     "%s"
     (concat
      (let ((inhibit-message t)) (funcall fn)) "\nDescription: "
      (get-char-code-property (following-char) 'name)))))



;;; Global modes:

;; Configuration for the Global modes utility library, and other
;; settings regarding global modes.

(mapc ($ (pushnew $1 gk-global-modes))
      '(auto-image-file-mode
        show-paren-mode
        transient-mark-mode
        whole-line-or-region-mode
        global-gk-minor-mode
        winner-mode
        global-paren-face-mode
        auto-insert-mode
        url-handler-mode
        which-key-mode
        context-menu-mode))

;; Mainly to enable GK keybindings there.
(defun gk-fundamental-mode-hook ()
  "Set up fundamental mode."
  (gk-minor-mode +1))

(add-hook 'fundamental-mode-hook #'gk-fundamental-mode-hook)

(mapc ($ (pushnew $1 gk-disabled-modes))
      '(electric-indent-mode
        pixel-scroll-mode))

;; Diminish global modes that are always on.
(diminish 'whole-line-or-region-mode)
(diminish 'buffer-face-mode "☺")
(diminish 'which-key-mode "⁈")



;;; Secrets:

(gk-load (expand-file-name "~/Documents/secrets") t)



;;; After Save™:

;; This is /the/ after save hook.  It's the one hook added to
;; =after-save-hook= that'll do all the things I might want automatically
;; done after when a file is saved.


(defvar gk-after-save-org-timer nil)
(defvar gk-after-save-org-idle-seconds 5)

(defun gk-after-save-hook ()
  "Göktuğ's After Save™, a man's best companion.

Does various tasks after saving a file, see it's definition."
  )

(add-hook 'after-save-hook 'gk-after-save-hook)



;;; Other after save hooks:

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)




;;; Keybindings:



;;;; Global overrides:

(gk-global-binding "\C-a" 'gk-bol)
(gk-global-binding "\M-j" 'gk-join-nl)
(gk-global-binding "\M-%" 'query-replace-regexp)
(gk-global-binding "\M-;" 'gk-comment-dwim)
(gk-global-binding "\C-\M-q" 'gk-indent-defun)
(gk-global-binding "\C-xd" (gk-interactively (dired default-directory)))
(gk-global-binding "\C-x\C-f" #'gk-find-file)
(gk-global-binding (kbd "C-x C-d") #'dired)
;; XXX: not working as intended yet.
;; (gk-global-binding [remap fill-paragraph] #'gk-fill-or-join-paragraph)
;; So that it doesnt override special-mode and the like.
;; (global-set-key [? ] 'gk-maybe-expand-abbrev-or-space)

;; Unset unused keys.
(global-unset-key (kbd "C-M-%"))  ; Now same as M-%
(global-unset-key (kbd "C-z"))    ; Has no utility
(global-unset-key (kbd "<menu>")) ; Like M-x, but I often hit accidentally.
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
;; Bound to ‘next-buffer’ and ‘previous-buffer’, and fuckin confusing.
(global-unset-key (kbd "C-x <right>"))
(global-unset-key (kbd "C-x <left>"))

;; Function keys.
(loop for i from 1 to 12 do
         (global-unset-key (kbd (format "<f%d>" i))))


;; Go to beginning/end of words.
(gk-global-binding "\M-e" (gk-interactively
                           "Go to the beginning of the next word."
                           (forward-word 2)
                           (forward-word -1)))

(gk-global-binding "\M-a" (gk-interactively
                           "Go to the end of the previous word"
                           (forward-word -2)
                           (forward-word 1)))

(define-key help-map "h" (gk-interactively "Go to the *Help* buffer"
                                           (display-buffer "*Help*")))

(gk-global-binding (kbd "C-M-j") #'gk-deft)



;;;; Navigation:

(gk-prefix-binding "j" 'ace-jump-mode)
(gk-prefix-binding "p" 'pop-to-mark-command)

(gk-prefix-binding "w" gk-window-management-bindings)

(gk-prefix-binding (kbd "C-;") 'goto-last-change)

(gk-global-binding (kbd "<f1>") #'gk-flash-current-line)



;;;; Shortcuts:

(gk-prefix-binding "\M-d" (gk-interactively (toggle-debug-on-error)
                                            (toggle-debug-on-quit)))
(gk-prefix-binding "r" 'rename-buffer)

(gk-prefix-binding "h" (gk-interactively
                        (when-let* ((b (get-buffer "*Help*")))
                          (switch-to-buffer b nil t))))

(gk-prefix-binding (kbd "C-#") 'display-line-numbers-mode)
(gk-prefix-binding "_" 'delete-other-windows-vertically)

(gk-prefix-binding "2" #'clone-indirect-buffer)

(gk-prefix-binding "t" #'gk-insert-today)
(gk-prefix-binding "#" #'gk-insert-todo-comment)

(gk-prefix-binding "=" #'menu-bar-mode) ;toggle
(gk-prefix-binding "g" #'magit-status)

(gk-prefix-binding "M-." #'repeat-complex-command)

(gk-prefix-binding "\M-i" #'gk-visit-user-init-file)

(gk-global-binding "\C-xw" #'gk-jump-to-window)

(gk-global-binding [mouse-8] #'scroll-down-command)
(gk-global-binding [mouse-9] #'scroll-up-command)

(gk-prefix-binding "x" #'gk-maximize)
(gk-prefix-binding (kbd "C-f") #'gk-flip)

(gk-prefix-binding "y" #'gk-yank-primary)

(gk-prefix-binding (kbd "M-q") #'gk-urls-web-search)



;;; Additional modules:

(when (eq system-type 'berkeley-unix)
  (require 'bsdpkg))



;;; EXWM:

(when (string= (getenv "EXWM") "yes")
  (load (locate-user-emacs-file "exwm-init")))



;;; Finalise initialisation:

(gk-load (file-name-sans-extension custom-file))

;; Record last know-good emacs build info. The
;; ‘gk-latest-good-build-info-file’ should only change when the build
;; config is changed, and it should only be committed to VCS when that
;; build config is known to be good.
;;
;; When building Emacs on a new system, this information can be
;; consulted to build from a VCS snapshot that’s known to accommodate
;; my configuration well.

(defvar gk-latest-good-build-info-file
  (locate-user-emacs-file "etc/latest-good-build")
  "File where info about last good build is stored
See ‘gk-dump-latest-good-build’.")

(defun gk-dump-latest-good-build ()
  "Dump latest good build info.
This funtion dump information that identifies the latest Emacs
build that’s known to work with my setup into the file identified
by ‘gk-latest-good-build-info-file’.

It’s intended to run as part of the interactive init process."
  (when (stringp emacs-repository-version)
    (let* ((file gk-latest-good-build-info-file)
           (buffer (find-file-noselect file))
           (data `( :system ,system-configuration
                    :commit ,emacs-repository-version
                    :branch ,emacs-repository-branch
                    :version ,(emacs-version)
                    :source source-directory))
           (old-data (ignore-errors
                       (read
                        (with-current-buffer buffer
                          (buffer-string)))))
           (timestamp (format-time-string "%F%T%z")))
      (if (equal data old-data)
          'up-to-date
        (prog1
            'updated
          (with-current-buffer buffer
            (erase-buffer)
            (insert ";; AUTO GENERATED FILE, DO NOT MODIFY!\n"
                    ";;\n"
                    ";; This file indicates the latest build from GNU Emacs git repo that is\n"
                    ";; known to work with my Emacs and dotfiles setup.\n"
                    ";;\n"
                    ";; Last updated: " timestamp ".\n\n"
                    (let ((pp-use-max-width t)
                          (pp-max-width 10))
                      (pp-to-string data)))
            (indent-sexp)
            (add-file-local-variable 'mode 'emacs-lisp)
            (let ((backup-inhibited t))
              (save-buffer))
            (message "Wrote ‘gk-latest-good-build-info-file’ (%s)."
                     gk-latest-good-build-info-file))
          (let ((kill-buffer-query-functions nil)
                (kill-buffer-hook nil))
            (kill-buffer buffer)))))))

(unless noninteractive
  ;; Start the server.
  (server-start)
  (add-hook 'server-switch-hook 'raise-frame)
  ;; Record build info.
  (add-hook 'after-init-hook #'gk-dump-latest-good-build))

;; If found, load system specific configuration.
(when-let* ((system-config-dir (getenv "MYSYSTEM"))
            (system-config-file
             (and
              (not (string-empty-p (string-trim system-config-dir)))
              (expand-file-name
               (concat (system-name) "-config.el")
               system-config-dir))))
  (when (file-exists-p system-config-file)
    (message "Loading system-specific config for %s..." (system-name))
    (gk-load (file-name-sans-extension system-config-file))))



;;; Auto-generated stuff:
(put 'not-modified 'disabled t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'timer-list 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'help-fns-edit-variable 'disabled nil)
