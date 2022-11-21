;;; gk-ui.el --- user interface customisations (non-gui specific)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;;; Commentary:

;; General user interface customisations.  Anything GUI-specific
;; should go into ‘gk-gui’.

;;; Code:

(require 'apropos)
(require 'bookmark)
(require 'face-remap)
(require 'files)
(require 'goto-addr)
(require 'help)
(require 'highlight-indent-guides)
(require 'ido)
(require 'ido-vertical-mode)
(require 'isearch)
(require 'minibuffer)
(require 'mwheel)
(require 'persistent-scratch)
(require 'register)
(require 'savehist)
(require 'saveplace)
(require 'select)
(require 'simple)
(require 'so-long)
(require 'switch-window)
(require 'tramp)
(require 'undo-tree)
(require 'uniquify)
(require 'whitespace)
(require 'window)
(require 'winner)

(require 'gk-global-mode-util)
(require 'gk-mac)
(require 'gk-minor-mode)


;;;; Lines:

(setf
 ;; Truncate long lines.
 truncate-lines t
 ;; Behave according to `truncate-lines'.
 truncate-partial-width-windows nil
 ;; Use default fringe indicators for ‘visual-line-mode’ too.
 visual-line-fringe-indicators
 (gk-assoca '(continuation) fringe-indicator-alist))



;;;; Long lines:
;; Adapted from so-long.el documentation.

(cl-pushnew 'global-so-long-mode gk-global-modes)

;; Basic settings.
(setf so-long-action 'so-long-minor-mode
      so-long-threshold 1000
      so-long-max-lines 100)

;; Additional target major modes to trigger for.
(mapc (apply-partially #'add-to-list 'so-long-target-modes)
      '(sgml-mode nxml-mode))

;; Additional buffer-local minor modes to disable.
(mapc (apply-partially #'add-to-list 'so-long-minor-modes)
      '(diff-hl-mode diff-hl-amend-mode diff-hl-flydiff-mode))

;; Additional variables to override.
(mapc (apply-partially #'add-to-list 'so-long-variable-overrides)
      '((show-trailing-whitespace . nil)
        (truncate-lines . nil)))



;;;; UI Semantics:
(setf
 find-file-suppress-same-file-warnings t
 ;; The following two setting silence the bell mechanism altogether,
 ;; both visually and audible ones.
 visible-bell nil
 ring-bell-function ($)
 uniquify-buffer-name-style 'post-forward-angle-brackets
 save-interprogram-paste-before-kill t
 ;; The default 80 is too much.
 split-height-threshold 75
 ;; Don't make seperate popup frames.
 pop-up-frames nil
 ;; Display buffers in current window.
 display-buffer-alist
 `(("\\*Quail Completions" . (display-buffer-in-side-window))
   ("\\*.*Completions\\*" . (display-buffer-in-side-window . ((side . bottom))))
   ("\\*Help\\*" . (display-buffer-reuse-window))
   ("Checkdoc" . (display-buffer-pop-up-window))
   ("Calendar[^\\.]" . (display-buffer-in-side-window . ((side . bottom))))
   ("help\\[R\\]" . (display-buffer-pop-up-window))
   ("\\*pager\\*.*" . (display-buffer-pop-up-window
                       . ((inhibit-same-window . t))))
   ("\\*\\(Compile-Log\\|Warnings\\)\\*" . (display-buffer-in-direction
                                            . ((direction . below))))
   ;; Use ‘display-buffer-same-window’ as a catch-all for everything
   ;; but...
   (,($ [b _] (not (memq (buffer-local-value 'major-mode (get-buffer b))
                         ;; ... these bad bois.
                         '(rmail-mode
                           rmail-summary-mode))))
    .
    (display-buffer-same-window)))
 ;; With C-v and M-v, scroll to beginning or end of buffer if at top
 ;; or bottom respectively, or if at beginning or end of buffer,
 ;; error.
 scroll-error-top-bottom t
 ;; Search help more extensively.
 apropos-do-all t
 ;; Always open a new buffer if default is occupied.
 async-shell-command-buffer 'new-buffer
 ;; Swift and smarter completion.
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 completion-styles '(basic substring partial-completion initials flex)
 ;; Add ‘<’ as a delimiter, and update the relevant regexp.
 completion-pcm-word-delimiters
 (let ((re "-_./:|< "))
   ;; Update an internal variable.
   (completion-pcm--prepare-delim-re re)
   ;; Return pristine regexp to be set.
   re)
 ;; Display completion details.
 completions-detailed t
 ;; Automatically select the completions buffer when it pops up.
 completion-auto-select t
 ;; Do not  ring the bell  when killing  in r/o buffers,  put the
 ;; kill in the kill ring but do not modify the buffer.
 kill-read-only-ok t
 ;; Save bookmarks after each bookmark command.
 bookmark-save-flag t
 ;; Search modes default to regexps.
 search-default-mode t
 ;; Only search in visible part.
 search-invisible nil
 ;; Case insensitive search.
 case-fold-search t
 ;; Move to trash instead of unlinking.
 delete-by-moving-to-trash t
 ;; Save abbrevs silently
 save-abbrevs 'silently
 ;; Display ‘default-directory’ when prompting for a shell command.
 shell-command-prompt-show-cwd t
 ;; Window manager’s focus follows mouse.
 focus-follows-mouse t
 ;; Focus follows mouse in Emacs too. Focus 100ms after the mouse
 ;; stops in a window.
 ;; mouse-autoselect-window -0.1
 ;; Disable interaction with clipboard manager, can cause freezes.
 x-select-enable-clipboard-manager nil
 ;; Indicate unused lines at the end of the buffer with marks in left
 ;; fringe.
 indicate-unused-lines t
 ;; Draw underlines lower.
 underline-minimum-offset 7
 ;; Properly maximise frames in non-DE window managers.
 frame-resize-pixelwise t
 ;; Split vertically in general
 split-width-threshold 140
 ;; Each buffer has its own goto-line history.
 goto-line-history-local t
 ;; Scale header lines with buffer when zooming.
 text-scale-remap-header-line t
 ;; Unlimited minibuffer history.
 history-length t
 history-delete-duplicates t
 ;; Potential speedup in some cases that relate to fonts and
 ;; font-lock. viz. https://github.com/integral-dw/org-bullets.
 inhibit-compacting-font-caches t
 ;; Use ‘outline-mode’ in C-h b (‘describe-bindings’)
 describe-bindings-outline t
 ;; Interpret key presses case sensitively so C-c e != C-c E.
 translate-upper-case-key-bindings nil
 ;; Make a backup of a file the first time it is saved.
 make-backup-files t
 ;; Make backup first, then copy to the original.
 backup-by-copying nil
 ;; Version-numbered backups.
 version-control t
 ;; Keep a lot of copies.  Only not version-controlled files (see
 ;; ‘vc-make-backup-files’.
 kept-old-versions 10000
 kept-new-versions kept-old-versions
 backup-directory-alist
 `(("/ssh:.*" . ".")
   ("." . ,(expand-file-name "~/.backups")))
 ;; Don't show annoying register previews that mess up my macro
 ;; recordings.  Hit C-h to bring it up if necessary.
 register-preview-delay nil)


(setq-default save-place t)

(setf frame-title-format
      '("%@%* %b")
      icon-title-format frame-title-format)

(cl-pushnew 'savehist-mode gk-global-modes)
(setf savehist-additional-variables
      (append savehist-additional-variables
              '(search-ring regexp-search-ring)))

;;;; File associations / ‘auto-mode-alist’:

;; Emacs can display fonts with ‘image-mode’.  It already does TTF
;; fonts, but do OTF also.
(add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))



;;;; Startup:

;; No start screens.
(setf
 inhibit-startup-screen t
 inhibit-startup-echo-area-message (eval-when-compile (user-login-name)))



;;;; Winner, windmove, and switch-window:

(setq winner-dont-bind-my-keys t)

(add-to-list 'gk-global-modes 'winner-mode)

(define-minor-mode gk-switch-window-minor-mode
  "Simple minor mode for ‘switch-window’ keybindings.

\\{gk-switch-window-minor-mode-map}"
  nil ""
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [remap other-window] #'switch-window)
      (define-key map [remap delete-other-windows] #'switch-window-then-maximize)
      (define-key map [remap split-window-below] #'switch-window-then-split-below)
      (define-key map [remap split-window-right] #'switch-window-then-split-right)
      (define-key map [remap delete-window] #'switch-window-then-delete)
      (define-key map [remap dired-other-window] #'switch-window-then-dired)
      (define-key map [remap find-file-other-window] #'switch-window-then-find-file)
      (define-key map [remap compose-mail-other-window] #'switch-window-then-compose-mail)
      (define-key map [remap find-file-read-only-other-window] #'switch-window-then-find-file-read-only)
      (define-key map [remap display-buffer] #'switch-window-then-display-buffer)
      (define-key map [remap kill-buffer-and-window] #'switch-window-then-kill-buffer)))
  (if gk-switch-window-minor-mode
      ;; Most notably makes ‘switch-window-auto-resize-window’ apply to
      ;; mouse movement too, whether via clicking or ‘focus-follows-mouse’.
      (switch-window-mouse-mode +1)
    (switch-window-mouse-mode -1)))


(define-globalized-minor-mode global-gk-switch-window-minor-mode
  gk-switch-window-minor-mode gk-switch-window-minor-mode)

(setf
 ;; Use alphanumeric shortcuts.
 switch-window-shortcut-style 'qwerty

 ;; Automatically and slightly enlarge active window.
 switch-window-auto-resize-window nil
 switch-window-default-window-size 0.51

 ;; Read input from minibuffer, instead of using ‘read-event’, in
 ;; order to preemptively prevent predictably preposterous problems
 ;; when using the preponderant program predicted to be pronounced
 ;; precisely as EXWM.
 switch-window-input-style 'minibuffer)

;; Flash the current line in the switched-to window after switching to
;; emphasise which window gained focus.
(define-advice other-window
    (:filter-return (ret) flash-line-after)
  "Flash the current line after switching windows."
  (gk-flash-current-line)
  ret)

(define-advice windmove-do-window-select
    (:filter-return (ret) flash-line-after)
  "Flash the current line after switching windows."
  (gk-flash-current-line)
  ret)



;;;; Minibuffer:

(setf enable-recursive-minibuffers nil)

(defun gk-minibuf-hook ()
  "Minibuffer setup."
  (gk-minor-mode +1))

(add-hook 'minibuffer-setup-hook 'gk-minibuf-hook)

(define-key minibuffer-local-map (kbd "\C-v") #'switch-to-completions)

;; Use M-1 through 9 to choose a completion.
(dotimes (i 9)
  (when (> i 0)
    (define-key
      minibuffer-local-map (kbd (format "M-%d" i))
      (gk-interactively
       (switch-to-completions)
       (goto-char (point-min))
       (next-completion i)
       (choose-completion)))))



;;;; Mode line:

(setf
 ;; If mode line is too long, compact it.
 mode-line-compact 'long)

(defconst gk-mode-line-pristine-format
  (copy-list mode-line-format)
  "Modeline before my modifications.")

(defconst gk-mode-line-base
  ;; Remove the infinite-spaces, the last item of the list.
  (butlast gk-mode-line-pristine-format 1)
  "The base for constructing a custom mode line.")

(defvar gk-mode-line-modes
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
        (major-mode-help-text "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes

Active minor modes: \n - "))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
          `(:eval
            (if (stringp mode-name)
                (propertize
                 (concat "#" mode-name)
                 'face '(:weight bold :underline t)
                 'help-echo
                 (concat
                  ,major-mode-help-text
                  (mapconcat
                   #'symbol-name
                   (cl-remove-if-not ($ (symbol-value $1)) (mapcar #'car minor-mode-alist))
                   "\n - "))
                 'mouse-face 'mode-line-highlight
                 'local-map mode-line-major-mode-keymap)
              mode-name))
          '("" mode-line-process)
          (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                      'mouse-face 'mode-line-highlight
                      'local-map (make-mode-line-mouse-map
                                  'mouse-2 #'mode-line-widen))
          (propertize "%]" 'help-echo recursive-edit-help-echo)
          " "))
  "Mode line construct for displaying major and minor modes.

An adaptation and simplification of ‘mode-line-modes’.")
(put 'gk-mode-line-modes 'risky-local-variable t)

(defvar gk-mode-line-buffer-file-name
  '(" "
    (:eval
     ;; Buffer's file if visiting one, the default directory
     ;; otherwise.  Ellipsise long names.
     (let* ((f (or (ignore-errors (abbreviate-file-name (buffer-file-name))) default-directory))
            (... (ignore-errors (gk-ellipsize-file-or-directory-name f 25))))
       (propertize
        (or ... f)
        'help-echo (concat f "\nmouse-1: Copy full path of buffer to clipboard")
        'mouse-face 'mode-line-highlight
        'local-map (make-mode-line-mouse-map
                    'mouse-1 (lambda (event)
                               (interactive "e")
                               (with-selected-window (posn-window (event-start event))
                                 (or (ignore-errors (gk-copy-buffer-file-name))
                                     (gk-save-string-as-kill default-directory))))))))))
(put 'gk-mode-line-buffer-file-name 'risky-local-variable t)


(defvar gk-mode-line-toggle-display-line-numbers-mode
  '(:eval
    (propertize
     "# "
     'help-echo  "mouse-1: Toggle line number display"
     'mouse-face 'mode-line-highlight

     'local-map
     (make-mode-line-mouse-map
      'mouse-1 #'display-line-numbers-mode))))
(put 'gk-mode-line-toggle-display-line-numbers-mode 'risky-local-variable t)

(defun gk-build-mode-line-format ()
  (-replace-first
   'mode-line-modes 'gk-mode-line-modes
   (cons gk-mode-line-toggle-display-line-numbers-mode
         (append gk-mode-line-base
                 gk-mode-line-buffer-file-name))))

(setq-default mode-line-format (gk-build-mode-line-format))



;;;; Goto-address mode:

(defun gk-start-global-address-mode ()
  (goto-address-mode +1))

(dolist (m '(text-mode-hook prog-mode-hook comint-mode-hook))
  (add-hook m 'gk-start-global-address-mode))

(diminish 'goto-address-mode "⚓")

(cl-pushnew "gemini://" goto-address-uri-schemes :test #'string=)
(cl-pushnew "gopher://" goto-address-uri-schemes :test #'string=)

;; C-Return on an adress follows it.
(define-key goto-address-highlight-keymap (kbd "<C-return>") #'goto-address-at-point)



;;;; Scrolling:

(define-advice scroll-up-command (:after (arg) gk-flash)
  "Scroll up, go ‘next-screen-context-lines’ more, and flash.\n
So that the reader knows where to continue reading."
  (ignore arg)
  (ignore-errors
    ;; Note the buffer, will return to it to disable ‘hl-line-mode’
    ;; only there.
    (let ((buf (current-buffer)))
      (forward-line next-screen-context-lines)
      (gk-flash-current-line buf))))



;;;; Mouse:

(pushnew 'pixel-scroll-mode gk-global-modes)

(setq
 ;; Scroll smoother, no hurries.
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil))
 mouse-wheel-progressive-speed nil
 ;; Horizontal scroll.
 mouse-wheel-tilt-scroll t
 mouse-wheel-flip-direction t)



;;;; TRAMP:

(setf
 ;; Errors and warnings only.
 tramp-verbose 1

 ;; Make TRAMP work with GuixSD machines.
 ;; Adapted from: https://lists.gnu.org/archive/html/help-guix/2016-10/msg00049.html
 tramp-remote-path
 (append tramp-remote-path
         (list "~/.guix-profile/bin"
               "~/.guix-profile/sbin"
               "/run/current-system/profile/bin"
               "/run/current-system/profile/sbin"))

 ;; Do not use ‘auth-sources’ or ‘netrc-file’, which causes an
 ;; annoying prompt.
 tramp-completion-use-auth-sources nil

 ;; Use scp(1) to directly copy instead of temporary directories.
 tramp-use-scp-direct-remote-copying t)



;;;; Whitespace:

(setf whitespace-style '(face trailing tabs)
      ;; When nil uses ‘fill-column’.
      whitespace-line-column nil)

(setcdr (assoc 'tab-mark whitespace-display-mappings) '(9 [?> 9]))
(setcdr (assoc 'newline-mark whitespace-display-mappings) '(10 [?$ 10]))

(pushnew 'global-whitespace-mode gk-global-modes)
(diminish 'global-whitespace-mode "¶")

(setq-default highlight-indent-guides-method 'column)



;;;; Persistent scratch:

(setf
 ;; Save all that's possible.
 persistent-scratch-what-to-save '(major-mode point narrowing text-properties)
 persistent-scratch-save-file (locate-user-emacs-file "etc/+scratch+"))
;; (persistent-scratch-setup-default)



;;;; Coding system:

;; Use UTF-8 encoding everywhere.

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;;; Completions:
;;;;; Ido & Smex:

(define-minor-mode gk-ido-smex-mode
  "Minor mode to govern ‘ido-mode’ and ‘smex-mode’.

\\{gk-ido-smex-mode-map}"
  :lighter "Gk-Ido/Sx"
  :global t
  :keymap
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [remap execute-extended-command] #'smex)
      (define-key map (kbd (concat gk-minor-mode-prefix "M-x")) #'smex-major-mode-commands)
      (define-key map (kbd (concat gk-minor-mode-prefix "C-M-x")) #'execute-extended-command)))
  (if gk-ido-smex-mode
      (progn
        (ido-mode +1)
        (ido-everywhere +1)
        (ido-vertical-mode +1)
        (smex-initialize))
    (ido-mode -1)
    (ido-everywhere -1)
    (ido-vertical-mode -1)
    ;; To revert ‘smex-initialize’ fully.
    (remove-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)))

(setf
 ido-use-filename-at-point nil
 ;; Don't show dotfiles if the prefix of the search string is not ‘.’
 ido-enable-dot-prefix t
 ido-confirm-unique-completion t
 ;; Show in the current frame, change window's buffer if necessary.
 ido-default-buffer-method 'selected-window
 ;; Include ‘.’ in completions for opening directory via dired.
 ido-show-dot-for-dired t
 ido-enable-flex-matching t
 ido-vertical-define-keys 'C-n-and-C-p-only)

(add-hook
 'ido-minibuffer-setup-hook
 (defun gk-ido-disable-line-truncation ()
   (set (make-local-variable 'truncate-lines) nil)))



;;;;; Icomplete:

(define-minor-mode gk-icomplete-mode
  "Minor mode to govern ‘icomplete-mode’.

\\{gk-icomplete-mode-map}"
  :lighter "Gk-Ic"
  ;; :global t ; doesn’t work for some reason...
  (if gk-icomplete-mode
      (progn
        (icomplete-mode)
        (icomplete-vertical-mode))
    (icomplete-vertical-mode -1)
    (icomplete-mode -1)))

(setf
 icomplete-show-matches-on-no-input t)



;;;;; Setup:

;; (cl-pushnew 'gk-icomplete-mode gk-global-modes)



;;;; Undo:

(setf
 ;; Do not persist undo history.
 undo-tree-auto-save-history nil)

;; Enable undo-tree.
(cl-pushnew 'global-undo-tree-mode gk-global-modes)



;;;; Saving files:

(define-advice write-file
    (:filter-args (args) always-confirm)
  "Make sure ‘write-file’ confirms overrides and asks to create directories.

When called interactively."
  (list (car args) (called-interactively-p 'interactive)))



;;;; Smarter ‘copyright-update’:

;; Automatically update copyright lines but be smart about skipping
;; where it’s not needed.

(setf
 ;; Always prompt before updating copyright.
 copyright-query t)

(add-hook 'before-save-hook #'copyright-update)

(defvar gk-copyright-update-skip-conditions
  (list "^COMMIT_EDITMSG$"
        '(eq major-mode 'diff-mode))
  "Conditions in which to skip ‘copyright-update’.

A condition can be

- a string, in which case the ‘buffer-name’ is matched against
  it

- a list, in which case it is evaluated as Lisp code within the
  buffer’s context.")

(defun gk-copyright-update-apply-check (condition)
  (cond ((stringp condition)
         (string-match condition (buffer-name)))
        ((listp condition)
         (eval condition))))

(define-advice copyright-update
    (:around (fn &rest args) skip-some-buffers)
  "Skip buffers which do have copyright lines but don’t need updating.
Like COMMIT_EDITMSG.
See ‘gk-copyright-update-skip-list’."
  (unless (cl-some #'gk-copyright-update-apply-check
                   gk-copyright-update-skip-conditions)
    (apply fn args)))




;;;; Layouts:

(defun gk-layouts-3col ()
  "Three column layout.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list))))
          (width (/ (frame-width) 3)))
      (delete-other-windows)
      (split-window-horizontally width)
      (other-window 1)
      (split-window-horizontally)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))


(defun gk-layouts-3col-and-follow ()
  "Call ‘gk-layouts-3col’ and then ‘follow-mode’."
  (interactive)
  ;; Only extend the current window.
  (delete-other-windows)
  (gk-layouts-3col)
  ;; Go to leftmost window, ‘windmove-left’ will signal on leftmost
  ;; window.
  (ignore-errors (while t (windmove-left)))
  (follow-mode))


(defun gk-layouts-main-and-sidekicks (&optional arg)
  "One horizontal split, the right window split in two.

Tries to preserve the order of window buffers and active window.

If ARG is non-nil, or if called with a prefix argument, the left
column will be split into two instead."
  (interactive "P")
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list)))))
      (delete-other-windows)
      (split-window-horizontally)
      (unless arg (other-window 1))
      (split-window-vertically)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))



;;;; Keybindings:

(defvar gk-window-management-bindings
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'winner-undo)
    (define-key map "r" 'winner-redo)
    (define-key map "p" 'windmove-up)
    (define-key map "n" 'windmove-down)
    (define-key map "f" 'windmove-right)
    (define-key map "b" 'windmove-left)
    (define-key map "s" 'gk-swap-windows)
    (define-key map "+" 'enlarge-window)
    (define-key map "-" 'shrink-window)
    map)
  "Keybindings for window management")

(gk-global-binding (kbd "<up>") #'windmove-up)
(gk-global-binding (kbd "<down>") #'windmove-down)
(gk-global-binding (kbd "<right>") #'windmove-right)
(gk-global-binding (kbd "<left>") #'windmove-left)

(gk-unbind-key (kbd "S-<up>"))
(gk-unbind-key (kbd "S-<down>"))
(gk-unbind-key (kbd "S-<right>"))
(gk-unbind-key (kbd "S-<left>"))



(provide 'gk-ui)
;;; gk-ui.el ends here
