;;; init.el --- Göktuğ's Emacs configuration. -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Version: ETERNAL
;; URL: https://www.gkayaalp.com/emacs.html
;; Package-Requires: ((emacs "27.0.50"))

;;; Commentary:

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



;;; Code:

;; The subsequent sections constitute the Emacs initialisation code.



;;; Prelude:

(when (version< emacs-version "28.0")
  (error "This configuration requires a recent build of Emacs master"))

;; Use elisp directory listing program.  This needs to be set before
;; loading ls-lisp.el.
(defvar ls-lisp-use-insert-directory-program nil)



;;;; Loadpaths:

(load (locate-user-emacs-file "loadpaths"))



;;;; Requires:

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
(require 'gk-greek)
(require 'gk-unilat)
(require 'goto-addr)
(require 'goto-last-change)
(require 'haskell-mode)
(require 'highlight-indent-guides)
(require 'highlight-parentheses)
(require 'hl-line)
(require 'ibuffer)
(require 'ibuffer-vc)
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
(require 'org-habit)
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
(require 'pdf-tools)
(require 'pdf-annot)
(require 'pdf-cache)
(require 'pdf-isearch)
(require 'pdf-misc)
(require 'pdf-outline)
(require 'pdf-sync)
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



;;; Files and directories:

(defconst gk-dropbox-dir
  (expand-file-name "~/fil")
  "Directory of Dropbox.")

(defun dropbox (path)
  "Return ~/Dropbox + PATH."
  (expand-file-name path gk-dropbox-dir))

(defconst gk-syndir
  (expand-file-name "~/syn")
  "Directory for syncing.")

(setf image-dired-dir (locate-user-emacs-file "etc/image-dired")
      url-configuration-directory (locate-user-emacs-file "etc/url")
      auto-save-list-file-prefix (locate-user-emacs-file
                                  "etc/auto-save-list/saves-")
      bookmark-default-file (dropbox "bookmarks.el")
      bbdb-file (expand-file-name "~/Documents/bbdb")
      savehist-file (locate-user-emacs-file "etc/history")
      eww-bookmarks-directory (dropbox ".")
      save-place-file (locate-user-emacs-file "etc/places")
      tramp-persistency-file-name (locate-user-emacs-file "etc/tramp")
      custom-file (locate-user-emacs-file "etc/custom.el")
      nsm-settings-file (locate-user-emacs-file "etc/network-security.data")
      mc/list-file (locate-user-emacs-file "etc/mc-lists.el"))

(defvar gk-website-settings
  (expand-file-name "~/Documents/not/www/publish")
  "Settings for publishing http://www.gkayaaalp.com.")



;;; Utility libraries:



;;;; Utility functions:

(defun gk-backup-file-name (directory extension)
  (let ((filename (concat directory
                          (format-time-string "%d-%m-%Y" (current-time))))
        (extension (concat "." extension)))
    (while (file-exists-p (concat filename extension))
      (setq filename (concat filename "+")))
    (concat filename extension)))

(defun gk-apropos-at-point-or-region ()
  (interactive)
  (let ((default (if (region-active-p)
		     (buffer-substring (region-beginning) (region-end))
		   (thing-at-point 'word))))
    (apropos (read-string "Search for command or function (word list or regexp): "
			  default nil default))))

(defun gk-indent-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun gk-which-mode (buffer)
  (interactive "bShow major mode for buffer")
  (with-current-buffer (get-buffer buffer)
    (set-register ?M (symbol-name major-mode))
    (message "Major mode for buffer '%s' is '%s'.  C-x r i M to insert it."
	     buffer
	     major-mode)))

(defun gk-comment-dwim (arg)
  "If region is active, or at the end of the line, call ‘comment-dwim’.

Pass ARG to ‘comment-dwim’ in that case.
Otherwise comment-out the whole line, or ARG lines."
  (interactive "*P")
  (cond
   ((save-excursion
      (beginning-of-line)
      (looking-at (concat "[[:blank:]]*" comment-start)))
    (uncomment-region (point-at-bol) (point-at-eol)))
   ((or (looking-at "[[:blank:]]*$")
        (region-active-p))
    (comment-dwim arg))
   (t (save-excursion (comment-line arg)))))

(defun gk-reformat-date (format date)
  "Parse DATE, then apply FORMAT to it.

For the format, see `format-time-string'."
  (format-time-string format (date-to-time date)))

(defun gk-executable-ensure (command &optional silent)
  "Err-out if COMMAND is not found."
  (if-let* ((ex (executable-find command)))
      ex
    (when (not silent)
      (warn "Program is absent: %s" command))))

(defun gk-get-file-contents (file)
  "Get the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring (point-min) (point-max))))

(defun gk-deadvice (sym)
  "Remove all the advice functions from the function named SYM."
  (interactive
   (list
    (let* ((sym (ignore-errors (intern (thing-at-point 'symbol))))
           (fn (and (fboundp sym) sym)))
      (read-command (concat "Remove advices from function"
                            (if fn
                                (format " (default: %S): " sym)
                              ": "))
                    fn))))
  (advice-mapc
   (lambda (x y)
     (ignore y)
     (remove-function (symbol-function sym) x))
   sym))

(defun gk-gui-p ()
  (or window-system (daemonp)))

(defun gk-swap-windows (&optional arg)
  "Swap the buffer of the selected window with that of the next one.

When ARG is a positive number, repeat that many times."
  (interactive "p")
  (dotimes (i (or arg 1))
    (ignore i)
    (let ((next (window-buffer (next-window)))
          (this (current-buffer)))
      (unless (equal this next)
        (switch-to-buffer next nil t)
        (switch-to-buffer-other-window this)))))

(defun gk-copy-buffer-file-name ()
  "Push the buffer's file name to the ‘kill-ring’."
  (interactive)
  (if-let* ((fil (buffer-file-name)))
      (with-temp-buffer
        (insert fil)
        (clipboard-kill-ring-save (point-min) (point-max))
        (message fil))
    (error "Buffer not visiting a file.")))

(defun gk-copy-last-message ()
  "Copy-as-kill the last echoed message."
  (interactive)
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (clipboard-kill-ring-save
       (line-beginning-position) (line-end-position)))))

(defun gk-copy-as-markdown-code-snippet (beg end)
  "Copy region, prepend four spaces to every line."
  (interactive "r")
  (let ((snip (buffer-substring beg end)))
    (with-temp-buffer
      (insert snip)
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match "    "))
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun gk-maybe-expand-abbrev-or-space ()
  (interactive)
  (when (null (expand-abbrev))
    (insert " ")))

(defun gk-numeronym (name &optional insert-p)
  "Generate a numeronym of NAME, an arbitrary string.

A numeronym is the initial letter, the length of the name in
characters, and the last letter,
i.e. i18n -> internationalisation.

If INSERT-P is non-nil or called interactively with prefix-arg,
insert the numeronym at point."
  (interactive (list (read-string "Enter the name to be numeronymified: ")
                     ;; convert to bool
                     (not (not current-prefix-arg))))
  (let ((len (length name))
        nym)
    (unless (>= len 2) (user-error "The name must be at least three characters long"))
    (setf nym (format "%c%d%c" (aref name 0) (- len 2) (aref name (1- len))))
    (message nym)
    (when insert-p (insert nym))))

(defun gk-unbind-key (keyseq)
    "Unset the KEYSEQ in ‘gk-minor-mode-map’."
    (interactive "kKey sequence to unset: ")
    (define-key gk-minor-mode-map keyseq nil)
    (message "Done."))

(defun gk-delete-buffer-file ()
  "Delete the file visited in the current buffer."
  (interactive)
  (if-let* ((f (buffer-file-name)))
      (when (yes-or-no-p
             (format
              "Delete file ‘%s’, visited by buffer ‘%s’" f (buffer-name)))
        (delete-file f delete-by-moving-to-trash)
        (message "Deleted %s." f))
    (user-error "Buffer ‘%s’ is not visiting a file" (buffer-name))))

(defun gk-copy-buffer-file (file dest)
  "Copy the file visited in the current buffer."
  (interactive
    (list (buffer-file-name)
          (read-file-name (format "Copy %s to: " (buffer-file-name)))))
  (copy-file file dest nil t t t))

(defun gk-rename-buffer-file (dest)
  "Rename the file visited in the current buffer."
  (interactive
    (list (read-file-name (format "Rename %s to: " (buffer-file-name)))))
  (rename-file (buffer-file-name) dest)
  (find-alternate-file dest))

(defun gk-truncate-and-fill-string (len s)
  (let ((slen (length s)))
    (if (> slen len)
        (s-truncate len s)
      (concat s (make-string (- len slen) ?\ )))))

(defun gk-find-file (arg)
  "Like ‘find-file’ but find file at point if ARG is non-nil."
  (interactive "P")
  ;; See http://lists.gnu.org/archive/html/help-gnu-emacs/2018-04/msg00280.html
  (let ((current-prefix-arg nil))
    (call-interactively (if arg #'ffap #'find-file))))

(defun // (&rest args)
  (apply #'/ (mapcar #'float args)))

(define-obsolete-function-alias 'gk-update-package-load-paths
  'gk-update-user-site-paths "2020-09-23")

(defun gk-send-desktop-notification (summary message &optional icon)
  "Show a notification on the desktop."
  (unless (gk-gui-p)
    (error "Cannot send desktop notification in non-GUI session"))
  (make-process
   :name "gk-desktop-notification"
   :buffer (get-buffer-create " *Desktop Notifications*")
   :command
   (cond
    ((executable-find "notify-send")
     (list "notify-send" (concat "[Emacs] " summary) "-i" (or icon "") message))
    ((executable-find "kdialog")
     (list "kdialog" "--passivepopup" message "10"
           "--title" (concat "[Emacs] " summary))))))

(defun gk-existing-file-name-or-nil (filename)
  (when (file-exists-p filename)
    filename))

(defun gk-insert-today (&optional full)
  "Insert today's date into the current buffer, before point.

FULL is the processed prefix argument from the interactive call.

With no prefix arguments, insert YYYY-MM-DD (ISO 8601 date).
With one prefix argument, insert YYYY-MM-DD (ISO 8601 date) with
HH:MM:SS.  With two prefix arguments, insert a full ISO 8601 date
together with current time and timezone information."
  (interactive "p")
  (insert
   (format-time-string
    (case full
      (1 "%F")                          ;ISO date format
      (4 "%F %T")                       ;ISO date format with time w/ seconds
      (16 "%FT%T%z")                    ;full ISO 8601
      ))))

(defun gk-toggle-wrap (&optional arg)
  "Toggle word wrap and line truncation.

Without a prefix ARG, toggle the latter off and the former on.
With a positive prefix, turn both on.  With a negative prefix,
turn both off.  With a zero prefix, toggle both."
  (interactive "p")
  (cond ((or (null arg) (= arg 1))
         (toggle-truncate-lines -1)
         (toggle-word-wrap +1))
        ((= arg 0)
         (toggle-truncate-lines (if truncate-lines -1 +1))
         (toggle-word-wrap (if word-wrap -1 +1)))
        ((> arg 1)
         (toggle-truncate-lines +1)
         (toggle-word-wrap +1))
        ((< arg 0)
         (toggle-truncate-lines -1)
         (toggle-word-wrap -1)))
  (message "truncate-lines: %S; word-wap: %S" truncate-lines word-wrap))

(defun gk-view-emacs-proc-file ()
  "Open the Emacs process status file under /proc."
  (interactive)
  (find-file (format "/proc/%d/status" (emacs-pid))))

(defun gk-ellipsize-file-or-directory-name (name maxlen)
  "Ellipsize the directory part of a file NAME.

If NAME is larget than MAXLEN, ellipsise the directory part,
preserving, ‘file-name-nondirectory’ if it's a file or the last
directory name if a directory, returning the ellipsized string as
the result."
  (if (> (length name) maxlen)
      (if (or (file-directory-p name)
              (save-match-data (string-match "/$" name)))
          (let* ((bits (split-string name "/" t))
                 (head (butlast bits))
                 (tail (car (last bits))))
            (concat
             (unless (equal (car bits) "~") "/")
             (substring (mapconcat #'identity head "/") 0
                        (- (- maxlen 4) (length bits)))
             ".../" tail "/"))
        (let ((fnod (file-name-nondirectory name)))
          (concat
           (substring (file-name-directory name) 0
                      (- (- maxlen 4) (length fnod)))
           ".../" fnod)))
    name))

(defun gk-next-theme ()
  "Switch to the next theme in ‘custom-known-themes’.

If exhausted, disable themes.  If run again thereafter, wrap to
the beginning of the list."
  (interactive)
  (let* ((ct (or (car custom-enabled-themes)
                 (car custom-known-themes)))
         (next (cadr (memq ct custom-known-themes))))
    (when (memq next '(user changed))
      (setq next nil))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if next
        (progn
          (load-theme next t)
          (message "Loaded theme ‘%S’" next))
      (message "All themes disabled"))))

(cl-defun gk-flash-current-line (&optional buffer &key (seconds 0.5))
  "Flash current line briefly for SECONDS in BUFFER.

BUFFER defaults to current buffer, and SECONDS to 1."
  (interactive)
  (unless hl-line-mode
    (let ((buf (or buffer (current-buffer))))
      (hl-line-mode +1)
      (run-with-idle-timer
       seconds nil
       ($ (with-current-buffer buf
            (hl-line-mode -1)))))))

(defun gk-empty-kill-ring ()
  "Empty the kill ring."
  (interactive)
  (when kill-ring
    (setq kill-ring nil)
    (garbage-collect)))

(defun gk-bol ()
  "Alternate between the first and the indentation on a line."
  (interactive)
  (cond
   ;; If this is an eshell buffer and we’re at a prompt line, jump to
   ;; prompt position.
   ((and (eq major-mode 'eshell-mode)
         (save-excursion
           (goto-char (line-beginning-position))
           (looking-at eshell-prompt-regexp)))
    (goto-char (line-end-position))
    (eshell-previous-prompt 0))
   ;; Otherwise, alternate bol/indentation.
   (t
    (let ((bolf (if visual-line-mode #'beginning-of-visual-line
                  #'beginning-of-line))
          (p  (point)))
      ;; We do this to prevent any flicker happening between
      ;; ‘back-to-indentation’ and ‘bolf‘ when going to
      ;; ‘beginning-of-line’.
      (goto-char
       (save-excursion
         ;; If visual-line-mode is on and we're on a continuation line,
         ;; go to the beginning of the continuation line.
         ;;
         ;; XXX: sometimes this goes to the previous line because of
         ;; word-wrapping
         (if (and visual-line-mode
                  (>= (- p (line-beginning-position))
                      (window-width)))
             (funcall bolf)
           ;; Else, do the toggling.
           (progn
             ;; Go back to indentation.
             (back-to-indentation)
             ;; If we didn't move, we were already at the indentation.
             ;; Go to the beginning of the line.
             (when (= p (point))
               (funcall bolf))))
         ;; Return the point.
         (point)))))))

(defvar gk-insert-todo-comment--history nil)
(defvar gk-insert-todo-comment-keywords '("TODO" "XXX" "HACK" "FIXME"))
(defvar gk-insert-todo-comment-default (car gk-insert-todo-comment-keywords))

(defun gk-insert-todo-comment (keyword)
  "Insert a TODO comment with date."
  (interactive
   (list
    (completing-read
     (format "Todo keyword to use (default: %s): "
             gk-insert-todo-comment-default)
     gk-insert-todo-comment-keywords
     nil nil nil 'gk-insert-todo-comment--history "TODO" t)))
  (gk-comment-dwim nil)
  (insert keyword)
  (insert (format-time-string "(%F): ")))

;; Adapted from https://www.reddit.com/r/emacs/comments/bwm94g/weekly_tipstricketc_thread/eq09l4k/
(defun gk-search-forward-1 (char &optional count)
  "Search forward for CHAR COUNT times in current line."
  (interactive
   (list (read-char "1> ")
         current-prefix-arg))
  (forward-char)
  (unwind-protect
      (search-forward (char-to-string char) (line-end-position) nil (or count 1))
    (backward-char)
    (point)))

(defun gk-search-backward-1 (char &optional count)
  "Search backward for CHAR COUNT times in current line."
  (interactive
   (list (read-char "1> ")
         current-prefix-arg))
  (backward-char)
  (unwind-protect
      (search-backward (char-to-string char) (line-beginning-position) nil
                       (or count 1))
    (forward-char)
    (point)))

(defun gk-build-emacs-master ()
  "Run Emacs git build wrapper script."
  (interactive)
  (let ((compilation-buffer-name-function ($ [_] "*Build Emacs Master*")))
    (compile "build-emacs-master.sh" t)))

(defun gk-visit-user-init-file ()
  "Visit ‘user-init-file’, reuse window if useful.

Flash the current line after that."
  (interactive)
  (let ((file (file-truename (expand-file-name user-init-file))))
    ;; If viewing the file, only flash current line.
    (unless (string= file (buffer-file-name (window-buffer)))
      ;; Otherwise, open it, or if the current frame already has a
      ;; window displaying it, switch to it.
      (select-window
       (display-buffer
        (find-file-noselect file)
        '(display-buffer-reuse-window . ((reusable-frames . nil)
                                         (inhibit-same-window . t)))))))
  (gk-flash-current-line))

(defun gk-decode-xml-entities-in-region (beginning end)
  (interactive "r")
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (with-temp-buffer
      (save-excursion (insert str))
      (message (substring-no-properties (xml-parse-string))))))

(defun gk-base64-decode-url (beg end)
  "Base64-decode the region between BEG and END, assume URL encoding.

This basically calls ‘base64-decode-region’ with the third
argument as t, so refer to that command for further details."
  (interactive "r")
  (base64-decode-region beg end t))


(defun gk-new-journal-entry ()
  (interactive)
  (find-file (format-time-string (gk-org-dir-file "Journal/%F")))
  (gk-insert-today 16))

(defun gk-cancel-last-timer ()
  "Cancel the most recently created timer."
  (interactive)
  (cancel-timer (car timer-list)))

(defun gk-jump-to-window ()
  "Jump to a window, completing from window’s buffer name."
  (interactive)
  (let ((winbufs
         (mapcar
          ($ (cons (buffer-name (window-buffer $1)) $1))
          (window-list))))
    (select-window
     (assoca
      (completing-read "Window with buffer: " winbufs)
      winbufs))))

(defun gk-append-to-list (list-name elt)
  "Append ELT to list names LIST-NAME."
  (set list-name (apply #'append (symbol-value list-name) (list elt))))


(defun concise (search-term)
  "Search in The Concise Oxford Dictionary of Linguistics (Matthews, 2003).

The file (a plain text extract of the ebook or PDF) is assumed to
be bookmarked as \"concise\".  SEARCH-TERM is used to run an
‘occur’ search in the file.

Matthews, P. H. (2003). The Concise Oxford Dictionary of
Linguistics (2nd ed.). Oxford University Press."
  (interactive
   (list
    (string-trim
     (read-string
      "Search term (in the Concise Dictionary (Matthews, 2003): "))))
  (when (string-empty-p search-term)
    (user-error "Empty search"))
  (with-current-buffer
      (find-file-noselect (bookmark-get-filename "concise"))
    (occur search-term)))


(defun gk-maximize (&optional buffer)
  "Make maximized frame with a sole window displaying BUFFER."
  (interactive
   (list
    (read-buffer "Select buffer to maximise in new frame: "
                 (current-buffer))))
  (let* ((buf (or buffer (current-buffer)))
         (frame (make-frame))
         (win (car (window-list frame))))
    (set-window-buffer win buf)
    (toggle-frame-maximized frame)
    (raise-frame frame)))


(defun gk-flip--1 (fn buf)
  "Subroutine of ‘gk-flip’."
  (delete-other-windows)
  (funcall fn)
  (other-window 1)
  (switch-to-buffer buf)
  (other-window 1))

(defun gk-flip--2 (&rest dirs)
  "Subroutine of ‘gk-flip’."
  (when-let* ((w (cl-remove-if #'null (mapcar #'window-in-direction dirs))))
    (apply #'window-buffer w)))

(defun gk-flip ()
  "Flip horizontal and vertical split when there are two windows."
  (interactive)
  (unless (= 2 (length (window-list)))
    (user-error "Can’t flip unless there are exactly two windows"))
  ;; Attempt flipping horizontal to vertical.
  (if-let* ((other-buffer (gk-flip--2 'left 'right)))
      (gk-flip--1 #'split-window-vertically
                  other-buffer)
    ;; If not possible, attempt vertical to horizontal.
    (let* ((other-buffer (gk-flip--2 'above 'below)))
      (gk-flip--1 #'split-window-horizontally
                  other-buffer))))


(defun gk-news ()
  "Open ‘rmail’ and ‘elfeed’, update both."
  (interactive)
  (delete-other-windows)
  (rmail)
  (split-window-sensibly)
  (other-window 1)
  (elfeed)
  (elfeed-search-fetch nil)
  (gk-fetch-mail))




;;;; Generic advices:

(defun gk-ad-stay-here (fun &rest args)
  "Stay in the current buffer when running FUN.

Pass ARGS to FUN."
  (save-window-excursion
    (apply fun args)))



;;;; Recompilation:

;; This bit of code helps with recompilation.  Various files external to
;; the configuration tree are loaded during the initialisation process.
;; Here we define a function called =gk-load= which makes note of each
;; file it loads in the variable =gk-loaded-files=, which is then used by
;; =gk-recompile= to determine which files need to be compiled to boost
;; the load speed next time.  This way, =gk-recompile= does not need a
;; manually curated list of files to be compiled, like it did up until
;; now.

(defvar gk-loaded-files nil)

(defun gk-load (&rest args)
  "Identical to ‘load’, but makes note of files.

This function passes its arguments untouched to ‘load’, but
conses the car of ARGS to ‘gk-loaded-files’.  The contents of
that variable is then to be used to byte compile all the files
explicitly loaded in this config without manually listing their
names."
  (when (apply #'load args)
    (pushnew (expand-file-name (car args)) gk-loaded-files)))

(defun gk-recompile (&optional force)
  "Recompile my configuration.

If FORCE is non-nil, force compilation, i.e. compile even if
up-to-date."
  (interactive "p")
  (mapcar ($ (byte-recompile-file $1 (> force 1) 0))
          (remove-if-not #'file-exists-p (cons custom-file gk-loaded-files)))
  (byte-recompile-directory (locate-user-emacs-file "lisp/site") 0 (> force 4)))



;;;; Footnotes:

;; Interact with plain-text footnotes.  These are bound to keys and
;; mouse clicks later on in this file.

(defun gk-find-text-footnote-definition ()
  (interactive)
  (when (looking-at "[[(]?\\([0-9*]+\\)[\])]?")
    (push-mark (point))
    (goto-char (point-max))
    (re-search-backward (concat "^" (match-string 1) "[^1234567890]"))))

(defun gk-find-text-footnote-definition--mouse (&optional event)
  "Find footnote definition according to plain text conventions."
  (interactive "@e")
  (when event (goto-char (cadadr event)))
  (gk-find-text-footnote-definition))



;;;; Scripts:

;; These are functions to help with Unixy tasks, which act like shell
;; scripts.

(defun gk-serve-directory (&optional dir port)
  (interactive (list (read-directory-name "Directory to serve: "
                                          default-directory)
                     (read-number "Port: " 8000)))
  (let ((default-directory dir))
    (async-shell-command (format "python2 -m SimpleHTTPServer %d"
                                 port))))

(defun gk-sudo (cmd)
  "Run CMD as superuser."
  (interactive (list (read-shell-command "Shell command (sudo): ")))
  (with-temp-buffer
    (cd (concat "/sudo::" (expand-file-name default-directory)))
    (prog1
        (shell-command cmd (current-buffer))
      (cd default-directory)
      (when (called-interactively-p 'any)
        ;; The command output can include ‘%’ which may cause message
        ;; to signal error.
        (message "%s" (buffer-string))))))

;; Adapted from https://crowding.github.io/blog/2014/08/16/replace-less-with-emacs/
(defun gk-less--proc-sentinel (proc string)
  (ignore proc string))

(defun gk-less--postprocess (proc)
  (goto-char (point-min))
  (cond
   ;; Man pages:
   ((save-excursion (search-forward "" nil t))
    (Man-fontify-manpage))
   ;; Diffs:
   ((save-excursion
      (and (looking-at "^diff")
           (re-search-forward "^---" nil t)
           (re-search-forward "^@@" nil t)))
    (diff-mode))
   (:else
    (special-mode))))

(defun gk-less--proc-filter (proc string)
  (let ((buf (process-buffer proc))
        (mark (process-mark proc)))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        ;; make sure point stays at top of window while process output
        ;; accumulates
        (save-excursion
          (goto-char mark)
          (insert string)
          (ansi-color-filter-region mark (point))
          (set-marker mark (point)))
        ;; Post-processing the buffer:
        (unless (process-live-p proc)
          (gk-less--postprocess proc))))))

(defun gk-less (fifo)
  "Companion function for ‘extras/eless.sh’."
  (let ((buf (generate-new-buffer "*pager*")))
    (make-process
     :name "gk-pager" :buffer buf :command `("cat" ,fifo)
     :sentinel #'gk-less--proc-sentinel
     :filter #'gk-less--proc-filter)
    (display-buffer buf)))

(setenv "PAGER" (locate-user-emacs-file "extras/eless.sh"))

(defalias 'dmesg
  (defun gk-dmesg (&optional lines)
    (interactive "P")
    (async-shell-command (format "dmesg | tail -n %d" (or lines 10)))))

(defun gk-screen-brightness (n)
  "Set screen brightness to N tenths of max.

10 >= N >= 1."
  (interactive
   (list (read-number "Brightness interval [1--10]: " 5)))
  (unless (>= 10 n 1)
    (user-error "Brightness interval not in range 10 >= N >= 1"))
  (with-current-buffer
      (find-file-noselect
       "/sudo::/sys/class/backlight/intel_backlight/brightness")
    (erase-buffer)
    (insert
     (number-to-string
      (* n
         (/
          (string-to-number
           (with-temp-buffer
             (insert-file-contents
              "/sys/class/backlight/intel_backlight/max_brightness")
             (buffer-string)))
          10))))
    (save-buffer)))



;;;; Diff regions:

;; Diffing two regions.

;; Adapted from: https://gist.github.com/zdavkeos/1279865.

;; To compare two regions, select the first region and run
;; =gk-diff-region=.  The region is now copied to a seperate diff-ing
;; buffer.  Next, navigate to the next region in question (even in
;; another file).  Mark the region and run =gk-diff-region-now=, the diff
;; of the two regions will be displayed by ediff.

;; You can re-select the first region at any time by re-calling
;; =gk-diff-region=.

(defun gk-diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p)  ; there is a region
    (let ((buf (get-buffer-create "*Diff-region A*")))
      (with-current-buffer buf
        (erase-buffer))
      (append-to-buffer buf (region-beginning) (region-end))))
  (message "Now select other region to compare and run `diff-region-now`"))

(defun gk-diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
    (let ((bufa (get-buffer-create "*Diff-region A*"))
          (bufb (get-buffer-create "*Diff-region B*")))
      (with-current-buffer bufb
        (erase-buffer))
      (append-to-buffer bufb (region-beginning) (region-end))
      (ediff-buffers bufa bufb))))



;;;; Illustrative Hex Colour Codes:

;; This section defines a face that can render hexadecimal colour
;; codes with the colour they denote as their background; and a
;; function to set it up meant for major mode hooks.

;; Adapted from http://www.emacswiki.org/emacs/HexColour.

(defvar gk-hexcolour-keywords
  '(("#[abcdefABCDEF[:digit:]]\\{3,6\\}"
     (0 (let ((colour (match-string-no-properties 0)))
          (if (or (= (length colour) 4)
                  (= (length colour) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)
                           :foreground
                           (if (>= (apply
                                    '+ (x-color-values
                                        (match-string-no-properties 0)))
                                   (* (apply '+ (x-color-values "white")) .6))
                               "black" ; light bg, dark text
                             "white"   ; dark bg, light text
                             )))))
        append))))

(defun gk-hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil gk-hexcolour-keywords t))



;;;; Testing init file:

;; This section provides a command, ‘gk-test-init’, for
;; uninteractively loading emacs configuration in a subprocess and
;; seeing if it does indeed load.

(defconst gk-emacs-executable
  (executable-find "emacs"))

(defvar gk-load-test-file
  (expand-file-name (locate-user-emacs-file "etc/load-test.el")))

(defvar gk-load-test-output-buffer-name
  "*Startup File Test*")

(defvar gk-load-test-process-name
  "*Startup Test Process*")

(defun gk-test-init ()
  (interactive)
  (compile (mapconcat
            #'identity
            (list gk-emacs-executable "-Q" "--batch" "-l" gk-load-test-file)
            " ")))



;;;; Utility macros:

;; Some lisp macros for this file.

(defmacro when-fbound (proc &rest args)
  "Run proc if bound.

\(when-fbound PROC ARGS...)"
  `(when (fboundp (quote ,proc))
     (,proc ,@args)))

(defmacro gk-interactively (&rest body)
  "Wrap the BODY in an interactive lambda form.

Return the lambda.  It has as its sole argument a catch-all ‘_’."
  `(lambda (&rest _)
     ,(if (stringp (car body))
          (pop body)
        "Not documented.")
     (interactive)
     ,@body))


(defmacro gk-with-new-frame (parameters &rest body)
  "Create a new frame and run BODY in it.

PARAMETERS is passed to ‘make-frame’.

The new frame is bound to the lexically scoped variable
‘new-frame’ inside BODY.

The newly created frame is centred and the mouse pointer is put
at the centre of the newly created frame.  This only happens when
‘display-graphic-p’ is truthy."
  (declare (indent defun))
  (let ((frame (gensym)))
    `(let ((,frame (make-frame ,parameters)))
       (raise-frame ,frame)
       (select-frame-set-input-focus ,frame)
       (select-window (frame-first-window ,frame))
       (when (display-graphic-p)
         ;; Center frame
         (set-frame-position
          ,frame
          (/ (- (x-display-pixel-width) (window-pixel-width)) 2)
          ;; XXX(2020-09-15): for some reason this works better than
          ;; dividing by 2 on my Linux Mint 20 with Cinnamon.
          (floor (/ (- (x-display-pixel-height) (window-pixel-height)) 2.5)))
         ;; Move mouse into the new frame
         (set-mouse-absolute-pixel-position
          (/ (x-display-pixel-width) 2)
          (/ (x-display-pixel-height) 2)))
       (let ((new-frame ,frame)) ,@body))))


(defmacro setc (variable value)
  "Exactly like setq, but handles custom."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))



;;;; Association lists:

;; Helper functions for association lists.

(defun dissoc (key list &optional test-fn)
  "Delete pairs whose car is equal to KEY from LIST.

TEST-FN defaults to ‘equal’."
  (dissoc--1 key list (or test-fn #'equal) nil))

(defun dissoc--1 (key list test-fn arg)
  (let ((p (car list))
        (r (cdr list)))
    (if list
        (if (funcall test-fn (car p) key)
            (dissoc--1 key r test-fn arg)
          (dissoc--1 key r test-fn (append arg (list p))))
      arg)))


(defmacro dissoc! (key sym test-fn)
  "Call ‘dissoc’ with args and set SYM to result."
  `(setq ,sym (dissoc ,key ,sym ,test-fn)))


(defun assoca (keyseq list)
  "Arbitrary depth multi-level alist query.

KEYSEQ is the list of keys to look up in the LIST.  The first key
from KEYSEQ is looked up in the LIST, then the next key from
KEYSEQ is looked up in the CDR of the return value of that
operation, and so on until all the KEYSEQ is exhausted.  The
resultant value is returned, or nil, in case one or more keys are
not found in the LIST.

If KEYSEQ is a symbol, then it's treated as if it were a
singleton list."
  (let ((ks (if (listp keyseq) keyseq (list keyseq)))
        (ret list))
    (dolist (k ks ret)
      (setq ret (cdr (assoc k ret))))))



;;;; Global modes:

;; This module provides utilities for global modes, like turning them on
;; and off collectively with a single command, registering and
;; unregistering them, disabling default modes etc.

;; All the modes listed in =gk-global-modes= are toggled on with an
;; =after-init-hook=, so modifications to this variable that happen up
;; until the execution of the named hook will actually determine which
;; modes are turned on.

;; =gk-disabled-modes= is a list of modes to disable.

;; Each of this lists contain symbols, actually =*-mode= functions.  The
;; ones in the former will be called with =+1= as the argument, and ones
;; in the latter with =-1=.

;; Do not use this as a hook, add to =after-init-hook= instead.

(defvar gk-global-modes nil "List of global modes to be enabled.")
(defvar gk-disabled-modes nil "List of disabled global modes.")

(defvar gk-toggle-global-modes nil)
(defun gk-toggle-global-modes ()
  "Enable or disable the modes listed in `gk-global-modes' at once."
  (interactive)
  (setf gk-toggle-global-modes (not gk-toggle-global-modes))
  (let (errors)
    ;; Enable global modes
    (dolist (mode gk-global-modes)
      (condition-case e
          (funcall mode (if gk-toggle-global-modes 1 -1))
        (error (push `(,mode ,e) errors))))
    ;; Disable modes in gk-disabled-modes
    (dolist (mode gk-disabled-modes)
      (condition-case e
          (funcall mode (if gk-toggle-global-modes -1 1))
        (error (push `(,mode ,e) errors))))
    (when errors
      (warn "Following errors occurred when activating global modes:\n%S"
            errors))))

(add-hook 'after-init-hook 'gk-toggle-global-modes)



;;;; Things:

;; In this section are defined a suite of functions to work with
;; ‘things’ in buffers, à la ‘thing-at-point’.

(defmacro gk-make-thing-marker (thing)
  (let ((thingname (symbol-name thing)))
    `(defun ,(intern (concat "gk-mark-" thingname)) ()
       ,(concat "Mark the " thingname " under cursor.")
       (interactive)
       (let ((b (bounds-of-thing-at-point (quote ,thing))))
         (set-mark (point))
         (goto-char (car b))
         (push-mark (cdr b) t t)))))

(defvar gk-things '(list sexp defun filename url email word paragraph
                         sentence whitespace line page symbol)
  "A list of known things")

(dolist (thing gk-things)
  (eval `(gk-make-thing-marker ,thing)))

(defun gk-mark-thing ()
  "Interactively find some THING to mark."
  (interactive)
  (funcall
   (intern
    (concat
     "gk-mark-"
     (completing-read
      "What to mark (hit TAB to complete): "
      (mapcar #'symbol-name gk-things)
      nil t)))))



;;;; Projects:

;; Functionality for opening and working with projects.

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
  (if-let* ((projbuf (get-buffer (assoca 'gk-project (frame-parameters)))))
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
           ((file-exists-p (expand-file-name ".git" path))
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
  (if (and (assoca 'window-side (window-parameters))
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
  (if (assoca 'gk-project-shell (frame-parameters))
      (let* ((fparam (frame-parameters))
             (vcs (assoca 'gk-project-vcs fparam))
             (dir (assoca 'gk-project-dir fparam)))
        (dired dir)
        (split-window-sensibly)
        (other-window 1)
        (funcall vcs dir))
    (other-window 1)
    (gk-flash-current-line)))



;;;; i3wm:

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



;;;; Reading setup:

(defvar gk-reading-modes
  '(doc-view-mode pdf-view-mode eww-mode)
  "Modes more likely to be used for reading documents.

Used by ‘gk-reading-setup’ in order to select a buffer that
contains a document to be read in a smart manner so that there is
no need to switch windows unnecessarily.")


(defvar gk-reading-notes-file
  (expand-file-name "~/Notes/Reading.org")
  "Default reading notes file.

Used in ‘gk-reading-setup’.")


(defun gk-reading-setup ()
  "Put windows into a reading setup.

Try to find a potential window containing a document to be read
\(see ‘gk-reading-modes’), give it a big window, and open
‘gk-reading-notes-file’ in a smaller window below it."
  (interactive)
  ;; Find a suitable window which probably contains the document I
  ;; want to read. If not found, the current window will be used.
  (pcase (cl-remove-if-not
          ($ (memq (buffer-local-value 'major-mode $1)
                   gk-reading-modes))
          (mapcar #'window-buffer (window-list)))
    ((or `(,buffer)
         `(,buffer . ,buffers))
     (select-window
      (display-buffer-reuse-window buffer '((reusable-frames . nil))))))
  (delete-other-windows)
  (display-buffer-below-selected
   (find-file-noselect gk-reading-notes-file)
   `((window-height . ,(/ (window-height) 3))))
  ;; XXX(2020-04-02): if point remains in the ‘pdf-view-mode’ window,
  ;; ‘pdf-view-mode’ behaves funny. It can be remedied via calling
  ;;
  ;;   (other-window 1)
  ;;   (redisplay t)
  ;;   (other-window 1)
  ;;
  ;; but having focus on ‘gk-reading-notes-file’ is both simpler and
  ;; kinda more logical (if reading a new document I’d probably set up
  ;; the notes buffer first.
  (other-window 1))



;;;; Screenshots:

;; Adapted from: https://www.reddit.com/r/emacs/comments/idz35e/g2c2c6y/
(defvar gk-save-screenshot-dir
  (expand-file-name
   "emacs-screenshots"
   (if (fboundp 'xdg-user-dir)
       (xdg-user-dir "PICTURES")
     (expand-file-name "~/Pictures")))
  "Where to save screenshots.")


(defvar gk-save-screenshot-default-output-file-name-template
  "%F%T%z.png"
  "Default basename template for ‘gk-save-screenshot’.

This string is passed to ‘format-time-string’ and then
concatenated to ‘gk-save-screenshot-dir’ using
‘expand-file-name’.")


(defun gk-save-screenshot (output-file)
  "Save a screenshot of the selected frame as an SVG image.

Save the output to OUTPUT-FILE. When called interactively, this
is read from the minibuffer, and a default value using the date
and time is provided.

Output file type is inferred from OUTPUT-FILE’s extension, which
must be one of ‘svg’, ‘pdf’, ‘ps’, ‘png’.

The default file path is constructed using
‘gk-save-screenshot-dir’ to determine the directory to save and
‘gk-save-screenshot-default-output-file-name-template’ to
generate a default file name."
  (interactive
   (list (read-file-name
          "Screenshot file name: "
          gk-save-screenshot-dir
          nil nil
          (format-time-string
           gk-save-screenshot-default-output-file-name-template))))
  ;; deps check
  (unless (fboundp 'x-export-frames)
    (user-error
     "This function depends on `x-export-frames’ which not available"))
  ;; ensure output directory
  (unless (file-directory-p gk-save-screenshot-dir)
    (make-directory gk-save-screenshot-dir t))
  ;; guess output file type
  (let ((type (downcase (file-name-extension output-file)))
        data)
    (setq type
          (cond ((string= type "svg") 'svg)
                ((string= type "pdf") 'pdf)
                ((string= type "ps")  'postscript)
                ((string= type "png") 'png)
                (t
                 (user-error
                  "Output file’s extension should be one of svg, pdf, ps or png"
                  type))))
    ;; write data
    (setq data (x-export-frames nil type))
    (with-temp-file output-file
      (insert data))
    (kill-new output-file)
    (message output-file)))



;;;; Window layouts:

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


(defun gk-layouts-main-and-sidekicks ()
  "One horizontal split, the right window split in two.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list)))))
      (delete-other-windows)
      (split-window-horizontally)
      (other-window 1)
      (split-window-vertically)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))



;;; The GK minor mode:

;; The GK minor mode is at the heart of this configuration.  Almost
;; all keybindings, except unmapping some keys from the global map,
;; and except bindings in specific modes, should be done with this
;; minor modes keymap.  This minor mode is active everywhere, except
;; the Minibuffer and the Fundamental mode buffers.

(defgroup GK nil
  "Group for my configuration."
  :group 'emacs
  :prefix "gk-")

(defvar gk-minor-mode-map
  (make-sparse-keymap)
  "Where to put all my bindings.")

(defvar gk-minor-mode-prefix-map
  (make-sparse-keymap)
  "Prefix map for my bindings.")

(fset 'gk-minor-mode-prefix-map gk-minor-mode-prefix-map)

(defvar gk-minor-mode-prefix "\C-c"
  "Keymap prefix for `gk-minor-mode'.")

(define-minor-mode gk-minor-mode
  "Global minor mode for customisations.

\\{gk-minor-mode-map}"
  nil "" gk-minor-mode-map
  (let ((map gk-minor-mode-map))
    (define-key map gk-minor-mode-prefix #'gk-minor-mode-prefix-map)))

(define-globalized-minor-mode global-gk-minor-mode gk-minor-mode
  gk-minor-mode)



;;; Customisations:



;;;; Fonts:

;; Default fonts to use in this config.

(defconst gk-default-fonts-plist
  (list :serif "DejaVu Serif Condensed"
        :sans "DejaVu Sans Condensed"
        :mono "DejaVu Sans Mono"
        :forecast-moon-phase (or (and (font-info "Quivira") "Quivira")
                                 "DejaVu Sans"))
  "A plist, default fonts.")

;; Set up so that there's 80-85 chars width for half-sized horizontal
;; windows.
(defconst gk-font-default-height 100)
(defconst gk-font-variable-pitch-height 110)

(defun gk-font (type)
  "Get default font for TYPE, a keyword.

nil if absent."
  (plist-get gk-default-fonts-plist type))



;;;; Outline:

;; Utility function for setting up outline minor mode.

(defun gk-turn-on-outline-minor-mode (headline-begin headline-end prefix)
  "Turn on the `outline-minor-mode'.

Set locally the variable `outline-regexp' to HEADLINE-BEGIN.
Set locally the variable `outline-heading-end-regexp' to HEADLINE-END.
Set locally the variable `outline-minor-mode-prefix' to PREFIX."
  (setq-local outline-regexp headline-begin)
  (setq-local outline-heading-end-regexp headline-end)
  (setq-local outline-minor-mode-prefix (kbd prefix))
  (outline-minor-mode +1)
  (local-set-key outline-minor-mode-prefix outline-mode-prefix-map))

;; Mainly for ‘C-c C-u’ in Org mode.
(define-advice outline-up-heading
    (:around (fn &rest args) previous-heading-on-toplevel)
  "Move to previous heading if at toplevel."
  (condition-case e
      (call-interactively fn)
    ('error (org-previous-visible-heading (car args)))))



;;;; Backups:

;; This section sets up file backups created when editing.  Backups
;; are put in a designated directory, and are made generously.  Better
;; safe than sorry.

(setf
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
   ("." . ,(expand-file-name "~/.backups"))))



;;;; Comint:

;; Settings for interpreter buffers.



;;;;; Common:

;; Settings and keybindings common to all comint buffers.

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



;;;;; Shell mode:

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
  (shell-dirtrack-mode -1)
  (gk-procfs-dirtrack-mode +1))

(add-hook 'shell-mode-hook 'gk-shell-mode-hook)




;;;;; Eshell:

(setf
 eshell-ls-initial-args
 (list "--group-directories-first" "-Fh"))

(dolist (key '(up down left right))
  (define-key eshell-hist-mode-map `[,key] nil))
(define-key eshell-hist-mode-map (kbd "M-p") #'eshell-previous-matching-input-from-input)
(define-key eshell-hist-mode-map (kbd "M-n") #'eshell-next-matching-input-from-input)



;;;; Dired:

(setf
 ;; Show ls switches in modeline
 dired-switches-in-mode-line 'as-is)



;;;;; The hook:

(defun gk-dired-hook ()
  "Main hook for `dired-mode'."
  ;; C-x M-o -> toggle omitting
  ;; * O -> mark omitted
  (dired-omit-mode 1)
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook #'gk-dired-hook)



;;;;; Utilities:

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
                ('0 " ")
                ('-1 ", ")
                ('64 ":")))))
    (with-temp-buffer
      (insert str)
      (clipboard-kill-ring-save (point-min) (point-max)))
    (message str)))


(defun gk-dired-update-default-directory-from-current-line (ret)
  "Set ‘default-directory’ to dirname of entity under point.

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




;;;;; Customisations:

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

(setf ls-lisp-dirs-first t)

(setf
 ;; Ask for confirmation
 wdired-confirm-overwrite t
 ;; Human readable size.
 dired-listing-switches "-alh")



;;;;; Keymappings:

(define-key dired-mode-map (kbd "W") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "w") 'gk-dired-copy-marked-file-paths-as-kill)
(define-key dired-mode-map (kbd "f") #'gk-dired-find-file-other-frame)



;;;;; Subtrees:

(define-key dired-mode-map "i" 'dired-subtree-toggle)

(setf dired-subtree-use-backgrounds nil)

(defun gk-dired-subtree-hook ()
  ;; Reset omissions when necessary.  Subtrees do not initially apply
  ;; omissions.
  (when dired-omit-mode
    (dired-omit-mode +1)))

(add-hook 'dired-subtree-after-insert-hook 'gk-dired-subtree-hook t)



;;;; Documents:

(defun gk-doc-view-open-externally ()
  "Open current document externally."
  (interactive)
  (browse-url-xdg-open (concat "file://" (buffer-file-name))))

(define-key doc-view-mode-map [?&] #'gk-doc-view-open-externally)



;;;;; PDF-tools:

;; TODO(2018-05-25): implement a smarter resizing addon where the
;; resize factor can vary

;; PDF tools is a sophisticated alternative to DocView for PDF files.

(setf
 pdf-info-epdfinfo-program
 (gk-executable-ensure
  (locate-user-emacs-file "lisp/site/pdf-tools/server/epdfinfo"))
 pdf-tools-enabled-modes
 '(pdf-isearch-minor-mode
   pdf-links-minor-mode
   pdf-misc-minor-mode
   pdf-outline-minor-mode
   pdf-misc-size-indication-minor-mode
   pdf-misc-menu-bar-minor-mode
   pdf-sync-minor-mode
   pdf-misc-context-menu-minor-mode
   pdf-cache-prefetch-minor-mode
   pdf-annot-minor-mode)
 ;; Manually change the page.
 pdf-view-continuous nil
 ;; Resize more granularly.
 pdf-view-resize-factor 1.2)

(pdf-tools-install-noverify)

(define-key pdf-view-mode-map (kbd "M-w") #'pdf-view-kill-ring-save)
(define-key pdf-view-mode-map "q" #'bury-buffer)



;;;; Input methods:



;;;;; Armenian input method:

;; XXX(2018-05-25): This was taken from somewhere, find the attribution.

(quail-define-package
 "armenian-translit" "Armenian" "Հ" nil
 "A transliteration scheme for Armenian characters.")

(quail-define-rules
 ("a" ?ա) ("b" ?բ) ("g" ?գ) ("d" ?դ) ("e" ?ե) ("ye" ?ե) ("z" ?զ)
 ("ee" ?է) ("e'" ?ը) ("t'" ?թ) ("zh" ?ժ) ("i" ?ի) ("l" ?լ) ("x" ?խ)
 ("c" ?ծ) ("k" ?կ) ("h" ?հ) ("j" ?ձ) ("g." ?ղ) ("ch." ?ճ) ("m" ?մ)
 ("y" ?յ) ("n" ?ն) ("sh" ?շ) ("o" ?ո) ("vo" ?ո) ("ch" ?չ) ("p" ?պ)
 ("jh" ?ջ) ("r." ?ռ) ("s" ?ս) ("v" ?վ) ("t" ?տ) ("r" ?ր) ("c'" ?ց)
 ("w" ?ւ) ("p'" ?փ) ("k'" ?ք) ("o'" ?օ) ("f" ?ֆ) ("u" ["ու"]) ("ev" ?և)
 ("?" ?՞) ("." ?։) (".'" ?՝) (";" ?՟) (";'" ?՛) ("!" ?՜)
 ("A" ?Ա) ("B" ?Բ) ("G" ?Գ) ("D" ?Դ) ("E" ?Ե) ("YE" ?Ե)
 ("Ye" ?Ե) ("Z" ?Զ) ("EE" ?Է) ("E'" ?Ը) ("T'" ?Թ) ("ZH" ?Ժ) ("I" ?Ի)
 ("L" ?Լ) ("X" ?Խ) ("C" ?Ծ) ("K" ?Կ) ("H" ?Հ) ("J" ?Ձ) ("G." ?Ղ)
 ("CH." ?Ճ) ("M" ?Մ) ("Y" ?Յ) ("N" ?Ն) ("SH" ?Շ) ("O" ?Ո) ("VO" ?Ո)
 ("Vo" ?Ո) ("CH" ?Չ) ("P" ?Պ) ("JH" ?Ջ) ("R." ?Ռ) ("S" ?Ս) ("V." ?Վ)
 ("T" ?Տ) ("R" ?Ր) ("C'" ?Ց) ("W" ?Ւ) ("P'" ?Փ) ("K'" ?Ք) ("O" ?Օ)
 ("F" ?Ֆ) ("U" ["Սւ"]))



;;;;; Syriac:

;; XXX(2018-05-25): This was taken from somewhere, find the attribution.

(quail-define-package
 "syriac-translit" "Syriac transliteration" "ܣܪ" nil
 "A transliteration scheme for Syriac characters.")
(quail-define-rules
 ;; Letters.
 ("a"	?ܐ)	("b"	?ܒ)	("g"	?ܓ)	("d"	?ܕ)
 ("h"	?ܗ)	("w"	?ܘ)	("z"	?ܙ)	("k"	?ܚ)
 ("t"	?ܛ)	("i"	?ܝ)	("c"	?ܟ)	("l"	?ܠ)
 ("m"	?ܡ)	("n"	?ܢ)	("s"	?ܣ)	("'"	?ܥ)
 ("p"	?ܦ)	("S"	?ܨ)	("q"	?ܩ)	("r"	?ܪ)
 ("sh"	?ܫ)	("T"	?ܬ)
 ;; Punctuation.
 ("."	?܁)	(":"	?܃))



;;;; Text editing:



;;;;; Utilites:

(defun gk-join-nl ()
  "Join the line under point with the next line."
  (interactive)
  (forward-line 1)
  (join-line))

(defun gk-eat-spaces-to-the-right ()
  (interactive)
  (save-excursion
    (when (re-search-forward "[ \t]+" nil t)
      (replace-match "" nil nil))))

(defun gk-reverse-rectangle (&optional start end)
  "Reverse each line of a rectangle.

START and END are corners of the rectangle, ignored if the region
is active."
  (interactive)
  (if (region-active-p)
      (setq start (region-beginning)
            end   (region-end))
    (error "Region not active and no corners specified."))
  (let ((rect (delete-extract-rectangle start end)))
    (save-excursion
      (goto-char start)
      (insert-rectangle (mapcar #'reverse rect)))))

(defun gk-lowercase-this-or-previous-word (arg)
  (interactive "P")
  (save-excursion
    (cond (arg
           (progn (backward-sexp)
                  (mark-sexp)
                  (downcase-region (region-beginning) (region-end))))
          (:else
           (progn
             (backward-word)
             (downcase-word 1))))))

(defun gk-upcase-this-or-previous-word (arg)
  (interactive "P")
  (save-excursion
    (cond (arg
           (progn (backward-sexp)
                  (mark-sexp)
                  (upcase-region (region-beginning) (region-end))))
          (:else
           (progn
             (unless (looking-at "\\<")
              (backward-word))
             (upcase-word 1))))))

(defun gk-end-sentence (punctuation)
  "Finish a sentence with a punctuation and two spaces.

PUNCTUATION is the punctuation to use"
  (interactive (list (read-char "Punctuation: ")))
  (insert (format "%c  " punctuation)))


(defun asterism ()
  "Insert an asterism, centered in the line."
  (interactive)
  (insert "⁂")
  (center-line))

(defun gk-fill-or-join-paragraph (&optional arg)
  "DWIM fill or join a paragraph.

With no prefix args, toggle the physical wrapping of the
paragraph's lines, i.e. if it seems to be filled, join all of
it's lines, else, fill the paragraph.  If any of the lines in a
paragraph is longer than ‘fill-column’, it's filled.

With one prefix arg, behave just as ‘fill-paragraph’, i.e. just
fill, do not toggle.  With two prefix arguments, justify the
paragraph if filling (and not if joining).  With three prefix
arguments, remove justification and fill.

If ‘sentence-end-double-space’ is non-nil, and if a line ends
with a period, insert two spaces afterwards instead of one."
  (interactive "p")
  (save-mark-and-excursion
    (let ((fill-paragraph-function
           (cl-case major-mode
             ('org-mode #'org-fill-paragraph)
             (otherwise #'fill-paragraph)))
          (para-beg
           (save-excursion
             (save-match-data
               (if (looking-at "^$") (point)
                 (progn (backward-paragraph) (1+ (point)))))))
          (para-end
           (save-excursion
             (progn (forward-paragraph) (1- (point)))))
          justify)
      (case arg
        (4
         (funcall fill-paragraph-function))
        (64
         (setq justify nil)
         (put-text-property para-beg para-end 'justification justify)
         (funcall fill-paragraph-function))
        (16 (funcall fill-paragraph-function 'both))
        (1
         (let* ((subs (buffer-substring para-beg para-end))
                (lines (split-string subs "\n" t "\\s+"))
                ;; We should wrap if any of the lines are longer than
                ;; ‘fill-column’.
                (should-wrap
                 (cl-reduce (lambda (x y) (or x y))
                            (mapcar (lambda (l)
                                      (> (length l) fill-column))
                                    lines))))
           (goto-char para-beg)
           (setq justify
                 ;; If we should wrap, learn justification from text properties.
                 (if should-wrap
                     (get-text-property (point) 'justification)
                   ;; Else, see if the right edge is justified.
                   (let ((right-edge-justified
                          (cl-reduce (lambda (&optional x y) (and x y (= x y fill-column)))
                                     (mapcar #'length (butlast lines)))))
                     (cond
                      ;; If the left edge is not justified, then the text
                      ;; is right aligned.
                      ((and right-edge-justified
                            (save-excursion
                              (goto-char para-beg)
                              (re-search-forward "^[ \\t]" para-end t)))
                       'right)
                      ;; Otherwise, it is justified both edges.
                      (right-edge-justified 'both)
                      ;; If neither, then the paragraph is not
                      ;; justified.  We can just allow ‘cond’ to
                      ;; return nil here.
                      ))))
           (if should-wrap
               (progn
                 (put-text-property para-beg para-end 'justification justify)
                 (fill-region para-beg para-end justify))
             (progn
               (goto-char para-beg)
               ;; Remove possible justification artifacts.
               (fill-region para-beg para-end nil)
               ;; 1- because we don't want to include the final \n of
               ;; the paragraph.  Will include the next paragraph too
               ;; then.
               (while (re-search-forward "\n" (1- para-end) t)
                 (goto-char (match-beginning 0))
                 (when (looking-at "\n\\([ \t]+\\)?"))
                 (replace-match
                  (if (and sentence-end-double-space
                           (looking-at "\\.")) "  "
                    " ")))
               (put-text-property
                para-beg (line-end-position) 'justification justify)))))))))

(defun gk-count-words (&rest args)
  "Call the correct word count function for context.

Pass ARGS to it, the first two will be set so that the function
will receive the region if active, or the entire buffer."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (eval
   `(funcall-interactively
     (if (eq major-mode 'org-mode)
         #'gk-org-word-count
       #'count-words-region)
     ,@args)))

(defalias 'wc #'gk-count-words)
(defalias 'cw #'gk-count-words)



;;;;; Common:

;; Settings common to all major/minor modes that edit text.

(diminish 'visual-line-mode "¬")
;; i.e. ‘auto-fill-mode’, but diminish does not like that.
(diminish 'auto-fill-function "=")

(defun gk-text-editing-modes-hook ()
  "Hook for `text-mode'."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (set-input-method default-input-method)
  (setq indent-tabs-mode nil)
  (highlight-indent-guides-mode)
  (git-gutter-mode +1))

(add-hook 'text-mode-hook 'gk-text-editing-modes-hook)

(add-to-list 'gk-global-modes 'electric-quote-mode)



;;;;; Automated editing:

;; This section contains various hooks that automate some editing
;; tasks.

;; XXX(2018-05-25): Maybe disable copyright-update on some paths, or
;; only enable selectively?  It can be annoying sometimes, especially
;; when working on/debugging generated files or files from external
;; projects.

(add-hook 'before-save-hook #'copyright-update)



;;;;; Configuration:

(setf
 ;; Sentence.  Other sentence.
 sentence-end-double-space t
 ;; Words:  other words.
 colon-double-space t
 ;; Guillemets
 electric-quote-chars '(?‘ ?’ ?« ?»))

;; Justify.
(setq-default default-justification 'left)



;;;;; Quail:

;; Quail is the Emacs core library for input methods.

(add-to-list
 'quail-keyboard-layout-alist
 `("dvorak"
   .
   ,(concat
     "                              "
     "  1!2@3#4$5%6^7&8*9(0)[{]}`~  "
     "  '\",<.>pPyYfFgGcCrRlL/?=+    "
     "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
     "  ;:qQjJkKxXbBmMwWvVzZ        "
     "                              ")))

(add-to-list
 'quail-keyboard-layout-alist
 `("brit-q"
   .
   ,(concat
     "                              "
     " 1!2\"3£4$5%6^7&8*9(0)[{]}    "
     "  ’@,<.>pPyYfFgGcCrRlL/?=+   "
     "   aAoOeEuUiIdDhHtTnNsS-_#~   "
     "  \\|;:qQjJkKxXbBmMwWvVzZ      "
     "                                ")))

(quail-set-keyboard-layout "brit-q")

;; TAB won’t show completion, C-i will.
(define-key quail-translation-keymap [tab] nil)

(defvar gk-input-methods '("unilat-gk" "ipa-x-sampa"))

(defun gk-cycle-input-methods (arg)
  "Activate the next input method from `gk-input-methods'.

If ARG is non-nil or called with a universal argument, cycle
backwards."
  (interactive "^p")
  (cond ((and (eq arg 1) (not current-input-method))
         (set-input-method (car gk-input-methods)))
        ((and (eq arg 1) current-input-method)
         (set-input-method
          (or (cadr (member current-input-method gk-input-methods))
              (car gk-input-methods))))
        ((and (eq arg 4) (not current-input-method))
         (set-input-method (car (last gk-input-methods))))
        ((and (eq arg 4) (equal current-input-method (car gk-input-methods)))
         (set-input-method (car (last gk-input-methods))))
        ((and (eq arg 4) current-input-method)
         (set-input-method
          (nth (or (1- (position current-input-method
                                    gk-input-methods
                                    :test #'equal))
                   (1- (length gk-input-methods)))
               gk-input-methods)))))



;;;;; Language environments:

;; Use `unilat-gk' whenever possible.
(dolist (lang gk-unilat-languages)
  (let* ((env (assoc lang language-info-alist))
         (im (assoc 'input-method env)))
    ;; Some language environments may  not have an input-method field,
    ;; namely English.
    (when im
      (setcdr im "unilat-gk"))))



;;;;; HTML:

(defun gk-html-mode-hook ()
  "Hook for `html-mode'."
  (setf indent-tabs-mode nil))

(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'gk-html-mode-hook)
(setq zencoding-preview-default nil)
(define-key zencoding-mode-keymap "\C-j" nil)



;;;;; Textile:

(add-to-list 'auto-mode-alist '("\\.textile" . textile-mode))

;; Fix the terrible faces of Textile using Emacs’ and Org mode’s
;; faces.
(set-face-attribute 'textile-link-face nil :foreground nil :inherit 'link)
(set-face-attribute 'textile-class-face nil :foreground nil :inherit 'font-lock-builtin-face)
(set-face-attribute 'textile-acronym-face nil :foreground nil :inherit 'font-lock-builtin-face)
(set-face-attribute 'textile-table-face nil :foreground nil :inherit 'font-lock-builtin-face)
(set-face-attribute 'textile-lang-face nil :foreground nil :inherit 'font-lock-keyword-face)
(set-face-attribute 'textile-ol-bullet-face nil :foreground nil :inherit 'org-level-1)
(set-face-attribute 'textile-ul-bullet-face nil :foreground nil :inherit 'org-level-1)
(dotimes (i 5)
  (set-face-attribute
   (intern (format "textile-h%d-face" (1+ i)))
   nil :foreground nil :inherit
   (intern (format "org-level-%d" (1+ i)))))




;;;;; Markdown:

;; Org-mode like movement.
(define-key markdown-mode-map (kbd "M-n")
  (gk-interactively (markdown-next-heading)))
(define-key markdown-mode-map (kbd "M-p")
  (gk-interactively (markdown-previous-heading)))



;;;;; Editing macros:

(setf
 ;; Don't show annoying register previews that mess up my macro
 ;; recordings.  Hit C-h to bring it up if necessary.
 register-preview-delay nil)



;;;;; Roff, Troff, Nroff:

(defvar gk-nroff-compile-command-template
  "< %s tbl | troff -Tps -me | dpost | ps2pdf - %s.pdf"
  "Template for use in producing the ‘compile-command’ for

‘nroff-mode’ buffers.  The first ‘%s’ will be replaced with the
name of the current buffer, the second with same but without the
file extension.")

(defun gk-nroff-mode-hook ()
  (let* ((bufnam (buffer-name))
         (bufnam2 (file-name-sans-extension bufnam)))
    (setq-local
     compile-command
     (format gk-nroff-compile-command-template bufnam bufnam2))))

(add-hook 'nroff-mode-hook 'gk-nroff-mode-hook)



;;;;; Gemini:

(define-key gemini-mode-map (kbd "C-c C-s")
  (lambda (arg)
    (interactive "P")
    (if arg (insert "[**]") (insert "[*]"))))
(define-key gemini-mode-map (kbd "C-c C-d")
  (lambda (arg)
    (interactive "P")
    (if arg (insert "[††]") (insert "[†]"))))
(define-key gemini-mode-map (kbd "C-c C-S-d")
  (lambda (arg)
    (interactive "P")
    (if arg (insert "[‡‡]") (insert "[‡]"))))
(define-key gemini-mode-map (kbd "C-c C-k")
  (gk-interactively (insert "``` \n```")
                    (forward-line -1)
                    (goto-char (line-end-position))))


(define-key gemini-mode-map (kbd "C-c C-l") (gk-interactively (insert "=> ")))




;;;;; Dictionary and spell checking:

;; Partially adapted from:
;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html

(setf ispell-program-name "hunspell"
      ispell-dictionary "en_GB,tr_TR,it_IT"
      ispell-personal-dictionary (dropbox "hunspell-personal-dictionary"))

(defun gk-spellcheck-hook ()
  "Hook to start spell-check in buffers."
  (ispell-set-spellchecker-params)
  ;; This uses ‘cl-pushnew’ so it should be okay to call this multiple
  ;; times.
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (f-touch ispell-personal-dictionary))
  (flyspell-mode +1))

(add-hook 'text-mode-hook 'gk-spellcheck-hook)



;;;;; Multiple cursors:

;; Just fucking run the commands.
(setf mc/always-run-for-all t)



;;;;; Keybindings:

;; These are keybindings specific to =text-mode= and descendants.

(define-key text-mode-map (kbd "C-M-a") #'backward-paragraph)
(define-key text-mode-map (kbd "C-M-e") #'forward-paragraph)



;;;; Global settings:



;;;;; Auto modes:

;; Emacs can display fonts with ‘image-mode’.  It already does TTF
;; fonts, but do OTF also.
(add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))



;;;;; Calendar:

(calendar-set-date-style 'iso)          ;The only unambiguous one.

(let* ((whereami (geoclue-location))
       (lat (assoca 'latitude whereami))
       (long (assoca 'longitude whereami))
       (desc (assoca 'description whereami)))
  (setf calendar-location-name
        (if (string-empty-p desc)
            (or (getenv "TZ") "wHEreVer yOu arEeeEEeEe")
          desc)
        calendar-latitude lat
        calendar-longitude long))



;;;;; Date time:

(setf
 ;; Time zones for ‘world-clock’.
 world-clock-list '(("Europe/Istanbul" "Istanbul")
                    ("Europe/London" "London")
                    ("Europe/Rome" "Rome")
                    ("America/New_York" "US East (NY)")
                    ("America/Los_Angeles" "US Pacific (Seattle)")
                    ("Asia/Hong_Kong" "Hong Kong")))



;;;;; forecast.el:

(setq forecast-language 'en
      forecast-units 'si
      forecast-time-format "%I:%M:%S%p, %F"
      forecast-rain-symbol "☔")



;;;;; Global modes:

;; Default mode is ‘text-mode’.  The actual default,
;; ‘fundamental-mode’ is rather useless.

(setq-default major-mode 'text-mode)

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
        which-key-mode))

;; Mainly to enable GK keybindings there.
(add-hook 'fundamental-mode-hook 'gk-minor-mode)

(mapc ($ (pushnew $1 gk-disabled-modes))
      '(electric-indent-mode
        pixel-scroll-mode))

;; Diminish global modes that are always on.
(diminish 'whole-line-or-region-mode)
(diminish 'buffer-face-mode "☺")
(diminish 'which-key-mode "⁈")



;;;;; Secrets:

(gk-load (dropbox "secrets") t)



;;;; Version Control:



;;;;; Common:

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



;;;;; Diff:

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



;;;;; RCS:

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



;;;;; Git:

(define-advice vc-git-push (:around (fn &rest args) always-prompt)
  "Always prompt for editing the push command."
  (funcall fn t))

(defun gk-git-commit-mode-hook ()
  "Set up git commit buffer."
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
              ;; The below is inserted in two steps so that undo
              ;; boundaries can be added and removing the
              ;; ‘current-defun’ string in case it is useless is as easy
              ;; as a single undo.
              (insert filename ": ")
              (undo-boundary)
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

(add-hook 'git-commit-mode-hook #'gk-git-commit-mode-hook)

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



;;;;;; Magit:

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
      (setenv "SSH_ASKPASS" (expand-file-name "sshpass.sh" (getenv "MYLIB")))
      (shell-command "ssh-add" (generate-new-buffer "*ssh-add*")))))

;; Disable with-editor stuff.
(gk-deadvice 'server-switch-buffer)

(add-hook 'magit-mode-hook
          ($ (when (eq gk-gui-theme 'yoshi)
               (hl-line-mode +1))))



;;;;; Mercurial:

(setf
 ;; Show the revision number in the mode line.
 vc-hg-symbolic-revision-styles '("{rev}/{branch}"))

;; Always prompt for editing the push command before pushing.
;; Requires prefix arg ortherwise.
(define-advice vc-hg-push (:around (fn &rest args) always-prompt)
  "Always prompt for editing the push command."
  (funcall fn t))



;;;; Programming:

;; Configurations for programming-related major/minor modes.



;;;;; Common:

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



;;;;; Snippets:

(setf yas-snippet-dirs
      (list
       (locate-user-emacs-file "etc/snippets")))

(pushnew 'yas-global-mode gk-global-modes)

(diminish 'yas-minor-mode)



;;;;; Eglot / LSP:

(setf
 ;; Shut down if no buffers need it.
 eglot-autoshutdown t
 ;; Don’t mess up echo area with full documentation.
 eglot-put-doc-in-help-buffer t)



;;;;; Flymake:

(setf
 ;; Don’t bother me unless I save.
 flymake-no-changes-timeout nil
 flymake-start-on-flymake-mode nil)

;; Colour backgrounds of error regions instead of squiggly lines,
;; because they are annoying.
(dolist (face '(flymake-error flymake-warning flymake-note))
  (let ((c (plist-get (face-attribute face :underline) :color)))
    (set-face-attribute face nil :background (color-lighten-name c 50))
    (set-face-attribute face nil :underline nil)))



;;;;; Lisps:



;;;;;; Common:

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



;;;;;; Emacs Lisp:

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



;;;;;;; Debugger:

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



;;;;;; Common Lisp:

(defvar gk-lisp-program (or (executable-find "ccl")
                            (executable-find "sbcl"))
  "Full path to the default Common Lisp implementation.")

(setf
 ;; Set default Lisp interpreter.
 inferior-lisp-program gk-lisp-program)

;; Hyperspec location
(setf common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "~/co/Lisp/doc/HyperSpec/")))



;;;;;; Scheme:

(setf scheme-program-name "guile")

(add-hook 'scheme-mode-hook 'gk-lisp-mode-hook)
(add-hook 'inferior-scheme-mode-hook 'gk-lisp-mode-hook)



;;;;; C family:

(setf
 ;; Default C style.
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "gnu")))

(add-hook 'c-mode-hook 'gk-algol-like-hook)



;;;;; Javascript:

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



;;;;; CSS:

(defun gk-css-mode-hook ()
  (gk-turn-on-outline-minor-mode "^/\\*\\*\\*+" ": \\*/" "C-'"))

(add-hook 'css-mode-hook #'gk-css-mode-hook)



;;;;; Ruby:

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
  (gk-turn-on-outline-minor-mode "###*" ":$" "C-'"))

(defun gk-inf-ruby-mode-hook ()
  (setf truncate-lines nil word-wrap t))

(add-hook 'ruby-mode-hook 'gk-ruby-mode-hook)
(add-hook 'inf-ruby-mode-hook 'gk-inf-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'gk-algol-like-hook)



;;;;; Perl:

(defalias 'perldoc 'cperl-perldoc)

(add-hook 'perl-mode-hook 'gk-algol-like-hook)

(pushnew
 '((perl-mode . "Perl source file")
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
   "use v5.24;\n\nuse strict;\nuse warnings;\nno warnings 'experimental::smartmatch';
\n\n")
 auto-insert-alist)



;;;;; Python:
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



;;;;; Makefiles:

(defun gk-makefile-hook ()
  "Generic hook for makefile modes."
  (gk-turn-on-outline-minor-mode "####* " ":$" "C-'")
  (setq-local indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'gk-makefile-hook)



;;;;; Shell scripts:

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



;;;;; Lua:

(defun gk-lua-mode-hook ()
  (imenu-add-to-menubar "Definitions"))

(add-hook 'lua-mode-hook #'gk-lua-mode-hook)



;;;;; Dart:

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))



;;;; Compilation:

;; Send desktop notification when a compilation is finished.
;; Adapted from https://www.reddit.com/r/emacs/comments/cw1eky/ey8tisj/
(add-to-list 'compilation-finish-functions
             ($ [_ status]
                (gk-send-desktop-notification "Compilation finished" status)))



;;;; Ibuffer:

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (let ((bs (buffer-size)))
    (cond ((> bs 1e6) (format "%7.1fmB" (/ bs 1e6)))
          ((> bs 1e3) (format "%7.1fkB" (/ bs 1e3)))
          (t          (format "%7d  " bs)))))

(setf ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process))
      ibuffer-show-empty-filter-groups nil)

(define-ibuffer-filter name-not
    "Negated buffer name match."
  (:description "buffer name not"
   :reader (read-from-minibuffer "Exclude buffers (regexp): "))
  (not (string-match qualifier (buffer-name buf))))

(defvar gk-ibuffer-filters
  '(("Emacs"
     (name . "^\\*\\(scratch\\|Messages\\)\\*$"))
    ("VC"
     (name . "^\\*\\(vc\\|log\\)-?"))
    ("Documentation"
     (name . "^\\*\\(Help\\|info\\|Man [0-9]?\\)"))
    ("Special buffers"
     (name-not . "compilation")
     (name . "^\\*.*\\*$"))
    ("EWW Reading"
     (mode . eww-mode)))
  "Fixed filter definitions for ibuffer.")

(defun gk-ibuffer-generate-filter-groups-by-dir ()
  "Create a set of ibuffer filter groups based on the dirs of buffers."
  (let* ((func (lambda (buf)
                 (when-let* ((bufnam (buffer-file-name buf)))
                   (if-let* ((linknam (file-symlink-p bufnam)))
                       (file-name-directory (expand-file-name linknam))
                     (file-name-directory (expand-file-name bufnam))))))
         (dirs (ibuffer-remove-duplicates (delq nil (mapcar func (buffer-list))))))
    (mapcar (lambda (dir) (cons (concat "Directory: " dir) `((dir . ,dir)))) dirs)))

(define-ibuffer-filter dir
    "Toggle current view to buffers with dir QUALIFIER."
  (:description "directory" :reader (read-from-minibuffer "Filter by dir (regexp): "))
  (ibuffer-awhen (buffer-file-name buf)
    (string= qualifier (file-name-directory it))))

(define-advice ibuffer-update (:before (&rest args) autogroups)
  "Group related buffers together using ‘ibuffer-vc’ and ‘dir’,

and special ones sepatarely."
  (ignore args)
  (setf ibuffer-filter-groups
        (append
         gk-ibuffer-filters
         (ibuffer-vc-generate-filter-groups-by-vc-root)
         (gk-ibuffer-generate-filter-groups-by-dir))))

;; Hide these buffers by default.
(defvar gk-ibuffer-collapsed-groups (list "Special buffers" "Emacs" "Documentation"
                                          "VC"))

(define-advice ibuffer (:after (&rest args) gk-hidden-groups)
  "Hide groups in ‘gk-ibuffer-collapsed-groups’."
  (ignore args)
  (save-excursion
    (dolist (group gk-ibuffer-collapsed-groups)
      (ignore-errors
        (ibuffer-jump-to-filter-group group)
        (ibuffer-toggle-filter-group)))))

(defun gk-ibuffer-hook ()
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-filename/process))
  (ibuffer-update nil t))

(add-hook 'ibuffer-hook 'gk-ibuffer-hook)

(define-key ibuffer-mode-map [?q] 'kill-this-buffer)




;;;; BBDB:

(setf
 ;; Don’t display the record after completion.
 bbdb-completion-display-record nil)



;;;; Mail:

;; Settings for reading and writing mail, and specific to mail clients
;; and related software.



;;;;; Paths:

(defvar gk-mail-home (expand-file-name "~/posta")
  "Where all mailboxes etc. are.")

(defvar gk-mail-temporary-file-directory
  (expand-file-name "tmp" gk-mail-home))

(defvar gk-mail-inboxes
  (list (expand-file-name "inbox" gk-mail-home))
  "Where to look for mail.")

;; XXX(2018-06-06): Maybe manually add it if not defined?  Or does
;; that belong to the .profile script.
(when-let* ((spool (getenv "MAIL")))
  (pushnew spool gk-mail-inboxes))



;;;;; Authentication:

(setf netrc-file (dropbox "authinfo.gpg")
      auth-sources (list netrc-file))



;;;;; User agent:

(setf message-mail-user-agent t
      read-mail-command 'rmail)



;;;;; Posting styles:

(setf
 ;; Gmail does not like parens.
 message-from-style 'angles)



;;;;; Utilities:

(defun gk-fetch-mail (&optional callback)
  "Run mail retrieval scripts.

If CALLBACK is non-nil, it’s called with a single argument, which
is non nil if there’s new mail."
  (interactive)
  (unless (string= "mergen" (system-name))
    (user-error "Won’t fetch mail on this device"))
  (make-process
   :name "gk-fetch-mail" :buffer (get-buffer-create "*Fetch Mail*")
   :command (list "mpop" "-Q" "-a")
   :sentinel
   (lambda (process event)
     (let ((msg ""))
       (unless (process-live-p process)
         (when (zerop (process-exit-status process))
           (dolist (f gk-mail-inboxes)
             (when-let* ((f (file-attribute-size (file-attributes f))))
               (when (> f 0)
                 (setf msg "You have unread mail! ")
                 (mairix-update-database))))
           (when (and (gk-gui-p) (not (string-empty-p msg)))
             (gk-send-desktop-notification "New mail" msg "mail-message-new")))
         (message "%sFetch mail process %s" msg (string-trim event))
         (when (functionp callback)
           (funcall callback (string-empty-p msg))))))))



;;;;; Movemail program:

;; Ensure that a safe movemail is used.  I configure Emacs to use system
;; movemail at build time, but if somehow it doesn't, try to ensure it
;; does here.

(unless (string-match "with-mailutils" system-configuration-options)
 (setf mail-source-movemail-program (gk-executable-ensure "movemail")))



;;;;; Sending mail:

(setf
 message-send-mail-function 'message-send-mail-with-sendmail
 message-sendmail-f-is-evil t
 message-sendmail-envelope-from 'header
 sendmail-program (gk-executable-ensure "msmtp"))


;; Spammers are everywhere.
(setf user-mail-address (concat "self" "@" "gkayaalp" "." "com")
      user-full-name "Göktuğ Kayaalp")


(defun gk-mail-set-msmtp-account ()
  "Find account name for email address in From: line."
  (let ((from (save-excursion
                (goto-char (point-min))
                (or (re-search-forward "^From: .*? <" nil t)
                    (user-error "No From: line or an empty one"))
                (buffer-substring (point) (1- (line-end-position))))))
    (with-current-buffer (find-file-noselect "~/.msmtprc")
      (goto-char (point-min))
      (or (re-search-forward (concat "^from " from) nil t)
          (user-error "No msmtp account for ‘%s’" from))
      (re-search-backward "^account ")
      (end-of-line)
      (setf
       message-sendmail-extra-arguments
       (list "-a" (substring-no-properties (thing-at-point 'symbol)))))))

(add-hook 'message-send-mail-hook #'gk-mail-set-msmtp-account)

(defun gk-runq ()
  "Run outgoing email queue."
  (interactive)
  (async-shell-command "msmtp-runqueue.sh" "*runq*"))

(defun gk-listq ()
  "Show email queue."
  (interactive)
  (async-shell-command "msmtp-listqueue.sh" "*listq*"))




;;;;; Message mode:

(add-hook 'message-setup-hook 'bbdb-mail-aliases)

(setf
 message-citation-line-function 'message-insert-formatted-citation-line
 message-citation-line-format "On %Y-%m-%d %R %Z, %f wrote:")

(setf
 message-default-headers (format "Fcc: %s/outbox" gk-mail-home)

 ;; Drafts directory.
 message-auto-save-directory (expand-file-name "drafts" gk-mail-home)
 ;; Ask for confirmation before sending a message.
 message-confirm-send t)

(add-hook 'message-sent-hook #'bury-buffer)

(defun gk-message-mode-hook ()
  "Set up the message buffer."
  ;; EasyPG assistant's mailing helper.
  (epa-mail-mode 1))

(add-hook 'message-mode-hook 'gk-message-mode-hook)
(define-key message-mode-map (kbd "C-c C-c") 'message-send)



;;;;; Rmail:

(setf
 rmail-primary-inbox-list gk-mail-inboxes
 rmail-secondary-file-directory gk-mail-home
 rmail-secondary-file-regexp "spam\\|outbox\\|archive$"
 rmail-file-name (expand-file-name "current" gk-mail-home)
 rmail-default-file (expand-file-name "archive" gk-mail-home)
 gk-rmail-archive-file (expand-file-name "archive" gk-mail-home)
 rmail-displayed-headers
 (rx (and bol (or "to" "date" "from" "cc" "subject" "message-id" "list-id"
                  "delivered-to")))
 rmail-mime-prefer-html nil)

(defun gk-rmail-view-html-part-in-browser ()
  "View the HTML part of the message in this buffer in the

browser."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (rx bol "["
         (optional digit (zero-or-more (and "/" digit)) ":")
         "text/html "
         (or "Hide" "Show")
         " Save:"))
    (point)
    (forward-char 1)
    (let ((button (button-at (point)))
          (filename
           (concat (make-temp-name
                    (expand-file-name
                     "gkrmailout" gk-mail-temporary-file-directory))
                   ".html")))
      (browse-url-qutebrowser
       (concat "file://" (gk-rmail-mime-save-to-tmp button filename))))))

(defun gk-rmail-mime-save-to-tmp (button output-file-name)
  "Save the attachment in BUTTON in OUTPUT-FILE-NAME.

Return the file name, expanded."
  ;; Adapted from ‘rmail-mime-save’ in order to automatically export
  ;; to HTML and open in external browser.
  (let* ((rmail-mime-mbox-buffer rmail-view-buffer)
	 (data (button-get button 'data)))
    (prog1 (expand-file-name output-file-name)
      (if (and (not (stringp data))
	       (rmail-mime-entity-truncated data))
	  (unless (y-or-n-p "This entity is truncated; save anyway? ")
	    (error "Aborted")))
      (with-temp-buffer
        (set-buffer-file-coding-system 'no-conversion)
        ;; Needed e.g. by jka-compr, so if the attachment is a compressed
        ;; file, the magic signature compares equal with the unibyte
        ;; signature string recorded in jka-compr-compression-info-list.
        (set-buffer-multibyte nil)
        (setq buffer-undo-list t)
        (if (stringp data)
	    (insert data)
	  ;; DATA is a MIME-entity object.
	  (let ((transfer-encoding (rmail-mime-entity-transfer-encoding data))
	        (body (rmail-mime-entity-body data)))
	    (insert-buffer-substring rmail-mime-mbox-buffer
				     (aref body 0) (aref body 1))
	    (cond ((string= transfer-encoding "base64")
		   (ignore-errors (base64-decode-region (point-min) (point-max))))
		  ((string= transfer-encoding "quoted-printable")
		   (quoted-printable-decode-region (point-min) (point-max))))))
        (write-region nil nil output-file-name nil nil nil t)))))

(defun gk-rmail-force-expunge-and-save ()
  "Force save the mail box, even if it seems to not be modified."
  (interactive)
  (set-buffer-modified-p t)
  (rmail-expunge-and-save))

(defun gk-rmail-advance (&optional arg)
  "Advance to the next message in default mbox.

This command will not run unless in an RMAIL buffer visiting
‘rmail-file-name’.  It will output the current message to
‘gk-rmail-archive-file’ and delete it, advancing to the next
message in the RMAIL file.  This is a utility for the email
workflow where a temporary inbox is used for working with current
email and archiving read mail in another file.

If ARG is non-nil, or called interactively with a prefix
argument, prompt for which mailbox to output to."
  (interactive "P")
  (unless (and (eq major-mode 'rmail-mode)
               (string= (buffer-file-name) rmail-file-name))
    (user-error
     "This is not your default RMAIL file, won't run ‘gk-rmail-advance’ here"))
  (let ((outfil (if (null arg)
                    gk-rmail-archive-file
                  (read-file-name
                   "Move to mailbox: " (concat gk-mail-home "/")
                   nil nil nil
                   ;; Exclude numbered split mbox files.
                   ($ (save-match-data (not (string-match "-[0-9]+\\'" $1))))))))
    (rmail-output outfil))
  (rmail-delete-forward))

(defun gk-rmail-forward-link-or-button (p)
  "Navigate both links and buttons in Rmail in a ring.

This replaces the use of ‘forward-button’ which only traverses
buttons and skips over links."
  (interactive (list (point)))
  (let (positions)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (memq (car (overlay-properties overlay))
                  '(goto-address button))
        (pushnew (overlay-start overlay) positions)))
    (setq positions (sort positions #'<))
    (if (>= p (car (last positions)))
        (goto-char (first positions))
      (goto-char (first (cl-remove-if ($ (<= $1 p)) positions))))))

(defun gk-rmail-backward-link-or-button (p)
  "Navigate both links and buttons in Rmail in a ring.

This replaces the use of ‘forward-button’ which only traverses
buttons and skips over links.

This is the reverse counterpart of
‘gk-rmail-forward-link-or-button’."
  (interactive (list (point)))
  (let (positions)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (memq (car (overlay-properties overlay))
                  '(goto-address button))
        (pushnew (overlay-start overlay) positions)))
    (setq positions (sort positions #'<))
    (if (<= p (first positions))
        (goto-char (car (last positions)))
      (goto-char (car (last (cl-remove-if ($ (>= $1 p)) positions)))))))

(defun posta ()
  "Set up and display an Rmail frame."
  (interactive)
  (gk-fetch-mail
   (lambda (_)
     (gk-with-new-frame nil
       (rmail)
       (rmail-summary)
       (window-resize (selected-window) -10)
       (other-window 1)))))

(defun gk-rmail-mode-hook ()
   (goto-address-mode +1)
   (setq-local word-wrap t))

(add-hook 'rmail-mode-hook #'gk-rmail-mode-hook)

(define-key rmail-mode-map (kbd "RET") nil) ;was: rmail-mime-toggle-hidden, brutally useless
(define-key rmail-mode-map "N" #'gk-rmail-advance)
(define-key rmail-mode-map "S" #'gk-rmail-force-expunge-and-save)
(define-key rmail-mode-map "b"
  (gk-interactively (gk-rmail-view-html-part-in-browser)
                    (gk-rmail-advance)))
(define-key rmail-mode-map (kbd "<tab>") #'gk-rmail-forward-link-or-button)
(define-key rmail-mode-map (kbd "<backtab>") #'gk-rmail-backward-link-or-button)

;; Unbind this
(define-key rmail-mode-map (kbd "C-M-f") nil)


;; ‘q’ is normally bound to ‘rmail-summary-quit’, which is simply
;; useless.
(define-key rmail-summary-mode-map "q" #'bury-buffer)

;; Quick search
(define-key rmail-mode-map "/" #'mairix-search)
(define-key rmail-summary-mode-map "/" #'mairix-search)



;;;;; Mairix:

;; XXX(2018-05-25): Use with Gnus?

(setf
 mairix-file-path (expand-file-name "mairix/" gk-mail-home)
 mairix-search-file "search")

(defalias 'search-mail 'mairix-widget-search)
(defalias 'mx 'mairix-widget-search)

(define-advice mairix-widget-search
    (:after (&rest args) enable-widget-minor-mode)
  "Activate ‘widget-minor-mode’ in the ‘mairix-widget-search’ buffer.

Wonder why this is not the default."
  (widget-minor-mode +1))



;;;; Parse-time:

;; Add Turkish month and day names, mainly for ‘org-time-stamp’ and
;; ‘org-time-stamp-inactive’.

(setf
 parse-time-months
 (append
  '(("oca" . 1) ("sub" . 2) ("mar" . 3) ("nis" . 4)  ("may" . 5)  ("haz" . 6)
    ("tem" . 7) ("agu" . 8) ("eyl" . 9) ("eki" . 10) ("kas" . 11) ("ara" . 12)

    ("ocak" . 1)   ("subat" . 2)   ("mart" . 3)  ("nisan" . 4) ("mayis" . 6)
    ("temmuz" . 7) ("agustos" . 8) ("eylul" . 9) ("ekim" . 10) ("kasim" . 11)
    ("aralik" . 12))
  parse-time-months)

 parse-time-weekdays
 (append
  '(("paz" . 0) ("pzt" . 1) ("sal" . 2) ("crs" . 3) ("prs" . 4) ("cum" . 5) ("cts" . 6)

    ("pazar" . 0) ("pazartesi" . 1) ("sali" . 2) ("carsamba" . 3) ("persembe" . 4)
    ("cuma" . 5)  ("cumartesi" . 6))
  parse-time-weekdays))






;;;; Org mode:



;;;;; Emphasis:
;; Slightly customise emphasis markers to work near Unicode
;; delimiters.  Also expand the range these markers work.

(defvar gk-original-org-emphasis-regexp-components
  (cl-copy-tree (default-value 'org-emphasis-regexp-components)))

(defvar gk-org-emphasis-additional-chars "‘’«»“”·﻿" ;includes zero-width-nbsp
  "New characters to add to Org emphasis delimiters")

(setq
 org-emphasis-regexp-components
 (list
  (concat (car gk-original-org-emphasis-regexp-components)
          gk-org-emphasis-additional-chars)
  (concat (cadr gk-original-org-emphasis-regexp-components)
          gk-org-emphasis-additional-chars)
  (caddr gk-original-org-emphasis-regexp-components)
  (cadddr gk-original-org-emphasis-regexp-components)
  20))

(org-reload)



;;;;; Citations:

;; Citation link for org-mode, based on
;; http://bastibe.de/2014-09-23-org-cite.html.

;; For use with =gk-article= Latex class.  See =org-latex-classes=.  Uses
;; biblatex.

;; Used like =[[cite:<citation-name>]]=.  Exports to
;; =\cite{<citation-name>}=.  Add this line to be able to follow these
;; links:

;; :  #+LaTeX_HEADER: \gkbib{<bibfile>}{<style>}{<bibsectname>}

(defun gk-org-cite-follow (name)
  "Open bibliography and jump to appropriate entry.

NAME is the citation name."
  (let (citfile)
    (setf
     citfile
     (save-excursion
       (goto-char (point-min))
       (save-match-data
         (re-search-forward
          "^#\\+LATEX_HEADER: \\\\gkbib{\\([^}]+\\)}" nil t)
         (match-string 1))))
    (unless citfile
      (user-error
       "No citation file specified"))
    (find-file-other-window citfile)
    (goto-char (point-min))
    (search-forward name)))

(defun gk-org-cite-export (path desc format)
  "Export a citation link.

For   LaTeX,    export   a   link   like    [[cite:cohen93]]   as
\cite{cohen93}.   For other  formats,  this function  returns
nil, telling Org to do what it defaults to.

For PATH, DESC and FORMAT see `org-add-link-type'."
  (case format
    ((quote latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s]{%s}" desc path)))))

(org-add-link-type "cite" #'gk-org-cite-follow #'gk-org-cite-export)



;;;;; Utilities:

(defun gk-org-babel-load-this-file ()
  "Load current `org-babel' file."
  (interactive)
  (org-babel-load-file (buffer-file-name)))

(defun gk-org-dir-file (path)
  "Get file PATH in `org-directory'."
  (expand-file-name
   (concat org-directory "/"
           (replace-regexp-in-string "^/+" "" path))))

(defun gk-org-dir-files (&rest paths)
  "Get a list of file PATHs in `org-directory'."
  (mapcar #'gk-org-dir-file paths))

(defun gk-org-dir-find-file ()
  "Find a file from the `org-directory'."
  (interactive)
  (let ((default-directory (concat org-directory "/")))
    (call-interactively #'find-file)))

(defun gk-org-confirm-elisp-link-function (prompt)
  "Skip confirmation PROMPT in `org-directory'."
  (when (buffer-file-name)
   (if (not (equal
             (file-truename
              (file-name-directory
               (expand-file-name "bob" org-directory)))
             (file-truename
              (file-name-directory (expand-file-name (buffer-file-name))))))
       (yes-or-no-p prompt)
     t)))

(setq org-confirm-elisp-link-function
      #'gk-org-confirm-elisp-link-function)

(defun gk-org-decrypt-element ()
  "Decrypt the element under point, show in a new buffer."
  (interactive)
  (save-excursion
    (let ((transient-mark-mode t))
      (org-mark-element)
      (epa-decrypt-region
       (region-beginning) (region-end)
       (lambda ()
         (let ((decrypted-elem (org-get-heading t t))
               (bufnam (buffer-name)))
           (prog1
               (switch-to-buffer (get-buffer-create "*Org Secret*"))
             (read-only-mode -1)
             (fundamental-mode)
             (erase-buffer)
             (insert ">>> " decrypted-elem " (" bufnam ")")
             (newline)
             (insert ">>> Hit `Q' in order to *kill* this buffer.")
             (newline 2)
             (special-mode)
             (local-set-key [?Q] 'kill-this-buffer))))))))

;; Adapted from: https://www.reddit.com/r/emacs/comments/8qm1lb/new_orgcountwords_command/
(defun gk-org-count-words-subtree ()
  "If region is active, count words in it; otherwise count words in subtree."
  (interactive)
  (if (use-region-p)
      (funcall-interactively #'count-words-region
                             (region-beginning) (region-end))
    (org-with-wide-buffer
     (cl-loop for (lines words characters)
              in (org-map-entries
                  (lambda ()
                    (gk-org-forward-to-entry-content 'unsafe)
                    (let ((end (org-entry-end-position)))
                      (list (count-lines (point) end)
                            (count-words (point) end)
                            (- end (point)))))
                  nil 'tree)
              sum lines into total-lines
              sum words into total-words
              sum characters into total-characters
              finally do
              (message
               "Subtree \"%s\" has %s lines, %s words, and %s characters."
               (org-get-heading t t)
               total-lines total-words total-characters)))))

(defun gk-org-forward-to-entry-content (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.

If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (org-back-to-heading))
  (cl-loop for element = (org-element-at-point)
           for pos = (pcase element
                       (`(headline . ,_)
                        (org-element-property :contents-begin element))
                       (`(,(or 'planning 'property-drawer 'drawer) . ,_)
                        (org-element-property :end element)))
           while pos
           do (goto-char pos)))

;; From https://orgmode.org/worg/org-hacks.html
;;
;; «Count words in an Org buffer
;;
;; Paul Sexton posted this function to count words in an Org buffer:»
;;
;; This version includes some fixes, so not only an aesthetic
;; modification.
(defun gk-org-word-count
    (beg end &optional count-latex-macro-args no-count-footnotes)
  "Report the number of words in the Org mode buffer or selected region.

Ignores:
- comments
- tables
- source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
- hyperlinks (but does count words in hyperlink descriptions)
- tags, priorities, and TODO keywords in headers
- sections tagged as 'not for export'.

If the optional argument NO-COUNT-FOOTNOTES is non-nil, footnote
text is ignored.

If the optional argument COUNT-LATEX-MACRO-ARGS is non-nil, the word count
includes LaTeX macro arguments (the material between {curly braces}).
Otherwise, and by default, every LaTeX macro counts as 1 word regardless
of its arguments.

If called interactively (including with a keybinding), report the
word count in the echo area and return nil.  If not, return the
number."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless mark-active
    (setf beg (point-min)
          end (point-max)))
  (let ((wc 0)
        (latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (cond
         ;; Ignore comments.
         ((or (org-at-comment-p) (org-at-table-p))
          nil)
         ;; Ignore hyperlinks. But if link has a description, count
         ;; the words within the description.
         ((looking-at org-bracket-link-analytic-regexp)
          (when (match-string-no-properties 5)
            (let ((desc (match-string-no-properties 5)))
              (save-match-data
                (incf wc (length (remove "" (org-split-string
                                             desc "\\W")))))))
          (goto-char (match-end 0)))
         ((looking-at org-any-link-re)
          (goto-char (match-end 0)))
         ;; Ignore source code blocks.
         ((org-between-regexps-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
          nil)
         ;; Ignore inline source blocks, counting them as 1 word.
         ((save-excursion
            (ignore-errors (backward-char))
            (equal 'inline-src-block (car (org-element-at-point))))
          (goto-char (match-end 0))
          (setf wc (+ 2 wc)))
         ;; Count latex macros as 1 word, ignoring their arguments.
         ((save-excursion
            (ignore-errors (backward-char))
            (looking-at latex-macro-regexp))
          (goto-char (if count-latex-macro-args
                         (match-beginning 2)
                       (match-end 0)))
          (setf wc (+ 2 wc)))
         ;; Ignore footnotes.
         ((and no-count-footnotes
               (or (org-footnote-at-definition-p)
                   (org-footnote-at-reference-p)))
          nil)
         (t
          (let ((contexts (org-context)))
            (cond
             ;; Ignore tags and TODO keywords, etc.
             ((or (assoc :todo-keyword contexts)
                  (assoc :priority contexts)
                  (assoc :keyword contexts)
                  (assoc :checkbox contexts))
              nil)
             ;; Ignore sections marked with tags that are
             ;; excluded from export.
             ((assoc :tags contexts)
              (if (intersection (org-get-tags-at) org-export-exclude-tags
                                :test 'equal)
                  (org-forward-heading-same-level 1)
                nil))
             (t
              (incf wc))))))
        (re-search-forward "\\w+\\W*")))
    (if (called-interactively-p 'any)
        (message (format "%d words in %s." wc
                         (if mark-active "region" "buffer")))
      ;; If non-interactive, just return the number
      wc)))

(defvar gk-org-reading-note--history '("0"))

(defun gk-org-refill-reading-note ()
  "Refill a list item when taking reading notes from a PDF.

Account for soft hyphens."
  (interactive)
  (goto-char
    (save-excursion
      (save-match-data
        (save-restriction
          (let* ((beg (or (and (looking-at "^-") (point))
                          (re-search-backward "^- ")))
                 (end (re-search-forward
                       (rx (or "\n\n" buffer-end))))
                 (lines (count-lines beg end)))
            (narrow-to-region beg (- end 2))
            (dotimes (_ lines)
              (join-line))
            ;; Deal with soft-hyphens
            (goto-char (+ 2 (point-min)))
            (while (re-search-forward "[-­‐] " nil t)
              (replace-match ""))
            (fill-paragraph)

            ;; return where to go
            (+ 2 (point-max))))))))

(defun gk-org-insert-reading-note (page)
  "Insert a reading note into the reading notes file.

A note in that file has a certain structure, i.e. a list item
with the page number as the first thing, then the quote text,
which comes from the ‘kill-ring’ via ‘yank’ wrapped in
guillemets.  PAGE is the page number, and can be any string,
given how page numbers are realised varies in the real world."
  (interactive
   (list
    (let ((def (car gk-org-reading-note--history)))
     (read-string
      (format "Page number (default: %s): " def)
      nil 'gk-org-reading-note--history def t))))
  (goto-char (line-beginning-position))
  (insert "- ")
  (insert "p. " page ": «")
  (insert
   (with-temp-buffer
     (yank)
     (string-trim
      (buffer-substring (point-min) (point-max)))))
  (insert "»\n\n")
  (gk-org-refill-reading-note))

(defun gk-org-reading-notes-ellipsise-last-note ()
  "Ellipsise the beginning and the end of the last note.

This means, look at the first char, if lowercase, assume it’s a
partial sentence.  Then look at the last char, if not a period,
assume again it’s a partial sentence.  Then ellipsise with
brackets and an ASCII ellipsis, i.e. three consecutive dots."
  (interactive)
  (save-excursion
    (condition-case err
        (progn
          (re-search-backward "^- p." nil)
          (re-search-forward ": «" (line-end-position))
          ;; We’re at the first char of the note, right after the
          ;; lquote. If it’s lower-case, upcase it and add the
          ;; ellipsis.
          (let ((case-fold-search nil))
           (save-match-data
             (when (looking-at (rx lower))
               (let ((char (buffer-substring (point) (1+ (point)))))
                 (delete-char 1)
                 (insert "[... " (upcase char) "]")))
             ;; We’re at the last char, right before rquote.  Check if
             ;; there is a period, or add one with ellipsis.
             (when (re-search-forward
                    "»\n\n" (save-excursion (org-forward-paragraph) (point)))
               (backward-char 4)
               ;; Delete punctuation
               (when (looking-at (rx (any ":;,")))
                 (delete-char 1)
                 (backward-char))
               (when (looking-at (rx (not (any ".!?\"”’"))))
                 (forward-char)
                 (insert "[... .]"))))))
      ('search-failed
       (user-error "Could not find note delimitation")))))

(defun gk-org-insert-reading-bibliograpy-note ()
  (interactive)
  (unless (org-insert-item)
    (goto-char (line-beginning-position))
    (insert "- "))
  (insert "-> ")
  (yank)
  (insert "\n")
  (gk-org-refill-reading-note)
  (when (y-or-n-p "Inserted bibliographic reference, save file now?")
    (save-buffer)))

(defun gk-org-reading-note-merge-last-n-notes (n)
  "Merge last N reading notes, at least 2.

In interactive mode N is read from the prefix argument.  If it’s
not given or is one, it’s taken as two.  If less than two, it’s a
user error and the command aborts."
  (interactive "p")
  ;; If no arg was given or it was one, assume two.
  (when (= n 1)
    (setq n 2))
  (unless (>= n 2)
    (user-error "Merging less than two notes is not really possible, no?"))
  (dotimes (_ (1- n))
    (save-excursion
      (condition-case err
       (progn
         (goto-char (line-beginning-position))
         (when (looking-at "^-")
           (re-search-forward "«."))
         (let ((pattern (rx (and "»\n\n- p\." (1+ (not (any "\n"))) ": «"))))
           (re-search-backward pattern)
           (delete-region (point) (re-search-forward ": «"))
           (insert " ")
           (gk-org-refill-reading-note)))
       ('search-failed
        (user-error "Could not find last research note"))))))

(cl-defun gk-org-forward-content (&optional (n 1))
  "Go forward in content view.

Toggle the contents view, go to Nth next visible entry, make it
visible.

N defaults to 1."
  (interactive "p")
  (org-content)
  (outline-next-visible-heading n)
  (org-show-entry)
  (recenter 1))

(cl-defun gk-org-backward-content (&optional (n 1))
  "Go backward in content view.

Toggle the contents view, go to Nth previous visible entry, make it
visible.

N defaults to 1."
  (interactive "p")
  (org-content)
  (outline-previous-visible-heading n)
  (org-show-entry)
  (recenter 1))

(defun gk-org-insert-all-stored-links ()
  "Insert the contents of ‘org-stored-links’, one per line."
  (interactive)
  (dolist (link org-stored-links)
    (insert (apply #'format "[[%s][%s]]" link))
    (newline 2)))

(define-advice org-tree-to-indirect-buffer
    (:around (fn arg) arg-for-dedicated-frame)
  "Use ARG not for selection but for toggling dedicated frames.

If called with prefix arg, open in a dedicated frame.  Otherwise,
respect ‘org-indirect-buffer-display’."
  (interactive "P")
  (let ((org-indirect-buffer-display
         (if arg 'dedicated-frame
           org-indirect-buffer-display)))
    (funcall fn)))

(defun gk-org-ensure-empty-line-before-headlines (beg end)
  "Make sure there is an empty line before each headline in the region.

Interactively, default to whole buffer if region is not active,
and report how many headlines were affected."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((count 0)
        (buf (clone-indirect-buffer
              (format " *gk-org-empty-lines:%s*" (buffer-name))
              nil t)))
    (with-current-buffer buf
      ;; Region will not be active in the indirect buffer.
      (unless (and (= beg (point-min))
                   (= end (point-max)))
        (narrow-to-region beg end))
      (org-show-all)
      (goto-char (point-min))
      (org-map-entries
       ($ (forward-line -1)
          (unless (looking-at
                   (rx (or (and bol eol)
                           (and bol "\n" "* ")
                           ;; Empty entry
                           (and bol "*" (0+ anychar) "\n" "* "))))
            (forward-line 1)
            (open-line 1)
            (cl-incf count))
          (forward-line 1))))
    (kill-buffer buf)))

(defun gk-org-export-this-tree (&optional async)
  "Export to PDF the toplevel tree the point is in.

If called with a prefix argument, or ASYNC is non-nil, run the
export process asynchronously and open/revert the file or file’s
buffer with ‘find-file-other-window’ when export process is
completed.

If for some reason the timer fails to cancel and the resulting
PDF is opened in many windows continuously, hit
\\[gk-cancel-last-timer] to cancel the most recent timer."
  (interactive (list (not (not current-prefix-arg))))
  (save-excursion
    (while (org-up-heading-safe))          ;go to toplevel
    (org-latex-export-to-pdf async t)
    (when async
      (setf gk-org-async-export-this-tree
            (run-with-timer
             0.5 0.5
             (lambda ()
               (let ((file? (caar org-export-stack-contents))
                     buf)
                 (when (and (stringp file?)
                            (file-exists-p file?))
                   (cancel-timer gk-org-async-export-this-tree)
                   (let ((revert-without-query (list ".")))
                     (find-file-other-window file?)
                     (other-window 1))
                   (setf gk-org-async-export-this-tree nil)))))))))


(defun gk-open-reading-note ()
  "Find a reading note and open it in a popup frame.

Narrow to the relevant heading.  Reading notes are toplevel headings in ‘gk-reading-notes-file’."
  (interactive)
  (with-current-buffer (find-file-noselect gk-reading-notes-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((entries (org-map-entries
                         ($ (let ((el (org-element-at-point)))
                              (when (and (eq 'headline (car el))
                                         (= 1 (plist-get (cadr el) :level)))
                                (cadr el))))))
               (hash (make-hash-table :test 'equal)))
          (dolist (e entries)
            (puthash (plist-get e :title) e hash))
          (let* ((pick (gethash
                        (completing-read
                         "Select reading note to view: " hash) hash))
                 (newnam (format "%s::%s" (buffer-name) (plist-get pick :title)))
                 (newbuf
                  (if-let* ((buf (get-buffer newnam)))
                      buf
                    (clone-indirect-buffer newnam nil t))))
            (with-current-buffer newbuf
              (goto-char (plist-get pick :begin))
              (org-narrow-to-subtree))
            (display-buffer-pop-up-frame newbuf nil)))))))


(defun gk-org-export-region-as-markdown (beg end)
  "Use ‘ox-hugo’ to translate the region to markdown.

Does not break current restrictions and does not write the output
to a file.  Just pops up a buffer with Markdown in Hugo format.

Beware: this means hard newlines are not expressed as two
consecutive spaces but with a <br/>, and the buffer has some
front matter added.  This means the output may beed to be
modified slightly before it’s used e.g. when posting to Reddit."
  ;; FIXME(2020-09-14): transform/eliminate hugo artefacts
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (org-hugo-export-as-md nil nil t)))


(defun gk-org-insert-list-of-stored-links ()
  "Insert ‘org-stored-links’ as a bulleted list."
  (interactive)
  (unless (= (point) (line-beginning-position))
    (newline))
  (dolist (link org-stored-links)
    (insert (apply #'format "- [[%s][%s]]\n" link))))


;; From: https://stackoverflow.com/a/28130043
(defun gk-org-todo-delayed-done (&optional arg)
  "Like org-todo-yesterday, but prompt the user for a date. The time
of change will be 23:59 on that day"
  (interactive "P")
  (let* ((hour (nth 2 (decode-time
                       (org-current-time))))
         (daysback (- (date-to-day (current-time-string))
                      (org-time-string-to-absolute (org-read-date))))
         (org-extend-today-until (+ 1 (* 24 (- daysback 1)) hour))
         (org-use-effective-time t)) ; use the adjusted timestamp for logging
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-todo arg)
      (org-todo arg))))

(define-advice org-agenda-redo-all
    (:around (fn &rest args) always-go-to-top-but-push-mark-before-movement)
  "Go to the top of the buffer after, but push mark before redoing.

Use \[pop-to-mark-command] to go back to where you were."
  (let ((p (point)))
    ;; This does go to the beginning of the buffer, but I don’t really
    ;; understand why exactly it does that, so...
    (save-mark-and-excursion
      (funcall fn args))
    ;; ... I’ll be redundant.
    (goto-char (point-min))
    (push-mark p)))



;;;;; Variables:

(setf
 ;; Open files in the same window.
 (cdr (assoc 'file org-link-frame-setup)) 'find-file
 ;; Place tags right after the title.
 org-tags-column 0
 ;; If an #+attr_*: :width xxx available, use xxx, else, car of this
 ;; value.
 org-image-actual-width
 (list (* 48 (aref (font-info (face-attribute 'default :family)) 10)))
 ;; The couple settings below are adapted from
 ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/.
 ;;
 ;; Open collapsed stuff when editing them.
 org-catch-invisible-edits 'smart
 ;; In collapsed view, hide empty lines between subtrees.
 org-cycle-separator-lines 0
 ;; RET on links follows link.
 org-return-follows-link t)

(setf
 ;; Week starts on Monday
 calendar-week-start-day 1)

;; Enforce subtasks are done before the parent.
(setf
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t)



;;;;; Org files and directories:

(setq org-export-coding-system 'utf-8
      org-directory (expand-file-name "~/Notes")
      org-default-notes-file (gk-org-dir-file "Start.org")
      org-icalendar-combined-agenda-file (expand-file-name "ajanda.ics" gk-syndir)
      org-id-locations-file (locate-user-emacs-file "etc/org-id-locations.el"))



;;;;; Apps:

;; Use system app to handle PDFs.
(setcdr (assoc "\\.pdf\\'" org-file-apps) "okular %s")



;;;;; Agenda:

;; Don’t fucking kill my buffers.  Why would I ever want that?
(defalias 'org-release-buffers #'ignore)

(define-advice org-agenda-switch-to
    (:around (fn arg) in-other-window)
  "Show the buffer in a bottom side window and switch to it."
  (interactive "P")
  (let (buf ret pos)
    (setq buf (save-window-excursion
                (setq ret (funcall fn)
                      pos (point))
                (message (buffer-name))
                (current-buffer)))
    (if arg
        (display-buffer buf)
      (display-buffer-in-side-window buf '((side . bottom))))
    (select-window (get-buffer-window buf))
    (goto-char pos)
    (org-back-to-heading)
    (recenter-top-bottom 0)
    ret))

(setf
 ;; Don't show done items.
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-timestamp-if-done t
 ;; Don't mess with my window setup.
 org-agenda-window-setup 'current-window
 ;; Make todays agenda appear first, then the rest grouped by
 ;; category.
 org-agenda-sorting-strategy
 '((agenda time-up category-up deadline-up scheduled-up
           tag-up habit-down priority-down)
   (todo priority-down category-up)
   (tags priority-down category-up)
   (search category-up))
 org-agenda-dim-blocked-tasks nil
 ;; Don’t colour unless deadline
 org-agenda-deadline-faces '((1.0 . org-warning)
                             (0.0 . default))
 ;; Compact display for scheduled items.
 org-agenda-use-time-grid nil)

(setf org-agenda-block-separator nil)

;; Custom agenda setup
(setf
 ;; Make space alloc’d prefixes slightly larger, my categories are a
 ;; bit descriptive.
 org-agenda-prefix-format
 '((agenda . " %i %-16:c%?-16t% s")
   (todo . " %i %-16:c")
   (tags . " %i %-16:c")
   (search . " %i %-16:c"))
 ;; More room for habit titles.
 org-habit-preceding-days 21
 org-habit-graph-column 49
 org-agenda-files
 (gk-org-dir-files
  "Todo.org" "Linguistics.org" "Statistics.org")
 org-agenda-hide-tags-regexp "."
 org-agenda-sticky t
 org-agenda-custom-commands
 `(("p" "Planner"
    (;; Today’s scheduled items
     (agenda "" ((org-agenda-overriding-header
                  "* Today’s schedule:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                 (org-deadline-warning-days 0)
                 (org-agenda-sorting-strategy '(time-up
                                                priority-down
                                                category-up
                                                deadline-up
                                                scheduled-up
                                                tag-up
                                                habit-down))
                 (org-default-priority org-lowest-priority)
                 (org-agenda-span 1)))

     ;; Approaching deadlines
     (agenda nil ((org-agenda-overriding-header
                   "\n* Approaching deadlines:")
                  (org-agenda-entry-types '(:deadline))
                  (org-agenda-format-date "")
                  (org-deadline-warning-days 21)
                  (org-agenda-sorting-strategy '( priority-down deadline-up todo-state-down))
                  (org-default-priority org-lowest-priority)
                  (org-agenda-span 1)))

     (tags-todo "CATEGORY=\"Tez@MALing\""
                ((org-agenda-overriding-header
                  "\n* Thesis:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                 (org-default-priority org-lowest-priority)
                 (org-agenda-span 1)))

     ;; Reading
     (todo "READING|READ" ((org-agenda-overriding-header
                            "\n* Reading:")
                           (org-agenda-sorting-strategy '(priority-down todo-state-down))
                           (org-default-priority org-lowest-priority)
                           (org-agenda-skip-function
                            '(org-agenda-skip-entry-if 'deadline))))))

   ("u" "Unsorted TODOs"
    ;; Unsorted TODO items
    ((tags-todo "-vault-research+TODO=\"TODO\""
                ((org-agenda-overriding-header
                  "* Stray  TODOs:")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline 'scheduled))
                 (org-default-priority org-lowest-priority)
                 (org-agenda-sorting-strategy '(priority-down alpha-up))))))))


(defun gk-org-display-single-pane-agenda-view (&optional arg)
  "Display a single pane planner agenda view.

If called with a prefix argument, or ARG is non-nil, delete other
windows and show the agenda window as the sole window.

This is coupled with custom agendas in
‘org-agenda-custom-commands’."
  (interactive "P")
  (when arg
   (delete-other-windows))
  ;; Show planner
  (let ((org-agenda-buffer-name "*Org Agenda: Planner*"))
    (org-agenda nil "p")))


(defun gk-org-display-two-pane-agenda-view ()
  "Display a two pane agenda view.

On the left is the actual agenda view, on the right a list of
TODOs.  This is coupled with custom agendas in
‘org-agenda-custom-commands’."
  (interactive)
  (gk-org-display-single-pane-agenda-view t)
  (split-window-horizontally)
  (other-window 1)
  ;; Show TODOs.
  (let ((org-agenda-buffer-name "*Org Agenda: Stray TODOs"))
    (org-agenda nil "u"))
  ;; Put cursor in planner
  (other-window 1))

(defun gk-org-display-planner-frame (&optional arg)
  "Open new frame with single pane agenda view.

When called with a prefix argument, or when ARG is non-nil,
display a two pane view in a maximised frame."
  (interactive "P")
  (gk-with-new-frame nil
    (if (not arg)
        (gk-org-display-single-pane-agenda-view)
      (toggle-frame-maximized)
      (gk-org-display-two-pane-agenda-view))))


(defun gk-org-agenda-mode-hook ()
  (hl-line-mode +1)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (gk-turn-on-outline-minor-mode "^\\*" ":$" "C-'"))

(add-hook 'org-agenda-mode-hook #'gk-org-agenda-mode-hook)

(defun gk-org-agenda-finalize-hook ()
  ;; Remove deadlines section if it’s empty.
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward (rx (and bol "* Approaching deadlines:" eol))))
    (when-let* ((next-heading-beg-pos (save-excursion
                                        (outline-next-visible-heading 1)
                                        (point)))
                (_ (string-empty-p (string-trim
                                    (buffer-substring (point)
                                                      next-heading-beg-pos)))))
      (delete-region (line-beginning-position) next-heading-beg-pos))))

(add-hook 'org-agenda-finalize-hook #'gk-org-agenda-finalize-hook)



;;;;; Exporting:



;;;;;; Common:

;; Defaults for exporting from org mode.

(setq org-export-with-smart-quotes t
      org-export-with-sub-superscripts t
      org-export-dispatch-use-expert-ui t)

;; From: https://lists.gnu.org/archive/html/emacs-orgmode/2013-07/msg00731.html
(defun gk-org-export-comment-removal-hook (backend)
  "Remove comments from export.

Prevent comments inline in paragraphs from splitting them."
  (delete-matching-lines "^[ \t]*#\\( \\|$\\)"))

(add-hook 'org-export-before-parsing-hook
          'gk-org-export-comment-removal-hook)



;;;;;; LaTeX->PDF:

(setf org-latex-compiler "xelatex"
      org-latex-pdf-process (list "latexmk -f -silent -bibtex-cond -xelatex %f")
      ;; Do remove many sorts of files the process generates...
      org-latex-remove-logfiles t
      ;; ...but keep some important log files.
      org-latex-logfiles-extensions
      (cl-remove-if ($ (member $1 (list "log" "blg")))
                    org-latex-logfiles-extensions))

;; Scale latex fragment previews up, 1.0 scale is barely legible.
(plist-put org-format-latex-options :scale 1.5)

(defun gk-org-pdf-subtree (arg)
  "Export subtree under point to pdf, show the output.

If called with  a universal prefix, org ARG is  ~'(1)~, output to
LaTeX buffer.  If region is active, narrows to region and exports
its contents."
  (interactive "p")
  (save-restriction
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (if (equal arg 1)
        (find-file (org-latex-export-to-pdf nil t))
      (org-latex-export-as-latex nil t))))

(setf org-latex-inactive-timestamp-format "%s")

(defun gk-ox-latex-format-inactive-timestamp (text backend info)
  (ignore info)
  (when (org-export-derived-backend-p backend 'latex)
    ;;; XXX  Doesn't work  as expected,  gives "30  November -001"  as
    ;;; result.
    ;; (org-timestamp-format text "%d %B %Y")
    (format-time-string
     ;; "%d \\monthname{%B} %Y"
     "\\formatdate{%d}{%m}{%Y}"
     (apply #'encode-time
            (org-parse-time-string
             text)))))

(add-to-list 'org-export-filter-timestamp-functions
             'gk-ox-latex-format-inactive-timestamp)



;;;;; Refile:

(setf
 org-refile-use-outline-path t
 org-refile-allow-creating-parent-nodes t
 org-log-refile 'time
 ;; New notes to the beginning of the file.
 org-reverse-note-order t)

(defun gk-org-after-refile-insert-hook ()
  "Save target if user agrees."
  (ignore-errors
    (when (y-or-n-p
           (format "Save %s?"
                   (buffer-file-name
                    (current-buffer))))
      (save-buffer))))

(add-hook 'org-after-refile-insert-hook 'gk-org-after-refile-insert-hook)



;;;;; Version control:

;; These following advices help keep Org files locked in RCS and edit via
;; agenda or other Org commands I frequently use seamlessly, so that they
;; don't pollute the vc buffer with unnecessary ‘edited’ bits.  Because
;; otherwise I have to keep them’t locked all the time.

;; TODO(2018-05-25): Fix repetition in Org RCS advices
;; Maybe add a macro/function =(gk-rcs-advice-for-unlock FUNCTION)= so
;; that:

;; (gk-rcs-advice-for-unlock #'org-todo)


;; [[file:/igk/config/emacs.d/gk.org::*Version%20control][Version control:1]]
(define-advice org-paste-subtree (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (gk-rcs-maybe-unlock f)))

;; Like above but for capturing.

(define-advice org-capture-fill-template (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let* ((buffer (org-capture-get :buffer))
         (file (buffer-file-name buffer)))
    (gk-rcs-maybe-unlock file)))

;; Like above but for ‘org-todo’.

(define-advice org-todo (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (gk-rcs-maybe-unlock f)))

;; For clocking.

(define-advice org-clock-in (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (message f)
    (gk-rcs-maybe-unlock f)))

(define-advice org-clock-out (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (message f)
    (gk-rcs-maybe-unlock f)))



;;;;; Structure:

(setf
 ;; Do not indent entry content.
 org-adapt-indentation nil
 ;; Allow alphabetical list elements and those that start with * above
 ;; initial level.
 org-list-allow-alphabetical t)

;; Required after setting ‘org-list-allow-alphabetical’.
(org-element-update-syntax)




;;;;;; Automatic insertion of blank lines between list items:
;; Org uses a complex heuristic to determine whether or not to insert
;; blank lines between list items.  I instead want to have a strict
;; style.  The advices below replace this heuristic with a regular
;; system:
;;
;; - Top level items are separated by a blank line.
;; - Non-top-level items are not separated.
;;
;; Besides, I advice list item indent/dedent functions to add a blank
;; line if dedenting to toplevel or remove it if indenting from
;; toplevel.

(define-advice org-list-separating-blank-lines-number
    (:override (pos struct prevs) do-it-my-way)
  "One line separates top level list items and none for other levels."
  (if (zerop (org-list-get-ind (point-at-bol) struct))
      1
    0))


(defun gk-org-after-indent-outdent-item ()
  "Insert/remove separating line like if ‘org-insert-item’ would."
  (let* ((struct (org-list-struct))
         (prevs  (org-list-prevs-alist struct))
         (p      (point))
         (nblank (org-list-separating-blank-lines-number p struct prevs))
         ;; Is the previous line blank?
         (blankp (save-excursion
                   (forward-line -1)
                   (zerop (- (line-end-position) (line-beginning-position))))))
    (cond ((and (zerop nblank) blankp)
           (save-excursion (goto-char (line-beginning-position))
                           (delete-backward-char 1)))
          ((and (/= 0 nblank) (not blankp))
           (save-excursion (goto-char (line-beginning-position))
                           (open-line 1))))))

(add-function :after (symbol-function 'org-indent-item) #'gk-org-after-indent-outdent-item
              '((name . fix-separator-line)))
(add-function :after (symbol-function 'org-outdent-item) #'gk-org-after-indent-outdent-item
              '((name . fix-separator-line)))




;;;;; Translating:

(defun gk-org-trans-show-paragraph ()
  "Show the ASCII export of current paragraph in a side window."
  (interactive)
  (let* ((org-export-show-temporary-export-buffer nil)
         (beg (save-excursion
                (save-match-data
                  (re-search-backward (make-string 2 ?\C-j)))))
         (end (save-excursion
                (save-match-data
                  (re-search-forward (make-string 2 ?\C-j)))))
         buf)
    (save-restriction
      (narrow-to-region beg end)
      (setq buf (org-ascii-export-as-ascii nil nil t))
      (with-current-buffer buf
        (save-excursion
          (save-match-data
            (push-mark (point-min) nil t)
            (re-search-forward (make-string 5 ?\C-j))
            (delete-region (region-beginning) (region-end))
            (deactivate-mark t)
            (text-mode))))
      (display-buffer-in-side-window buf '((side . bottom))))))

(defvar gk-org-trans--pageno-hist nil)
(defvar gk-org-trans--parno-hist nil)
(defvar gk-org-trans--sentenceno-hist nil)

(defun gk-org-trans-insert-sentence-reference (page paragraph sentence)
  "Insert sentence number.

PAGE is the page number.

PARAGRAPH is the paragraph number where the if a paragraph
continues to the page from the last one that paragraph is
counted as the first.

SENTENCE is the sentence number, counted similarly to
paragraphs.

Interactively, prompts for these numbers.  These numbers should
be represented as strings, in order to account for non-Arabic
numerals which regularly appear in texts."
  (interactive
   (list (read-string (format "Page # [default: %S]: "
                              (car gk-org-trans--pageno-hist))
                      nil 'gk-org-trans--pageno-hist
                      (car gk-org-trans--pageno-hist))
         (read-string (format "Paragrap # [default: %S]: "
                              (car gk-org-trans--parno-hist))
                      nil 'gk-org-trans--parno-hist
                      (car gk-org-trans--parno-hist))
         (read-string (format "Sentence # [default: %S]: "
                              (car gk-org-trans--sentenceno-hist))
                      nil 'gk-org-trans--sentenceno-hist
                      (car gk-org-trans--sentenceno-hist))))
  (insert "<<" page "." paragraph "." sentence ">> "))



;;;;; Store link:

(define-advice  org-store-link (:around (fn &rest args) fuck-pdf-version-cruft)
  "Disable context in PDFs and other non-text documents"
  (let ((org-context-in-file-links
         (and org-context-in-file-links
              (not (member major-mode
                           '(pdf-view-mode doc-view-mode))))))
    (apply fn args)))



;;;;; Visuals:

;; Show sub/superscript notation with ^ _ w/ UTF8 characters by
;; default. C-c C-x \ toggles.
(setq-default org-pretty-entities t)

(when (gk-gui-p)
  (setf
   ;; If non-nil, Do not show emphasis markers //, __ etc.
   org-hide-emphasis-markers nil
   ;; Fontify all the heading line.
   org-fontify-whole-heading-line t))

;; Make today stand out more.
(set-face-attribute 'org-agenda-date-today nil :inverse-video t)

;; Make blocked todo items legible.
(set-face-attribute 'org-agenda-dimmed-todo-face nil
                    :foreground nil :slant 'italic)

;; Make some links look more distinct.
(org-link-set-parameters
 "file"
 :face '(:inherit font-lock-string-face :underline t))

(set-face-attribute 'org-ellipsis nil :underline nil :height .5 :bold t)

(setf
 org-num-face 'org-verbatim
 org-ellipsis "  ⋱")



;;;;;; Variable pitch:

;; OVP minor mode allows to effectively use variable pitch fonts to
;; represent text in Org mode, while retaining monospace where necessary,
;; i.e. for source code, verbatim text, and structure and indentation.

(pushnew 'org-footnote org-variable-pitch-fixed-faces)
(pushnew 'highlight-indent-guides-even-face org-variable-pitch-fixed-faces)
(pushnew 'highlight-indent-guides-odd-face org-variable-pitch-fixed-faces)
(pushnew 'org-link org-variable-pitch-fixed-faces)

(defun gk-ovp-hook ()
  "Hook for ‘org-variable-pitch-minor-mode’."
  )

(add-hook 'org-variable-pitch-minor-mode-hook #'gk-ovp-hook)

(diminish 'org-variable-pitch-minor-mode "~")

(add-hook 'after-init-hook #'org-variable-pitch-setup)



;;;;;; The hook:

(defun gk-org-visuals-hook ()
  "Set up how an Org buffer look."
  ;; (valign-mode +1)
  (set-face-attribute 'org-footnote nil :underline nil)
  ;; (setq-local truncate-lines t)
  )

(add-hook 'org-mode-hook 'gk-org-visuals-hook)



;;;;;; LaTeX previews:

(define-advice org-format-latex
    (:around (fn &rest args) take-theme-into-account)
  "Adapt LaTeX previews to current theme.

The LaTeX preview image file names are generated using a hash
that takes various variables into account, including
‘org-format-latex-header’.  This advice prepends the value of
that variable, a string that contains some LaTeX prelude for
generating images from fragments, with the list of currently
active themes, thus allowing the currently active theme(s) to
influence which images are picked.  Thus, after the theme
changes, there’s no need to manually regenerate these images.
You can just run \\[org-mode] in the buffer after switching the
theme.  If necessary, new images will be created."
  (let ((org-format-latex-header
         (concat
          (format
           "%%%% Enabled themes: %S\n\n\n\n"
           custom-enabled-themes)
          org-format-latex-header)))
    (apply fn args)))



;;;;; Custom links:


;;;;;; Gemini and Gopher links:

(dolist (proto (list "gopher" "gemini"))
  (org-link-set-parameters
   proto
   :follow 'gk-org-elpher-follow
   :store  'gk-org-elpher-store))

(defun gk-org-elpher-follow (path arg)
  (ignore arg)
  (elpher-go path))

(defun gk-org-elpher-store ()
  (when (and (eq major-mode 'elpher-mode))
    (let ((proto (elpher-address-protocol (cadr elpher-current-page))))
      (org-link-store-props
       :type proto
       :link (elpher-address-to-url (cadr elpher-current-page))
       :description (car elpher-current-page))
      t)))



;;;;;; Annotations:

;; Annotations are for making unobstrusive comments on parts of
;; documents.  Supports exporting to HTML and LaTeX.

(org-add-link-type
 "comment"
 (lambda (linkstring)
   (ignore linkstring)
   (let ((elm (org-element-context))
         (use-dialog-box nil))
     (when (y-or-n-p "Delete comment? ")
       (setf (buffer-substring
	      (org-element-property :begin elm)
	      (org-element-property :end elm))
	     (cond
	      ((org-element-property :contents-begin elm)
	       (buffer-substring
		(org-element-property :contents-begin elm)
		(org-element-property :contents-end elm)))
	      (t
	       ""))))))
 (lambda (keyword desc format)
   (cond
    ((eq format 'html)
     (format
      "<abbr title=\"%s\" color=\"red\">%s</abbr>"
      keyword
      (or desc "COMMENT")))
    ((eq format 'latex)
     (format "\\todo[inline]{%s}{%s}" keyword (or desc ""))))))

(defun gk-org-add-comment (begin end)
  "Add a comment link.

BEGIN and END are bounds of the region."
  (interactive "r")
  (if (region-active-p)
      (let ((selected-text (buffer-substring begin end)))
	(setf (buffer-substring begin end)
	      (format "[[comment:%s][%s]]"
		      (read-string "Comment: ") selected-text)))
  (insert (format  "[[comment:%s]]" (read-string "Comment: ")))))



;;;;;; OLP:

;; Follow the OLP on click.
(org-add-link-type
 "olp"
 (lambda (path)
   (let ((pathlist (split-string path "/" t)))
     (condition-case msg
         (progn
           (push-mark (point) t nil)
           (goto-char (org-find-olp pathlist t))
           (org-show-context))
       (error (nth 1 msg))))))



;;;;;; Mairix:

(dissoc! "rmail" org-link-parameters #'string=)

(org-link-set-parameters
   "mairix"
   :follow 'gk-org-mairix-open
   :store  'gk-org-mairix-store)

(defun gk-org-mairix-open (path arg)
  (mairix-search path arg))

(defun gk-org-mairix-store ()
  (when-let* ((_ (memq major-mode '(rmail-mode rmail-summary-mode)))
              (id (rmail-get-header "Message-ID"))
              (subj (or (rmail-get-header "Subject")
                        ""))
              (from (or (rmail-get-header "From")
                        "{Unknown}")))
    (org-store-link-props
     :type "mairix"
     :link (concat "mairix:m:" (string-trim id "<" ">"))
     :description (concat "Message from " from ": «" subj "»"))
    t))



;;;;;; Man page:

(org-add-link-type
 "man"
 (lambda (path)
   (man (substring-no-properties path))))



;;;;; Dynamic blocks:

(defun org-dblock-write:vcdirty (&rest args)
  "Update a vcdirty dynamic block.

Generates a "
  (let ((vcs-dirs (append
                   (list (expand-file-name "~/cf")
                         org-directory
                         (expand-file-name "~/Documents/not/www2")
                         (expand-file-name "~/.mozilla"))
                   (cl-remove-if
                    ($ (member $1 '("." ".." "Playground" "External" "Go" "Lisp"
                                    "Attic")))
                    (directory-files "~/co" t nil t))))
        (outbuf " gk-org-dynablock-cmd")
        (errbuf " gk-org-dynablock-cmd-err")
        dirty)
    (dolist (d vcs-dirs dirty)
      (cond
       ((file-exists-p (expand-file-name ".git" d))
        (when
            (zerop
             (let ((default-directory d))
               (shell-command "git status -s | egrep ." outbuf errbuf)))
          (push d dirty)))
       ((file-exists-p (expand-file-name ".hg" d))
        (when
            (zerop
             (let ((default-directory d))
               (shell-command "hg status | egrep ." outbuf errbuf)))
          (push d dirty)))))
    (when dirty
      (insert "Dirty repos:")
      (dolist (d dirty)
        (insert "\n[[elisp:(vc-dir \"" d "\")][" d "]]")))))



;;;;; Source code:

;; Editing source code elements.

(setf
 ;; Don’t indent content’s of souce blocks.
 org-edit-src-content-indentation 0
 ;; Don't change the window layout when editing source blocks, open
 ;; them instead in the current window.
 org-src-window-setup 'current-window)



;;;;; Babel:

(setf
 ;; Just do it™.
 org-confirm-babel-evaluate nil
 ;; Always wrap output in blocks.
 org-babel-min-lines-for-block-output 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (R . t)))

(setq org-babel-python-command "run-python.sh")

(defun gk-org-python-clean-spurious-indentation (body)
  (let* ((extra-indentation
	  (save-match-data
	    (string-match "\\`\\([ \t]+\\)" body)
	    (match-string 1 body)))
	 (xlen (length extra-indentation)))
    (if (zerop xlen)
	body
      (mapconcat
       (lambda (line) (if (<= (length line) xlen)
			  line
			(if (string= extra-indentation
				     (substring line 0 xlen))
			    (substring line xlen)
			  line)))
       (split-string body "\n")
       "\n"))))

(define-advice org-babel-execute:python
    (:filter-args (args) clean-spurious-indentation)
  (cons
   (gk-org-python-clean-spurious-indentation (car args))
   (cdr args)))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)



;;;;; Auto-insert:

(push
 '((org-mode . "Org-mode document")
   nil
   "#+title: " - n
   "#+date: \\today\n#+options: toc:nil\n")
 auto-insert-alist)



;;;;; Capture:

(defun gk-org-capture-book ()
  "Capture template for a book in to-read list."
  (let ((kitap (read-string "Kitabın ismi: "))
        (yazar (read-string "yazarı: "))
        (yayınevi (read-string "yayınevi: "))
        (yılı (read-string "yayın yılı: ")))
    (when (string-empty-p kitap)
      (user-error "Kitap ismi girilmedi"))
    (concat "- [ ] " kitap
            (unless (string-empty-p yazar) (concat "; " yazar))
            (unless (string-empty-p yayınevi) (concat ".  " yayınevi))
            (unless (string-empty-p yılı) (concat " (" yılı ")"))
            ".")))


(defun gk-org-define-capture-template (&rest args)
  "Define a capture template.

Creates a list and adds it to ‘org-capture-templates’.  ARGS is a
plist, which in addition to all the ‘additional options’
‘org-capture-templates’ accepts, takes the following and places
them accordingly: :keys, :description, :type, :target, :template;
which correspond to homonymous fields listed in
‘org-capture-template’s docstring (which see)."
  (setf org-capture-templates
        (append org-capture-templates
                (list
                 (append
                  (list (plist-get args :keys)
                        (plist-get args :description)
                        (plist-get args :type)
                        (plist-get args :target)
                        (plist-get args :template))
                  (cl-loop for i from 0 below (length args) by 2
                           unless
                           (member (nth i args)
                                   (list :keys :description
                                         :type :target :template))
                           append (list (nth i args)
                                        (plist-get
                                         args (nth i args)))))))))


(progn
  ;; Zero it out, the populate.
  (setf org-capture-templates nil)

  (gk-org-define-capture-template
   :keys "b"
   :description "New blurb"
   :type 'item
   :target `(file ,(gk-org-dir-file "Blurbs.org"))
   :template "- %?"
   :prepend nil
   :empty-lines-before 1
   :unnarrowed t)

  (gk-org-define-capture-template
   :keys "t"
   :description "Random task (not scheduled)"
   :type 'entry
   :target `(file+olp ,(car org-agenda-files) "Tasks")
   :template "* TODO %?"
   :prepend t
   :empty-lines-after 1
   :unnarrowed t)

  (gk-org-define-capture-template
   :keys "r"
   :description "Reading task"
   :type 'entry
   :target `(file+olp ,(car org-agenda-files) "Tasks")
   :template "* READ %?\n"
   :prepend t
   :empty-lines-after 1
   :unnarrowed t)

  (gk-org-define-capture-template
   :keys "e"
   :description "Add selected entry in ebib to reading list"
   :type 'item
   :template '(function
               (lambda ()
                 (unless (memq major-mode '(ebib-index-mode
                                            ebib-entry-mode))
                   (user-error "This template (e) should be called from within Ebib"))
                 (with-current-buffer
                     (or (assoca 'index ebib--buffer-alist)
                         ;; XXX(2021-03-26): this can be replaced with
                         ;; an interactive search maybe?
                         (user-error "Ebib not running, can’t use ebib capture template"))
                   (let* ((key (progn (ebib-copy-key-as-kill)
                                      (pop kill-ring)))
                          (maybe-pdf
                           (if-let*
                               ((attch (gk-existing-file-name-or-nil
                                        (expand-file-name (concat key ".pdf")
                                                          gk-bib-attachments-dir))))
                               (format " [[file:%s][(pdf)]]" attch)
                             ""))
                          (newitem (concat "- [ ] " key maybe-pdf "\n"
                                           "  - " (replace-regexp-in-string
                                                   "\n" " "
                                                   (progn (ebib-copy-reference-as-kill)
                                                          (pop kill-ring))))))
                     (with-temp-buffer
                       (org-mode)
                       (setq-local fill-column 70)
                       (insert newitem)
                       (org-fill-paragraph)
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))))))
   :target `(file+olp ,(car org-agenda-files) "reading inbox")
   :prepend t
   :empty-lines-after 1
   :unnarrowed t)

  (gk-org-define-capture-template
   :keys "R"
   :description "Reading task (make active)"
   :type 'entry
   :target `(file+olp ,(car org-agenda-files) "Tasks")
   :template "* READING %?\n"
   :prepend t
   :empty-lines-after 1
   :unnarrowed t)

  (gk-append-to-list
   'org-capture-templates
   (list (list "c" "Coursework")))

  (gk-org-define-capture-template
   :keys "cr"
   :description "Coursework: reading"
   :type 'entry
   :target `(file+olp ,(car org-agenda-files)
                      "Current semester"
                      "Readings")
   :template "* TODO %?\nDEADLINE:%^{Deadline}t"
   :prepend t
   :empty-lines-after 1
   :unnarrowed t)

  (gk-append-to-list
   'org-capture-templates
   (list (list "T" "Thesis")))

  (gk-org-define-capture-template
   :keys "Tt"
   :description "Thesis: task"
   :type 'entry
   :target `(file+olp ,(car org-agenda-files) "MA thesis")
   :template "* TODO %?"
   :prepend t
   :empty-lines-after 1
   :unnarrowed t))




;;;;; Attachments:

(setf
 ;; Use relative paths for attachment links.
 org-attach-dir-relative t)

(defun gk-org-attach-screenshot ()
  "Little wrapper around ‘org-attach-screenshot’.

Don’t hide the frame and don’t ask me shit."
  (interactive)
  (org-attach-screenshot
   (list 4)
   (format-time-string "screenshot-%Y%m%d-%H%M%S.png")))




;;;;; org-zotxt:

(setf
 ;; Sane defaults.
 org-zotxt-default-search-method :title-creator-year
 org-zotxt-link-description-style :betterbibtexkey)


(define-advice org-zotxt-insert-reference-links-to-items
    (:override (items) dont-fucking-insert-new-fucking-lines-ffs)
  "Insert links to Zotero ITEMS in buffer, without fucking everything up."
  (mapc (lambda (item)
          (org-zotxt-insert-reference-link-to-item item)
          (insert ", "))
        items)
  (delete-backward-char 2))




;;;;; Icalendar:

;; Do not sync deadlines and schedules.
(setf org-icalendar-use-scheduled nil
      org-icalendar-use-deadline  nil)




;;;;; Dynamic previews for LaTeX fragments:

;; Adapted from https://gist.github.com/cvcore/760008a4dfb2eadf42afdc9cf01ef979

(defvar-local gk-org-last-fragment nil
  "Holds the type and position of last valid fragment we were on.
Format: (FRAGMENT_TYPE FRAGMENT_POINT_BEGIN)")

(defvar gk-org-valid-fragment-type
  '(latex-fragment latex-environment link))

(defun gk-org-curr-fragment ()
  "Returns the type and position of the current fragment.

Returns the type and position of the current fragment available
for preview inside org-mode. Returns nil at non-displayable
fragments"
  (let* ((fr (org-element-context))
         (fr-type (car fr)))
    (when (memq fr-type gk-org-valid-fragment-type)
      (list fr-type
            (org-element-property :begin fr)))))

(defun gk-org--list-latex-overlays (&optional beg end)
  (cl-remove-if-not
   (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
   (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun gk-org-remove-fragment-overlay (fr)
  "Remove fragment overlay at FR."
  (let ((fr-type (nth 0 fr))
        (fr-begin (nth 1 fr)))
    (goto-char fr-begin)
    (cond ((or (eq 'latex-fragment fr-type)
               (eq 'latex-environment fr-type))
           (let ((ov (loop for ov in (gk-org--list-latex-overlays)
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov))))
          ((eq 'link fr-type) nil))))

(defun gk-org-preview-fragment (fr)
  "Preview org fragment at fr"
  (let ((fr-type (nth 0 fr))
        (fr-begin (nth 1 fr)))
    (goto-char fr-begin)
    (cond ((or (eq 'latex-fragment fr-type) ;; latex stuffs
               (eq 'latex-environment fr-type))
           ;; Only toggle preview when we're in a valid region (for
           ;; inserting in the front of a fragment).
           (when (gk-org-curr-fragment)
             (org-preview-latex-fragment)))
          ((eq 'link fr-type) ;; for images
           (let ((fr-end (org-element-property :end (org-element-context))))
             (org-display-inline-images nil t fr-begin fr-end))))))

(defun gk-org-auto-toggle-fragment-display ()
  "Automatically toggle a displayable org mode fragment"
  (and (eq 'org-mode major-mode)
       (let ((curr (gk-org-curr-fragment)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            gk-org-last-fragment
            ;; and are on a fragment now
            curr
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (equal curr gk-org-last-fragment)))

           ;; go back to last one and put image back, provided there
           ;; is still a fragment there
           (save-excursion
             (gk-org-preview-fragment gk-org-last-fragment)
             ;; now remove current image
             (gk-org-remove-fragment-overlay curr))
           ;; and save new fragment (?)
           (setq gk-org-last-fragment curr))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not curr)
            ;; but we were on one
            gk-org-last-fragment)
           ;; put image back on, provided that there is still a
           ;; fragment here.
           (save-excursion
             (gk-org-preview-fragment gk-org-last-fragment))

           ;; unset last fragment
           (setq gk-org-last-fragment nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not gk-org-last-fragment)
            ;; but now we are
            curr)
           ;; remove image
           (save-excursion
             (gk-org-remove-fragment-overlay curr)
             )
           (setq gk-org-last-fragment curr))))))




;;;;; Links & files:

(setf
 ;; Open file: links in other window.
 (cdr (assoc 'file org-link-frame-setup)) 'find-file-other-window)



;;;;; Keybindings:

;; Disable confusing bindings
(org-defkey org-mode-map (kbd "C-c C-x C-c") nil)

(org-defkey org-mode-map (kbd "C-M-<return>") 'org-insert-subheading)
;; Heading navigation
(org-defkey org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
(org-defkey org-mode-map (kbd "M-n") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-M-n") #'gk-org-forward-content)
(define-key org-mode-map (kbd "C-M-p") #'gk-org-backward-content)
;; Paragraphs
(define-key org-mode-map [remap backward-paragraph] nil)
(define-key org-mode-map [remap forward-paragraph] nil)
(define-key org-mode-map (kbd "C-c q") #'gk-org-refill-reading-note)
(define-key org-mode-map (kbd "C-c C-# n") #'gk-org-insert-reading-note)
(define-key org-mode-map (kbd "C-c C-# b") #'gk-org-insert-reading-bibliograpy-note)
;; Reading notes
(define-key org-mode-map (kbd "C-c M-r") #'gk-org-insert-reading-note)
(define-key org-mode-map (kbd "C-c M-e") #'gk-org-reading-notes-ellipsise-last-note)
(define-key org-mode-map (kbd "C-c M-b") #'gk-org-insert-reading-bibliograpy-note)
(define-key org-mode-map (kbd "C-c M-m") #'gk-org-reading-note-merge-last-n-notes)

;; Translation
(define-key org-mode-map (kbd "C-c M-t") #'gk-org-trans-show-paragraph)
(define-key org-mode-map (kbd "C-@") #'gk-org-trans-insert-sentence-reference)

;; Attachments
(define-key org-mode-map (kbd "C-c C-M-s") #'gk-org-attach-screenshot)
(define-key org-mode-map (kbd "<f2>") #'gk-org-attach-screenshot)



;;;;; Hook:

(defun gk-org-hook ()
  "Default hook for `org-mode' buffers."
  (setq-local default-justification 'left)
  (auto-fill-mode 1)
  ;; Disable in org mode
  (when (boundp goto-address-mode)
    (goto-address-mode -1))
  (org-zotxt-mode +1)
  (org-num-mode +1)
  (imenu-add-to-menubar "Entries")
  (setq-local truncate-lines nil)
  (setq-local indent-tabs-mode nil)
  (setq-local post-command-hook (cons #'gk-org-auto-toggle-fragment-display
                                      post-command-hook)))

(add-hook 'org-mode-hook 'gk-org-hook)



;;;;; Private settings:

(gk-load (gk-org-dir-file "settings") t)



;;;;; LaTeX, AuCTeX, Ebib:

;; TODO(2021-02-28): move these to appropriate sections.
;; Accomodate AuCTeX.
(setenv "TEXINPUTS" (concat "::" (expand-file-name "auctex/texmf" gk-elisp-site-dir)))
(require 'auctex)
(require 'preview-latex)

(defvar gk-bib-dir (gk-org-dir-file "Library")
  "Location for global Bib(La)TeX files.")

(defvar gk-bib-attachments-dir
  (expand-file-name "Attachments" gk-bib-dir)
  "Global store for bibliography attachments.")


(defun gk-ebib-set-bibtex-dialect (dialect)
  "Set the default dialect for Ebib and bibtex.el.

This sets the values of ‘bibtex-dialect’ and
‘ebib-bibtex-dialect’ and calls ‘bibtex-set-dialect’.

The value of DIALECT should be one of the symbols in
‘bibtex-dialect-list’.  The symbol ‘bibtex’ is synonymous with
‘BibTeX’."
  (let ((d (or (and (eq dialect 'bibtex) 'BibTeX)
               dialect)))
    (setq bibtex-dialect d
          ebib-bibtex-dialect d)
    (bibtex-set-dialect d)))

(gk-ebib-set-bibtex-dialect 'biblatex)

(setf
 ebib-file-associations nil
 ebib-preload-bib-files (list (expand-file-name "All.bib" gk-bib-dir))
 ebib-file-search-dirs (list gk-bib-attachments-dir)
 ebib-index-columns '(("Entry Key" 20 t) ("Author/Editor" 40 t) ("Year" 6 t) ("Title" 50 t))
 ;; See: ‘bibtex-generate-autokey’.
 bibtex-autokey-year-length 4
 bibtex-autokey-year-title-separator ""
 bibtex-autokey-titleword-length 10
 bibtex-autokey-titlewords-stretch 0
 bibtex-autokey-titlewords 1
 ;; Manually maintain a list of canonical keywords.
 ebib-keywords '()
 ebib-keywords-add-new-to-canonical nil
 ebib-keywords-save-on-exit nil
 ;; Record when new entries are added.
 ebib-use-timestamp t
 ;; Split the current window into two.
 ebib-layout 'window

 ;; see ‘ebib-extra-fields’, can be used to mark collections; ‘a’ adds
 ;; extra fields in entry buffer.

 ;; see ‘ebib-hidden-fields’, and kbd ‘H’

 ;; see ‘ebib-citation-description-function’ for org mode links
 ;; and org-ebib.el
 )


(define-key ebib-multiline-mode-map
  "\C-c\C-c" 'ebib-quit-multiline-buffer-and-save)
(define-key ebib-multiline-mode-map
  "\C-c\C-k" 'ebib-cancel-multiline-buffer)
(define-key ebib-multiline-mode-map
  "\C-c\C-s" 'ebib-save-from-multiline-buffer)

(define-key ebib-index-mode-map (kbd "C-x b") nil)
(define-key ebib-entry-mode-map (kbd "C-x b") nil) ;just stay where you are.  It’s going to be alright.  Don’t
                                                   ;run away.

(define-key ebib-index-mode-map [?g] #'ebib-reload-current-database)
(define-key ebib-index-mode-map [?q] #'ebib-lower)
(define-key ebib-entry-mode-map [?q] #'ebib-lower)
(define-key ebib-index-mode-map [?Q] #'ebib-quit)
(define-key ebib-entry-mode-map [?Q] #'ebib-quit)


;; Switch databases
(define-key ebib-index-mode-map (kbd "C-n") #'ebib-next-database)
(define-key ebib-index-mode-map (kbd "C-p") #'ebib-prev-database)
(define-key ebib-entry-mode-map (kbd "C-n") #'ebib-next-database)
(define-key ebib-entry-mode-map (kbd "C-p") #'ebib-prev-database)



;;;; Deft:
;; Gotta be set up after Org mode, depends on ‘org-directory’.

(setf
 ;; Finding the files to be searched.
 deft-directory org-directory
 ;; Search recursively from ‘deft-directory’.
 deft-recursive t
 ;; The ‘car’ of this list is the default extension when creating
 ;; files from Deft.
 deft-extensions '("org" "txt" "md" "markdown" "textile")
 ;; Destination for ‘C-c C-a’ in deft.
 deft-archive-directory "Attic/deft/"
 ;; Disable auto save.
 deft-auto-save-interval 0)

(defun gk-deft (&optional arg)
  "Run ‘deft’.

With no prefix arguments, just run ‘deft’; it’ll open in the
current window.

With one prefix argument, it’ll open in a new frame.

With two prefix arguments, it’ll open in the current frame and
will become the only window."
  (interactive "p")
  (cl-case arg
    (1 (deft))
    (4 (gk-with-new-frame nil (deft)))
    (16 (delete-other-windows) (deft))))



;;;; Multimedia:



;;;;; Images:

;; Viewing and editing images.



;;;;;; Scaling images:

;; The following set of key rebindings and the ‘gk-fit-image-to-window’
;; function help with viewing images bigger than the window they are in.
;; By default, the images overflow in such a situation.  With these
;; modifications, the images are rescaled to fit the window as ‘n’ and
;; ‘p’ keys are pressed to navigate them. ‘=’ manually fits the image to
;; the window, and ‘N’ and ‘P’ navigates images wihout resizing.

(define-obsolete-function-alias 'gk-fit-image-to-window
  'image-transform-fit-both "idk (g@)")

(define-key image-mode-map "=" #'gk-fit-image-to-window)
(define-key image-mode-map "n" (gk-interactively
                                (image-next-file 1)
                                (gk-fit-image-to-window)))
(define-key image-mode-map "p" (gk-interactively
                                (image-previous-file 1)
                                (gk-fit-image-to-window)))
(define-key image-mode-map "N" #'image-next-file)
(define-key image-mode-map "P" #'image-previous-file)

;;;;;; Keybindings:

;; By default animated images in Emacs don't loop when played.  We set up
;; so that when animated with RET, they play once; and when animated with
;; SPC, they loop.  Also, when hit ‘q‘, kill the image buffer, don't bury
;; it.  This'd presumably save some memory.

(define-key image-mode-map [?q] 'kill-this-buffer)
(define-key image-mode-map [?\ ] (gk-interactively
                                   (let ((image-animate-loop t))
                                     (image-toggle-animation))))



;;;; OS-specific settings:



;;;;; POSIX:

(when (executable-find "man")
  (require 'man)
  (setf
   ;; Make man page current buffer in current window of current frame.
   ;; Or, show the fucking man page where I want it.
   Man-notify-method 'pushy))



;;;; User interface:



;;;;; GUI:

(defvar gk-preferred-themes '( :light modus-operandi
                               :dark dracula
                               ;; just being explicit...
                               :no-preference nil)
  "Light and dark theme preferences.")


(defvar gk-gui-theme nil
  "The default theme's name to load at startup.")


;; TODO(2020-06-13): stub
(defun gk-preferred-colour-scheme ()
  "Find out the system’s preferred colour scheme.

Returns :light if the preferred colour scheme is light,
:no-preference if no preference is set, or :dark if the user
prefers dark themes."
  (cond
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


(defun gk-setup-frame-looks (&optional frame)
  "Customisations that modify frame behaviour.

Groups such customisations which might need to be re-ran when a
new frame is created."
  (interactive)

  (ignore frame)

  (setf paper-tint-factor 70
        paper-base-font-size 70
        paper-use-varying-heights-for-org-title-headlines nil)

  (setf gk-gui-theme (plist-get gk-preferred-themes
                                (gk-preferred-colour-scheme)))

  (when (and gk-gui-theme
             (not (equal custom-enabled-themes
                         (list gk-gui-theme))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme gk-gui-theme t))

  ;; Customise wombat
  (when (eq gk-gui-theme 'wombat)
    ;; With wombat the active window is hard to tell.
    (set-face-attribute 'mode-line nil
                        :background "black"
                        :foreground "white")
    ;; Default added face renders foreground unreadable.
    (set-face-attribute 'diff-refine-added nil
                        :background "dark olive green")
    (set-face-attribute 'region nil :foreground nil)

    ;; Nicer, unambigous headline colours for Org mode headlines that
    ;; form a scale from green to red.  Also, make headlines a little
    ;; bit larger if ‘org-variable-pitch-minor-mode’ is enabled.
    (let ((colours ["yellow green"
                    "khaki"
                    "dark sea green"
                    "light sea green"
                    "steel blue"
                    "slate blue"
                    "orchid"
                    "hotpink"]))
      (dotimes (i 7)
        (let ((face (intern (format "org-level-%d" (1+ i)))))
          (set-face-attribute face nil :foreground (aref colours i) :height 1.0)
          (when (with-temp-buffer
                  (gk-org-visuals-hook)
                  org-variable-pitch-minor-mode)
            (set-face-attribute face nil :height 1.2))))))

  ;; Customise misterioso.
  (when (eq gk-gui-theme 'misterioso)
    (set-face-attribute 'header-line nil :background "black"
                        :foreground "white")
    (set-face-attribute 'show-paren-match nil
                        :foreground "green yellow"
                        :background nil)
    (set-face-attribute 'diff-refine-added nil
                        :background "sea green"))

  ;; Common customisations for misterioso and wombat.
  (when (memq gk-gui-theme '(wombat misterioso))
    ;; Make the cursor more visible, the default grey colour is
    ;; indistinguishable, especially with the bar cursor.
    (set-face-attribute 'cursor nil :background "hotpink")
    ;; Don't change the foreground or decorate the text when
    ;; ‘hl-line-mode’ is on.
    (set-face-attribute 'highlight nil
                        :foreground nil
                        :underline nil))

  (when (eq gk-gui-theme 'paper)
    ;; Better background colour for region.
    (set-face-attribute 'region nil :background "medium spring green"))

  (when (eq gk-gui-theme 'dracula)
    ;; Less prominent inactive modeline.
    (set-face-attribute
     'mode-line nil :box t :foreground nil :background nil :inherit 'mode-line)
    (set-face-attribute 'region nil :background "black"))

  (when (eq gk-gui-theme 'modus-operandi)
    (let ((bg (face-attribute 'org-block-begin-line :background))
          (fg (face-attribute 'org-block-begin-line :foreground)))
      (dolist (f '(org-block-begin-line org-block-end-line))
        (set-face-attribute f nil :background bg :extend t :foreground fg))
      (set-face-attribute 'org-block-begin-line nil :overline t)
      (set-face-attribute 'org-block-end-line nil :overline nil)
      (set-face-attribute 'org-block-end-line nil :underline t)
      (set-face-attribute 'org-block nil :background bg :extend t)))

  ;; Settings for when using default theme specifically.
  (unless gk-gui-theme
    (set-face-attribute 'region nil :background "yellow green")

    ;; Distinctively fontify org mode blocks.
    (let ((color "steel blue"))
      (set-face-attribute 'org-meta-line nil
                          :background color :extend t
                          :foreground "white")
      (set-face-attribute 'org-block nil
                          :background (color-lighten-name color 40)
                          :inherit 'normal :extend t)))

  (when (eq gk-gui-theme 'yoshi)
    (set-face-attribute 'org-level-1 nil :underline nil)
    (set-face-attribute 'org-level-2 nil :weight 'regular)
    (set-face-attribute 'org-level-3 nil :italic nil))

  ;; Region should not have a foreground colour.
  (set-face-attribute 'region nil :foreground nil)

  (set-face-attribute 'default nil
                      :height gk-font-default-height
                      :family (gk-font :mono)
                      :weight 'light)

  (set-face-attribute 'variable-pitch nil
                      :height gk-font-variable-pitch-height
                      :family (gk-font :sans))

  (loop for attr in '(mode-line mode-line-inactive) do
        (set-face-attribute attr nil
                            :family (gk-font :sans)
                            :weight 'normal))

  ;; This inherits from mode-line, so we need to fix it for its proper
  ;; display.
  (set-face-attribute 'header-line nil
                      :family (gk-font :mono)
                      :height gk-font-default-height)

  ;; Special font for moon phase visualisation in forecast.el.
  (set-face-attribute 'forecast-moon-phase nil
                      :font (gk-font :forecast-moon-phase))

  ;; Make docstrings stand out.
  (set-face-attribute 'font-lock-doc-face nil :bold t)

  ;; Make parentheses more obvious.
  (set-face-attribute 'parenthesis nil :foreground nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'show-paren-match nil :background nil  :inverse-video t)
  (set-face-attribute 'show-paren-mismatch nil :inherit 'warning)
  (set-face-attribute 'hl-paren-face nil :underline t :bold t)

  ;; Adapts ‘highlight-parentheses-mode’ colours to theme.
  (let ((c (cond ((eq gk-gui-theme 'zenburn) "#ff4500") ;orange red
                 ((eq gk-gui-theme 'wombat)  "#b22222") ;firebrick
                 ((eq gk-gui-theme 'dichromacy) "#8b008b") ;navy
                 (t (face-attribute 'font-lock-keyword-face :foreground)))))
    (setf hl-paren-colors
          (list
           (color-lighten-name c 10)
           (color-lighten-name c 20)
           (color-lighten-name c 30)
           (color-lighten-name c 40))))

  ;; Have a bit more line-spacing.
  (setq-default line-spacing 0.2))

(when (gk-gui-p)
  (add-to-list 'gk-disabled-modes 'tool-bar-mode)
  (add-to-list 'gk-disabled-modes 'menu-bar-mode)
  (add-to-list 'gk-disabled-modes 'scroll-bar-mode)

  ;; Fixes blank area above window after startup with Athena.
  (setf x-frame-normalize-before-maximize t)

  (add-hook 'after-init-hook #'gk-setup-frame-looks)

  (add-hook 'after-make-frame-functions #'gk-setup-frame-looks)

  ;; Set up cursors:
  (setq-default cursor-type 'box)
  (setq-default cursor-in-non-selected-windows 'hollow)

  (dolist (hook '(special-mode-hook dired-mode-hook rmail-mode-hook rmail-summary-mode-hook))
    (add-hook hook ($ (setq-local cursor-type 'box)))))



;;;;; Lines:

(setf
 ;; Truncate long lines.
 truncate-lines t
 ;; Behave according to `truncate-lines'.
 truncate-partial-width-windows nil
 ;; Use default fringe indicators for ‘visual-line-mode’ too.
 visual-line-fringe-indicators
 (assoca '(continuation) fringe-indicator-alist))



;;;;; UI Semantics:
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
   ("Calendar" . (display-buffer-in-side-window . ((side . bottom))))
   ("help\\[R\\]" . (display-buffer-pop-up-window))
   ("\\*pager\\*.*" . (display-buffer-pop-up-window
                       . ((inhibit-same-window . t))))
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
 ;; «Skip ‘fontification_functions‘ when there is input pending.»
 redisplay-skip-fontification-on-input t
 ;; Each buffer has its own goto-line history.
 goto-line-history-local t
 ;; Scale header lines with buffer when zooming.
 text-scale-remap-header-line t)


(setq-default save-place t)

(setf frame-title-format
      '("%@%*[" (:eval (or (frame-parameter nil 'gk-project) "main")) "] %b")
      icon-title-format frame-title-format)



;;;;; Startup:

;; No start screens.
(setf
 inhibit-startup-screen t
 inhibit-startup-echo-area-message (eval-when-compile (user-login-name)))



;;;;; Winner, windmove, and switch-window:

(setq winner-dont-bind-my-keys t)

(add-to-list 'gk-global-modes 'winner-mode)



(windmove-default-keybindings)
(when (fboundp 'windmove-delete-default-keybindings)
 (windmove-delete-default-keybindings))


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



;;;;; Minibuffer:

(setf enable-recursive-minibuffers t)

(defun gk-minibuf-hook ()
  "Do not fiddle with minibuffer keys."
  (gk-minor-mode -1))

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



;;;;; Mode line:

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
     (let* ((f (or (buffer-file-name) default-directory))
            (... (gk-ellipsize-file-or-directory-name f 25)))
       (propertize
        ...
        'help-echo (concat f "\nmouse-1: Copy full path of buffer to clipboard")
        'mouse-face 'mode-line-highlight
        'local-map (make-mode-line-mouse-map
                    'mouse-1 (lambda (event)
                               (interactive "e")
                               (with-selected-window (posn-window (event-start event))
                                 (gk-copy-buffer-file-name)))))))))
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



;;;;; Goto-address mode:

(defun gk-start-global-address-mode ()
  (goto-address-mode +1))

(dolist (m '(text-mode-hook prog-mode-hook comint-mode-hook))
  (add-hook m 'gk-start-global-address-mode))

(diminish 'goto-address-mode "⚓")

(cl-pushnew "gemini://" goto-address-uri-schemes :test #'string=)
(cl-pushnew "gopher://" goto-address-uri-schemes :test #'string=)

;; C-Return on an adress follows it.
(define-key goto-address-highlight-keymap (kbd "<C-return>") #'goto-address-at-point)



;;;;; Scrolling:

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



;;;;; Mouse:

(pushnew 'pixel-scroll-mode gk-global-modes)

(setq
 ;; Scroll smoother, no hurries.
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil))
 mouse-wheel-progressive-speed nil
 ;; Horizontal scroll.
 mouse-wheel-tilt-scroll t
 mouse-wheel-flip-direction t)



;;;;; TRAMP:

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
               "/run/current-system/profile/sbin")))



;;;;; Whitespace:

(setf whitespace-style '(face trailing tabs)
      ;; When nil uses ‘fill-column’.
      whitespace-line-column nil)

(setcdr (assoc 'tab-mark whitespace-display-mappings) '(9 [?> 9]))
(setcdr (assoc 'newline-mark whitespace-display-mappings) '(10 [?$ 10]))

(pushnew 'global-whitespace-mode gk-global-modes)
(diminish 'global-whitespace-mode "¶")

(setq-default highlight-indent-guides-method 'column)



;;;;; Persistent scratch:

(setf
 ;; Save all that's possible.
 persistent-scratch-what-to-save '(major-mode point narrowing text-properties)
 persistent-scratch-save-file (locate-user-emacs-file "etc/+scratch+"))
(persistent-scratch-setup-default)



;;;;; Coding system:

;; Use UTF-8 encoding everywhere.

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;;;; Ido and Smex:

(define-minor-mode gk-ido-smex-mode
  "Minor mode to govern ‘ido-mode’ and ‘smex-mode’.

\\{gk-ido-smex-mode-map}"
  nil ""
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

(define-globalized-minor-mode global-gk-ido-smex-mode
  gk-ido-smex-mode gk-ido-smex-mode)

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



;;;;; Undo:

;; Enable undo-tree.
(cl-pushnew 'global-undo-tree-mode gk-global-modes)



;;;;; Saving files:

(define-advice write-file
    (:filter-args (args) always-confirm)
  "Make sure ‘write-file’ confirms overrides and asks to create directories.

When called interactively."
  (list (car args) (called-interactively-p)))



;;;; Internet:
;;;;; URLs:

;; This is my URL browsing system, which is a big customisation of the
;; Emacs browse-url system which modifies some sorts of URLs for
;; better browsing experience, uses apt Emacs modes to display some
;; files instead of the browser, and prompts whether or not to open
;; URLs in EWW or not.

;; Zero this out first.
(setf browse-url-browser-function #'gk-browse-url)



;;;;;; Common:

(defconst gk-ytl-format
  "http://localhost:3991/ytl.html?v=%s"
  "The url for lite youtube player, %s for where to insert video id.")

(defalias 'gk-urls-external-browser 'browse-url-xdg-open)
(setf browse-url-firefox-program
      (or (gk-executable-ensure "firefox" t)
          (expand-file-name "~/Applications/firefox/firefox")))

;; TODO Check if still relevant when switch to Emacs 25.
;; Replacement for odd standard implementation.
;; See: http://emacshorrors.com/posts/computer-says-no.html
(defun browse-url-can-use-xdg-open ()
  "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser."
  (and window-system (executable-find "xdg-open")))


(defvar browse-url-qutebrowser-program "qutebrowser")
(defvar browse-url-qutebrowser-arguments nil)

(defun browse-url-qutebrowser (url &optional new-window)
  "Ask Qutebrowser to load URL. "
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "qutebrowser " url) nil
           browse-url-qutebrowser-program
           (append
            browse-url-qutebrowser-arguments
            (when new-window
              '("--target=window"))
            (list url)))))



;;;;;; Browser functions:

;; Functions specific for browsing some websites, and the browser
;; function.

(defun gk-urls-browse-github/gitlab-commit (url &rest args)
  "Browse a Github/Gitlab URL.

Append .diff to the url.  Pass ARGS on to ‘gk-browse-url’."
  (browse-url (concat url ".diff")))

(defun gk-urls-browse-github-file (url &rest args)
  "Browse a file on github.

Redirect to the raw file url."
  (let* ((rawprefix "https://raw.githubusercontent.com/")
         (bits (split-string
                (car (url-path-and-query (url-generic-parse-url url))) "/"))
         (rawurl
          (s-join "/" (cons (cadr bits) (cons (caddr bits) (cddddr bits))))))
    (browse-url (concat rawprefix rawurl))))

(defun gk-urls-browse-github-raw (url &rest args)
  "Browse a GitHub raw URL as an Emacs file."
  (gk-urls-browse-file
   (replace-regexp-in-string
    "\\.github\\.com/" ".githubusercontent.com/" url)))

(defun gk-urls-browse-mpv (url &rest args)
  "Browse a URL via mpv."
  (if (y-or-n-p "Watch with mpv?")
      (and
       (save-window-excursion
         (let* ((dir "~/co/External/youtube-dl"))
           (async-shell-command
            (format "PYTHONPATH=%s %s -o- '%s' | mpv -"
                    dir
                    (expand-file-name "bin/youtube-dl" dir)
                    url)
            (generate-new-buffer "*MPV*"))))
       (message "Started mpv process for: %s" url))
    (gk-urls-external-browser url)))

(defun gk-urls-browse-cornucopia (url &rest args)
  "Browse a cornucopia URL."
  (apply #'gk-browse-url
         (replace-regexp-in-string "\\.local" ".net" url)
         args))

(defun gk-urls-add-to-emms (url &rest args)
  "Add an URL to EMMS."
  (emms-add-url url))

(defalias 'gk-browse-url 'browse-url-generic)




;;;;;; Download and open files with Emacs:

;; This mechanism here allows for downloading and opening files with
;; emacs where that makes sense.  See the section ‘File adapters’ for
;; the adapters.

;; To add a new adapter, simply: =(gk-urls-make-file-adapter "ext")=
;; where =ext= is the filename extension.

(defvar gk-urls-file-adapters nil)

(defun gk-urls-browse-file (url &optional ext cb)
  "Browse a file with the given extension.

URL is the URL to browse.
EXT is the extension, omit the leading dot.
CB is the optional callback, run after downloading the file,
given the path as the only argument.
Writes the data to a temporary file."
  (url-retrieve
   url (lambda (status &optional cbargs)
         (ignore cbargs)
         (unless (plist-get status :error)
           (let ((fil  (make-temp-file
                        (concat "gkbrowse-" ext)
                        nil
                        (when ext
                          (concat "." ext)))))
             (write-region
              ;; Two consequtive newlines delimit the headers section.
              (save-excursion
                (goto-char (point-min))
                (re-search-forward "\n\n") (point))
              (point-max) fil)
             (kill-buffer)
             (when cb (funcall cb fil))
             (find-file fil))))))

;; TODO(2018-05-25): Make this support regexps as EXT.
(defmacro gk-urls-make-file-adapter (ext &optional arg &rest body)
  "Create adapters for `gk-urls-browse-file'.

ARG and BODY are used to make a callback to that function, if both
provided."
  (declare (indent defun))
  (when (string= ext "file")
    ;; It would override `gk-urls-browse-file'.
    (error
     "`file' can't be an extension for `gk-urls-make-file-adapter'"))
  (let ((funsym (intern (concat "gk-urls-browse-file--" ext)))
        ;; Make case insensitive match for extension.
        (reg (concat
              "\\."
              (let* (ret
                     (bits (reverse
                            (dolist (ch (string-to-list ext) ret)
                              (push
                               (let* ((ch1 (char-to-string ch))
                                      (ch2 (upcase ch1)))
                                 (concat "[" ch2 ch1 "]"))
                               ret)))))
                (mapconcat 'identity bits ""))
              "/?$")))
    `(progn
       (pushnew
        '(,reg . ,funsym)
        gk-urls-file-adapters
        :test 'equal)
       (defun ,funsym (url &rest args)
         (ignore args)
         ,(concat (upcase ext) " adapter for `gk-urls-browse-file'.")
         (gk-urls-browse-file
          url ,ext ,(when (and arg body)
                      `(lambda (,arg) ,@body)))))))



;;;;;; File adapters:

(gk-urls-make-file-adapter "pdf")
(gk-urls-make-file-adapter "jpeg")
(gk-urls-make-file-adapter "jpg")
(gk-urls-make-file-adapter "png")
(gk-urls-make-file-adapter "gif")
(gk-urls-make-file-adapter "patch")
(gk-urls-make-file-adapter "diff")
(gk-urls-make-file-adapter "txt")
(gk-urls-make-file-adapter "md")
(gk-urls-make-file-adapter "tex")
;;(gk-urls-make-file-adapter "c\\(c\\|pp\\|++\\|xx\\)?")
;;(gk-urls-make-file-adapter "h\\(h\\|pp\\|++\\|xx\\)?")
(gk-urls-make-file-adapter "el")
(gk-urls-make-file-adapter "scm")
(gk-urls-make-file-adapter "lisp")
(gk-urls-make-file-adapter "py")
(gk-urls-make-file-adapter "rb")
;;(gk-urls-make-file-adapter "p[lm]?6?")



;;;;;; Set the browse-url functions:

(setf browse-url-generic-program
      (executable-find "xdg-open")
      browse-url-handlers
      `(("\\(youtube\\.com\\|youtu\\.be\\)/" . gk-urls-browse-mpv)
        ("invidio\\.us/" . gk-urls-browse-mpv)
        ("^https?://\\(github\\|gitlab\\).com/.*?/.*?/\\(commit\\|compare\\)/[a-z0-9]+$" .
         gk-urls-browse-github/gitlab-commit)
        ("^https?://github\\.com/.*?/.*?/blob/" . gk-urls-browse-github-file)
        ("^https?://raw\\.github\\.com/" . gk-urls-browse-github-raw)
        ("^http://www.cornucopia\\.local/" . gk-urls-browse-cornucopia)
        ("file:///home/.+/co/lisp/doc/HyperSpec/" . gk-browse-url)
        ,@gk-urls-file-adapters))



;;;;; SHR:

(setf shr-use-colors nil)



;;;;; EWW:

(setf eww-search-prefix "https://duckduckgo.com/html/?q=")

(defun gk-eww-download ()
  "Download URL under point."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url))
        (filename))
    (if (not url)
        (message "No URL under point")
      (setq filename
            (read-file-name "Download to: "
                            eww-download-directory
                            nil nil
                            (url-file-nondirectory url)))
      (url-retrieve url
                    'gk-eww-download-callback
                    `(,url ,filename)))))

(defun gk-eww-download-callback (status url filename)
  (ignore url)
  (unless (plist-get status :error)
    (let* ((file (eww-make-unique-file-name filename "")))
      (write-file file)
      (message "Saved %s" file))))

;; Use my version of /eww-download/ for allowing the user to set the
;; target file.
(defalias 'eww-download #'gk-eww-download)

(defun gk-eww-save-link-as-kill (point)
  (interactive "d")
  (if-let* ((uri (get-text-property point 'shr-url)))
      (prog1 (kill-new uri)
        (message "Saved ‘%s’ to kill-ring" uri))
    (user-error "No URL under point")))

(defun gk-eww-up ()
  "Remove last directory or file part from the URI, go there."
  (interactive)
  (eww (replace-regexp-in-string "^\\([a-z]+:/+.+\\)/[^/]+/?$" "\\1/"
                                 (eww-current-url))))

(define-key eww-mode-map "^" 'gk-eww-up)
(define-key eww-mode-map "k" 'gk-eww-save-link-as-kill)

(defun gk-eww-mode-hook ()
  "Set up `eww' for easier reading."
  )

(add-hook 'eww-mode-hook 'gk-eww-mode-hook)




;;;;; Elpher:

(setf
 ;; Move bookmarks file to a private location.
 elpher-bookmarks-file (gk-org-dir-file "elpher-bookmarks.el"))

;; Some more emacsy keybindings.
(define-key elpher-mode-map "n" #'elpher-next-link)
(define-key elpher-mode-map "p" #'elpher-prev-link)
(define-key elpher-mode-map "l" #'elpher-back)
(define-key elpher-mode-map "^" #'elpher-back-to-start)



;;;;; Elfeed:



;;;;;; Variables:

;; Set the default filter.
(defvar gk-elfeed-default-filter "+unread ")
(setq-default elfeed-search-filter gk-elfeed-default-filter)

;; Set up the url-queue variables for swift and complete operation. The
;; defaults are too bad. Especially /url-queue-timeout/ is way too short
;; for loading feeds.
(setf url-queue-parallel-processes 20
      url-queue-timeout 10)



;;;;;; Some utility functions:

(defun gk-feeds-youtube (hash)
  "Return the feed URL for the channel with HASH."
  (concat "http://www.youtube.com/feeds/videos.xml?channel_id=" hash))

(defun gk-feeds-youtube-pl (playlist-id)
  "Return the feed URL for the playlist with PLAYLIST-ID."
  (concat
   "https://www.youtube.com/feeds/videos.xml?playlist_id="
   playlist-id))

(defun gk-elfeed-browse-article ()
  "View elfeed article with browser."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (browse-url link)))

(defun gk-elfeed-catch-up ()
  "C-x h, r, g in *elfeed-search* buffer."
  (interactive)
  (when (y-or-n-p "Catch-up on visible entries?")
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)
    (elfeed-search-update--force)))

(defun gk-elfeed-backup ()
  (interactive)
  (let ((name (gk-backup-file-name (dropbox "Backups/OPML/") "opml")))
    (elfeed-export-opml name)
    (message "Wrote %s." name)))

(defun gk-elfeed-filter (filter)
  "Set search filter, do not update live.

It is rather slow to do so."
  (interactive (list (read-string "Filter: " elfeed-search-filter)))
  (setq elfeed-search-filter filter)
  (elfeed-search-update :force))

(defun gk-elfeed-search-kill-url ()
  "Copy the URL for entry under point as kill."
  (interactive)
  (dolist (entry (elfeed-search-selected))
    (when-let* ((link (elfeed-entry-link entry)))
      (with-temp-buffer
        (insert link)
        (clipboard-kill-ring-save (point-min) (point-max))
        (message link)))))

(defalias 'gk-elfeed-next 'next-logical-line)
(defalias 'gk-elfeed-prev 'previous-logical-line)

(defun gk-elfeed-search-mode-hook ()
  )

(add-hook 'elfeed-show-mode-hook 'gk-eww-mode-hook)
(add-hook 'elfeed-search-mode-hook 'gk-elfeed-search-mode-hook)




;;;;;; Print entry function:

(defface gk-elfeed-feed-host
  '()
  "Face for displaying URLs’ host parts in Elfeed.")

(set-face-attribute 'gk-elfeed-feed-host nil :height .7 :italic nil)

(defun gk-elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer, with style."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    (cl-remove-if ($ (string= $1 "unread")) tags)
                    ","))
         (title-width (* (window-width)  .7))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               (window-width))
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (let ((f (split-string
              (buffer-local-value
               'elfeed-search-filter (get-buffer "*elfeed-search*"))
              " " t)))
      ;; If we’re not looking at a stored search, hide tags and don’t
      ;; limit title length.  Otherwise print the truncated title and
      ;; include the filtered tags.  Commit logs and VCS releases get
      ;; special treatment.
      (cond ((cl-member "logiciels" tags :test #'string=)
             (setq-local word-wrap t)
             (setq-local truncate-lines nil)
             (let* ((url (elfeed-feed-url feed))
                    (host (url-host (url-generic-parse-url url)))
                    (path (url-filename (url-generic-parse-url url))))
               (insert
                (cond ((cl-member "commits" tags :test #'string=)  "Commit  ")
                      ((cl-member "releases" tags :test #'string=) "Release ")
                      (t                                           "News    ")))
               (insert
                (cond ((string= host "github.com")
                       (format "gh:%-27s"
                               (mapconcat #'identity (butlast (split-string path "/" t)) "/")))
                      (t
                       (format "%30s" url)))
                " ")
               (insert (propertize title 'face title-faces 'kbd-help title))))
            ((and (string= (car f) "+unread")
                  (member (cadr f) gk-elfeed-search-ring-tags))
             (setq-local word-wrap t)
             (setq-local truncate-lines nil)
             (insert (propertize title 'face title-faces 'kbd-help (concat title ": " feed-title)) " ")
             (insert (propertize (concat
                                  "("
                                  (url-host
                                   (url-generic-parse-url
                                    (elfeed-feed-url feed)))
                                  ")")
                                 'face 'gk-elfeed-feed-host)))
            (t
             (setq-local truncate-lines t)
             (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
             (when tags
               (insert "(" tags-str ")")))))))


(setf elfeed-search-print-entry-function #'gk-elfeed-search-print-entry)



;;;;;; Keys:

(define-key elfeed-show-mode-map (kbd "v") #'gk-elfeed-browse-article)
(define-key elfeed-show-mode-map (kbd "!") #'gk-eww-download)
(define-key elfeed-search-mode-map (kbd "c") #'gk-elfeed-catch-up)
(define-key elfeed-search-mode-map (kbd "e") #'gk-elfeed-backup)
(define-key elfeed-search-mode-map (kbd "s") #'gk-elfeed-filter)
(define-key elfeed-search-mode-map (kbd "S") (gk-interactively (elfeed-db-save)))
(define-key elfeed-search-mode-map (kbd "J") #'elfeed-unjam)
(define-key elfeed-search-mode-map (kbd "n") #'gk-elfeed-next)
(define-key elfeed-search-mode-map (kbd "p") #'gk-elfeed-prev)
(define-key elfeed-search-mode-map (kbd "w") #'gk-elfeed-search-kill-url)



;;;;;; Search ring:

;; Go cycling a list of tags in the search mode.

(defvar gk-elfeed-search-ring-tags nil)

(defvar-local gk-elfeed-search-ring-current-search nil)

(defun gk-elfeed-search-ring-next ()
  (interactive)
  (let ((search (or (cadr (member gk-elfeed-search-ring-current-search
                                  gk-elfeed-search-ring-tags))
                    (car gk-elfeed-search-ring-tags))))
    (gk-elfeed-filter
     (concat gk-elfeed-default-filter
             (setf gk-elfeed-search-ring-current-search search)))))

(defun gk-elfeed-search-ring-previous ()
  (interactive)
  (gk-elfeed-filter
   (concat gk-elfeed-default-filter
           (setf
            gk-elfeed-search-ring-current-search
            (if (or (not gk-elfeed-search-ring-current-search)
                    (string= gk-elfeed-search-ring-current-search
                             (car gk-elfeed-search-ring-tags)))
                (car (last gk-elfeed-search-ring-tags))
              (nth (1- (or (position
                            gk-elfeed-search-ring-current-search
                            gk-elfeed-search-ring-tags
                            :test #'equal)
                           (length gk-elfeed-search-ring-tags)))
                   gk-elfeed-search-ring-tags))))))

(define-key elfeed-search-mode-map [?k] 'gk-elfeed-search-ring-previous)
(define-key elfeed-search-mode-map [?j] 'gk-elfeed-search-ring-next)



;;;;;; Faces:

(mapc
 (lambda (x) (apply #'set-face-attribute x))
 `((elfeed-search-title-face nil :foreground "normal" :strike-through t)
   (elfeed-search-unread-title-face nil :foreground "normal" :strike-through nil)
   (elfeed-search-tag-face nil :foreground "normal")
   (elfeed-search-date-face nil :foreground "normal")
   (elfeed-search-feed-face nil :foreground "normal" :weight bold)
   (elfeed-search-unread-count-face nil :foreground nil)))



;;;;;; Scoring:

;; Adapted from http://kitchingroup.cheme.cmu.edu/blog/2017/01/05/Scoring-elfeed-articles/

(defface gk-relevant-elfeed-entry `()
  "Marks a relevant Elfeed entry.")

(defface gk-important-elfeed-entry `()
  "Marks an important Elfeed entry.")

(push '(relevant gk-relevant-elfeed-entry)
      elfeed-search-face-alist)

(push '(important gk-important-elfeed-entry)
      elfeed-search-face-alist)

(defvar gk-elfeed-scoring-patterns
  nil
  "Patterns for scoring Elfeed entries.

An association list where car is a regexp to match the title or
the body of the entry, and the cdr is the score, an integer.")

(defun gk-score-elfeed-entry (entry)
  (let ((title (elfeed-entry-title entry))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (score 0))
    (let ((case-fold-search t))
      (loop for (pattern . n) in gk-elfeed-scoring-patterns
            if (string-match pattern title)
            do (incf score n)
            if (string-match pattern content)
            do (incf score n)))

    ;; store score for later in case I ever integrate machine learning
    (setf (elfeed-meta entry :my/score) score)

    ;; (cond
    ;;  ((= score 1)
    ;;   (elfeed-tag entry 'relevant))
    ;;  ((> score 1)
    ;;   (elfeed-tag entry 'important)))
    ;;
    ;; XXX(2018-12-21): this is an experiment where anything that’s
    ;; relevant is also important, given the distinction is irrelevant
    ;; given the scale.
    (when (>= score 1)
      (elfeed-tag entry 'relevant)
      (elfeed-tag entry 'important))
    entry))

;; Uncomment to enable
;; (remove-hook 'elfeed-new-entry-hook 'gk-score-elfeed-entry)




;;;;;; Show mode:

(add-hook
 'elfeed-show-mode-hook
 (defun gk-elfeed-show-mode-hook ()
   "Hook for ‘elfeed-show-mode’."
   (setq-local truncate-lines nil)
   (setq-local word-wrap t)))



;;;;;; Update completion notification:

(defun gk-elfeed-notify-update-completion (url)
  (ignore url)
  (when
      (and (null elfeed-curl-queue)
           (zerop elfeed-curl-queue-active))
    (gk-send-desktop-notification
     "elfeed: updated all feeds"
     "All feeds have been updated"
     "application-rss+xml")))

(add-hook 'elfeed-update-hooks #'gk-elfeed-notify-update-completion)



;;;;;; Feeds:

;; Load feeds from external source.
(gk-load (dropbox "feeds2") t)



;;;; After Save™:

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



;;;;; Other after save hooks:

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)




;;; Interactive aliases:




;;; Global keybindings:



;;;; Utilities:

(defmacro gk-global-binding (&rest args)
  (declare (indent defun))
  `(define-key gk-minor-mode-map ,@args))

(defmacro gk-prefix-binding (&rest args)
  (declare (indent defun))
  `(define-key gk-minor-mode-prefix-map ,@args))

(gk-prefix-binding "\M-u" 'gk-unbind-key)



;;;; Global overrides:

(gk-global-binding "\C-a" 'gk-bol)
(gk-global-binding "\M-j" 'gk-join-nl)
(gk-global-binding "\M-%" 'query-replace-regexp)
(gk-global-binding "\C-x\C-b" 'ibuffer)
(gk-global-binding "\M-;" 'gk-comment-dwim)
(gk-global-binding "\C-\M-q" 'gk-indent-defun)
(gk-global-binding "\C-xd" (gk-interactively (dired default-directory)))
(gk-global-binding "\C-x\C-f" #'gk-find-file)
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

(gk-prefix-binding (kbd "C-,") 'winner-undo)
(gk-prefix-binding (kbd "C-.") 'winner-redo)
(gk-prefix-binding (kbd "w p") 'windmove-up)
(gk-prefix-binding (kbd "w n") 'windmove-down)
(gk-prefix-binding (kbd "w f") 'windmove-right)
(gk-prefix-binding (kbd "w b") 'windmove-left)
(gk-prefix-binding "ws" 'gk-swap-windows)
(gk-global-binding (kbd "C-.") 'other-window)
(gk-global-binding (kbd "C-,") (gk-interactively (other-window -1)))
(gk-global-binding (kbd "M-.") 'other-frame)
(gk-global-binding (kbd "M-,") (gk-interactively (other-frame -1)))
(gk-prefix-binding (kbd "+") 'enlarge-window)
(gk-prefix-binding (kbd "-") 'shrink-window)
(gk-prefix-binding (kbd "C-;") 'goto-last-change)

(gk-prefix-binding "f" 'gk-find-text-footnote-definition)
(gk-prefix-binding (kbd "<mouse-1>")
  'gk-find-text-footnote-definition--mouse)

;; Flash to show point
(gk-prefix-binding "\M-f" #'gk-flash-current-line)
(gk-global-binding (kbd "<f1>") #'gk-flash-current-line)

(gk-global-binding (kbd "C-M-s") #'gk-search-forward-1)
(gk-global-binding (kbd "C-M-r") #'gk-search-backward-1)

;; Windmove
(gk-global-binding (kbd "<up>") #'windmove-up)
(gk-global-binding (kbd "<down>") #'windmove-down)
(gk-global-binding (kbd "<right>") #'windmove-right)
(gk-global-binding (kbd "<left>") #'windmove-left)



;;;; Editing:

(gk-prefix-binding "\C-\ " 'gk-eat-spaces-to-the-right)
(gk-prefix-binding "i" 'gk-cycle-input-methods)
(gk-prefix-binding "u" 'gk-upcase-this-or-previous-word)
(gk-prefix-binding "l" 'gk-lowercase-this-or-previous-word)
(gk-global-binding "\C-z" 'gk-cycle-input-methods)

;;(gk-prefix-binding (kbd "L") gk-lingua-prefix-map)
(gk-global-binding (kbd "\M-\ ") (gk-interactively (insert " ")))

;; Transpose
(gk-prefix-binding "\M-p" 'transpose-paragraphs)
(gk-prefix-binding "\M-l" 'transpose-lines)
(gk-prefix-binding "\M-s" 'transpose-sentences)

;; Comparison
(gk-prefix-binding (kbd "C-=") 'diff-buffer-with-file)



;;;; Current buffer:

;; Do things with the current buffer.

(gk-prefix-binding "bc" #'gk-copy-buffer-file)
(gk-prefix-binding "bn" #'gk-copy-buffer-file-name)
(gk-prefix-binding "bD" #'gk-delete-buffer-file)
(gk-prefix-binding "br" #'gk-rename-buffer-file)



;;;; Programming:

(gk-prefix-binding "d" 'xref-find-definitions)
(gk-prefix-binding [?\r] #'gk-project-compile)

;;;; Shortcuts:

(gk-prefix-binding "k" 'recompile)
(gk-prefix-binding "\M-d" (gk-interactively (toggle-debug-on-error)
                                            (toggle-debug-on-quit)))
(gk-prefix-binding "r" 'rename-buffer)
(gk-prefix-binding "n" 'other-frame)
(gk-global-binding [home] 'gk-home)
(gk-prefix-binding "h" (gk-interactively
                        (when-let* ((b (get-buffer "*Help*")))
                          (switch-to-buffer b nil t))))
(gk-prefix-binding (kbd "C-#") 'display-line-numbers-mode)
(gk-prefix-binding "_" 'delete-other-windows-vertically)
(gk-prefix-binding "2" #'clone-indirect-buffer)
(gk-prefix-binding "t" #'gk-insert-today)
(gk-prefix-binding "\M-#" #'gk-insert-todo-comment)
(gk-prefix-binding "~" #'gk-toggle-wrap)
(gk-prefix-binding "]" #'gk-pop-shell)
(gk-prefix-binding "\C-]" #'gk-display-shell)
(gk-prefix-binding "[" #'window-toggle-side-windows)
(gk-prefix-binding "=" #'menu-bar-mode) ;toggle
(gk-prefix-binding "g" #'magit-status)
(gk-prefix-binding "M-." #'repeat-complex-command)
(gk-prefix-binding "\C-f" #'project-find-file)
(gk-prefix-binding "\C-p" #'gk-open-project)
(gk-prefix-binding (kbd "C-+") #'gk-create-project)

(gk-prefix-binding "\M-i" #'gk-visit-user-init-file)

(gk-prefix-binding (kbd "R") #'gk-reading-setup)

(gk-global-binding "\C-xw" #'gk-jump-to-window)

(gk-global-binding [mouse-8] #'scroll-down-command)
(gk-global-binding [mouse-9] #'scroll-up-command)

(gk-prefix-binding "x" #'gk-maximize)
(gk-prefix-binding (kbd "C-f") #'gk-flip)

(gk-prefix-binding "e" #'ebib)



;;;; Multiple cursors:

(gk-prefix-binding "mm" 'mc/edit-lines)
(gk-prefix-binding "mn" 'mc/mark-next-like-this-symbol)
(gk-prefix-binding "mp" 'mc/mark-previous-like-this-symbol)
(gk-prefix-binding "m*" 'mc/mark-all-dwim)
(gk-prefix-binding "ma" 'mc/edit-beginnings-of-lines)
(gk-prefix-binding "me" 'mc/edit-ends-of-lines)



;;;; Org-mode related bindings:

(gk-prefix-binding "of" 'gk-org-dir-find-file)
(gk-prefix-binding "os" 'org-store-link)
(gk-prefix-binding "oe" 'gk-org-pdf-subtree)
(gk-prefix-binding "od" 'gk-org-decrypt-element)
(gk-prefix-binding "c" 'org-capture)
(gk-prefix-binding "oj" #'org-babel-tangle-jump-to-org)
(gk-prefix-binding "o." #'gk-org-export-this-tree)

;; Agenda:

;; no surprises
(gk-prefix-binding "oa" 'org-agenda)
(gk-prefix-binding "a" 'org-agenda)

;; Custom views
(gk-prefix-binding "o1" #'gk-org-display-single-pane-agenda-view)
(gk-prefix-binding "o2" #'gk-org-display-two-pane-agenda-view)
(gk-prefix-binding "oo" #'gk-org-display-single-pane-agenda-view)

(gk-global-binding (kbd "C-M-o") #'gk-org-display-planner-frame)




;;;; Text editing:

(gk-prefix-binding [? ] 'gk-mark-thing)



;;;; Spelling:

(gk-prefix-binding "ss" 'flyspell-mode)
(gk-prefix-binding "sw" 'ispell-word)
(gk-prefix-binding "sb" 'ispell-buffer)
(gk-prefix-binding "sr" 'ispell-region)



;;;; Mail:

(gk-prefix-binding "<" #'gk-fetch-mail)

(gk-prefix-binding ">" #'gk-runq)
(gk-prefix-binding "?" #'mairix-widget-search)



;;; Additional modules:

(when (eq system-type 'berkeley-unix)
  (require 'bsdpkg))



;;; EXWM:

(when (string= (getenv "EXWM") "yes")
  (load (locate-user-emacs-file "exwm-init")))



;;; Finalise initialisation:

(unless noninteractive
  ;; Start the server.
  (server-start)
  (add-hook 'server-switch-hook 'raise-frame)
  (setf initial-buffer-choice (gk-org-dir-file "Todo.org")))

(gk-load (file-name-sans-extension custom-file))



;;; Footer:
(provide 'init)
;;; init.el ends here
;;;; Auto-generated stuff:
(put 'not-modified 'disabled t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'timer-list 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)
