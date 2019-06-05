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

;; Use elisp directory listing program.  This needs to be set before
;; loading ls-lisp.el.
(defvar ls-lisp-use-insert-directory-program nil)

;; Prefer newer files when loading:
(setq load-prefer-newer t)



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
(require 'desktop)
(require 'diff)
(require 'diminish)
(require 'dired)
(require 'dired-narrow)
(require 'dired-subtree)
(require 'dired-x)
(require 'doc-view)
(require 'dollar)
(require 'eimp)
(require 'eldoc)
(require 'elfeed)
(require 'epa)
(require 'epa-mail)
(require 'epg)
(require 'etags)
(require 'eval-sexp-fu)
(require 'eww)
(require 'f)
(require 'face-remap) ; buffer-face-mode
(require 'ffap)
(require 'files)
(require 'flyspell)
(require 'forecast)
(require 'git-commit)
(require 'gk-greek)
(require 'gk-unilat)
(require 'goto-addr)
(require 'goto-last-change)
(require 'highlight-parentheses)
(require 'hl-line)
(require 'ibuffer)
(require 'ibuffer-vc)
(require 'image)
(require 'image-dired)
(require 'imenu)
(require 'inf-lisp)
(require 'inf-ruby)
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
(require 'message)
(require 'mm-url)
(require 'multiple-cursors)
(require 'netrc)
(require 'nnfolder)
(require 'nsm)
(require 'org)
(require 'org-capture)
(require 'org-checklist)
(require 'org-eww)
(require 'org-inlinetask)
(require 'org-mobile)
(require 'org-protocol)
(require 'org-variable-pitch)
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
(require 'pass-listing)
(require 'perl-mode)
(require 'persistent-scratch)
(require 'pixel-scroll)
(require 'pp)
(require 'project)
(require 'python)
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
(require 'smtpmail)
(require 'subr-x)
(require 'thingatpt)
(require 'thinks)
(require 'time)
(require 'tramp)
(require 'tramp-cache)
(require 'uniquify)
(require 'url)
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

(setf image-dired-dir (locate-user-emacs-file "etc/image-dired")
      url-configuration-directory (locate-user-emacs-file "etc/url")
      auto-save-list-file-prefix (locate-user-emacs-file
                                  "etc/auto-save-list/saves-")
      bookmark-default-file (dropbox "bookmarks.el")
      bbdb-file (expand-file-name "~/doc/bbdb")
      savehist-file (locate-user-emacs-file "etc/history")
      eww-bookmarks-directory (dropbox ".")
      save-place-file (locate-user-emacs-file "etc/places")
      tramp-persistency-file-name (locate-user-emacs-file "etc/tramp")
      custom-file (locate-user-emacs-file "etc/custom.el")
      nsm-settings-file (locate-user-emacs-file "etc/network-security.data")
      mc/list-file (locate-user-emacs-file "etc/mc-lists.el"))

(defvar gk-website-settings
  (expand-file-name "~/doc/not/www/publish")
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

(defun gk-executable-ensure (command)
  "Err-out if COMMAND is not found."
  (or (executable-find command)
      (warn "Program is absent: %s" command)))

(defun gk-get-file-contents (file)
  "Get the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring (point-min) (point-max))))

(defun gk-deadvice (sym)
  "Remove all the advice functions from the function named SYM."
  (interactive "aRemove advices from function: ")
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

(defun gk-maybe-expand-abbrev-or-space ()
  (interactive)
  (when (null (expand-abbrev))
    (insert " ")))

(defun gk-numeronym (name)
  "Generate a numeronym of NAME, an arbitrary string.
A numeronym is the initial letter, the length of the name in
characters, and the last letter,
i.e. i18n -> internationalisation."
  (interactive (list (read-string "Enter the name to be numeronymified: ")))
  (let ((len (length name)))
    (unless (>= len 2) (user-error "The name must be at least three characters long"))
    (message (format "%c%d%c" (aref name 0) (- len 2) (aref name (1- len))))))

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

(defun gk-wikipedia-link-to-wikizero (url)
  "Convert a Wikipedia URL to a WikiZero one.
WikiZero is a mirror of wikipedia."
  (interactive
   (list (read-string "Wikipedia URL: ")))
  (let* ((baseurl "http://www.wiki-zero.net/index.php?q=")
         (wiki64 (replace-regexp-in-string
                  "=+$" ""
                  (base64-encode-string (url-unhex-string url) t))))
     (concat baseurl wiki64)))

(defun gk-update-package-load-paths ()
  "Clean up the ‘load-path’, find and add new packages."
  (interactive)
  (setf load-path (seq-uniq load-path #'string=))
  (let ((packages (directory-files (locate-user-emacs-file "packages")
                                   directory-files-no-dot-files-regexp))
        new)
    (if (dolist (package packages new)
          (let ((l (length load-path)))
            (unless (= l (length (pushnew package load-path :test #'string=)))
              (push package new))))
        (message "New package(s): %S" new)
      (message "No new packages were found"))))

(defun gk-send-desktop-notification (summary message)
  "Show a notification on the desktop."
  (unless (gk-gui-p)
    (error "Cannot send desktop notification in non-GUI session"))
  (make-process
   :name "gk-desktop-notification"
   :buffer (get-buffer-create " *Desktop Notifications*")
   :command
   (cond
    ((executable-find "notify-send")
     (list "notify-send" (concat "[Emacs] " summary) message))
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
       (point)))))

(defvar gk-insert-todo-comment--history nil)
(defvar gk-insert-todo-comment-default "TODO")

(defun gk-insert-todo-comment (keyword)
  (interactive
   (list
    (read-string
     (format "Todo keyword to use (default: %s): "
             gk-insert-todo-comment-default)
     nil 'gk-insert-todo-comment--history "TODO" t)))
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
  (byte-recompile-directory (locate-user-emacs-file "lisp/site") 0 (> force 4))
  (when custom-file
    (byte-recompile-file custom-file (> force 1) 0))
  (byte-recompile-file user-init-file))



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
    (view-buffer buf 'kill-buffer)))

;; Make sure that emacs itself uses ‘more’,  necessary for ‘man’ command.
(setenv "PAGER" "more")

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
  (let ((out-buf (get-buffer-create gk-load-test-output-buffer-name)))
    (switch-to-buffer-other-window out-buf)
    (insert "Testing emacs initialisation...\n\n")
    (start-process
     gk-load-test-process-name
     out-buf
     gk-emacs-executable
     "-Q" "--batch" "-l" gk-load-test-file)))



;;;; Utility macros:

;; Some lisp macros for this file.

(defmacro when-fbound (proc &rest args)
  "Run proc if bound.
\(when-fbound PROC ARGS...)"
  `(when (fboundp (quote ,proc))
     (,proc ,@args)))

(defmacro gk-interactively (&rest body)
  "Wrap the BODY in an interactive lambda form.
Return the lambda."
  `(lambda nil (interactive) ,@body))

(defmacro gk-with-new-frame (parameters &rest body)
  "Create a new frame and run BODY in it.
PARAMETERS are passed into ‘make-frame’."
  (declare (indent defun))
  (let ((frame (gensym)))
    `(let ((,frame (make-frame ,parameters)))
       (raise-frame ,frame)
       (select-frame-set-input-focus ,frame)
       (progn ,@body))))

(defmacro setc (variable value)
  "Exactly like setq, but handles custom."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))



;;;; Association lists:

;; Helper functions for association lists.

(defun dissoc (key list &optional arg)
  "Delete pairs whose car is `equal' to KEY from LIST.

ARG is an internal argument."
  (let ((p (car list))
        (r (cdr list)))
    (if list
        (if (equal (car p) key)
            (dissoc key r arg)
          (dissoc key r (append arg (list p))))
      arg)))

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

(defun gk-toggle-global-modes (&optional disable)
  "Enable or disable the modes listed in `gk-global-modes'.

If DISABLE is  non-nil, call each of those modes  with a negative
integer argument, otherwise positive."
  (interactive "P")
  (let (errors)
    ;; Enable global modes
    (dolist (mode gk-global-modes)
      (condition-case e
          (funcall mode (if disable -1 1))
        (error (push `(,mode ,e) errors))))
    ;; Disable modes in gk-disabled-modes
    (dolist (mode gk-disabled-modes)
      (condition-case e
          (funcall mode -1)
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
        :forecast-moon-phase "Quivira")
  "A plist, default fonts.")

;; Set up so that there's 75-80 chars width for half-sized horizontal
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
 backup-directory-alist `(("." . ,(expand-file-name "~/.backups"))))



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

;; Comint settings specific to Shell mode, and settings for this mode.

(add-hook 'shell-mode-hook 'gk-shell-mode-hook)

(defun gk-pop-shell (arg)
  "Pop a shell in a side window.
Pass arg to ‘shell’.  If already in a side window that displays a
shell, toggle the side window.

If there is a project shell associated to the frame, just show
that instead."
  (interactive "P")
  (if (and (assoca 'window-side (window-parameters))
           (equal major-mode 'shell-mode))
      (window-toggle-side-windows)
    (when-let* ((win (display-buffer-in-side-window
                      (save-window-excursion
                        (let ((prefix-arg arg))
                          ;; If can find a project shell, show that
                          ;; instead.
                          (if-let* ((project-shell
                                     (ignore-errors
                                       (get-buffer
                                       (assoca
                                        'gk-project-shell (frame-parameters))))))
                              project-shell
                            (call-interactively #'shell))))
                      '((side . bottom)))))
      (select-window win))))

(defun gk-shell-mode-hook ()
  "Hook for `shell-mode'."
  ;; BSD /bin/sh echoes.
  (when (and (not (memq system-type '(gnu gnu/linux gnu/kfreebsd)))
             (string-match "/k?sh$" (getenv "SHELL")))
    (setq-local comint-process-echoes t))
  ;; Compilation shell minor mode activates certain parts of command
  ;; output as clickable links to parts of files (e.g. grep -Hn).
  (compilation-shell-minor-mode 1))



;;;; Dired:



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



;;;;; Customisations:

(setf
 ;; Omit ., .., #*, *~, ~,v; and some other stuff.
 dired-omit-files
 (rx (or (or (and bol (or "." "#") (optional (1+ ".")))
             (and (or "~" ",v") eol))
         (and bol (or "__pycache__"))))
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
(define-key dired-mode-map (kbd "C-c y") 'gk-dired-copy-marked-file-paths-as-kill)



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



;;;;; Unicode input minor mode (obsolete?):

(defvar gk-ucins-entry-mode-map
  (make-sparse-keymap))

(define-minor-mode gk-ucins-entry-mode
  "Minor mode for definition of some shortcuts for UTF8 characters."
  nil " Ucins"
  gk-ucins-entry-mode-map)

(defvar gk-ucins-combining-diacritic-keybindings-alist nil)
(defvar gk-ucins-combining-diacritic-keybindings-prefix nil)
(defvar gk-ucins-character-shortcuts-alist nil)
(defvar gk-ucins-character-shortcuts-prefix nil)

(defun gk-ucins-set-bindings (prefix binding-alist)
  "Set Ucins bindings.
Use PREFIX as prefix key.
Bindings come from BINDING-ALIST."
  (dolist (i binding-alist)
    (let* ((key (car i))
           (char (cdr i))
           (binding (concat prefix " " key))
           (fun `(lambda ()
                   (interactive)
                   (insert-char ,char))))
      (define-key gk-ucins-entry-mode-map
        (kbd binding) (eval fun)))))

(defun gk-ucins--update-hook ()
  "Hook for updating Ucins binding definitions."
  (gk-ucins-set-bindings
   gk-ucins-character-shortcuts-prefix
   gk-ucins-character-shortcuts-alist)
  (gk-ucins-set-bindings
   gk-ucins-combining-diacritic-keybindings-prefix
   gk-ucins-combining-diacritic-keybindings-alist))

(add-hook 'gk-ucins-entry-mode-hook #'gk-ucins--update-hook)



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
             (backward-word)
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
(diminish 'gk-ucins-entry-mode)
;; i.e. ‘auto-fill-mode’, but diminish does not like that.
(diminish 'auto-fill-function "=")

(defun gk-text-editing-modes-hook ()
  "Hook for `text-mode'."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (gk-ucins-entry-mode 1)
  (set-input-method default-input-method)
  (setq indent-tabs-mode nil))

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

(setf
 quail-keyboard-layout
 (concat
  "                              "
  " 1!2\"3£4$5%6^7&8*9(0)[{]}    "
  "  ’@,<.>pPyYfFgGcCrRlL/?=+   "
  "   aAoOeEuUiIdDhHtTnNsS-_#~   "
  "  \\|;:qQjJkKxXbBmMwWvVzZ      "
  "                                "))

(defvar gk-input-methods
  '("unilat-gk"
    "greek-translit-gk"
    "greek-ibycus4" ; ancient greek
    "ipa-x-sampa"
    "arabic"
    "hebrew"
    "armenian-translit"))

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



;;;;; Unicode input minor mode configuration:

;; This section configures the Unicode entry minor mode from above.

(setf gk-ucins-combining-diacritic-keybindings-alist
      '(("," . 807) ; COMBINING CEDILLA
        ("p" . 801) ; COMBINING PALATALIZED HOOK BELOW
        ("-" . 772) ; COMBINING MACRON
        (":" . 776) ; COMBINING DIAERESIS
        ))

(setf gk-ucins-character-shortcuts-alist
      '(("s" . ?ʃ) ; IPA Voiceless palato-alveolar sibilan fricative
        ("z" . ?ʒ) ; IPA Voiced palato-alveolar sibilan fricative
        ("!" . ?ʔ) ; IPA Glottal stop
        ("v" . ?✓)
        ("x" . ?❌)
        ("n" . ?№)
        ("h" . ?♥)
        ("t" . ?₺)
        ("+" . ?±)
        ("S" . ?§)
        ("P" . ?¶)
        ))

(setf gk-ucins-character-shortcuts-prefix
      "C-c 8")
(setf gk-ucins-combining-diacritic-keybindings-prefix
      gk-ucins-character-shortcuts-prefix)

(add-hook 'gk-minor-mode-hook #'gk-ucins-entry-mode)



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

;; Instead of using a Textile mode, just use =text-mode=.

(add-to-list 'auto-mode-alist '("\\.textile" . text-mode))



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



;;;;; Dictionary and spell checking:

(setq-default ispell-program-name "aspell")

(defun gk-ispell-hook ()
  "Hook to start spell-check in buffers."
  (flyspell-mode +1))

;; (add-hook 'text-mode-hook 'gk-ispell-hook)



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
(diminish 'gk-ucins-entry-mode)
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
          (issue-re "^[+\\- ]\\*+ \\(TODO\\|DONE\\) ")
          current-defun filename addp onlyp issuep)
      (save-excursion
        (with-current-buffer "COMMIT_EDITMSG"
          (goto-char (point-min))
          (unless (looking-at "^$")
            (goto-char (line-end-position))
            (throw 'dirty nil))
          (re-search-forward "^# Changes to be committed:" nil t)
          (forward-line)
          (beginning-of-line)
          (cond ((looking-at modified-re)
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

(defun gk-copy-git-forge-url-as-kill (file)
  "Generate a Github/Gitlab url for FILE and copy it as kill."
  (interactive (list (buffer-file-name)))
  (unless file (user-error "Buffer not visiting a file"))
  (if-let* ((dir (expand-file-name
                  (locate-dominating-file file ".git/config"))))
      (with-current-buffer (find-file-noselect
                            (expand-file-name ".git/config" dir))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (rx (and bol "[remote \"origin\"]")) nil t)
            (re-search-forward (rx (and bol "	url = ")))
            (if-let* ((str (gk--copy-git-forge-url-as-kill-1 dir file)))
                (progn
                  (when (save-match-data (string-match "github" str))
                    (setq str (replace-regexp-in-string "/tree/" "/blob/" str)))
                  (with-temp-buffer
                    (insert str)
                    (clipboard-kill-ring-save (point-min) (point-max))
                    (message str)
                    str))
              (error "Failed building Git forge url for %s" file)))))
    (user-error "Could not build a Git forge url")))

(defun gk--copy-git-forge-url-as-kill-1 (dir file)
  "Subroutine of ‘gk-copy-github-url-as-kill’."
  (cond
   ((looking-at (rx "https://git" (or "hub" "lab") ".com"))
    (concat
     (buffer-substring-no-properties (point) (line-end-position))
     "tree/"
     (magit-get-current-branch)
     "/"
     (replace-regexp-in-string
      (concat "^" (regexp-quote dir)) "" file)))
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
                  "/tree/"
                  (magit-get-current-branch)
                  "/"
                  (replace-regexp-in-string
                   (concat "^" (regexp-quote dir)) "" file))))))))



;;;;;; Magit:

(setf
 ;; No autorevert.
 magit-auto-revert-mode nil
 magit-auto-revert-immediately nil
 ;; Don’t pop up diff, commit --verbosely instead.
 magit-commit-show-diff nil
 magit-commit-arguments '("--verbose")
 ;; Exclude 3rd-party lisp from todos search.
 magit-todos-exclude-globs '("emacs.d/lisp/site/*" "*/patches/*"))

(cl-pushnew 'magit-todos-mode gk-global-modes)



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
  (setq-local indent-tabs-mode nil))

(add-hook 'prog-mode-hook #'gk-prog-mode-hook)



;;;;; Snippets:

(setf yas-snippet-dirs
      (list
       (locate-user-emacs-file "etc/snippets")))

(pushnew 'yas-global-mode gk-global-modes)

(diminish 'yas-minor-mode)



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
 c-default-style "k&r")

(add-hook 'c-mode-hook 'gk-algol-like-hook)



;;;;; Javascript:

(defun gk-javascript-hook ()
  "Standard JS hook."
  (highlight-parentheses-mode 1)
  (setq indent-tabs-mode nil
        js-indent-level 2))

(add-hook 'js-mode-hook 'gk-javascript-hook)
(add-hook 'js-mode-hook 'gk-algol-like-hook)



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
      ;; Please don't annoy me, and fuck you.
      python-indent-guess-indent-offset nil)

(defun gk-python-mode-hook ()
  )

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



;;;;; Makefiles:

(defun gk-makefile-hook ()
  "Generic hook for makefile modes."
  (gk-turn-on-outline-minor-mode "####* " ":$" "C-'")
  (setq-local indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'gk-makefile-hook)



;;;;; Shell scripts:

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

(defun gk-fetch-mail ()
  "Run mail retrieval scripts."
  (interactive)
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
                 (setf msg "You have unread mail! "))))
           (when (and (gk-gui-p) (not (string-empty-p msg)))
               (gk-send-desktop-notification "New mail" msg)))
         (message "%sFetch mail process %s" msg (string-trim event)))))))



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
 sendmail-program (gk-executable-ensure "msmtp-enqueue.sh"))

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

;; Spammers are everywhere.
(setf user-mail-address (concat "self" "@" "gkayaalp" "." "com")
      user-full-name "Göktuğ Kayaalp")



;;;;; Message mode:

(add-hook 'message-setup-hook 'bbdb-mail-aliases)

(setf
 message-citation-line-function 'message-insert-formatted-citation-line
 message-citation-line-format "On %Y-%m-%d %R %Z, %f wrote:")

(setf
 ;; Save a copy of outgoing messages to an mbox.
 message-default-headers (format "Fcc: %s/outbox" gk-mail-home)
 ;; Drafts directory.
 message-auto-save-directory (expand-file-name "drafts" gk-mail-home))

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
 (rx (and bol (or "to" "date" "from" "cc" "subject" "message-id" "list-id")))
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
      (gk-urls-external-browser
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

(defun gk-rmail-advance ()
  "Advance to the next message in default mbox.
This command will not run unless in an RMAIL buffer visiting
‘rmail-file-name’.  It will output the current message to
‘gk-rmail-archive-file’ and delete it, advancing to the next
message in the RMAIL file.  This is a utility for the email
workflow where a temporary inbox is used for working with current
email and archiving read mail in another file."
  (interactive)
  (unless (string= (buffer-file-name) rmail-file-name)
    (user-error
     "This is not your default RMAIL file, won't run ‘gk-rmail-advance’ here"))
  (rmail-output gk-rmail-archive-file)
  (rmail-delete-forward))

(dolist (f '(rmail-summary-previous-msg rmail-summary-next-msg))
 (advice-add f :around #'gk-ad-stay-here))

(define-key rmail-mode-map "<" #'gk-fetch-mail)
(define-key rmail-mode-map "N" #'gk-rmail-advance)
(define-key rmail-mode-map "S" #'gk-rmail-force-expunge-and-save)
(define-key rmail-mode-map "b"
  (gk-interactively (gk-rmail-view-html-part-in-browser)
                    (gk-rmail-advance)))

;; ‘q’ is normally bound to #'rmail-summary-quit, which is simply
;; useless.
(define-key rmail-summary-mode-map "q" #'bury-buffer)

(add-hook
 'rmail-mode-hook
 (defun gk-rmail-mode-hook ()
   (goto-address-mode +1)
   (setq-local word-wrap t)))



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



;;;; Org mode:



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

(defun gk-org-refill-reading-note ()
  "Refill a list item when taking reading notes from a PDF.
Account for soft hyphens."
  ;; TODO (2018-09-11): account for dash used as hyphen.
  (interactive)
  (let (g)
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
            (while (re-search-forward "­" nil t)
              (replace-match "")
              (while (looking-at " ")
                (delete-forward-char 1)))
            (fill-paragraph)
            (setq g (point-max))))))
    (goto-char g)))

(defun gk-org-insert-reading-note (page)
  "Insert a reading note into the reading notes file.
A note in that file has a certain structure, i.e. a list item
with the page number as the first thing, then the quote text,
which comes from the ‘kill-ring’ via ‘yank’ wrapped in
guillemets.  PAGE is the page number, and can be any string,
given how page numbers are realised varies in the real world."
  (interactive (list (read-string "Page number: ")))
  (unless (org-insert-item)
    (goto-char (line-beginning-position))
    (insert "- "))
  (insert "p. " page ": «")
  (yank)
  (insert "»\n")
  (gk-org-refill-reading-note)
  (when (y-or-n-p "Inserted reading note, save file now?")
    (save-buffer)))

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



;;;;; Variables:

(setf
 ;; Open files in the same window.
 (cdr (assoc 'file org-link-frame-setup)) 'find-file
 ;; Place tags right after the title.
 org-tags-column 0
 ;; If an #+attr_*: :width xxx available, use xxx, else, car of this
 ;; value.
 org-image-actual-width (list 300)
 ;; The minibuffer prompt is enough.
 org-popup-calendar-for-date-prompt nil
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
      org-directory (expand-file-name "~/doc/not/org")
      org-default-notes-file (gk-org-dir-file "start.org")
      org-icalendar-combined-agenda-file (gk-org-dir-file "ajanda.ics")
      org-id-locations-file (locate-user-emacs-file "etc/org-id-locations.el"))

(defvar gk-org-agenda-files nil
  "My agenda files.")

(defvar gk-org-project-agenda-files nil
  "List of files that contain per-project TODO items.")

(setf
 ;; gk-org-agenda-files (gk-org-dir-files "ajanda.org" "caldav.org")
 org-agenda-files gk-org-agenda-files)



;;;;; Mobile (obsolete):

(setf
 ;; Files to sync.
 org-mobile-files '("listeler.org")
 ;; Remote for org-mobile
 org-mobile-directory (gk-org-dir-file "mobile")
 ;; Buffer file for Android app
 org-mobile-inbox-for-pull (gk-org-dir-file "buffer.org"))



;;;;; Agenda:

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
 '((agenda time-up category-up habit-down priority-down)
   (todo priority-down category-up)
   (tags priority-down category-up)
   (search category-up))
 org-agenda-dim-blocked-tasks nil)

(setf org-agenda-block-separator nil)

(defun gk-org-agenda-mode-hook ()
  (orgstruct-mode +1))

(add-hook 'org-agenda-mode-hook #'gk-org-agenda-mode-hook)



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

(defconst gk-org-latex-classes-default-value
  (copy-list org-latex-classes)
  "Default value of `org-latex-classes'.")

(setf org-latex-toc-command "\\newpage\\gktoc\n\n")

(defun gk-org-latex-set-classes (backend)
  (when (equal backend 'latex)
    (let ((sect '(("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
      (setf org-latex-classes
            `(,@gk-org-latex-classes-default-value
              ;; From not/tex/*.tex.
              ,@(mapcar
                 (lambda (x)
                   (let ((snam (symbol-name x)))
                     `(,(concat "gk-" snam)
                       ,(gk-get-file-contents
                         (gk-org-dir-file (format "../tex/%s.tex" snam)))
                       ,@sect)))
                 '(article beamer cv)))))))

(add-hook 'org-export-before-parsing-hook 'gk-org-latex-set-classes)

(setf org-latex-pdf-process (list "latexmk -f -silent -bibtex-cond -xelatex %f")
      ;; Do remove many sorts of files the process generates...
      org-latex-remove-logfiles t
      ;; ...but keep some important log files.
      org-latex-logfiles-extensions
      (cl-remove-if ($ (member $1 (list "log" "blg")))
                    org-latex-logfiles-extensions))

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
 org-refile-targets
 `((,(gk-org-dir-file "ajanda.org") . (:level . 2))
   (,(gk-org-dir-file "listeler.org") . (:level . 1))
   (,(gk-org-dir-file "notlar.org") . (:level . 1)))
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



;;;;;; Variable pitch:

;; OVP minor mode allows to effectively use variable pitch fonts to
;; represent text in Org mode, while retaining monospace where necessary,
;; i.e. for source code, verbatim text, and structure and indentation.

(setf org-variable-pitch-fixed-font (gk-font :mono))

;; Links in tables mess with alignment.
(pushnew 'org-link org-variable-pitch-fixed-faces)
(pushnew 'org-footnote org-variable-pitch-fixed-faces)

(defun gk-ovp-hook ()
  "Hook for ‘org-variable-pitch-minor-mode’."
  )

(add-hook 'org-variable-pitch-minor-mode-hook #'gk-ovp-hook)

(diminish 'org-variable-pitch-minor-mode "~")



;;;;;; The hook:

(defun gk-org-visuals-hook ()
  "Set up how an Org buffer look."
  (org-variable-pitch-minor-mode +1)
  (set-face-attribute 'org-footnote nil :underline nil))

(add-hook 'org-mode-hook 'gk-org-visuals-hook)



;;;;; Custom links:



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

(defun gk-org-mail-open (path)
  (mairix-search path nil))

(org-add-link-type "mairix" 'gk-org-mail-open)



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
                         (expand-file-name "~/doc/not/www2")
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
 ;; Normally, org indents the src block after editing source code with
 ;; C-c ', disable that behaviour.
 org-src-preserve-indentation t
 ;; Don't change the window layout when editing source blocks, open
 ;; them instead in the current window.
 org-src-window-setup 'current-window)



;;;;; Babel:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))

(defun gk-org-confirm-babel-evaluate (&rest ignore)
  "Evaluate code blocks straight away if they are in the ‘org-directory’.
Ask otherwise."
  (member
   (buffer-file-name)
   (directory-files-recursively org-directory "\\.org$")))

(setf org-confirm-babel-evaluate #'gk-org-confirm-babel-evaluate)



;;;;; Auto-insert:

(push
 '((org-mode . "Org-mode document")
   nil
   "# $Id$\n#+title: " - n
   "#+date: \\today\n#+options: toc:nil\n#+latex_class: gk-article")
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



;;;;; Icalendar:

;; Do not sync deadlines and schedules.
(setf org-icalendar-use-scheduled nil
      org-icalendar-use-deadline  nil)



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



;;;;; Keybindings:

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
(define-key org-mode-map (kbd "C-c M-b") #'gk-org-insert-reading-bibliograpy-note)

;; Translation
(define-key org-mode-map (kbd "C-c M-t") #'gk-org-trans-show-paragraph)
(define-key org-mode-map (kbd "C-@") #'gk-org-trans-insert-sentence-reference)



;;;;; Hook:

(defun gk-org-hook ()
  "Default hook for `org-mode' buffers."
  (setq-local default-justification 'left)
  (auto-fill-mode 1)
  ;; Disable in org mode
  (when (boundp goto-address-mode)
    (goto-address-mode -1))
  (imenu-add-to-menubar "Entries")
  (setq truncate-lines nil)
  (setq indent-tabs-mode nil))

(add-hook 'org-mode-hook 'gk-org-hook)



;;;;; Private settings:

(gk-load (gk-org-dir-file "settings") t)



;;;;; Reload org:
;; Reload org so that some settings, like emphasis markers, are
;; applied.

(org-reload)

;;;; Multimedia:



;;;;; Images:

;; Viewing and editing images.



;;;;;; EIMP mode:

;; EIMP allows for modifying images in some ways, and saving the end
;; result in a new file.

(add-hook 'image-mode-hook 'eimp-mode)



;;;;;; Scaling images:

;; The following set of key rebindings and the ‘gk-fit-image-to-window’
;; function help with viewing images bigger than the window they are in.
;; By default, the images overflow in such a situation.  With these
;; modifications, the images are rescaled to fit the window as ‘n’ and
;; ‘p’ keys are pressed to navigate them. ‘=’ manually fits the image to
;; the window, and ‘N’ and ‘P’ navigates images wihout resizing.

(defun gk-fit-image-to-window ()
  "Ensure all of the current image is visible in the current window."
  (interactive)
  (image-transform-set-scale nil))

(define-key image-mode-map "=" #'gk-fit-image-to-window)
(define-key image-mode-map "n" (gk-interactively
                                (image-next-file)
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

(defvar gk-gui-theme 'dracula
  "The default theme's name to load at startup.")

(defun gk-setup-frame-looks (&optional frame)
  "Customisations that modify frame behaviour.
Groups such customisations which might need to be re-ran when a
new frame is created."
  (interactive)

  (ignore frame)

  (when gk-gui-theme
    (load-theme gk-gui-theme t))

  ;; Customise wombat
  (when (eq gk-gui-theme 'wombat)
    ;; With wombat the active window is hard to tell.
    (set-face-attribute 'mode-line nil
                        :background "khaki"
                        :foreground "black"))

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
    ;; Region should not have a foreground colour.
    (set-face-attribute 'region nil :foreground nil)
    ;; Don't change the foreground or decorate the text when
    ;; ‘hl-line-mode’ is on.
    (set-face-attribute 'highlight nil
                        :foreground nil
                        :underline nil))

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

  ;; Project name for frame title.
  (set-frame-parameter nil 'title (or (frame-parameter nil 'gk-project) "main"))

  ;; Special font for moon phase visualisation in forecast.el.
  (set-face-attribute 'forecast-moon-phase nil
                      :font (gk-font :forecast-moon-phase))

  ;; Have a bit more line-spacing.
  (setq-default line-spacing 0.2))

(when (gk-gui-p)
  (add-to-list 'gk-disabled-modes 'tool-bar-mode)
  (add-to-list 'gk-disabled-modes 'scroll-bar-mode)

  ;; Fixes blank area above window after startup with Athena.
  (setf x-frame-normalize-before-maximize t)

  (add-hook 'after-init-hook #'gk-setup-frame-looks)

  (add-hook 'after-make-frame-functions #'gk-setup-frame-looks))



;;;;; Lines:

(setf
 ;; Behave according to `truncate-lines'.
 truncate-partial-width-windows nil
 ;; Use default fringe indicators for ‘visual-line-mode’ too.
 visual-line-fringe-indicators
 (assoca '(continuation) fringe-indicator-alist))



;;;;; UI Semantics:
(defun gk-display-buffer-please-no (buf &rest ignore)
  "Do not display a buffer.
Made specifically to give the middle finger to Quail.  Stop
popping your useless completions up, idiot."
  (error "Inhibited stupid buffer: %s" (buffer-name buf)))

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
 '(("\\*Quail Completions" . (gk-display-buffer-please-no))
   ("\\*.*Completions\\*" . (display-buffer-in-side-window . ((side . right))))
   ("\\*Help\\*" . (display-buffer-reuse-window))
   ("Checkdoc" . (display-buffer-pop-up-window))
   (".*" . (display-buffer-same-window)))
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
 (progn
   (let ((re "-_./:|< "))
     (completion-pcm--prepare-delim-re re)
     re))
 ;; Do not  ring the bell  when killing  in r/o buffers,  put the
 ;; kill in the kill ring but do not modify the buffer.
 kill-read-only-ok t
 ;; Save bookmarks after each bookmark command.
 bookmark-save-flag t
 ;; Search modes default to regexps.
 search-default-mode t
 ;; Only search in visible part.
 search-invisible nil
 ;; Move to trash instead of unlinking.
 delete-by-moving-to-trash t
 ;; Save abbrevs silently
 save-abbrevs 'silently)

(setq-default save-place t)



;;;;; Startup:

;; No start screens.
(setf
 inhibit-startup-screen t
 inhibit-startup-echo-area-message (eval-when-compile (user-login-name)))



;;;;; Winner and windmove:

(setq winner-dont-bind-my-keys t)

(add-to-list 'gk-global-modes 'winner-mode)

(windmove-default-keybindings)
(windmove-delete-default-keybindings)



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

(defun gk-build-mode-line-format ()
  (-replace-first
   'mode-line-modes 'gk-mode-line-modes
   (append gk-mode-line-base
           gk-mode-line-buffer-file-name)))

(setq-default mode-line-format (gk-build-mode-line-format))



;;;;; Goto-address mode:

(defun gk-start-global-address-mode ()
  (goto-address-mode +1))

(dolist (m '(text-mode-hook prog-mode-hook comint-mode-hook))
  (add-hook m 'gk-start-global-address-mode))

(diminish 'goto-address-mode "⚓")



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
 mouse-wheel-progressive-speed nil)



;;;;; TRAMP:

(setf
 ;; Errors and warnings only.
 tramp-verbose 1)



;;;;; Whitespace:

(setf whitespace-style '(face trailing tabs)
      ;; When nil uses ‘fill-column’.
      whitespace-line-column nil)

(setcdr (assoc 'tab-mark whitespace-display-mappings) '(9 [?> 9]))
(setcdr (assoc 'newline-mark whitespace-display-mappings) '(10 [?$ 10]))

(pushnew 'global-whitespace-mode gk-global-modes)
(diminish 'global-whitespace-mode "¶")



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



;;;; Internet:
;;;;; URLs:

;; This is my URL browsing system, which is a big customisation of the
;; Emacs browse-url system which modifies some sorts of URLs for
;; better browsing experience, uses apt Emacs modes to display some
;; files instead of the browser, and prompts whether or not to open
;; URLs in EWW or not.

;; Zero this out first.
(setf browse-url-browser-function nil)



;;;;;; Common:

(defconst gk-ytl-format
  "http://localhost:3991/ytl.html?v=%s"
  "The url for lite youtube player, %s for where to insert video id.")

(defalias 'gk-urls-external-browser 'browse-url-firefox)
(setf browse-url-firefox-program (gk-executable-ensure "firefox"))

;; TODO Check if still relevant when switch to Emacs 25.
;; Replacement for odd standard implementation.
;; See: http://emacshorrors.com/posts/computer-says-no.html
(defun browse-url-can-use-xdg-open ()
  "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser."
  (and window-system (executable-find "xdg-open")))



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

(defun gk-urls-browse-wikipedia (url &rest args)
  "Browse a wikipedia URL."
  (apply #'gk-browse-url
         (gk-wikipedia-link-to-wikizero url)
         args))

(defun gk-urls-add-to-emms (url &rest args)
  "Add an URL to EMMS."
  (emms-add-url url))

(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW, if no browse
with external browser."
  (apply
   (if (y-or-n-p "Browse with EWW? ")
       #'eww-browse-url
     #'gk-urls-external-browser)
   args))



;;;;;; Download and open files with Emacs:

;; This mechanism here allows for downloading and opening files with
;; emacs where that makes sense.  See the section ‘File adapters’ for
;; the adapters.

;; To add a new adapter, simply: =(gk-urls-make-file-adapter "ext")=
;; where =ext= is the filename extension.

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
        browse-url-browser-function
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
      (executable-find "firefox")
      browse-url-browser-function
      `(("\\(youtube\\.com\\|youtu\\.be\\)/" . gk-urls-browse-mpv)
        ("invidio\\.us/" . gk-urls-browse-mpv)
        ("^https?://\\(github\\|gitlab\\).com/.*?/.*?/\\(commit\\|compare\\)/[a-z0-9]+$" .
         gk-urls-browse-github/gitlab-commit)
        ("^https?://github\\.com/.*?/.*?/blob/" . gk-urls-browse-github-file)
        ("^https?://raw\\.github\\.com/" . gk-urls-browse-github-raw)
        ("^http://www.cornucopia\\.local/" . gk-urls-browse-cornucopia)
        ("^https?://\\w+\\.wikipedia\\.org/" . gk-urls-browse-wikipedia)
        ("file:///home/.+/co/lisp/doc/HyperSpec/" . gk-browse-url)
        ;;("\\.\\(mp3\\|ogg\\)$" . gk-urls-add-to-emms)
        ,@browse-url-browser-function
        (".*" . gk-browse-url)))



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



;;;;; Elfeed:



;;;;;; Variables:

;; Set the default filter.
(defvar gk-elfeed-default-filter "@1-week-old +unread ")
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
  "View elfeed article with EWW."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (message "elfeed: Opened in browser: %s" link)
    (eww link)))

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

(add-hook 'elfeed-new-entry-hook 'gk-score-elfeed-entry)




;;;;;; Show mode:

(add-hook
 'elfeed-show-mode-hook
 (defun gk-elfeed-show-mode-hook ()
   "Hook for ‘elfeed-show-mode’."
   (setq-local truncate-lines nil)
   (setq-local word-wrap t)))



;;;;;; Feeds:

;; Load feeds from external source.
(gk-load (dropbox "feeds") t)



;;;; After save hooks:

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)



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

;; Use the touchpad to move the point around:
(gk-global-binding (kbd "<mouse-4>") #'next-line)
(gk-global-binding (kbd "<mouse-5>") #'previous-line)
(gk-global-binding (kbd "<mouse-6>") #'forward-char)
(gk-global-binding (kbd "<mouse-7>") #'backward-char)

(gk-global-binding (kbd "<C-M-mouse-4>") #'mwheel-scroll)
(gk-global-binding (kbd "<C-M-mouse-5>") #'mwheel-scroll)
(gk-global-binding (kbd "<C-M-mouse-6>") #'mwheel-scroll)
(gk-global-binding (kbd "<C-M-mouse-7>") #'mwheel-scroll)

;; Flash to show point
(gk-prefix-binding "\M-f" #'gk-flash-current-line)
(gk-global-binding (kbd "<f1>") #'gk-flash-current-line)

(gk-global-binding (kbd "C-M-s") #'gk-search-forward-1)
(gk-global-binding (kbd "C-M-r") #'gk-search-backward-1)



;;;; Editing:

(gk-prefix-binding "\C-\ " 'gk-eat-spaces-to-the-right)
(gk-prefix-binding "i" 'gk-cycle-input-methods)
(gk-prefix-binding "u" 'gk-upcase-this-or-previous-word)
(gk-prefix-binding "l" 'gk-lowercase-this-or-previous-word)
(gk-global-binding "\C-z" 'gk-cycle-input-methods)
(gk-prefix-binding "e" "\C-xe")

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
(gk-prefix-binding "[" #'window-toggle-side-windows)
(gk-prefix-binding "=" #'menu-bar-mode) ;toggle
(gk-prefix-binding "g" #'magit-status)
(gk-prefix-binding "M-." #'repeat-complex-command)
(gk-prefix-binding (kbd "<C-backspace>") #'delete-frame)
(gk-prefix-binding "\C-f" #'project-find-file)
(gk-prefix-binding "\C-p" #'gk-open-project)



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
(gk-prefix-binding "oa" 'org-agenda)
(gk-prefix-binding "a" 'org-agenda)
(gk-prefix-binding "c" 'org-capture)
(gk-prefix-binding "oo" (gk-interactively (org-agenda nil "g")))
(gk-prefix-binding "oj" #'org-babel-tangle-jump-to-org)



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
(gk-prefix-binding (kbd "C-r") #'rmail)
(gk-prefix-binding "/" #'mairix-search)
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
  (add-hook 'server-switch-hook 'raise-frame))

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
