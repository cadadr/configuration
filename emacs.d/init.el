;;; init.el --- Göktuğ's Emacs configuration. -*- lexical-binding: t; -*-

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

;; Allow debugging initialisation errors even w/o --debug-init.
(unless debug-on-error
  (message "Temporarily enable debugging on errors...")
  (setq debug-on-error t)
 (add-hook
  'after-init-hook
  (lambda ()
    (message "Finished initialisation, turn debugging off...")
    (setq debug-on-error nil))))



;;;; Loadpaths:

(require 'cl-lib)

;; These settings are grouped into a ‘progn’ so that they can be run
;; together with a single keystroke in interactive mode.
(progn
  (defun gk-directory-files (directory &optional include-dotfiles relative-names)
    "Saner version of ‘directory-files’.
Exclude dot-files, don't sort, and return full paths by default."
    (directory-files
     directory
     (not include-dotfiles)
     directory-files-no-dot-files-regexp
     t))

  (defvar gk-elisp-site-dir
    (locate-user-emacs-file "vendored-lisp")
    "Directory where 3rd party Elisp is contained.")

  (defvar gk-ext-site-dir "~/co/Lisp/elisp/site"
    "Source for ‘gk-elisp-site-dir’.")

  (defun gk-load-from-ext-site-dir (subpath &rest args)
    "Load SUBPATH from external vendor site.
That which emacs.d/vendored-lisp pulls from.
This redirects to ‘load’ using ‘apply’."
    (apply #'load (expand-file-name subpath gk-ext-site-dir) args))

  ;; Sanitise.
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

  ;; Add custom paths.
  (add-to-list 'load-path (expand-file-name  "~/co/elisp"))
  (dolist (p '("lisp" "themes" "ext"))
    (add-to-list 'load-path
                 (expand-file-name
                  (locate-user-emacs-file p))))

  (let ((dirs (cl-remove-if-not
               #'file-directory-p
               (gk-directory-files gk-elisp-site-dir))))
    (dolist (dir dirs)
      (add-to-list 'load-path dir)))

  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list
     'Info-directory-list
     (locate-user-emacs-file "vendored-lisp/docs")))

  ;; Custom themes:
  (add-to-list 'custom-theme-load-path
               (expand-file-name (locate-user-emacs-file "themes")))
  (add-to-list 'custom-theme-load-path
               (expand-file-name (locate-user-emacs-file "vendored-lisp/themes")))
  (add-to-list 'custom-theme-load-path
               (expand-file-name  "~/co/elisp"))
  (message "Load paths are set up."))



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
(require 'bibliothek)
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
(require 'dired-subtree)
(require 'dired-x)
(require 'doc-view)
(require 'dollar)
(require 'eimp)
(require 'eldoc)
(require 'elfeed)
(require 'emms)
(require 'emms-setup)
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
(require 'gnus-sum)
(require 'goto-addr)
(require 'goto-last-change)
(require 'highlight-parentheses)
(require 'ibuffer)
(require 'ibuffer-vc)
(require 'ido)
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
(require 'mail-source)
(require 'mairix)
(require 'mastodon)
(require 'message)
(require 'mm-url)
(require 'multiple-cursors)
(require 'netrc)
(require 'nnfolder)
(require 'nsm)
(require 'olivetti)
(require 'org)
(require 'org-capture)
(require 'org-checklist)
(require 'org-eww)
(require 'org-mobile)
(require 'org-protocol)
(require 'org-variable-pitch)
(require 'outline)
(require 'ox)
(require 'ox-beamer)
(require 'ox-groff)
(require 'ox-hugo)
(require 'ox-latex)
(require 'ox-odt)
(require 'ox-publish)
(require 'paredit)
(require 'paren-face)
(require 'pass-listing)
(require 'pdf-cache)
(require 'pdf-isearch)
(require 'pdf-misc)
(require 'pdf-outline)
(require 'pdf-sync)
(require 'pdf-tools)
(require 'perl-mode)
(require 'persistent-scratch)
(require 'pixel-scroll)
(require 'pp)
(require 'python-django)
(require 'quail)
(require 'rect)
(require 'rmail)
(require 'ruby-mode)
(require 'rx)
(require 's)
(require 'saveplace)
(require 'scheme)
(require 'seq)
(require 'shell)
(require 'shr)
(require 'simple)
(require 'smtpmail)
(require 'spam)
(require 'subr-x)
(require 'thingatpt)
(require 'thinks)
(require 'time)
(require 'tls)
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
      eww-bookmarks-directory (dropbox "/")
      save-place-file (locate-user-emacs-file "etc/places")
      tramp-persistency-file-name (locate-user-emacs-file "etc/tramp")
      custom-file (locate-user-emacs-file "etc/custom.el")
      nsm-settings-file (locate-user-emacs-file "etc/network-security.data")
      mc/list-file (locate-user-emacs-file "etc/mc-lists.el")
      emms-directory (locate-user-emacs-file "etc/emms"))

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

(defun gk-home ()
  "Take me to the home view."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'maximized)
  (delete-other-windows)
  (find-file (gk-org-dir-file "start.org"))
  (split-window-sensibly)
  (other-window 1)
  (org-agenda nil "g")
  (other-window 1))

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
  (byte-recompile-directory (locate-user-emacs-file "packages") 0 (> force 4))
  (byte-recompile-directory (locate-user-emacs-file "site") 0 (> force 4))
  (when custom-file
    (byte-recompile-file custom-file (> force 1) 0)))



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

;; [[file:/igk/config/emacs.d/gk.org::*Association%20lists][Association lists:1]]
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
  (list :serif "DejaVu Serif"
        :sans "DejaVu Sans"
        :mono "DejaVu Sans Mono"
        :forecast-moon-phase "Quivira")
  "A plist, default fonts.")

;; Set up so that there's 75-80 chars width for half-sized horizontal
;; windows.
(defconst gk-font-default-height 100)
(defconst gk-font-variable-pitch-height 100)

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



;;;; Things:

;; XXX(2018-05-25): Consider moving this into the Utility libraries
;; section.

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
 backup-directory-alist `(("." . ,(expand-file-name "~/.emacs-backups"))))



;;;; Comint:

;; Settings for interpreter buffers.



;;;;; Common:

;; Settings and keybindings common to all comint buffers.

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

(defun gk-shell-mode-hook ()
  "Hook for `shell-mode'."
  ;; BSD /bin/sh echoes.
  (when (and (not (memq system-type '(gnu gnu/linux gnu/kfreebsd)))
             (string-match "/k?sh$" (getenv "SHELL")))
    (setq-local comint-process-echoes t)))



;;;; Dired:



;;;;; The hook:

(defun gk-dired-hook ()
  "Main hook for `dired-mode'."
  ;; C-x M-o -> toggle omitting
  ;; * O -> mark omitted
  (dired-omit-mode 1)
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook #'gk-dired-hook)



;;;;; Customisations:

(setf
 ;; Omit ., .., #*, *~, ~,v.
 dired-omit-files "^\\(\\.\\|#\\)\\(.+\\|.+\\(~\\|,v\\)\\)?$"
 ;; Show symlinks' targets: it's useful, and dired-subtree is stupid
 ;; otherwise.
 dired-hide-details-hide-symlink-targets nil)

(set-face-attribute dired-symlink-face nil
                    :foreground "blue")

(setf ls-lisp-dirs-first t)

(setf
 ;; Ask for confirmation
 wdired-confirm-overwrite t
 ;; Human readable size.
 dired-listing-switches "-alh")



;;;;; Keymappings:

(define-key dired-mode-map (kbd "W") 'wdired-change-to-wdired-mode)



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



;;;;; PDF-tools:

;; TODO(2018-05-25): implement a smarter resizing addon where the
;; resize factor can vary

;; PDF tools is a sophisticated alternative to DocView for PDF files.

(setf
 pdf-info-epdfinfo-program
 (gk-executable-ensure
 "~/co/External/github-politza-emacs-pdf-tools/server/epdfinfo")
 pdf-tools-enabled-modes
 '(pdf-isearch-minor-mode
   pdf-links-minor-mode
   pdf-misc-minor-mode
   pdf-outline-minor-mode
   pdf-misc-size-indication-minor-mode
   pdf-misc-menu-bar-minor-mode
   pdf-sync-minor-mode
   pdf-misc-context-menu-minor-mode
   pdf-cache-prefetch-minor-mode)
 ;; Manually change the page.
 pdf-view-continuous nil
 ;; Resize more granularly.
 pdf-view-resize-factor 1.01)

(pdf-tools-install)

(define-key pdf-view-mode-map (kbd "M-w") #'pdf-view-kill-ring-save)
(define-key pdf-view-mode-map (kbd "C-w") #'pdf-view-kill-ring-save)

;;;;; PDF utilites:

(setf bibliothek-path
      (nconc
       (list "~/fil/PDFs" "~/wrk/papers" "~/wrk/leggi" "~/wrk/print"
             "~/wrk")
       (remove-if-not
        #'file-directory-p
        (directory-files "~/wrk/rsc" t "[a-z]" t))))

(defalias 'library 'bibliothek)
(defalias 'bib 'bibliothek)



;;;; Input methods:

;; Load them up.

(require 'gk-greek)
(require 'gk-unilat)



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



;;;;; UTF-8 input minor mode (obsolete):

;; TODO(2018-05-25): Move into Utility libraries section.

(defvar gk-utf8-entry-mode-map
  (make-sparse-keymap))

(define-minor-mode gk-utf8-entry-mode
  "Minor mode for definition of some shortcuts for UTF8 characters."
  nil " u8"
  gk-utf8-entry-mode-map)

(defvar gk-utf8-combining-diacritic-keybindings-alist nil)
(defvar gk-utf8-combining-diacritic-keybindings-prefix nil)
(defvar gk-utf8-character-shortcuts-alist nil)
(defvar gk-utf8-character-shortcuts-prefix nil)

(defun gk-utf8-set-bindings (prefix binding-alist)
  "Set utf-8bindings.
Use PREFIX as prefix key.
Bindings come from BINDING-ALIST."
  (dolist (i binding-alist)
    (let* ((key (car i))
           (char (cdr i))
           (binding (concat prefix " " key))
           (fun `(lambda ()
                   (interactive)
                   (insert-char ,char))))
      (define-key gk-utf8-entry-mode-map
        (kbd binding) (eval fun)))))

(defun gk-utf8--update-hook ()
  "Hook for updating utf8 binding definitions."
  (gk-utf8-set-bindings
   gk-utf8-character-shortcuts-prefix
   gk-utf8-character-shortcuts-alist)
  (gk-utf8-set-bindings
   gk-utf8-combining-diacritic-keybindings-prefix
   gk-utf8-combining-diacritic-keybindings-alist))

(add-hook 'gk-utf8-entry-mode-hook #'gk-utf8--update-hook)



;;;;; Utilites:

;; XXX(2018-05-25): Consider moving to Utility libraries.

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

(defun gk-lowercase-this-or-previous-word ()
  (interactive)
  (save-excursion
    (backward-word)
    (mark-word)
    (downcase-word 1)))

(defun gk-upcase-this-or-previous-word ()
  (interactive)
  (save-excursion
    (backward-word)
    (mark-word)
    (upcase-word 1)))

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



;;;;; Common:

;; Settings common to all major/minor modes that edit text.

(diminish 'visual-line-mode "¬")
(diminish 'gk-utf8-entry-mode)
(diminish 'olivetti-mode "𝍌")
;; i.e. ‘auto-fill-mode’, but diminish does not like that.
(diminish 'auto-fill-function "=")

(setq-default olivetti-body-width 85)

(defun gk-text-editing-modes-hook ()
  "Hook for `text-mode'."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (gk-utf8-entry-mode 1)
  (set-input-method default-input-method)
  ;; (olivetti-mode 1)
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



;;;;; UTF-8 input minor mode configuration:

;; This section configures the UTF-8 entry minor mode from above.

(setf gk-utf8-combining-diacritic-keybindings-alist
      '(("," . 807) ; COMBINING CEDILLA
        ("p" . 801) ; COMBINING PALATALIZED HOOK BELOW
        ("-" . 772) ; COMBINING MACRON
        (":" . 776) ; COMBINING DIAERESIS
        ))

(setf gk-utf8-character-shortcuts-alist
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

(setf gk-utf8-character-shortcuts-prefix
      "C-c 8")
(setf gk-utf8-combining-diacritic-keybindings-prefix
      gk-utf8-character-shortcuts-prefix)

(add-hook 'gk-minor-mode-hook #'gk-utf8-entry-mode)



;;;;; HTML:

(defun gk-html-mode-hook ()
  "Hook for `html-mode'."
  (setf indent-tabs-mode nil))

(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'gk-html-mode-hook)
(setq zencoding-preview-default nil)



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



;;;;; Evil:

(defvar-local gk-evil-default-cursor nil
  "The value of ‘cursor-type’ before ‘gk-toggle-evil-mode’ was
run.")

(defun gk-toggle-evil-mode ()
  "Toggle ‘evil-mode’, trying to deal with how it interacts with
my configurations."
  (interactive)
  (if evil-mode
      (setq-local cursor-type gk-evil-default-cursor)
    (setq-local gk-evil-default-cursor cursor-type))
  (evil-mode (if evil-mode -1 +1)))



;;;;; Multiple cursors:

;; Just fucking run the commands.
(setf mc/always-run-for-all t)



;;;;; Keybindings:

;; These are keybindings specific to =text-mode= and descendants.

(define-key text-mode-map (kbd "C-M-a") #'backward-paragraph)
(define-key text-mode-map (kbd "C-M-e") #'forward-paragraph)



;;;; Global settings:



;;;;; Calendar:

(calendar-set-date-style 'iso)          ;The only unambiguous one.



;;;;; forecast.el:

(setq forecast-language 'en
      forecast-units 'si
      forecast-time-format "%I:%M:%S%p, %F"
      forecast-rain-symbol "☔")



;;;;; Global modes:

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
        line-number-mode
        column-number-mode))

(mapc ($ (pushnew $1 gk-disabled-modes))
      '(electric-indent-mode
        pixel-scroll-mode))

;; Diminish global modes that are always on.
(diminish 'gk-utf8-entry-mode)
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
 vc-follow-symlinks t)

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



;;;;; Diff:

(defun gk-diff-mode-hook ()
  "Diffs."
  )

(setf
 vc-cvs-diff-switches "-uNp"
 vc-diff-switches "-uNp"
 diff-switches "-uNp")

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



;;;;; Mercurial:

(setf
 ;; Show the revision number in the mode line.
 vc-hg-symbolic-revision-styles '("{rev}/{branch}"))

;; Always prompt for editing the push command before pushing.
;; Requires prefix arg ortherwise.
(define-advice vc-hg-push (:around (fn &rest args) always-prompt)
  "Always prompt for editing the push command."
  (funcall fn t))



;;;;; Magit:

(setf magit-commit-show-diff nil)



;;;; Programming:

;; Configurations for programming-related major/minor modes.



;;;;; Common:

(diminish 'highlight-parentheses-mode)
(diminish 'eldoc-mode)
(diminish 'paredit-mode "☮")
(diminish 'outline-minor-mode "*")

(defun gk-algol-like-hook ()
  "Hook for Algol-like programming languages editing."
  (electric-pair-local-mode +1))



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

;; Pretty-printing:
(define-key emacs-lisp-mode-map (kbd "C-c C-M-x") 'pp-macroexpand-expression)
(define-key emacs-lisp-mode-map (kbd "C-c C-x C-e") 'pp-eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-c C-x C-e") 'pp-macroexpand-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c !") #'ert-run-tests-interactively)



;;;;;;; Debugger:

;; Configuaration for the Elisp debugger.

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
 c-default-style "gnu")

(add-hook 'c-mode-hook 'gk-algol-like-hook)



;;;;; Javascript:

(defun gk-javascript-hook ()
  "Standard JS hook."
  (highlight-parentheses-mode 1)
  (setq indent-tabs-mode nil
        js-indent-level 2))

(add-hook 'js-mode-hook 'gk-javascript-hook)
(add-hook 'js-mode-hook 'gk-algol-like-hook)



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

(defun gk-ri (what)
  "Interface to ri(1) documentation browser."
  (interactive (list (read-string "Search in Ruby documentation: ")))
  (let ((buf (get-buffer-create (format "*ri %s*" what))))
    (with-current-buffer buf
      (erase-buffer)
      (shell-command (format "ri -f ansi '%s'" what) buf)
      (ansi-color-filter-region (goto-char (point-min)) (point-max))
      (gk-minor-mode)
      (view-mode))))

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

(setf python-shell-interpreter "python3"
      ;; Please don't annoy me, and fuck you.
      python-indent-guess-indent-offset nil)

(defalias 'django 'python-django-open-project)

(add-hook 'python-mode-hook 'gk-algol-like-hook)

(defun gk-python-send-line ()
  "Send current-line to inferior Python."
  (interactive)
  (message
   "=> %s"
   (python-shell-send-string-no-output
    ;; Hackish, but it seems to me to be the only way to get what one
    ;; would expect from an inferior interpreter process in Emacs.
    (concat "print(" (buffer-substring
                      (line-beginning-position)
                      (line-end-position))
            ")"))))

(define-key python-mode-map "\C-c\C-l" #'gk-python-send-line)



;;;;; Makefiles:

(defun gk-makefile-hook ()
  "Generic hook for makefile modes."
  (gk-turn-on-outline-minor-mode "####* " ":$" "C-'"))

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

(setf message-mail-user-agent nil
      read-mail-command 'gnus)



;;;;; Posting styles:

(setf
 ;; Gmail does not like parens.
 message-from-style 'angles)



;;;;; Utilities:

(defun posta (&optional arg)
  "Start the selected mail application.
If arg is non-nil, or universal argument given fetch new mail.
Otherwise start mail program in offline mode."
  (interactive "P")
  (gk-with-new-frame ()
    (if arg
        (gnus)
      (gnus-unplugged))))

(defun gk-fetch-mail ()
  "Run mail retrieval scripts."
  (interactive)
  (make-process
   :name "gk-fetch-mail" :buffer (get-buffer-create "*Fetch Mail*")
   :command (list "do-netrc.sh" "mpop" "-Q" "-a")
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



;;;;; Spam-checking and splitting mail:

(spam-initialize)

(setf
 spam-use-BBDB t
 spam-use-spamassassin t
 spam-assassin-program (gk-executable-ensure "spamc")
 gnus-spam-newsgroup-contents '(("spam" gnus-group-spam-classification-spam)))

(setf
 nnmail-split-methods 'nnmail-split-fancy
 ;; FUCK THIS.  GODAWFUCK THIS.  The docstring reads:
 ;;
 ;; > Normally, regexes given in ‘nnmail-split-fancy’ are implicitly
 ;; > surrounded by "\<...\>".  If this variable is true, they are not
 ;; > implicitly surrounded by anything.
 ;;
 ;; My god.
 nnmail-split-fancy-match-partial-words t)
(gk-load (dropbox "split-mail") t)



;;;;; Sending mail:

(setf
 message-send-mail-function 'smtpmail-send-it
 send-mail-function 'smtpmail-send-it ; mail-mode
 smtpmail-local-domain (system-name)
 smtpmail-sendto-domain (system-name)
 smtpmail-stream-type 'ssl
 smtpmail-smtp-service 465)

(gk-load (dropbox "smtp") t)

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
 message-default-headers (format "Fcc: %s/outbox" gk-mail-home))

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
 rmail-file-name (expand-file-name "current" gk-mail-home)
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
     (rx bol "[" digit ":" "text/html "
         (or "Hide" "Show")
         " Save:"))
    (point)
    (forward-char 1)
    (let ((button (button-at (point)))
          (filename
           (concat (make-temp-name
                    (expand-file-name
                     "gkrmailout" temporary-file-directory))
                   ".html")))
      (browse-url (concat "file://"
                          (gk-rmail-mime-save-to-tmp button filename))))))

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

(define-key rmail-mode-map "S" #'gk-rmail-force-expunge-and-save)
(define-key rmail-mode-map "b" #'gk-rmail-view-html-part-in-browser)

(add-hook
 'rmail-mode-hook
 (defun gk-rmail-mode-hook ()
   (goto-address-mode +1)))



;;;;; Mairix:

;; XXX(2018-05-25): Use with Gnus?

(setf
 mairix-file-path (expand-file-name "mairix/" gk-mail-home)
 mairix-search-file "search")

(defalias 'search-mail 'mairix-widget-search)

(define-advice mairix-widget-search
    (:after (&rest args) enable-widget-minor-mode)
  "Activate ‘widget-minor-mode’ in the ‘mairix-widget-search’ buffer.
Wonder why this is not the default."
  (widget-minor-mode +1))



;;;;; Gnus:



;;;;;; Files and directories:

;; Set up a consistent tree for Gnus and don't allow it to pollute the
;; user home directory.

(setf gnus-default-directory gk-mail-home)

(defun gnus-file (file)
  "Locate FILE in ‘gnus-default-directory’."
  (expand-file-name file gnus-default-directory))

(setf
 gnus-home-directory gnus-default-directory
 gnus-directory (gnus-file "news")
 message-directory (gnus-file "mail") ; nnml-directory is set from this.
 mail-source-directory (gnus-file "mail")
 gnus-startup-file (gnus-file "newsrc")
 nnmail-message-id-cache-file (gnus-file "nnmail-cache")
 nnfolder-directory (gnus-file "mail/archive")
 gnus-init-file (gnus-file "gnus-init"))



;;;;;; Mail sources:

(setf
 ;; Rmail knows better...
 mail-source-movemail-program rmail-movemail-program
 mail-sources
 `(,@(mapcar ($ (list 'file :path $1)) gk-mail-inboxes)))



;;;;;; Methods:

(setf
 gnus-select-method '(nnml "")
 gnus-secondary-select-methods
 '((nntp "gmane"
         (nntp-address "news.gmane.org"))
   ;; (nntp "eternal-september"
   ;;       (nntp-address "news.eternal-september.org"))
   ))



;;;;;; Mime:

(setf
 mm-enable-external 'ask
 mm-discouraged-alternatives '("text/richtext" "text/html")
 ;; Resize images to fit the view.
 mm-inline-large-images 'resize
 gnus-buttonized-mime-types
 '("multipart/signed" "multipart/alternative" "text/html" "text/x-diff"
   "text/x-patch" "text/vcard" "text/x-org"))



;;;;;; Keybindings:

(define-key gnus-article-mode-map "\C-cw" 'gnus-article-browse-html-article)
(define-key gnus-summary-mode-map "BS" (gk-interactively
                                        (gnus-summary-move-article nil "spam")))
(define-key gnus-summary-mode-map "B@" (gk-interactively
                                        (gnus-summary-move-article nil "self")))



;;;;;; Settings:

(setf
 ;; «Number of seconds to wait before an nntp connection times out.»
 ;; Gmane hangs indefinitely at times.
 nntp-connection-timeout 5)



;;;;;; User interface:

(setf
 ;; U: read status
 ;; R: A if replied to,‘ ’ if not
 ;; z: zscore (char)
 ;; B: trn-style indentation based on thread level
 ;; f: contents of the from or to headers.
 ;; s: subject or empty str if not thread root
 gnus-summary-line-format "[ %U%R%z ] %B[ %(%-23,23f%) ] %s \n"
 ;; Don't mess up my window configuration.
 gnus-use-full-window nil)

(define-advice gnus-summary-exit
    (:before (&rest args) delete-article-window)
  "Before exiting summary mode, delete the related Article buffer's window."
  (let* ((w (next-window))
         (nwb (window-buffer w))
         (nwm (with-current-buffer nwb major-mode)))
    (when (equal 'gnus-article-mode nwm)
      (delete-window w))))

(setf gnus-thread-sort-functions
      '(gnus-thread-sort-by-date))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)

(define-key gnus-group-mode-map [?_] 'delete-other-windows-vertically)
(define-key gnus-summary-mode-map [?_] 'delete-other-windows-vertically)



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
  (if (not (equal
            (file-truename
             (file-name-directory
              (expand-file-name "bob" org-directory)))
            (file-truename
             (file-name-directory (expand-file-name (buffer-file-name))))))
      (yes-or-no-p prompt)
    t))

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



;;;;; Variables:

(setf
 ;; Open files in the same window.
 (cdr (assoc 'file org-link-frame-setup)) 'find-file
 ;; Let emacs show the pdf files.
 org-file-apps (dissoc "\\.pdf\\'" org-file-apps)
 ;; Place tags right after the title.
 org-tags-column 0
 ;; If an #+attr_*: :width xxx available, use xxx, else, car of this
 ;; value.
 org-image-actual-width (list 300)
 ;; The minibuffer prompt is enough.
 org-popup-calendar-for-date-prompt nil)

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
 gk-org-agenda-files (gk-org-dir-files "ajanda.org" "caldav.org")
 org-agenda-files gk-org-agenda-files
 ;; Find all the relevant Readme.org files.
 gk-org-project-agenda-files
 (cons (expand-file-name "~/doc/not/www2/Readme.org")
       ;; Find Readme.org files.
       (seq-filter
        ($ (string= (file-name-nondirectory $1) "Readme.org"))
        (let ((re directory-files-no-dot-files-regexp))
          (apply #'append
                 (mapcar
                  ($ (when (file-directory-p $1)
                       (directory-files $1 t re)))
                  (directory-files "~/co" t re)))))))



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
   (search category-up)))

(setf
 org-agenda-custom-commands
 '(("g" "Agenda for today and all agenda TODOs"
    ((alltodo "" ((org-agenda-files gk-org-project-agenda-files)))
     (alltodo "" ((org-agenda-files (gk-org-dir-files "listeler.org"))))
     (alltodo "" ((org-agenda-files (gk-org-dir-files "../yazi/defter.org"))))))))

(defun gk-org-agenda-mode-hook ()
    (gk-turn-on-outline-minor-mode "^[A-Z]" ".$" "C-'"))

(add-hook 'org-agenda-mode-hook #'gk-org-agenda-mode-hook)



;;;;; Exporting:



;;;;;; Common:

;; Defaults for exporting from org mode.

(setq org-export-with-smart-quotes t
      org-export-with-sub-superscripts t
      org-export-dispatch-use-expert-ui t)



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



;;;;;; Groff->PDF:

(defconst gk-org-pristine-groff-process (copy-tree org-groff-pdf-process))

(let ((process "< %f preconv -e utf8 | pic | tbl | eqn | groff -mm -Tpdf > %b.pdf"))
  (setf org-groff-pdf-process (list process)))



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
 org-adapt-indentation nil)



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

(set-face-attribute 'org-variable-pitch-face
                     nil :inherit 'org-level-1
                     :weight 'bold)

;; Links in tables mess with alignment.
(pushnew 'org-link org-variable-pitch-fixed-faces)

(defun gk-ovp-hook ()
  "Hook for ‘org-variable-pitch-minor-mode’."
  (setq-local cursor-type 'bar))

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
     (format "\\todo{%s}{%s}" keyword (or desc ""))))))

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
   (shell . t)))

(defun gk-org-confirm-babel-evaluate (&rest ignore)
  "Evaluate code blocks straight away if they are in the ‘org-directory’.
Ask otherwise."
  (not (file-in-directory-p (buffer-file-name) org-directory)))

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



;;;;; Keybindings:

(org-defkey org-mode-map (kbd "C-M-<return>") 'org-insert-subheading)
;; Heading navigation
(org-defkey org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
(org-defkey org-mode-map (kbd "M-n") 'outline-next-visible-heading)
;; Paragraphs
(define-key org-mode-map [remap backward-paragraph] nil)
(define-key org-mode-map [remap forward-paragraph] nil)



;;;;; Hook:

(defun gk-org-hook ()
  "Default hook for `org-mode' buffers."
  (setq-local default-justification 'left)
  (auto-fill-mode 1)
  ;; Disable in org mode
  (when (boundp goto-address-mode)
    (goto-address-mode -1))
  (imenu-add-to-menubar "Entries")
  (setq truncate-lines t)
  (setq indent-tabs-mode nil))

(add-hook 'org-mode-hook 'gk-org-hook)



;;;;; Private settings:

(gk-load (gk-org-dir-file "settings") t)



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



;;;;; EMMS:

(emms-standard)
(emms-default-players)



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

(defvar gk-gui-theme nil
  "The default theme's name to load at startup.")

(setf gk-gui-theme nil)

(when (gk-gui-p)
  (add-to-list 'gk-disabled-modes 'tool-bar-mode)
  (add-to-list 'gk-disabled-modes 'scroll-bar-mode)

  ;; This needs to be set manually for solarized.
  (setf frame-background-mode nil)
  ;; Update all the existing frames.
  (mapc 'frame-set-background-mode (frame-list))

  (when gk-gui-theme
    (load-theme gk-gui-theme t))

  (when (eq gk-gui-theme 'wombat)
    ;; With wombat the active window is hard to tell.
    (set-face-attribute 'mode-line nil
                        :background "khaki"
                        :foreground "black")
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

  (setf frame-title-format '("%b - " invocation-name "@" system-name))

  ;; Special font for moon phase visualisation in forecast.el.
  (set-face-attribute 'forecast-moon-phase nil
                      :font (gk-font :forecast-moon-phase))

  ;; Have a bit more line-spacing.
  (setq-default line-spacing 0.2))



;;;;; Lines:

;; Truncate lines by default.
(setq-default truncate-lines t)

(setf
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
 '(("\\*.*Completions\\*" . (display-buffer-pop-up-window))
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
 completion-styles '(basic substring partial-completion)
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



;;;;; Ido:

(setf
 ido-use-filename-at-point nil
 ;; Don't show dotfiles if the prefix of the search string is not ‘.’
 ido-enable-dot-prefix t
 ido-confirm-unique-completion t
 ;; Show in the current frame, change window's buffer if necessary.
 ido-default-buffer-method 'selected-window)

;;(push 'ido-mode gk-global-modes)
;;(push 'ido-everywhere gk-global-modes)

(setf ido-enable-flex-matching t)

;; Display completions vertically:
(setf ido-decorations
      '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]"))

(add-hook
 'ido-minibuffer-setup-hook
 (defun gk-ido-disable-line-truncation ()
   (set (make-local-variable 'truncate-lines) nil)))

(add-hook
 'ido-setup-hook
 (defun gk-ido-define-keys () ;; C-n/p is more intuitive in vertical layout
   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))



;;;;; Mode line:

(defconst gk-mode-line-pristine-format
  (copy-list mode-line-format)
  "Modeline before my modifications.")

(defconst gk-mode-line-base
  ;; Remove the infinite-spaces, the last item of the list.
  (butlast gk-mode-line-pristine-format 1)
  "The base for constructing a custom mode line.")

(setq-default
 mode-line-format
 (append gk-mode-line-base
         '(" "
           ;; Buffer's file if visiting one, the default directory otherwise.
           (:eval (or (buffer-file-name) default-directory)))))



;;;;; Cursor:

(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'hollow)

;; Box cursor in special-mode and some other modes when the default
;; cursor type is 'bar.
(when (eq cursor-type 'bar)
 (let ((modes '(special-mode-hook term-mode-hook forecast-mode-hook))
       (hook (defun gk-special-mode-box-cursor ()
               (setq-local cursor-type 'box)))
       gnus-modes hookies)
   ;; Find all gnus modes.
   (dolist (cns (custom-group-members 'gnus nil))
     (let* ((sym (car cns))
            (symnam (symbol-name sym))
            (hooknam (intern (concat symnam "-mode-hook"))))
       (when (and (string-prefix-p "gnus-" symnam)
                  (boundp hooknam))
         (pushnew hooknam gnus-modes))))
   ;; Add the hook to all modes.
   (dolist (hookvar `(,@modes ,@gnus-modes) hookies)
     (add-hook hookvar hook)
     (push hookvar hookies))))



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
      ;; Flash if ‘hl-line-mode’ is inactive.  If it's active, the
      ;; cursor is on the line to continue anyways.
      (unless hl-line-mode
        (hl-line-mode +1)
        (run-with-idle-timer
         1 nil
         ($ (with-current-buffer buf
              (hl-line-mode -1))))))))



;;;;; Time:

(setf display-time-format " {%d %a %Y %H:%M}"
      ;; Don't show load average.
      display-time-default-load-average nil)
;;(add-to-list 'gk-global-modes 'display-time-mode)



;;;;; Sessions:

(let ((desktop-dir (expand-file-name (locate-user-emacs-file "etc/"))))
  (setf
   ;; Always save desktops.
   desktop-save t
   ;; Load all buffers.
   desktop-restore-eager t
   ;; Make sure there's only one place to look for desktops.
   desktop-path (list desktop-dir)
   desktop-dirname desktop-dir
   desktop-base-file-name "desktop"
   desktop-base-lock-name "desktop.lock"
   ;; Don't save these.
   desktop-buffers-not-to-save
   "^\\*"
   desktop-restore-frames t
   desktop-globals-to-save nil))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; Enable desktop:
;; (add-to-list 'gk-global-modes 'desktop-save-mode)
;; (add-hook 'after-init-hook 'desktop-revert)



;;;;; Mouse:

;; (pushnew 'pixel-scroll-mode gk-global-modes)

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



;;;;; TLS:

;; This section contains some settings adapted from this blog post [1]
;; for making sure that ~tls.el~ always checks certificates.  If I'm
;; not mistaken this is not necessary for normal usage as actually
;; ~nsm.el~ handles the certificate checking and other network
;; security (see (info "(emacs)Network Security") and that's what most
;; if not all internet connections go through.

;; [1] https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(setf
 ;; Enable checking the certificates against root certificates.
 tls-checktrust t
 tls-program
 (mapcar
  ($ (format $1 (getenv "SSL_CERT_FILE")))
  (list
   "gnutls-cli --x509cafile %s -p %%p %%h"
   "gnutls-cli --x509cafile %s -p %%p %%h --protocols ssl3")))



;;;;; URLs:

;; This is my URL browsing system, which is a big customisation of the
;; Emacs browse-url system which modifies some sorts of URLs for
;; better browsing experience, uses apt Emacs modes to display some
;; files instead of the browser, and prompts whether or not to open
;; URLs in EWW or not.

;; Zero this out first.
(setf browse-url-browser-function nil)



;;;;;; Qutebrowser:

;; Qutebrowser is a webkit based web browser, which is currently my
;; default one.  The following code adds browse-url support for it,
;; and is based on the code for Firefox in browse-url.el.

(defcustom browse-url-qutebrowser-arguments nil
  "A list of strings to pass to Qutebrowser as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

;; qute.pl is a script to invoke Qutebrowser with my setup.
(defcustom browse-url-qutebrowser-program "qute.pl"
  "The name by which to invoke Qutebrowser."
  :type 'string
  :group 'browse-url)

(defun browse-url-qutebrowser (url &optional new-window)
  "Ask Qutebrowser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-qutebrowser-arguments' to Qutebrowser.

Interactively, if the variable `browse-url-new-window-flag' is
non-nil, loads the document in a new Qutebrowser window.  A
non-nil prefix argument reverses the effect of
`browse-url-new-window-flag'.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (let* ((encoded-url (browse-url-encode-url url))
         (process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "qutebrowser " encoded-url) nil
           browse-url-qutebrowser-program
           (append
            browse-url-qutebrowser-arguments
            (when (browse-url-maybe-new-window new-window)
              '("--target" "window"))
            (list url)))))



;;;;;; Common:

(defconst gk-ytl-format
  "http://localhost:3991/ytl.html?v=%s"
  "The url for lite youtube player, %s for where to insert video id.")

(defalias 'gk-urls-external-browser 'browse-url-firefox)
(setf browse-url-firefox-program
      (gk-executable-ensure "~/opt/firefox/firefox"))

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

(defun gk-urls-browse-github-commit (url &rest args)
  "Browse a github URL.
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

(defun gk-urls-browse-youtube (url &rest args)
  "Browse a youtube URL via mpv."
  (and
   (start-process "gk-youtube-mpv" (generate-new-buffer-name "*Youtube MPV*")
                  "mpv" url)
   (message "Started mpv process for: %s" url)))

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
      `(("\\(youtube\\.com\\|youtu\\.be\\)/" . gk-urls-browse-youtube)
        ("^https?://github.com/.*?/.*?/\\(commit\\|compare\\)/[a-z0-9]+$" .
         gk-urls-browse-github-commit)
        ("^https?://github\\.com/.*?/.*?/blob/" . gk-urls-browse-github-file)
        ("^https?://raw\\.github\\.com/" . gk-urls-browse-github-raw)
        ("^http://www.cornucopia\\.local/" . gk-urls-browse-cornucopia)
        ("^https?://\\w+\\.wikipedia\\.org/" . gk-urls-browse-wikipedia)
        ("file:///home/.+/co/lisp/doc/HyperSpec/" . gk-browse-url)
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
  "Set up `eww' for easier reading.")

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



;;;;;; Print entry function:

(setf elfeed-search-print-entry-function 'gk-elfeed-print-entry)
(defun gk-elfeed-print-entry (entry)
  "Print ENTRY to the buffer.
Custom version of `elfeed-search-print-entry--default'."
  (let* ((date (format-time-string
                "%d %B, %R" (seconds-to-time (elfeed-entry-date entry))))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (window-width (- (window-width) 2))
         (feed-name (elfeed-feed-title feed))
         (feed-column
          (gk-truncate-and-fill-string
           (/ window-width 3) feed-name))
         (title-column
          (gk-truncate-and-fill-string
           (* (/ window-width 3) 2) title)))
    (insert (propertize title-column 'face title-faces
                        'help-echo (format "%s (%s)" title date)))
    (insert " ")
    (insert (propertize feed-column 'face 'elfeed-search-feed-face
                        'help-echo feed-name))))



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



;;;;;; Quicksearch:

(defun gk-elfeed-quicksearch (keys &optional search)
  "Define an elfeed quicksearch activated by «t» + the KEYS string

Define an ‘elfeed-serch-mode’ keybinding prepending the letter
«t» to the KEYS.  If SEARCH is nil, the search becomes
«+unread +KEYS», otherwise «+unread SEARCH»."
  (define-key elfeed-search-mode-map
    (concat "t" keys)
    (if search (gk-interactively
                (gk-elfeed-filter
                 (concat "+unread " search)))
      (gk-interactively
       (gk-elfeed-filter
        (concat "+unread +" keys))))))

(gk-elfeed-quicksearch "iu")
(gk-elfeed-quicksearch "yt")
(gk-elfeed-quicksearch "hn")
(gk-elfeed-quicksearch "ist")
(gk-elfeed-quicksearch "edu")
(gk-elfeed-quicksearch "mag")
(gk-elfeed-quicksearch "u" "")
(gk-elfeed-quicksearch "log" "+blog")
(gk-elfeed-quicksearch "eatro" "+teatro")



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
 `((elfeed-search-title-face nil :foreground "normal")
   (elfeed-search-unread-title-face nil :foreground "normal" :weight bold)
   (elfeed-search-tag-face nil :foreground "normal")
   (elfeed-search-date-face nil :foreground "normal")
   (elfeed-search-feed-face nil :foreground "normal" :slant italic)))



;;;;;; Scoring:

;; Adapted from http://kitchingroup.cheme.cmu.edu/blog/2017/01/05/Scoring-elfeed-articles/

(defface gk-relevant-elfeed-entry
  `((t :background "orange1"))
  "Marks a relevant Elfeed entry.")

(defface gk-important-elfeed-entry
  `((t :background "hotpink"))
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

    (cond
     ((= score 1)
      (elfeed-tag entry 'relevant))
     ((> score 1)
      (elfeed-tag entry 'important)))
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



;;;;; Mastodon:

(setf mastodon-instance-url "https://mastodon.sdf.org/")

;; Quick toot.
(defalias 'toot #'mastodon-toot)

;; Emacsify keybindings.
(define-key mastodon-mode-map "n" #'mastodon-tl--goto-next-toot)
(define-key mastodon-mode-map "p" #'mastodon-tl--goto-prev-toot)
(define-key mastodon-mode-map "+" #'mastodon-toot)
(define-key mastodon-mode-map "q" #'bury-buffer)
(define-key mastodon-mode-map "s" #'mastodon-tl--get-tag-timeline)
(define-key mastodon-mode-map "Q" #'kill-this-buffer)
(define-key mastodon-mode-map "/f" #'mastodon-tl--get-federated-timeline)
(define-key mastodon-mode-map "/h" #'mastodon-tl--get-home-timeline)
(define-key mastodon-mode-map "/l" #'mastodon-tl--get-local-timeline)

(add-hook
 'mastodon-mode-hook
 (defun gk-mastodon-mode-hook ()
   ;; Unmap unused keys.
  (dolist (key (map #'list #'string "jkFHLT"))
    (define-key mastodon-mode-map key nil))))



;;;; After Save™:

;; This is /the/ after save hook.  It's the one hook added to
;; =after-save-hook= that'll do all the things I might want automatically
;; done after when a file is saved.


(defvar gk-after-save-org-timer nil)
(defvar gk-after-save-org-idle-seconds 5)

(defun gk-after-save-hook ()
  "Göktuğ's After Save™, a man's best companion.
Does various tasks after saving a file, see it's definition."
  ;; Export agenda files when edited.
  (when-let* ((file (ignore-errors      ;expand-file-name signals if
                                        ;its first argument is nil.
                      (expand-file-name (buffer-file-name)))))
    (when (and (not gk-after-save-org-timer) ;check if there is an
                                        ;active timer, the timer
                                        ;callback nulls this
                                        ;variable
               (eq major-mode 'org-mode)
               (member file (mapcar #'expand-file-name org-agenda-files)))
      (message "Wrote an agenda file and there were no active\
 timers, will export ICS files when Emacs is idle for %d seconds"
               gk-after-save-org-idle-seconds)
      ;; Do not rush, query only if Emacs is idle.
      (setf
       gk-after-save-org-timer
       (run-with-idle-timer
        gk-after-save-org-idle-seconds nil
        (lambda ()
          (message "Regenerating ICS files...")
          (if (file-exists-p org-icalendar-combined-agenda-file)
              (org-icalendar-combine-agenda-files)
            (org-icalendar-export-agenda-files))
          (message "Done!")
          ;; Reset the timer.
          (setf gk-after-save-org-timer nil)))))))

(add-hook 'after-save-hook 'gk-after-save-hook)

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
(global-set-key [? ] 'gk-maybe-expand-abbrev-or-space)

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
(gk-prefix-binding [?\r] 'compile)

;;;; Shortcuts:

(gk-prefix-binding "k" 'recompile)
(gk-prefix-binding "\M-d" (gk-interactively (toggle-debug-on-error)
                                            (toggle-debug-on-quit)))
(gk-prefix-binding "r" 'rename-buffer)
(gk-prefix-binding "n" 'other-frame)
(gk-prefix-binding "\M-k" 'gk-kill-buffer-file-name)
(gk-prefix-binding "\M-k" 'gk-kill-buffer-file-name)
(gk-global-binding [home] 'gk-home)
(gk-prefix-binding "h" (gk-interactively
                        (when-let* ((b (get-buffer "*Help*")))
                          (switch-to-buffer b nil t))))
(gk-prefix-binding (kbd "C-#") 'display-line-numbers-mode)
(gk-prefix-binding "_" 'delete-other-windows-vertically)
(gk-prefix-binding "2" #'clone-indirect-buffer)
(gk-prefix-binding "t" #'gk-insert-today)
(gk-prefix-binding "~" #'gk-toggle-wrap)



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



;;; Additional modules:

(when (eq system-type 'berkeley-unix)
  (require 'bsdpkg))

(autoload 'twit "twittering-mode" "Emacs twitter client." t)



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
