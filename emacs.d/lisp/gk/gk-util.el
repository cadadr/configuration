;;; gk-util.el --- utility functions



;;; Code:

(require 'cl-lib)
(require 'hl-line)

(require 'gk-minor-mode)

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
            (unless (= l (length (cl-pushnew package load-path :test #'string=)))
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
    (cl-case full
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



;;; Footer:

(provide 'gk-util)
;;; gk-util.el ends here
