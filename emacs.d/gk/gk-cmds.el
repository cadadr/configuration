;;; gk-cmds.el --- miscellaneous interactive commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023  Göktuğ Kayaalp

;;; Commentary:

;; Unafilliated custom commands.

;;; Code:

(require 'cl-lib)
(require 'custom)
(require 'em-prompt)
(require 'rmail)
(require 'savehist)

(require 'gk-minor-mode)

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
      (looking-at
       (concat "[[:blank:]]*"
               (regexp-quote comment-start))))
    (uncomment-region (point-at-bol) (point-at-eol)))
   ((or (looking-at "[[:blank:]]*$")
        (region-active-p))
    (comment-dwim arg))
   (t (save-excursion (comment-line arg)))))

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

(defun gk-find-file (arg)
  "Like ‘find-file’ but find file at point if ARG is non-nil."
  (interactive "P")
  ;; See http://lists.gnu.org/archive/html/help-gnu-emacs/2018-04/msg00280.html
  (let ((current-prefix-arg nil))
    (call-interactively (if arg #'ffap #'find-file))))

(define-obsolete-function-alias 'gk-update-package-load-paths
  'gk-update-user-site-paths "2020-09-23")

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

(defun gk-view-emacs-proc-file ()
  "Open the Emacs process status file under /proc."
  (interactive)
  (find-file (format "/proc/%d/status" (emacs-pid))))

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
(cl-pushnew 'gk-insert-todo-comment--history savehist-additional-variables)
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

(defun gk-build-emacs (&optional nofetch)
  "Build Emacs \"master\" branch from local git checkout.

If called with prefix argument or NOFETCH is non-nil, do not
fetch from upstream repo, only build the local checkout."
  (interactive (list (not (null current-prefix-arg))))
  (let ((compilation-buffer-name-function ($ [_] "*Build Emacs*"))
        (process-environment (if nofetch
                                 (cons "NOFETCH=yes" process-environment)
                               process-environment)))
    (compile "build-emacs.sh" t)))

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
  "Decode XML entities in region (interactively) or between BEGINNING and END."
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
  (find-file (format-time-string
              (gk-org-dir-file "Günlük/Kişisel/%F")))
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
     (gk-assoca
      (completing-read "Window with buffer: " winbufs)
      winbufs))))

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
  (gk-fetch-mail (lambda (new-mail-p)
                   (when new-mail-p
                     (message "Getting new mail from inboxes...")
                     (save-window-excursion
                       (with-current-buffer
                           (progn
                             (rmail)
                             (current-buffer))
                         (rmail-get-new-mail)))))))


;; Adapted from: https://christiantietze.de/posts/2021/06/emacs-center-window-on-current-monitor/

(defun gk-frame-recenter (&optional frame)
  "Center a frame on the current display."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((w (frame-pixel-width frame))
           (h (frame-pixel-height frame))
           (cw (caddr (frame-monitor-workarea frame)))
           (ch (cadddr (frame-monitor-workarea frame)))
           (center (list (/ (- cw w) 2) (/ (- ch h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))


(defun gk-face-and-font-family-at-point (point)
  "Face identifier and font family at point.

Interactively, POINT is the current location of the point in the
current buffer; the information will be revealed in the echo
area.

When called from Lisp, a plist with the same information is
returned, where the face name is a symbol and the font family is
a string, the family name."
  (interactive (list (point)))
  (let* ((face (save-excursion (goto-char (point)) (face-at-point)))
         (ffam (font-get (font-at point) :family)))
    (when (called-interactively-p 'any)
      (message "At %s:%d: face is ‘%s’; font is ‘%s’."
               (buffer-name) point face ffam))
    (list :family ffam :face face)))

(defun gk-reload-enabled-themes ()
  "Reload all the loaded themes, preserving order."
  (interactive)
  (let ((themes custom-enabled-themes))
    (mapc #'disable-theme themes)
    (mapc ($ (when $1 (load-theme $1 t))) themes)))

(defun gk-yank-primary ()
  "Yank from the primary X clipboard."
  (interactive)
  ;; XXX(2022-01-02): Following taken from ‘mouse-yank-primary’.
  (when select-active-regions
    (let (select-active-regions)
      (deactivate-mark)))
  (let ((primary (gui-get-primary-selection)))
    (push-mark)
    (insert-for-yank primary)))


(defun gk-auto-rename-directory-files
    (source-directory target-directory &optional trash-sources-p overwrite-p)
  "Bulk rename SOURCE-DIRECTORY files using hashes and proper extensions.

For each file under SOURCE-DIRECTORY, determine if file(1) knows
a canonical file name extension. If yes, copy those files to
TARGET-DIRECTORY with a file name generated using the hash of the
contents of the file, and the appropriate suffix file(1)
suggests.

If TRASH-SOURCES-P is non-nil, use ‘move-file-to-trash’ to trash
the source file after copying it to the new path.

If OVERWRITE-P is non-nil, allow overwriting if target files
exist.

In Lisp, returns an alist of source paths and target paths of
those files that were actually copied over."
  (interactive
   (list
    (read-directory-name "Source directory: " nil nil t)
    (read-directory-name
     "Target directory (will be created if doesn't exist): ")
    (yes-or-no-p "Trash source files after copying? ")
    (yes-or-no-p "Allow overwriting when target file exists? ")))
  (when (string= source-directory target-directory)
    (user-error "Source and target directories cannot be the same"))
  (let* ((begin-time (current-time))
         (files (directory-files
                 source-directory t
                 directory-files-no-dot-files-regexp))
         (transformation-data (gk-auto-rename-directory-files-1 files))
         processed-files end-time)
    (make-directory target-directory t)
    (pcase-dolist
        (`(,source-path ,maybe-true-extension ,hash-string) transformation-data)
      (let* ((true-extension
              (if (string= "???" maybe-true-extension)
                  (gk-auto-rename-directory-files-3 source-path)
                maybe-true-extension))
             (target-path (expand-file-name
                           (concat hash-string "." true-extension)
                           target-directory)))
        (condition-case sig
            (progn
              (unless (null true-extension)
                (copy-file source-path target-path overwrite-p t t t)
                (push (cons source-path target-path) processed-files)
                (message "Transformed %s -> %s" source-path target-path)))
          ('file-already-exists
           (message "File %s exists, skipping (source: %s)"
                    target-path source-path)))))
    (when trash-sources-p
      (mapc ($ (move-file-to-trash (car $1))
               (message "Trashed %s (new path: %s)" (car $1) (cdr $1)))
            processed-files))
    (setf end-time (current-time))
    (message "Copied %d files in %d seconds"
             (length processed-files)
             (round (float-time (time-subtract end-time begin-time))))
    processed-files))


(defun gk-auto-rename-directory-files-1 (files)
  "Subroutine of ‘gk-auto-rename-directory-files’."
  (mapcar
   (lambda (file)
     (list file
           ;; Determine a fitting extension for the current file using
           ;; file(1) utility. This will not be fooled by mistaken
           ;; extensions.
           (with-temp-buffer
             (call-process
              ;; TODO(2022-07-24): use --print0
              "file" nil t nil "-F" "///" "--extension" file)
             (car (split-string
                   (s-trim (cadr (split-string
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))
                                  "/// ")))
                   "/")))
           ;; sha1sum of file in order to rename it uniquely.
           (let ((buf (find-file-noselect file t t)))
             (prog1
                 (secure-hash 'sha1 buf)
               (kill-buffer buf)))))
   files))

(defun gk-auto-rename-directory-files--mime2ext (mimetype)
  ;; Shared mimetype to file extension mappings for subroutines of
  ;; ‘gk-auto-rename-directory-files’.
  (cond ((string= mimetype "image/jpeg")     "jpeg")
        ((string= mimetype "image/png")      "png")
        ((string= mimetype "image/svg+xml")  "svg")
        ((string= mimetype "video/mp4")      "mp4")
        ((string= mimetype "video/webm")     "webm")
        ( t                                  nil)))

(defun gk-auto-rename-directory-files-2 (file)
  "Subroutine of ‘gk-auto-rename-directory-files’."
  ;; Try to determine an appropriate file name extension for those
  ;; files for which file(1) fails to return one.
  ;;
  ;; *Do not use this*, just looks at the file name extension
  ;; apparently...
  (gk-auto-rename-directory-files--mime2ext (mm-default-file-type file)))

(defun gk-auto-rename-directory-files-3 (file)
  "Subroutine of ‘gk-auto-rename-directory-files-2’."
  ;; Like gk-auto-rename-directory-files-2, but uses the output of
  ;; file(1) instead of ‘mm-default-file-type’. We ignore any errors,
  ;; as it makes sense to consider an error case a case of "can’t
  ;; determine info about this file".
  (ignore-errors
    (gk-auto-rename-directory-files--mime2ext
     (with-temp-buffer
       (call-process "file" nil t nil "--brief" "--mime" file)
       (car (split-string
             (buffer-substring-no-properties (point-min) (point-max))
             ";"))))))



(provide 'gk-cmds)
;;; gk-cmds.el ends here
