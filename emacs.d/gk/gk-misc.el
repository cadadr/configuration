;;; gk-misc.el --- miscellaneous utility functions   -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2023  Göktuğ Kayaalp

;;; Commentary:

;; Utility functions not associated to any package, library or
;; feature.

;;; Code:

(defun gk-backup-file-name (directory extension)
  (let ((filename (concat directory
                          (format-time-string "%d-%m-%Y" (current-time))))
        (extension (concat "." extension)))
    (while (file-exists-p (concat filename extension))
      (setq filename (concat filename "+")))
    (concat filename extension)))

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

(defun gk-truncate-and-fill-string (len s)
  (let ((slen (length s)))
    (if (> slen len)
        (s-truncate len s)
      (concat s (make-string (- len slen) ?\ )))))

(defun // (&rest args)
  (apply #'/ (mapcar #'float args)))

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

(defun gk-append-to-list (list-name elt)
  "Append ELT to list names LIST-NAME."
  (set list-name (apply #'append (symbol-value list-name) (list elt))))

(defun gk-save-string-as-kill (str)
  "Push STR on kill ring, syncing with the clipboard."
  (with-temp-buffer
    (insert str)
    (clipboard-kill-ring-save (point-min) (point-max))
    (message "Copied %s" str)))

;; ‘gk-dissoc’ and ‘gk-assoca’: helper functions for association
;; lists.

(defun gk-dissoc (key list &optional test-fn)
  "Delete pairs whose car is equal to KEY from LIST.

TEST-FN defaults to ‘equal’."
  (gk-dissoc--1 key list (or test-fn #'equal) nil))

(defun gk-dissoc--1 (key list test-fn arg)
  (let ((p (car list))
        (r (cdr list)))
    (if list
        (if (funcall test-fn (car p) key)
            (gk-dissoc--1 key r test-fn arg)
          (gk-dissoc--1 key r test-fn (append arg (list p))))
      arg)))


(defmacro gk-dissoc! (key sym test-fn)
  "Call ‘gk-dissoc’ with args and set SYM to result."
  `(setq ,sym (gk-dissoc ,key ,sym ,test-fn)))


(defun gk-assoca (keyseq list)
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

(define-obsolete-function-alias 'assoca 'gk-assoca "2021-10-14")


;; Swap string case:

(defun gk-invert-case (str)
  "Invert the letter case of each character of STR."
  (apply #'string
         (mapcar ($ (if (= $1 (upcase $1))
                        (downcase $1)
                      (upcase $1)))
                 (string-to-list str))))

(defalias 'gk-swap-case #'gk-invert-case)



(provide 'gk-misc)
;;; gk-misc.el ends here
