;;; firefox-bookmarks-to-xombrero.el --- Firefox bookmarks to xombrero. -*- lexical-binding: t; -*-

;; Copyright (c) 2015, Göktuğ Kayaalp
;; All rights reserved.

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: xombrero, firefox, bookmarks, utils
;; Package-Version: 0.0.0
;; Package-Requires: (())
;; URL: http://www.gkayaalp.com/emacs.html

;; This file with its contents, is placed on the public domain.

;;; Commentary:

;; Export bookmarks from bookmarks.json to xombrero favorites format.

;; - From firefox, C-S-o  (show all bookmarks), "Import  and Export" >
;;   "Backup..."

;; - Call `fbtx-do' interactively

;; - If   everything   is  OK,   it'll   pop   up  a   buffer   called
;;   =*fbtx-exporting bookmarks*=,  copy its contents and  append into
;;   =~/.xombrero/favorites=.

;;; Code:
(require 'gk-mac)
(require 'gk-utils)

(defvar fbtx-buffer nil)

(defvar fbtx-json nil)

(defvar fbtx-out-buffer nil)

(defvar fbtx-exported 0)

(defvar fbtx-ignored 0)

(defun fbtx-write-bookmark (b)
  "Write bookmarks.  Skips keywords, as they should be represented as aliases.

B is a `cons', representing a bookmark."
  (if (assoca '(keyword) b)
      (progn
        (incf fbtx-ignored))
    (progn
      (when-let (uri (assoca '(uri) b))
        (with-current-buffer fbtx-out-buffer
          (let ((title (assoca '(title) b)))
            (when (string-empty-p title)
              (setq title nil))
            (goto-char (point-max))
            (insert (format "%s\n%s\n"
                            (or title
                                uri)
                            uri))
            (incf fbtx-exported)))))))

(defun fbtx-do-all (d)
  "Work out all bookmarks in D."
  (cond
   ((eq 'vector (type-of d))
    (mapc #'fbtx-do-all d))
   ((eq 'children (caar d))
    (fbtx-do-all (cdar d)))
   ((eq 'cons (type-of d))
    (fbtx-write-bookmark d))
   (t
    (message "%S" d))))

(defun fbtx-load (file)
  "Load FILE."
  (interactive (list (read-file-name "Your .json file of bookmarks: ")))
  (setf fbtx-buffer (find-file-noselect file))
  (setf fbtx-json (with-current-buffer fbtx-buffer
                    (json-read-from-string
                     (buffer-substring (point-min)
                                       (point-max)))))
  (when (buffer-live-p fbtx-out-buffer)
    (kill-buffer fbtx-out-buffer))
  (setf fbtx-out-buffer (get-buffer-create "*fbtx--exporting bookmarks*")))

(defun fbtx-cleanup ()
  "Clean-up after run."
  (ignore-errors
    (kill-buffer fbtx-buffer)
    (setf fbtx-json nil
          fbtx-exported 0
          fbtx-ignored 0)))

(defun fbtx-go ()
  "Run actions, then show buffer."
  (interactive)
  (call-interactively 'fbtx-load)
  (fbtx-do-all fbtx-json)
  (switch-to-buffer-other-window fbtx-out-buffer)
  (message "%d exported, %d ignored, %d processed of %d total"
           fbtx-exported fbtx-ignored
           (+ fbtx-exported fbtx-ignored)
           (with-current-buffer fbtx-buffer
             (count-matches "\"uri\"")))
  (fbtx-cleanup))

(provide 'firefox-bookmarks-to-xombrero)
;;; firefox-bookmarks-to-xombrero.el ends here
