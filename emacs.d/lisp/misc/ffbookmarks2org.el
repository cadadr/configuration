;;; ffbookmarks2org.el --- convert firefox bookmarks to an org file -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements an interactive function, ff2org, which can
;; generate an org file representing a hierarchy of bookmarks given a
;; JSON bookmarks export of Firefox.

;;; Code:

(require 'json)
(require 'cl-lib)

(defun ff2org-assoca (keyseq list)
  (let ((ks (if (listp keyseq) keyseq (list keyseq)))
        (ret list))
    (dolist (k ks ret)
      (setq ret (cdr (assoc k ret))))))

(defun ff2org-unix-timestamp-to-time-string (ts)
  (format-time-string
   "%FT%T%z"
   (seconds-to-time
    ;; We need a decimap place after the 10th number from left, see
    ;; (float-time)
    (/ ts 1e6))))

(defalias 'ff2org-ts 'ff2org-unix-timestamp-to-time-string)

(defun ff2org-print (item level make-todos)
  (if (vectorp item)
      (cl-loop for c across-ref item
               do (ff2org-print c level make-todos))
    (let* ((prefix (make-string level ?*))
           (item-title (assoca 'title item))
           (item-added (ff2org-ts (assoca 'dateAdded item)))
           (item-modified (ff2org-ts (assoca 'lastModified item)))
           (item-uri (if-let* ((uri (assoca 'uri item)))
                         (concat "\n[[" uri "]]\n")
                       ""))
           (item-tags (if-let* ((tags (ignore-errors
                                        (mapconcat
                                         #'identity
                                         (split-string (assoca 'tags item) ",") ":"))))
                          (concat " :" tags ":")
                        ""))
           (item-keyword (if-let* ((kw (assoca 'keyword item)))
                             (concat "\n:KEYWORD:\t" kw)
                           ""))
           (todo (if (and make-todos (not (or (= level 1) (string-empty-p item-uri))))
                     " TODO"
                   "")))
      (insert
       (format
        "%s%s %s%s\n:PROPERTIES:\n:ADDED:\t\t%s\n:MODIFIED:\t%s%s\n:END:\n%s\n"
        prefix todo item-title item-tags item-added item-modified item-keyword item-uri))
      (when-let* ((children (assoca 'children item)))
        (ff2org-print children (1+ level) make-todos)))))

(defun ff2org (json-file initial-level make-todos)
  "Generate an Org buffer from Firefox bookmarks JSON.
Generate from JSON-FILE an Org file, with equivalent hierarchy.
INITIAL-LEVEL is the heading level to start from.  If MAKE-TODOS
is non-nil, mark items with links as TODO."
  (interactive (list (read-file-name "Bookmarks json: ")
                     (read-number "Initial level: " 1)
                     (y-or-n-p "Make todos?")))
  (unless (and (integerp initial-level)
               (< 0 initial-level))
    (user-error "Initial level must be a natural number"))
  (let ((json (json-read-file json-file)))
    (with-current-buffer (get-buffer-create "*firefox output*")
      (erase-buffer)
      (ff2org-print json initial-level make-todos)
      (goto-char (point-min))
      (goto-char (line-end-position))
      (insert "Firefox Bookmarks")
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'ffbookmarks2org)
;;; ffbookmarks2org.el ends here
