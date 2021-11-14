;;; doifetch.el --- fetch DOIs as bibtex             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Package-Requires: ((emacs "26") (request "0.3.2") (s "1.9.0") (ebib "2.31"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TBD.

;;; Code:

(require 'request)
(require 's)
(require 'bibtex)
(require 'ebib)
(require 'widget)
(require 'thingatpt)

(defcustom doifetch-success-callback
  #'doifetch-widget-display
  "Function to call after a successful retrieval.

This function will be called with two arguments:
DOI, a string, the full DOI url.
DATA, the BibTeX data retrieved."
  :type 'function)


(defvar doifetch--history nil
  "Minibuffer history for ‘doifetch’.")

;;;###autoload
(defun fetch-doi (doi &optional sync)
  "Fetch a DOI.

If not prefixed with \"https://doi.org/\", it will be added.

If SYNC is non nil, or called with a prefix argument, the DOI
will be fetched synchronously."
  (interactive (list (let ((str (s-trim
                                 (read-string "DOI to fetch: "
                                              (doifetch--doi-at-point)
                                              'doifetch--history))))
                       (if (s-starts-with? "http" str)
                           str
                         (concat "https://"
                                 (if (s-starts-with? "doi.org/" str)
                                     str
                                   (concat "doi.org/" str)))))
                     (not (not current-prefix-arg))))
  (message "Fetching %s" doi)
  (doifetch--do-fetch doi sync))


(defun doifetch--doi-at-point ()
  (let ((maybe-doi
         (or (thing-at-point-url-at-point t)
             ;; XXX(2021-04-22): gotta check doi spec for this, or
             ;; bin/doi2bib.pl.
             (when-let* ((thing (thing-at-point 'symbol)))
               (when (string-match-p (rx (and (1+ numeric) "." (1+ numeric) "/"))
                                     thing)
                 thing)))))
    ;; Could’ve done regex but it’s complex even with ‘rx’...
    (when (or (s-starts-with? "https://doi.org/" maybe-doi)
              (s-starts-with? "http://doi.org/" maybe-doi)
              (s-starts-with? "doi.org/" maybe-doi)
              (not (s-starts-with? "http" maybe-doi)))
      maybe-doi)))


(defun doifetch--do-fetch (doi &optional sync)
  (request doi
           :sync sync
           :headers '(("Accept" . "application/x-bibtex; charset=utf-8"))
           :parser 'buffer-string
           :success
           (cl-function
            (lambda (&key data &allow-other-keys)
              (funcall
               doifetch-success-callback doi
               ;; Modify the data to generate a nice BibTeX key.
               (with-temp-buffer
                 (insert data)
                 (let ((key (bibtex-generate-autokey)))
                   (goto-char (point-min))
                   (save-match-data
                     (goto-char (re-search-forward "{"))
                     (kill-line)
                     (insert key ",")))
                 (buffer-string)))))))


(defun doifetch-widget-display (doi data)
  "Display the BibTeX DATA for DOI.

Display the BibTeX in a widget buffer, where it can be edited,
yanked, and/or imported to Ebib."
  (with-current-buffer (generate-new-buffer (format "*bibtex for %s*" doi))
    (let ((inhibit-read-only t)
          edit)
      (erase-buffer)
      (setq-local
       header-line-format
       (concat "Retrieved bibtex: "
               (propertize doi 'face 'link)))
      (newline)
      (widget-insert
       (mapconcat
        #'identity
        (list
         "The following is the bibtex response to the given DOI. You can"
         "edit the contents as you see fit, and then hit the ‘Yank’ button"
         "in order to save the current contents of the field to the kill"
         "ring, or ‘Add to Ebib...’ to start ‘ebib’ and insert it into the"
         "database, or ‘Quit’ to exit.")
        " "))
      (fill-paragraph)
      (newline 2)
      (setq edit (widget-create 'editable-field :value data))
      (newline)
      (widget-create
       'push-button
       :value "Yank"
       :notify ($ (with-temp-buffer
                    (insert (widget-value edit))
                    (clipboard-kill-ring-save (point-min) (point-max))
                    (message "BibTeX yanked!"))))
      (widget-insert "\t")
      (widget-insert "\t")
      (widget-create
       'push-button
       :value "Add to Ebib..."
       :notify ($ (let (key)
                    (ebib)
                    (with-temp-buffer
                      (insert (widget-value edit))
                      (goto-char (point-min))
                      (setq key
                            (cdr (assoc
                                  "=key="
                                  (bibtex-parse-entry) #'string=)))
                      (ebib-import))
                    (ebib--update-index-buffer)
                    (ebib--goto-entry-in-index key)
                    (ebib--update-entry-buffer))))
      (widget-insert "\t")
      (widget-create
       'push-button
       :value "Quit"
       :notify ($ (bury-buffer)))
      (use-local-map
       (let ((map widget-keymap))
         (prog1 map
           (define-key map [?q] #'bury-buffer))))
      ;; Put the cursor on the [Yank] button
      (goto-char (line-beginning-position))
      (widget-setup)
      (pop-to-buffer (current-buffer)))))

(provide 'doifetch)
;;; doifetch.el ends here
