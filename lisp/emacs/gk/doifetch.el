;;; doifetch.el --- fetch DOIs as bibtex             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>

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
  (interactive (list (let ((str (read-string "DOI to fetch: "
                                             (doifetch--doi-at-point)
                                             'doifetch--history)))
                       (if (s-starts-with? "http" str)
                           str
                         (concat "https://"
                                 (if (s-starts-with? "doi.org/" str)
                                     str
                                   (concat "doi.org/" str)))))
                     (not (not current-prefix-arg))))
  (message "Fetching %s" doi)
  (doifetch-do-fetch doi sync))


(defun doifetch--doi-at-point ()
  (thing-at-point-url-at-point t (cons (line-beginning-position)
                                       (line-end-position))))


(defun doifetch--do-fetch (doi &optional sync)
  (request doi
           :sync sync
           :headers '(("Accept" . "application/x-bibtex; charset=utf-8"))
           :parser 'buffer-string
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (funcall doifetch-success-callback doi data)))))


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
       "The following is the bibtex response to the given DOI. You can"
       "edit the contents as you see fit, and then hit the ‘Yank’ button"
       "in order to save the current contents of the field to the kill"
       "ring, or ‘Add to Ebib...’ to start ‘ebib’ and insert it into the"
       "database, or ‘Quit’ to exit.")
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
