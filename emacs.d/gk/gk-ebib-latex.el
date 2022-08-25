;;; gk-ebib-latex.el --- configurations for ebib, latex, auctex, etc  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;;; Commentary:

;; Configurations for TeX based authoring and bibliography tools.

;;; Code:

(require 'bibtex)
(require 'ebib)
(require 'ebib-keywords)
(require 'ebib-notes)
(require 'ebib-utils)

;;;; AuCTeX:

;; Accomodate AuCTeX.
(setenv "TEXINPUTS" (concat "::" (expand-file-name "auctex/texmf" (symbol-value 'gk-elisp-site-dir))))
(require 'auctex)
(require 'preview-latex)



;;;; BibTeX:

(defvar gk-bib-dir (expand-file-name "~/Library")
  "Location for global Bib(La)TeX files.")

(defvar gk-bib-attachments-dir
  (expand-file-name "Attachments" gk-bib-dir)
  "Global store for bibliography attachments.")



;;;; Ebib:

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


;; Add a ‘%G’ specifier that generates the title the way I like it.
(cl-pushnew
 '(?G . gk-ebib-create-org-title)
 ebib-notes-template-specifiers)

(defun gk-ebib-create-org-title (key db)
  "Modified version of ‘ebib-create-org-title’."
  (if (null key)
      "[No title]"
    (let ((author (replace-regexp-in-string
                   " and "
                   "; "
                   (or (ebib-get-field-value "author" key db 'noerror 'unbraced 'xref)
                       (ebib-get-field-value "editor" key db 'noerror 'unbraced 'xref)
                       "")))
          (title (or (ebib-get-field-value "title" key db 'noerror 'unbraced 'xref)
                     (user-error "Reading note for item without title disallowed"))))
      (remove ?\n (format "%s%s" title (if (string-empty-p author)
                                           ""
                                         (concat ". " author)))))))

(define-advice ebib-create-org-file-link
    (:override (key db) be-eager)
  "Eagerly return an associated file or something that can help find one.

Ebib only looks at the file field.  This function looks there, or
if there’s a file indirectly associated to key it returns that.
If not found, it attempts a DOI.  Failing that also, the
associated URL is returned.  Worst case, an error report is
included in the generated template."
  (let ((file (ebib--select-file
               (ebib-get-field-value
                "file"
                key db 'noerror 'unbraced 'xref)
               nil key))
        (url (ebib-get-field-value
              "url"
              key db 'noerror 'unbraced 'xref))
        (doi (ebib-get-field-value
              "doi"
              key db 'noerror 'unbraced 'xref)))
    (cond ((and file (not (string-empty-p file)))
           (format "[[file:%s]]"
                   (ebib--expand-file-name file)))
          ((and doi (not (string-empty-p doi)))
           (format "[[doi:%s]]"
                   ;; Ensure doi does not include a
                   ;; "https://(dx.)?doi.org/?" prefix.
                   (replace-regexp-in-string
                    "^/" ""
                    (car (url-path-and-query (url-generic-parse-url doi))))))
          ((and url (not (string-empty-p url)))
           (format "[[%s]]" url))
          (t
           "{no file, DOI, or URL}" key))))

(setf
 ebib-file-associations nil
 ebib-preload-bib-files (list (expand-file-name "Library.bib" gk-bib-dir))
 ebib-file-search-dirs (list gk-bib-dir)
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

 ;; Use org-capture template with the key = E to add reading notes.
 ebib-notes-use-org-capture "E"
 ebib-notes-template (mapconcat #'identity
                                (list "* %G"
                                      ":PROPERTIES:"
                                      "%K"
                                      ":END:"
                                      "- %C"
                                      "- %F"
                                      "- Created at %%U"
                                      "\n-----\n"
                                      "%%?\n")
                                "\n")

 ;; see ‘ebib-extra-fields’, can be used to mark collections; ‘a’ adds
 ;; extra fields in entry buffer.

 ;; see ‘ebib-hidden-fields’, and kbd ‘H’

 ;; Combine author-year and title.
 ebib-citation-description-function
 ($ [key db]
    (replace-regexp-in-string
     (rx "\\") (rx "\\\\")
     (format "%s. (%s). %s {@%s}"
             (ebib--get-field-value-for-display "Author/Editor" key db)
             (ebib--get-field-value-for-display "Year" key db)
             (ebib-get-field-value "Title" key db "(Untitled)" 'unbraced 'xref)
             key))))


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

;; Override ‘org-capture’ in Ebib to use helper for org-capture based
;; reading notes.
(define-key ebib-index-mode-map [remap org-capture] #'ebib-org-capture)
(define-key ebib-entry-mode-map [remap org-capture] #'ebib-org-capture)



;;;; Sci-Hub:

(defvar gk-scihub-url "https://sci-hub.se"
  "Base URL for Sci-Hub.")

(defvar gk--fetch-sci-hub-pdf--history nil)

(defun gk-scihub-pdf (identifier &optional callback)
  "Try to fetch the PDF from Sci-Hub given an IDENTIFIER.

Viz. ‘gk-scihub-url’.

Called interactively, try to retrieve the PDF for a given
IDENTIFIER, and on success, visit it in a temporary file.

Called from Lisp, an optional CALLBACK can be passed, which on
success will be called with a single argument: the path of the
temporary file for the retrieved PDF."
  ;; TODO(2022-08-25): in ebib find from item at point
  (interactive
   (list
    (read-string "Sci-Hub identifier: "
                 nil 'gk--fetch-sci-hub-pdf--history nil t)))
  (let ((url (concat gk-scihub-url "/" identifier)))
    (url-retrieve url #'gk--scihub-pdf-1 callback t t)
    (message "Fetching %s..." url)))

(defun gk--scihub-pdf-1 (status &rest callback)
  "Subroutine of ‘gk-scihub-pdf’."
  (pcase-dolist (`(,type . ,data) status)
    (when (equal type :error)
      (user-error "Error fetching Sci-Hub page: %S" data)))
  (let* ((body-start (progn (goto-char (point-min))
                            (re-search-forward "\n\n" nil t)))
         (dom (libxml-parse-html-region body-start (point-max)))
         (pdf-elem (progn (when (string= "Sci-Hub: article not found"
                                         (dom-text (dom-by-tag dom 'title)))
                            (user-error "Sci-Hub: article not found"))
                          (dom-by-id dom "pdf")))
         (pdf-path-dirty (dom-attr pdf-elem 'src))
         (pdf-path (car (split-string pdf-path-dirty "#")))
         (pdf-url (concat gk-scihub-url pdf-path)))
    (url-retrieve pdf-url #'gk--scihub-pdf-2 callback t t)
    (message "Fetching %s..." pdf-url)))

(defun gk--scihub-pdf-2 (status &rest callback)
  "Subroutine of ‘gk--scihub-pdf-1’."
  (pcase-dolist (`(,type . ,data) status)
    (when (equal type :error)
      (user-error "Error fetching Sci-Hub PDF: %S" data)))
  (let* ((body-start (progn (goto-char (point-min))
                            (re-search-forward "\n\n" nil t)))
         (pdf-content (buffer-substring-no-properties body-start (point-max)))
         (temporary-pdf-file (make-temp-file "gk-scihub-pdf" nil ".pdf" pdf-content)))
    (apply (or callback #'find-file) (list temporary-pdf-file))))




(provide 'gk-ebib-latex)
;;; gk-ebib-latex.el ends here
