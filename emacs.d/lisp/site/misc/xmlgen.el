;;; xmlgen.el --- A DSL for generating XML.

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.5

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Generate xml using sexps with the function `xmlgen':

;; (xmlgen '(p :class "big"))      => "<p class=\"big\" />")
;; (xmlgen '(p :class "big" "hi")) => "<p class=\"big\">hi</p>")

;; (xmlgen '(html
;;           (head
;;            (title "hello")
;;            (meta :something "hi"))
;;           (body
;;            (h1 "woohhooo")
;;            (p "text")
;;            (p "more text"))))

;; produces this (though wrapped):

;; <html>
;;   <head>
;;     <title>hello</title>
;;     <meta something="hi" />
;;   </head>
;;   <body>
;;     <h1>woohhooo</h1>
;;     <p>text</p>
;;     <p>more text</p>
;;   </body>
;; </html>

;;; Code:

(require 'cl-lib)

(defvar xmlgen-escape-attribute-vals t
  "When non-nil xmlgen will escape the characters <>\\='\"& in an attribute value.")

(defvar xmlgen-escape-elm-vals t
  "When non-nil xmlgen will escape the characters <>\\='\"& in an elements content.")

(defvar xmlgen-escapees
  '(("&"  . "&amp;")
    ("'"  . "&apos;")
    ("\"" . "&quot;")
    ("<"  . "&lt;")
    (">"  . "&gt;"))
    "List of (FIND . REPLACE) pairs for escaping.
See `xmlgen-escape-elm-vals' and `xmlgen-escape-attribute-vals'.")

;;;###autoload
(defun xmlgen (form &optional in-elm level)
  "Convert a sexp FORM to xml:
\\='(p :class \"big\")) => \"<p class=\\\"big\\\" />\".
IN-ELM is ignored.  LEVEL is the element level and defaults to 0."
  (let ((level (or level 0)))
    (cond
      ((numberp form) (number-to-string form))
      ((stringp form) form)
      ((listp form)
       (cl-destructuring-bind (xml attrs) (xmlgen-extract-plist form)
         (let ((el (car xml)))
           (unless (symbolp el)
             (error "Element must be a symbol (got %S)" el))
           (if (member el '(!unescape !escape))
               (let ((xmlgen-escape-elm-vals (if (equal '!escape el) t nil)))
                 (mapconcat
                  (lambda (s) (xmlgen s in-elm (1+ level)))
                  (cdr xml)
                  ""))
             (progn
                 (setq el (symbol-name el))
                 (concat "<" el (xmlgen-attr-to-string attrs)
                         (if (> (length xml) 1)
                             (concat ">" (mapconcat
                                          (lambda (s) (xmlgen s el (1+ level)))
                                          (if xmlgen-escape-elm-vals
                                              (mapcar 'xmlgen-string-escape (cdr xml))
                                            (cdr xml))
                                          "")
                                     "</" el ">")
                           "/>"))))))))))

(defun xmlgen-string-escape (string)
  "Escape STRING for inclusion in some XML."
  (when (stringp string)
    (mapc
     (lambda (e)
      (setq string
       (replace-regexp-in-string (car e) (cdr e) string)))
     xmlgen-escapees))
  string)

(defun xmlgen-attr-to-string (plist)
  "Convert a PLIST to xml style attributes."
  (let ((res ""))
    (while plist
      (let* ((sym (pop plist))
             (val (pop plist))
             (treated (cond
                        ((numberp val)
                         (number-to-string val))
                        ((stringp val)
                         val))))
        (setq res
              (concat res " " (substring (symbol-name sym) 1 ) "=\""
                      (if xmlgen-escape-attribute-vals
                          (xmlgen-string-escape treated)
                          treated)
                      "\""))))
    res))

(defun xmlgen-extract-plist (list)
  "Extract a plist from LIST returning the original list without the plist and the plist."
  (let ((nlist '())
        (plist '())
        (last-keyword nil))
    (mapc
     (lambda (item)
       (let ((item (pop list)))
         (cond
          (last-keyword
           (setq plist (append plist (list last-keyword)))
           (setq plist (append plist (list item)))
           (setq last-keyword nil))
          ((keywordp item) (setq last-keyword item))
          (t (setq nlist (append nlist (list item)))))))
     list)
    (when last-keyword
      (error "No value to satisfy keyword '%s'"
             (symbol-name last-keyword)))
    (list nlist plist)))

(provide 'xmlgen)

;;; xmlgen.el ends here
