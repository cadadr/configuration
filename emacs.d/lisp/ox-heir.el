;;; ox-heir.el --- Heirloom Doctools Back-End for Org Export Engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: outlines, hypermedia, wp, calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an export backend for Org Mode to create structured
;; documents using the heirloom-doctools [1] package, which “provides
;; troff, nroff, and related utilities to format manual pages and
;; other documents for output on terminals and printers”.  While there
;; already exists a package to translate Org documents into groff
;; memorandum macroes [2], heirloom-doctools comes with many
;; enhancements for outputting to portable formats (e.g. using
;; OpenType and TrueType fonts directly, many typographical
;; enhancements) and for processing the input to itself (e.g. UTF-8
;; i/o).

;; ox-groff.el [2] has been used as a guide and inspiration in the
;; implementation of this library.  Thanks goes to its authors.

;; [1] http://heirloom.sourceforge.net/doctools.html

;; [2] ox-groff.el, included in Org mode source repo as a contributed
;; package.

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'ox)


;;; Back-end Definition:

(org-export-define-backend 'heir
  '((bold . org-heir-bold)
    (center-block . org-heir-center-block)
    (clock . org-heir-clock)
    (code . org-heir-code)
    (drawer . org-heir-drawer)
    (dynamic-block . org-heir-dynamic-block)
    (entity . org-heir-entity)
    (example-block . org-heir-example-block)
    (export-block . org-heir-export-block)
    (export-snippet . org-heir-export-snippet)
    (fixed-width . org-heir-fixed-width)
    (footnote-definition . org-heir-footnote-definition)
    (footnote-reference . org-heir-footnote-reference)
    (headline . org-heir-headline)
    (horizontal-rule . org-heir-horizontal-rule)
    (inline-src-block . org-heir-inline-src-block)
    (inlinetask . org-heir-inlinetask)
    (italic . org-heir-italic)
    (item . org-heir-item)
    (keyword . org-heir-keyword)
    (line-break . org-heir-line-break)
    (link . org-heir-link)
    (node-property . org-heir-node-property)
    (paragraph . org-heir-paragraph)
    (plain-list . org-heir-plain-list)
    (plain-text . org-heir-plain-text)
    (planning . org-heir-planning)
    (property-drawer . org-heir-property-drawer)
    (quote-block . org-heir-quote-block)
    (radio-target . org-heir-radio-target)
    (section . org-heir-section)
    (special-block . org-heir-special-block)
    (src-block . org-heir-src-block)
    (statistics-cookie . org-heir-statistics-cookie)
    (strike-through . org-heir-strike-through)
    (subscript . org-heir-subscript)
    (superscript . org-heir-superscript)
    (table . org-heir-table)
    (table-cell . org-heir-table-cell)
    (table-row . org-heir-table-row)
    (target . org-heir-target)
    (template . org-heir-template)
    (timestamp . org-heir-timestamp)
    (underline . org-heir-underline)
    (verbatim . org-heir-verbatim)
    (verse-block . org-heir-verse-block))
  :menu-entry
  '(?g "Export to Heirloom Doctools Troff"
       ((?t "As Troff file" org-heir-export-to-heir)
	(?p "As PostScript file" org-heir-export-to-ps)
	(?P "As PDF file" org-heir-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (async subtreep visible-only body-only)
	      (if async
                  (org-heir-export-to-pdf t subtreep visible-only body-only)
		(org-open-file 
                 (org-heir-export-to-pdf nil subtreep visible-only body-only)))))))
  :options-alist
  '((:heir-class "HEIRLOOM_CLASS" nil org-heir-default-class t)
    (:heir-class-options "HEIRLOOM_CLASS_OPTIONS" nil nil t)
    (:heir-header-extra "HEIRLOOM_HEADER" nil nil newline)))



;;; Configurables:

(defgroup org-export-heir nil
  "Options for exporting Org mode files to Heirloom Doctools."
  :tag "Org Export Heirloom Doctools"
  :group 'org-export)




(provide 'ox-heir)
;;; ox-heir.el ends here
