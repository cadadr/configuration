;;; ankara-theme.el --- colour theme based on the city of Ankara  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: faces

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

;; This colour theme is a love letter to Ankara, the second largest
;; and the capital city of Turkey, where I did my master’s.

;;; Code:

(require 'color)

(deftheme ankara "A colour theme based on the city of Ankara")

(let* ((bg1 "gray99")
       (bg2 "gray88")
       (bg3 "navy")

       ;; match these with bg*
       (fg1 "black")
       (fg2 "dark blue")
       (fg3 "snow")

       ;; bg highlights, including diffs
       (hl1 "turquoise")
       (hl2 "lavender blush")
       (hl3 "lightyellow1")
       (hl4 "azure1")

       ;; font lock palette
       (fl1 "royal blue")
       (fl2 "dark magenta")
       (fl3 "navy")
       (fl4 "sienna")
       (fl5 "dark olive green")
       (fl6 "gray15"))
  (apply
   #'custom-theme-set-faces
   'ankara
   (mapcar
    (lambda (decl)
      ;; `(face-name ((t (decls....))))
      (cons (car decl) (list (list (list t (cdr decl))))))
    `(;; == Basics ==
      (default :foreground ,fg1 :background ,bg1)
      (region  :background ,hl1 :foreground ,fg1)
      (fringe  :background ,bg1)
      ;; link, highlight

      ;; == Mode line ==
      (mode-line           :foreground ,fg3 :background ,bg3)
      (mode-line-inactive  :foreground ,fg2 :background ,bg2)
      (mode-line-highlight :box nil :underline t)

      ;; == Org mode ==
      (org-meta-line        :foreground ,fl4)
      (org-block            :foreground ,fg1 :backround ,bg1)
      (org-block-begin-line :underline t :foreground ,fl3)
      (org-block-end-line   :overline  t :foreground ,fl3)
      ;; org-code, org-document-title

      (org-level-1 :foreground "magenta4" :bold t)
      (org-level-2 :foreground "orchid4" :bold t)
      (org-level-3 :foreground "dodgerblue3")
      (org-level-4 :foreground "orchid3")
      (org-level-5 :foreground "dodgerblue2" :italic t)
      (org-level-6 :foreground "orchid4" :italic t)
      (org-level-7 :foreground "dodgerblue1" :italic t)
      (org-level-8 :foreground "orchid3" :italic t)

      ;; == Widgets ==

      ;; == Syntax ==
      (font-lock-keyword-face :foreground ,fl1 :bold t)
      (font-lock-builtin-face :foreground ,fl3)
      (font-lock-comment-face :foreground ,fl5 :background ,hl3
                              :italic t)
      (font-lock-string-face  :foreground ,fl2 :background ,hl2)
      (font-lock-type-face    :bold t)
      (font-lock-doc-face     :background ,hl4 :foreground ,fl6 :italic t
                              :bold nil)
      ;; font-lock-function-name-face
      ;; font-lock-variable-name-face
      ;; font-lock-constant-face

      ;; sh-heredoc

      ;; == Rmail ==
      ;; rmail-highlight

      ;; == Flymake ==

      ;; == Diffs ==
      ;; diff-header
      ;; diff-file-header
      ;; diff-context
      ;; (diff-added :background ,hl4 :foreground nil)
      ;; diff-removed
      ;; diff-index
      ;; diff-indicator-added
      ;; diff-indicator-removed

      ;; == Magit ==
      (magit-diff-added-highlight   :background "azure2")
      (magit-diff-removed-highlight :background "lavender blush")
      (diff-refine-removed          :background "palevioletred1")
      (diff-refine-added            :background "medium spring green")

      ;; == Dired ==


      ;; == Whitespace ==
      ;; whitespace-tab
      ;; whitespace-line
      ;; whitespace-trailing
      ))

      ))

(provide 'ankara-theme)
;;; ankara-theme.el ends here
