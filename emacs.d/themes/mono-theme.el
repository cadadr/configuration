;;; mono-theme.el --- A monochromatic Emacs colour theme. -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017, 2018 Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp
;; Keywords: theme scratch
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; URL: http://gkayaalp.com/emacs.html#scratch

;; Permission  is  hereby  granted,  free of  charge,  to  any  person
;; obtaining  a copy  of  this software  and associated  documentation
;; files   (the  "Software"),   to  deal   in  the   Software  without
;; restriction, including without limitation  the rights to use, copy,
;; modify, merge, publish, distribute,  sublicense, and/or sell copies
;; of the  Software, and  to permit  persons to  whom the  Software is
;; furnished to do so, subject to the following conditions:

;; The  above copyright  notice and  this permission  notice shall  be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE  IS PROVIDED  "AS IS", WITHOUT  WARRANTY OF  ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY,   FITNESS    FOR   A   PARTICULAR    PURPOSE   AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING FROM, OUT  OF OR IN
;; CONNECTION WITH  THE SOFTWARE OR THE  USE OR OTHER DEALINGS  IN THE
;; SOFTWARE.

;;; Commentary:
;;; Code:
(deftheme mono
  "A monochrome Emacs colour theme.")

(let ((a "black")
      (b "white")
      (c "mediumblue")
      (d "#dee")
      (e "violetred4")
      (f "khaki")
      (g "beige")
      (h "hotpink"))
  (custom-theme-set-faces
   'mono
   ;; === Frame ===
   `(default ((t (:foreground ,a :background ,b))))
   `(mode-line ((t (:foreground ,b :background ,a :box nil))))
   `(mode-line-inactive ((t (:background ,b :box t))))
   `(mode-line-highlight ((t (:foreground ,a
                                          :background ,h
                                          :box nil))))
   `(region ((t (:background ,h))))

   ;; === General ===
   `(link
     ((t (:foreground ,c :background ,b :underline t))))
   `(highlight
     ((t (:background ,d))))

   ;; === Syntax ===
   `(font-lock-builtin-face ((t (:foreground ,a))))
   `(font-lock-comment-face ((t (:foreground ,a :weight bold))))
   `(font-lock-string-face ((t (:foreground ,a))))
   `(font-lock-function-name-face ((t (:foreground ,a))))
   `(font-lock-variable-name-face ((t (:foreground ,a))))
   `(font-lock-keyword-face ((t (:foreground ,a :weight bold))))
   `(font-lock-type-face ((t (:foreground ,a))))
   `(font-lock-constant-face ((t (:foreground ,a :weight bold))))

   ;; === Dired ===
   `(dired-directory ((t (:weight bold))))

   ;; === Diff ===
   `(diff-header  ((t (:foreground ,a :background ,b))))
   `(diff-file-header
     ((t (:foreground ,a :background ,b :weight bold))))
   `(diff-context  ((t (:foreground ,a :background ,b))))
   `(diff-added  ((t (:foreground ,a :background ,b))))
   `(diff-removed  ((t (:foreground ,a :background ,b))))
   `(diff-index  ((t (:foreground ,a :background ,b :weight bold))))
   `(diff-indicator-added
     ((t (:foreground ,a :background ,b :weight bold))))
   `(diff-indicator-removed
     ((t (:foreground ,a :background ,b :weight bold))))
   ;; === Shell scripts ===
   `(sh-heredoc ((t (:foreground ,a))))

   ;; === Org ===
   ;; XXX Make some org stuff italic (footnotes, links, etc,
   ;; or monochrome in some other way.
   `(org-level-1 ((t (:foreground ,e))))
   `(org-level-2 ((t (:foreground ,e))))
   `(org-level-3 ((t (:foreground ,e))))
   `(org-level-4 ((t (:foreground ,e))))
   `(org-level-5 ((t (:foreground ,e))))
   `(org-level-6 ((t (:foreground ,e))))
   `(org-level-7 ((t (:foreground ,e))))
   `(org-level-8 ((t (:foreground ,e))))
   `(org-block ((t (:foreground ,a :background ,g))))
   `(org-block-begin-line ((t (:background ,f))))
   `(org-code ((t (:foreground ,a :background ,f :box t))))
   `(org-document-title ((t (:foreground ,a :weight bold))))

   ;; === RMAIL ===
   `(rmail-highlight
     ((t (:foreground ,a :background ,b :weight bold :underline t))))

   ;; === Whitespace-mode ===
   `(whitespace-tab ((t (:background ,f))))
   `(whitespace-line ((t (:background ,f))))
   `(whitespace-trailing ((t (:background ,f))))))

(provide-theme 'mono)
;;; mono-theme.el ends here
