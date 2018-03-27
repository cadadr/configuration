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

(let ((black "black")
      (white "white")
      (blue "#0087af")
      (grey "#dee")
      (green "#004953")                 ; midnight green
      (khaki "khaki")
      (beige "beige"))
  (custom-theme-set-faces
   'mono
   ;; === Frame ===
   `(default ((t (:foreground ,black :background ,white))))
   `(mode-line ((t (:foreground ,white :background ,black :box nil))))
   `(mode-line-inactive ((t (:background ,white :box t))))
   `(mode-line-highlight ((t (:foreground ,black :background "grey" :box nil))))
   `(region ((t (:background "hotpink"))))

   ;; === General ===
   `(link
     ((t (:foreground ,blue :background ,white :underline t))))
   `(highlight
     ((t (:background ,grey))))

   ;; === Syntax ===
   `(font-lock-builtin-face ((t (:foreground ,black))))
   `(font-lock-comment-face ((t (:foreground ,black :weight bold))))
   `(font-lock-string-face ((t (:foreground ,black))))
   `(font-lock-function-name-face ((t (:foreground ,black))))
   `(font-lock-variable-name-face ((t (:foreground ,black))))
   `(font-lock-keyword-face ((t (:foreground ,black :weight bold))))
   `(font-lock-type-face ((t (:foreground ,black))))
   `(font-lock-constant-face ((t (:foreground ,black :weight bold))))

   ;; === Dired ===
   `(dired-directory ((t (:weight bold))))

   ;; === Diff ===
   `(diff-header  ((t (:foreground ,black :background ,white))))
   `(diff-file-header
     ((t (:foreground ,black :background ,white :weight bold))))
   `(diff-context  ((t (:foreground ,black :background ,white))))
   `(diff-added  ((t (:foreground ,black :background ,white))))
   `(diff-removed  ((t (:foreground ,black :background ,white))))
   `(diff-index  ((t (:foreground ,black :background ,white :weight bold))))
   `(diff-indicator-added
     ((t (:foreground ,black :background ,white :weight bold))))
   `(diff-indicator-removed
     ((t (:foreground ,black :background ,white :weight bold))))
   ;; === Shell scripts ===
   `(sh-heredoc ((t (:foreground ,black))))

   ;; === Org ===
   ;; XXX Make some org stuff italic (footnotes, links, etc,
   ;; or monochrome in some other way.
   `(org-level-1 ((t (:foreground ,green))))
   `(org-level-2 ((t (:foreground ,green))))
   `(org-level-3 ((t (:foreground ,green))))
   `(org-level-4 ((t (:foreground ,green))))
   `(org-level-5 ((t (:foreground ,green))))
   `(org-level-6 ((t (:foreground ,green))))
   `(org-level-7 ((t (:foreground ,green))))
   `(org-level-8 ((t (:foreground ,green))))
   `(org-document-title ((t (:foreground ,black :weight bold))))
   `(org-code ((t (:foreground ,green))))
   `(org-block ((t (:foreground ,black))))
   `(org-block-begin-line ((t (:background ,khaki))))
   `(org-block ((t (:background ,beige))))

   ;; === RMAIL ===
   `(rmail-highlight
     ((t (:foreground ,black :background ,white :weight bold :underline t))))

   ;; === Whitespace-mode ===
   `(whitespace-tab ((t (:background ,khaki))))
   `(whitespace-line ((t (:background ,khaki))))
   `(whitespace-trailing ((t (:background ,khaki))))))

(provide-theme 'mono)
;;; mono-theme.el ends here
