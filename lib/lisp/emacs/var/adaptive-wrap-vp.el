;;; adaptive-wrap-vp.el --- Adaptive wrapping for variable-pitch buffers

;; Copyright (C) 2015       Brenton Kenkel
;; Copyright (C) 2011-2013  Free Software Foundation, Inc.

;; Author: Brenton Kenkel <brenton.kenkel@gmail.com>
;;         Stephen Berman <stephen.berman@gmx.net>
;;         Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.1

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

;; This package provides the `adaptive-wrap-prefix-vp-mode' minor mode, a
;; modified version of the adaptive-wrap-prefix-mode minor mode tuned for
;; variable-pitch buffers.  Single-long-line paragraphs get word-wrapped in a
;; way similar to what M-q would produce using adaptive-fill-mode, but without
;; actually changing the buffer's text, and in a way that is visually
;; appealing with variable-pitch fonts.

;;; Code:

(require 'easymenu)

(defun vp-fill-context-prefix (beg en)
  (let* ((fcp (fill-context-prefix beg en))
         (fcp-len (string-width fcp))
         (lbp (line-beginning-position 0)))
    (buffer-substring lbp (+ lbp fcp-len))))

(defun adaptive-wrap-prefix-vp-function (beg end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while (< (point) end)
    (let ((lbp (line-beginning-position)))
      (put-text-property (point)
                         (progn (search-forward "\n" end 'move) (point))
                         'wrap-prefix
                         (propertize
                          (vp-fill-context-prefix lbp (point))
                          'face `(:foreground ,(face-attribute 'default :background)))))))

;;;###autoload
(define-minor-mode adaptive-wrap-prefix-vp-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  :group 'visual-line
  (if adaptive-wrap-prefix-vp-mode
      (progn
        ;; HACK ATTACK!  We need to run after font-lock, but jit-lock-register
        ;; doesn't accept an `append' argument, so we add ourselves beforehand,
        ;; to make sure we're at the end of the hook (bug#15155).
        (add-hook 'jit-lock-functions
                  #'adaptive-wrap-prefix-vp-function 'append t)
        (jit-lock-register #'adaptive-wrap-prefix-vp-function))
    (jit-lock-unregister #'adaptive-wrap-prefix-vp-function)
    (with-silent-modifications
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

(define-key-after (lookup-key menu-bar-options-menu [line-wrapping])
  [adaptive-wrap]
  '(menu-item "Adaptive Wrap (variable pitch)" adaptive-wrap-prefix-vp-mode
	      :visible (menu-bar-menu-frame-live-and-visible-p)
	      :help "Show wrapped long lines with an adjustable prefix"
	      :button (:toggle . (bound-and-true-p adaptive-wrap-prefix-vp-mode)))
  word-wrap)

(provide 'adaptive-wrap-vp)
;;; adaptive-wrap-vp.el ends here
