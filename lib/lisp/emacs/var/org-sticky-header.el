;;; org-sticky-header.el --- Show off-screen Org heading at top of window -*- lexical-binding: t -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-sticky-header
;; Version: 0.1.0-pre
;; Package-Requires: ((emacs "24.4"))
;; Keywords: hypermedia, outlines, Org

;;; Commentary:

;; This package displays in the header-line the Org heading for the
;; node that's at the top of the window.  This way, if the heading for
;; the text at the top of the window is beyond the top of the window,
;; you don't forget which heading the text belongs to.

;; The code is very simple and is based on `semantic-stickyfunc-mode'.

;;; Installation:

;; Install from MELPA and run `org-sticky-header-mode'.

;; To install manually, put this file in your `load-path', require
;; `org-sticky-header' in your init file, and run the same command.

;; You probably want to add `org-sticky-func-mode' to your `org-mode-hook'.

;;; License:

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

;;; Code:

(defvar org-sticky-header-old-hlf nil
  "Value of the header line when entering org-sticky-header mode.")

(defconst org-sticky-header-header-line-format
  '(:eval (list
           (propertize " " 'display '((space :align-to 0)))
           (org-sticky-header--fetch-stickyline)))
  "The header line format used by stickyfunc mode.")

(defcustom org-sticky-header-full-path nil
  "Show the full outline path."
  :type 'boolean
  :group 'org)

(defun org-sticky-header--fetch-stickyline ()
  "Make the heading at the top of the current window sticky.
Capture its heading line, and place it in the header line.
If there is no heading, disable the header line."
  (save-excursion
    (goto-char (window-start))
    (unless (org-at-heading-p)
      (org-back-to-heading)
      ;; TODO: 3 spaces seems to be almost right, but it's still not
      ;; perfect, and it's probably not universally right.  Something
      ;; related to org-indent might be good.
      (if org-sticky-header-full-path
          (org-format-outline-path (org-get-outline-path t) nil "   ")
        (concat "   " (buffer-substring (line-beginning-position) (line-end-position)))))))

;;;###autoload
(define-minor-mode org-sticky-header-mode
  "Minor mode to show the current Org heading in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Return non-nil if the minor mode is enabled."
  :group 'org
  (if org-sticky-header-mode
      (progn
        (when (and (local-variable-p 'header-line-format (current-buffer))
                   (not (eq header-line-format org-sticky-header-header-line-format)))
          ;; Save previous buffer local value of header line format.
          (set (make-local-variable 'org-sticky-header-old-hlf)
               header-line-format))
        ;; Enable the mode
        (setq header-line-format org-sticky-header-header-line-format))
    ;; Disable mode
    (when (eq header-line-format org-sticky-header-header-line-format)
      ;; Restore previous buffer local value of header line format if
      ;; the current one is the sticky func one.
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'org-sticky-header-old-hlf (current-buffer))
        (setq header-line-format org-sticky-header-old-hlf)
        (kill-local-variable 'org-sticky-header-old-hlf)))))

(provide 'org-sticky-header)

;;; org-sticky-header.el ends here
