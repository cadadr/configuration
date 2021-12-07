;;; gk-ibuffer.el --- obsolete ibuffer config        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: 

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

;; Somewhat heavy configuration for ibuffer, obsoleted for being
;; somewhat heavy...

;;; Code:

(require 'ibuffer)
(require 'ibuffer-vc)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (let ((bs (buffer-size)))
    (cond ((> bs 1e6) (format "%7.1fmB" (/ bs 1e6)))
          ((> bs 1e3) (format "%7.1fkB" (/ bs 1e3)))
          (t          (format "%7d  " bs)))))

(setf ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process))
      ibuffer-show-empty-filter-groups nil)

(define-ibuffer-filter name-not
    "Negated buffer name match."
  (:description "buffer name not"
   :reader (read-from-minibuffer "Exclude buffers (regexp): "))
  (not (string-match qualifier (buffer-name buf))))

(defvar gk-ibuffer-filters
  '(("Emacs"
     (name . "^\\*\\(scratch\\|Messages\\)\\*$"))
    ("VC"
     (name . "^\\*\\(vc\\|log\\)-?"))
    ("Documentation"
     (name . "^\\*\\(Help\\|info\\|Man [0-9]?\\)"))
    ("Special buffers"
     (name-not . "compilation")
     (name . "^\\*.*\\*$"))
    ("EWW Reading"
     (mode . eww-mode)))
  "Fixed filter definitions for ibuffer.")

(defun gk-ibuffer-generate-filter-groups-by-dir ()
  "Create a set of ibuffer filter groups based on the dirs of buffers."
  (let* ((func (lambda (buf)
                 (when-let* ((bufnam (buffer-file-name buf)))
                   (if-let* ((linknam (file-symlink-p bufnam)))
                       (file-name-directory (expand-file-name linknam))
                     (file-name-directory (expand-file-name bufnam))))))
         (dirs (ibuffer-remove-duplicates (delq nil (mapcar func (buffer-list))))))
    (mapcar (lambda (dir) (cons (concat "Directory: " dir) `((dir . ,dir)))) dirs)))

(define-ibuffer-filter dir
    "Toggle current view to buffers with dir QUALIFIER."
  (:description "directory" :reader (read-from-minibuffer "Filter by dir (regexp): "))
  (ibuffer-awhen (buffer-file-name buf)
    (string= qualifier (file-name-directory it))))

(define-advice ibuffer-update (:before (&rest args) autogroups)
  "Group related buffers together using ‘ibuffer-vc’ and ‘dir’,

and special ones sepatarely."
  (ignore args)
  (setf ibuffer-filter-groups
        (append
         gk-ibuffer-filters
         (ibuffer-vc-generate-filter-groups-by-vc-root)
         (gk-ibuffer-generate-filter-groups-by-dir))))

;; Hide these buffers by default.
(defvar gk-ibuffer-collapsed-groups (list "Special buffers" "Emacs" "Documentation"
                                          "VC"))

(define-advice ibuffer (:after (&rest args) gk-hidden-groups)
  "Hide groups in ‘gk-ibuffer-collapsed-groups’."
  (ignore args)
  (save-excursion
    (dolist (group gk-ibuffer-collapsed-groups)
      (ignore-errors
        (ibuffer-jump-to-filter-group group)
        (ibuffer-toggle-filter-group)))))

(defun gk-ibuffer-hook ()
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-filename/process))
  (ibuffer-update nil t))

(make-obsolete 'gk-ibuffer-hook nil "2021-12-07")
(make-obsolete 'gk-ibuffer-collapsed-groups nil "2021-12-07")
(make-obsolete 'gk-ibuffer-generate-filter-groups-by-dir nil "2021-12-07")
(make-obsolete 'gk-ibuffer-filters nil "2021-12-07")

(add-hook 'ibuffer-hook 'gk-ibuffer-hook)

(define-key ibuffer-mode-map [?q] 'kill-this-buffer)



(provide 'gk-ibuffer)
;;; gk-ibuffer.el ends here
