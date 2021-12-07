;;; gk-eless.el --- elisp pager                      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Use an Emacs buffer as a pager. Adapted from
;; https://crowding.github.io/blog/2014/08/16/replace-less-with-emacs/

;;; Code:

(defun gk-less--proc-sentinel (proc string)
  (ignore proc string))

(defun gk-less--postprocess (proc)
  (goto-char (point-min))
  (cond
   ;; Man pages:
   ((save-excursion (search-forward "" nil t))
    (Man-fontify-manpage))
   ;; Diffs:
   ((save-excursion
      (and (looking-at "^diff")
           (re-search-forward "^---" nil t)
           (re-search-forward "^@@" nil t)))
    (diff-mode))
   (:else
    (special-mode))))

(defun gk-less--proc-filter (proc string)
  (let ((buf (process-buffer proc))
        (mark (process-mark proc)))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        ;; make sure point stays at top of window while process output
        ;; accumulates
        (save-excursion
          (goto-char mark)
          (insert string)
          (ansi-color-filter-region mark (point))
          (set-marker mark (point)))
        ;; Post-processing the buffer:
        (unless (process-live-p proc)
          (gk-less--postprocess proc))))))

(defun gk-less (fifo)
  "Companion function for ‘extras/eless.sh’."
  (let ((buf (generate-new-buffer "*pager*")))
    (make-process
     :name "gk-pager" :buffer buf :command `("cat" ,fifo)
     :sentinel #'gk-less--proc-sentinel
     :filter #'gk-less--proc-filter)
    (display-buffer buf)))

(setenv "PAGER" (locate-user-emacs-file "extras/eless.sh"))



(provide 'gk-eless)
;;; gk-eless.el ends here
