;;; gk-diff-regions.el --- diff two regions in a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Adapted from: https://gist.github.com/zdavkeos/1279865.

;; To compare two regions, select the first region and run
;; =gk-diff-region=.  The region is now copied to a seperate diff-ing
;; buffer.  Next, navigate to the next region in question (even in
;; another file).  Mark the region and run =gk-diff-region-now=, the diff
;; of the two regions will be displayed by ediff.

;; You can re-select the first region at any time by re-calling
;; =gk-diff-region=.

;;; Code:

(defun gk-diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p)  ; there is a region
    (let ((buf (get-buffer-create "*Diff-region A*")))
      (with-current-buffer buf
        (erase-buffer))
      (append-to-buffer buf (region-beginning) (region-end))))
  (message "Now select other region to compare and run `diff-region-now`"))

(defun gk-diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
    (let ((bufa (get-buffer-create "*Diff-region A*"))
          (bufb (get-buffer-create "*Diff-region B*")))
      (with-current-buffer bufb
        (erase-buffer))
      (append-to-buffer bufb (region-beginning) (region-end))
      (ediff-buffers bufa bufb))))



(provide 'gk-diff-regions)
;;; gk-diff-regions.el ends here
