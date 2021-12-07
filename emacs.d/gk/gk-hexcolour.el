;;; gk-hexcolour.el --- illustrate hexadecimal colour codes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Defines a face that can render hexadecimal colour codes with the
;; colour they denote as their background; and a function to set it up
;; meant for major mode hooks.

;; Adapted from http://www.emacswiki.org/emacs/HexColour.


;;; Code:


(defvar gk-hexcolour-keywords
  '(("#[abcdefABCDEF[:digit:]]\\{3,6\\}"
     (0 (let ((colour (match-string-no-properties 0)))
          (if (or (= (length colour) 4)
                  (= (length colour) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)
                           :foreground
                           (if (>= (apply
                                    '+ (x-color-values
                                        (match-string-no-properties 0)))
                                   (* (apply '+ (x-color-values "white")) .6))
                               "black" ; light bg, dark text
                             "white"   ; dark bg, light text
                             )))))
        append))))

(defun gk-hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil gk-hexcolour-keywords t))



(provide 'gk-hexcolour)
;;; gk-hexcolour.el ends here
