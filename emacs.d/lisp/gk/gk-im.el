;;; gk-im.el --- input methods configs, quail or otherwise  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Configurations for input methods and some minor/WIP Quail input
;; method definitions.

;;; Code:

;;;; Quail customisations:

(add-to-list
 'quail-keyboard-layout-alist
 `("dvorak"
   .
   ,(concat
     "                              "
     "  1!2@3#4$5%6^7&8*9(0)[{]}`~  "
     "  '\",<.>pPyYfFgGcCrRlL/?=+    "
     "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
     "  ;:qQjJkKxXbBmMwWvVzZ        "
     "                              ")))

(add-to-list
 'quail-keyboard-layout-alist
 `("brit-q"
   .
   ,(concat
     "                              "
     " 1!2\"3£4$5%6^7&8*9(0)[{]}    "
     "  ’@,<.>pPyYfFgGcCrRlL/?=+   "
     "   aAoOeEuUiIdDhHtTnNsS-_#~   "
     "  \\|;:qQjJkKxXbBmMwWvVzZ      "
     "                                ")))

(quail-set-keyboard-layout "brit-q")

;; TAB won’t show completion, C-i will.
(define-key quail-translation-keymap [tab] nil)

(defvar gk-input-methods '("unilat-gk" "ipa-x-sampa" "japanese"))

(defun gk-cycle-input-methods (arg)
  "Activate the next input method from `gk-input-methods'.

If ARG is non-nil or called with a universal argument, cycle
backwards."
  (interactive "^p")
  (cond ((and (eq arg 1) (not current-input-method))
         (set-input-method (car gk-input-methods)))
        ((and (eq arg 1) current-input-method)
         (set-input-method
          (or (cadr (member current-input-method gk-input-methods))
              (car gk-input-methods))))
        ((and (eq arg 4) (not current-input-method))
         (set-input-method (car (last gk-input-methods))))
        ((and (eq arg 4) (equal current-input-method (car gk-input-methods)))
         (set-input-method (car (last gk-input-methods))))
        ((and (eq arg 4) current-input-method)
         (set-input-method
          (nth (or (1- (position current-input-method
                                    gk-input-methods
                                    :test #'equal))
                   (1- (length gk-input-methods)))
               gk-input-methods)))))

;; Use `unilat-gk' whenever possible.
(dolist (lang gk-unilat-languages)
  (let* ((env (assoc lang language-info-alist))
         (im (assoc 'input-method env)))
    ;; Some language environments may  not have an input-method field,
    ;; namely English.
    (when im
      (setcdr im "unilat-gk"))))



;;;; Input method definitions:



;;;;; Armenian input method:

;; XXX(2018-05-25): This was taken from somewhere, find the attribution.

(quail-define-package
 "armenian-translit" "Armenian" "Հ" nil
 "A transliteration scheme for Armenian characters.")

(quail-define-rules
 ("a" ?ա) ("b" ?բ) ("g" ?գ) ("d" ?դ) ("e" ?ե) ("ye" ?ե) ("z" ?զ)
 ("ee" ?է) ("e'" ?ը) ("t'" ?թ) ("zh" ?ժ) ("i" ?ի) ("l" ?լ) ("x" ?խ)
 ("c" ?ծ) ("k" ?կ) ("h" ?հ) ("j" ?ձ) ("g." ?ղ) ("ch." ?ճ) ("m" ?մ)
 ("y" ?յ) ("n" ?ն) ("sh" ?շ) ("o" ?ո) ("vo" ?ո) ("ch" ?չ) ("p" ?պ)
 ("jh" ?ջ) ("r." ?ռ) ("s" ?ս) ("v" ?վ) ("t" ?տ) ("r" ?ր) ("c'" ?ց)
 ("w" ?ւ) ("p'" ?փ) ("k'" ?ք) ("o'" ?օ) ("f" ?ֆ) ("u" ["ու"]) ("ev" ?և)
 ("?" ?՞) ("." ?։) (".'" ?՝) (";" ?՟) (";'" ?՛) ("!" ?՜)
 ("A" ?Ա) ("B" ?Բ) ("G" ?Գ) ("D" ?Դ) ("E" ?Ե) ("YE" ?Ե)
 ("Ye" ?Ե) ("Z" ?Զ) ("EE" ?Է) ("E'" ?Ը) ("T'" ?Թ) ("ZH" ?Ժ) ("I" ?Ի)
 ("L" ?Լ) ("X" ?Խ) ("C" ?Ծ) ("K" ?Կ) ("H" ?Հ) ("J" ?Ձ) ("G." ?Ղ)
 ("CH." ?Ճ) ("M" ?Մ) ("Y" ?Յ) ("N" ?Ն) ("SH" ?Շ) ("O" ?Ո) ("VO" ?Ո)
 ("Vo" ?Ո) ("CH" ?Չ) ("P" ?Պ) ("JH" ?Ջ) ("R." ?Ռ) ("S" ?Ս) ("V." ?Վ)
 ("T" ?Տ) ("R" ?Ր) ("C'" ?Ց) ("W" ?Ւ) ("P'" ?Փ) ("K'" ?Ք) ("O" ?Օ)
 ("F" ?Ֆ) ("U" ["Սւ"]))



;;;;; Syriac:

;; XXX(2018-05-25): This was taken from somewhere, find the attribution.

(quail-define-package
 "syriac-translit" "Syriac transliteration" "ܣܪ" nil
 "A transliteration scheme for Syriac characters.")
(quail-define-rules
 ;; Letters.
 ("a"	?ܐ)	("b"	?ܒ)	("g"	?ܓ)	("d"	?ܕ)
 ("h"	?ܗ)	("w"	?ܘ)	("z"	?ܙ)	("k"	?ܚ)
 ("t"	?ܛ)	("i"	?ܝ)	("c"	?ܟ)	("l"	?ܠ)
 ("m"	?ܡ)	("n"	?ܢ)	("s"	?ܣ)	("'"	?ܥ)
 ("p"	?ܦ)	("S"	?ܨ)	("q"	?ܩ)	("r"	?ܪ)
 ("sh"	?ܫ)	("T"	?ܬ)
 ;; Punctuation.
 ("."	?܁)	(":"	?܃))


(provide 'gk-im)
;;; gk-im.el ends here
