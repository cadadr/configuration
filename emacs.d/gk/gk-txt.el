;;; gk-txt.el --- texte-editing utilities and customisations  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;;; Commentary:

;; Customisations and utilities for text editing modes, minus
;; ‘org-mode’ related customisations (which are in ‘gk-org’).

;;; Code:


(require 'electric)
(require 'gemini-mode)
(require 'ispell)
(require 'markdown-mode)
(require 'multiple-cursors-core)
(require 'outline)
(require 'zencoding-mode)

(require 'gk-mac)
(require 'gk-minor-mode)


(setf
 ;; Sentence.  Other sentence.
 sentence-end-double-space t
 ;; Words:  other words.
 colon-double-space t
 ;; Guillemets
 electric-quote-chars '(?‘ ?’ ?« ?»))

;; Justify.
(setq-default default-justification 'left)



;;;; Outline:

;; Utility function for setting up outline minor mode.

(defun gk-turn-on-outline-minor-mode (headline-begin headline-end prefix)
  "Turn on the `outline-minor-mode'.

Set locally the variable `outline-regexp' to HEADLINE-BEGIN.
Set locally the variable `outline-heading-end-regexp' to HEADLINE-END.
Set locally the variable `outline-minor-mode-prefix' to PREFIX."
  (setq-local outline-regexp headline-begin)
  (setq-local outline-heading-end-regexp headline-end)
  (setq-local outline-minor-mode-prefix (kbd prefix))
  (outline-minor-mode +1)
  (local-set-key outline-minor-mode-prefix outline-mode-prefix-map))

;; Mainly for ‘C-c C-u’ in Org mode.
(define-advice outline-up-heading
    (:around (fn &rest args) previous-heading-on-toplevel)
  "Move to previous heading if at toplevel."
  (condition-case e
      (call-interactively fn)
    ('error (org-previous-visible-heading (car args)))))

(setf
 ;; Start outline-enabled buffers in overview mode.
 outline-default-state 'outline-show-only-headings)



;;;; Utilites:

(defun gk-join-nl ()
  "Join the line under point with the next line."
  (interactive)
  (forward-line 1)
  (join-line))

(defun gk-eat-spaces-to-the-right ()
  (interactive)
  (save-excursion
    (when (re-search-forward "[ \t]+" nil t)
      (replace-match "" nil nil))))

(defun gk-reverse-rectangle (&optional start end)
  "Reverse each line of a rectangle.

START and END are corners of the rectangle, ignored if the region
is active."
  (interactive)
  (if (region-active-p)
      (setq start (region-beginning)
            end   (region-end))
    (error "Region not active and no corners specified."))
  (let ((rect (delete-extract-rectangle start end)))
    (save-excursion
      (goto-char start)
      (insert-rectangle (mapcar #'reverse rect)))))

(defun gk-lowercase-this-or-previous-word (arg)
  (interactive "P")
  (save-excursion
    (cond (arg
           (progn (backward-sexp)
                  (mark-sexp)
                  (downcase-region (region-beginning) (region-end))))
          (:else
           (progn
             (backward-word)
             (downcase-word 1))))))

(defun gk-upcase-this-or-previous-word (arg)
  (interactive "P")
  (save-excursion
    (cond (arg
           (progn (backward-sexp)
                  (mark-sexp)
                  (upcase-region (region-beginning) (region-end))))
          (:else
           (progn
             (unless (looking-at "\\<")
              (backward-word))
             (upcase-word 1))))))

(defun gk-end-sentence (punctuation)
  "Finish a sentence with a punctuation and two spaces.

PUNCTUATION is the punctuation to use"
  (interactive (list (read-char "Punctuation: ")))
  (insert (format "%c  " punctuation)))


(defun asterism ()
  "Insert an asterism, centered in the line."
  (interactive)
  (insert "⁂")
  (center-line))

(defun gk-fill-or-join-paragraph (&optional arg)
  "DWIM fill or join a paragraph.

With no prefix args, toggle the physical wrapping of the
paragraph's lines, i.e. if it seems to be filled, join all of
it's lines, else, fill the paragraph.  If any of the lines in a
paragraph is longer than ‘fill-column’, it's filled.

With one prefix arg, behave just as ‘fill-paragraph’, i.e. just
fill, do not toggle.  With two prefix arguments, justify the
paragraph if filling (and not if joining).  With three prefix
arguments, remove justification and fill.

If ‘sentence-end-double-space’ is non-nil, and if a line ends
with a period, insert two spaces afterwards instead of one."
  (interactive "p")
  (save-mark-and-excursion
    (let ((fill-paragraph-function
           (cl-case major-mode
             ('org-mode #'org-fill-paragraph)
             (otherwise #'fill-paragraph)))
          (para-beg
           (save-excursion
             (save-match-data
               (if (looking-at "^$") (point)
                 (progn (backward-paragraph) (1+ (point)))))))
          (para-end
           (save-excursion
             (progn (forward-paragraph) (1- (point)))))
          justify)
      (case arg
        (4
         (funcall fill-paragraph-function))
        (64
         (setq justify nil)
         (put-text-property para-beg para-end 'justification justify)
         (funcall fill-paragraph-function))
        (16 (funcall fill-paragraph-function 'both))
        (1
         (let* ((subs (buffer-substring para-beg para-end))
                (lines (split-string subs "\n" t "\\s+"))
                ;; We should wrap if any of the lines are longer than
                ;; ‘fill-column’.
                (should-wrap
                 (cl-reduce (lambda (x y) (or x y))
                            (mapcar (lambda (l)
                                      (> (length l) fill-column))
                                    lines))))
           (goto-char para-beg)
           (setq justify
                 ;; If we should wrap, learn justification from text properties.
                 (if should-wrap
                     (get-text-property (point) 'justification)
                   ;; Else, see if the right edge is justified.
                   (let ((right-edge-justified
                          (cl-reduce (lambda (&optional x y) (and x y (= x y fill-column)))
                                     (mapcar #'length (butlast lines)))))
                     (cond
                      ;; If the left edge is not justified, then the text
                      ;; is right aligned.
                      ((and right-edge-justified
                            (save-excursion
                              (goto-char para-beg)
                              (re-search-forward "^[ \\t]" para-end t)))
                       'right)
                      ;; Otherwise, it is justified both edges.
                      (right-edge-justified 'both)
                      ;; If neither, then the paragraph is not
                      ;; justified.  We can just allow ‘cond’ to
                      ;; return nil here.
                      ))))
           (if should-wrap
               (progn
                 (put-text-property para-beg para-end 'justification justify)
                 (fill-region para-beg para-end justify))
             (progn
               (goto-char para-beg)
               ;; Remove possible justification artifacts.
               (fill-region para-beg para-end nil)
               ;; 1- because we don't want to include the final \n of
               ;; the paragraph.  Will include the next paragraph too
               ;; then.
               (while (re-search-forward "\n" (1- para-end) t)
                 (goto-char (match-beginning 0))
                 (when (looking-at "\n\\([ \t]+\\)?"))
                 (replace-match
                  (if (and sentence-end-double-space
                           (looking-at "\\.")) "  "
                    " ")))
               (put-text-property
                para-beg (line-end-position) 'justification justify)))))))))

(defun gk-count-words (&rest args)
  "Call the correct word count function for context.

Pass ARGS to it, the first two will be set so that the function
will receive the region if active, or the entire buffer."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (eval
   `(funcall-interactively
     (if (eq major-mode 'org-mode)
         #'gk-org-word-count
       #'count-words-region)
     ,@args)))

(defalias 'wc #'gk-count-words)
(defalias 'cw #'gk-count-words)



;;;; Common:

;; Settings common to all major/minor modes that edit text.

(diminish 'visual-line-mode "¬")
;; i.e. ‘auto-fill-mode’, but diminish does not like that.
(diminish 'auto-fill-function "=")

(defun gk-text-editing-modes-hook ()
  "Hook for `text-mode'."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (set-input-method default-input-method)
  (setq indent-tabs-mode nil)
  ;; Started causing slowdowns with large org mode files recently.
  ;; (highlight-indent-guides-mode)
  (git-gutter-mode +1))

(add-hook 'text-mode-hook 'gk-text-editing-modes-hook)

(add-to-list 'gk-global-modes 'electric-quote-mode)



;;;; Automated editing:

;; This section contains various hooks that automate some editing
;; tasks.

;; XXX(2018-05-25): Maybe disable copyright-update on some paths, or
;; only enable selectively?  It can be annoying sometimes, especially
;; when working on/debugging generated files or files from external
;; projects.

(add-hook 'before-save-hook #'copyright-update)



;;;; Dictionary and spell checking:

;; Partially adapted from:
;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html

(defvar gk-expected-dictionaries (list "en_GB" "it_IT" "tr_TR")
  "Dictionaries to be attempted to load.")

(setf ispell-program-name "hunspell"
      ;; Only include dictionary names if it’s found on the system
      ispell-dictionary
      (string-join
       (cl-remove-if-not
        ($ [dict]
           (let ((dic-name (concat dict ".dic")))
             ;; XXX(2022-01-11): turn this into an or-statement if need be.
             (file-exists-p (expand-file-name dic-name "/usr/share/hunspell"))))
        gk-expected-dictionaries)
       ",")
      ispell-personal-dictionary
      (expand-file-name "~/Documents/hunspell-personal-dictionary"))

(defun gk-spellcheck-hook ()
  "Hook to start spell-check in buffers."
  (ispell-set-spellchecker-params)
  ;; This uses ‘cl-pushnew’ so it should be okay to call this multiple
  ;; times.
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (f-touch ispell-personal-dictionary))
  (flyspell-mode +1))

(add-hook 'text-mode-hook 'gk-spellcheck-hook)



;;;; Multiple cursors:

;; Just fucking run the commands.
(setf mc/always-run-for-all t)

(defvar gk-multiple-cursors-bindings
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'mc/edit-lines)
    (define-key map "n" 'mc/mark-next-like-this-symbol)
    (define-key map "p" 'mc/mark-previous-like-this-symbol)
    (define-key map "*" 'mc/mark-all-dwim)
    (define-key map "a" 'mc/edit-beginnings-of-lines)
    (define-key map "e" 'mc/edit-ends-of-lines)
    map)
  "Keymap for multiple cursors.")



;;;; Keybindings:

;; These are keybindings specific to =text-mode= and descendants.

(define-key text-mode-map (kbd "C-M-a") #'backward-paragraph)
(define-key text-mode-map (kbd "C-M-e") #'forward-paragraph)

(gk-prefix-binding "\C-\ " 'gk-eat-spaces-to-the-right)
(gk-prefix-binding "i" 'gk-cycle-input-methods)
(gk-prefix-binding "u" 'gk-upcase-this-or-previous-word)
(gk-prefix-binding "l" 'gk-lowercase-this-or-previous-word)
(gk-global-binding "\C-z" 'gk-cycle-input-methods)

(gk-prefix-binding "m" gk-multiple-cursors-bindings)

;; Transpose
(gk-prefix-binding "\M-p" 'transpose-paragraphs)
(gk-prefix-binding "\M-l" 'transpose-lines)
(gk-prefix-binding "\M-s" 'transpose-sentences)

;; Comparison
(gk-prefix-binding (kbd "C-=") 'diff-buffer-with-file)

;;;; Configurations for text-editing related modes:

;;;;; HTML:

(defun gk-html-mode-hook ()
  "Hook for `html-mode'."
  (setf indent-tabs-mode nil))

(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'gk-html-mode-hook)
(setq zencoding-preview-default nil)
(define-key zencoding-mode-keymap "\C-j" nil)



;;;;; Textile:

(add-to-list 'auto-mode-alist '("\\.textile" . textile-mode))

;; Fix the terrible faces of Textile using Emacs’ and Org mode’s
;; faces.
(set-face-attribute 'textile-link-face nil :foreground nil :inherit 'link)
(set-face-attribute 'textile-class-face nil :foreground nil :inherit 'font-lock-builtin-face)
(set-face-attribute 'textile-acronym-face nil :foreground nil :inherit 'font-lock-builtin-face)
(set-face-attribute 'textile-table-face nil :foreground nil :inherit 'font-lock-builtin-face)
(set-face-attribute 'textile-lang-face nil :foreground nil :inherit 'font-lock-keyword-face)
(set-face-attribute 'textile-ol-bullet-face nil :foreground nil :inherit 'org-level-1)
(set-face-attribute 'textile-ul-bullet-face nil :foreground nil :inherit 'org-level-1)
(dotimes (i 5)
  (set-face-attribute
   (intern (format "textile-h%d-face" (1+ i)))
   nil :foreground nil :inherit
   (intern (format "org-level-%d" (1+ i)))))



;;;;; Markdown:

;; Org-mode like movement.
(define-key markdown-mode-map (kbd "M-n")
  (gk-interactively (markdown-next-heading)))
(define-key markdown-mode-map (kbd "M-p")
  (gk-interactively (markdown-previous-heading)))



;;;;; Roff, Troff, Nroff:

(defvar gk-nroff-compile-command-template
  "< %s tbl | troff -Tps -me | dpost | ps2pdf - %s.pdf"
  "Template for use in producing the ‘compile-command’ for

‘nroff-mode’ buffers.  The first ‘%s’ will be replaced with the
name of the current buffer, the second with same but without the
file extension.")

(defun gk-nroff-mode-hook ()
  (let* ((bufnam (buffer-name))
         (bufnam2 (file-name-sans-extension bufnam)))
    (setq-local
     compile-command
     (format gk-nroff-compile-command-template bufnam bufnam2))))

(add-hook 'nroff-mode-hook 'gk-nroff-mode-hook)



;;;;; Gemini:

(define-key gemini-mode-map (kbd "C-c C-s")
  (lambda (arg)
    (interactive "P")
    (if arg (insert "[**]") (insert "[*]"))))
(define-key gemini-mode-map (kbd "C-c C-d")
  (lambda (arg)
    (interactive "P")
    (if arg (insert "[††]") (insert "[†]"))))
(define-key gemini-mode-map (kbd "C-c C-S-d")
  (lambda (arg)
    (interactive "P")
    (if arg (insert "[‡‡]") (insert "[‡]"))))
(define-key gemini-mode-map (kbd "C-c C-k")
  (gk-interactively (insert "``` \n```")
                    (forward-line -1)
                    (goto-char (line-end-position))))


(define-key gemini-mode-map (kbd "C-c C-l") (gk-interactively (insert "=> ")))




(provide 'gk-txt)
;;; gk-txt.el ends here
