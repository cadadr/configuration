;;; dart-mode.el --- Major mode for editing Dart files -*- lexical-binding: t; -*-

;; Author: Natalie Weizenbaum
;; URL: https://github.com/nex3/dart-mode
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.5") (cl-lib "0.5") (dash "2.10.0") (flycheck "0.23") (s "1.10"))
;; Keywords: language

;; Copyright (C) 2011 Google Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file contains several functions and variables adapted from the
;; code in https://github.com/dominikh/go-mode.el
;;
;; go-mode.el uses this license:
;;
;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;    * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; To install, see https://github.com/nex3/dart-mode/blob/master/README.md
;;
;; Known bugs:
;;
;; * Multiline strings using """ and ''' are not recognized. They fontify
;;   correctly, but only because they look like three strings in a row.
;; * In a map with identifier keys, the first key is fontified like a label.
;; * Return values for operator methods aren't fontified correctly.
;; * Untyped parameters aren't fontified correctly.
;; * Quotes immediately after string interpolation are marked as unclosed.
;; * Sexp movement doesn't properly ignore quotes in interpolation.
;; * Methods using "=>" can cause indentation woes.
;; * C and C++ modes seem to be hosed.

;; Definitions adapted from go-mode.el are
;;
;; gofmt-command gofmt-args gofmt-show-errors gofmt go--apply-rcs-patch
;; gofmt--kill-error-buffer gofmt--process-errors gofmt-before-save
;; go--goto-line go--delete-whole-line

;;; Code:

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile (c-add-language 'dart-mode 'java-mode))

(require 'cl-lib)
(require 'compile)
(require 'dash)
(ignore-errors
 (require 'flycheck))
(require 'help-mode)
(require 'json)
(require 's)


;;; Utility functions and macros

(defun dart-beginning-of-statement ()
  "Moves to the beginning of a Dart statement.

Unlike `c-beginning-of-statement', this handles maps correctly
and will move to the top level of a bracketed statement."
  (while
      (progn
        (back-to-indentation)
        (while (eq (char-after) ?})
          (forward-char)
          (forward-sexp -1)
          (back-to-indentation))
        (when (not (dart--beginning-of-statement-p)) (forward-line -1)))))

(defun dart--beginning-of-statement-p ()
  "Returns whether the point is at the beginning of a statement.

Statements are assumed to begin on their own lines. This returns
true for positions before the start of the statement, but on its line."
  (and
   (save-excursion
     (skip-syntax-forward " ")
     (not (or (bolp) (eq (char-after) ?}))))
   (save-excursion
     (skip-syntax-backward " ")
     (when (bolp)
       (cl-loop do (forward-char -1)
             while (looking-at "^ *$"))
       (skip-syntax-backward " ")
       (cl-case (char-before)
         ((?} ?\;) t)
         ((?{) (dart-in-block-p (c-guess-basic-syntax))))))))

(defconst dart--identifier-re
  "[a-zA-Z_$][a-zA-Z0-9_$]*"
  "A regular expression that matches keywords.")

(defun dart--forward-identifier ()
  "Moves the point forward through a Dart identifier."
  (when (looking-at dart--identifier-re)
    (goto-char (match-end 0))))

(defun dart--kill-buffer-and-window (buffer)
  "Kills BUFFER, and its window if it has one.

This is different than `kill-buffer' because, if the buffer has a
window, it respects the `quit-restore' window parameter. See
`quit-window' for details."
  (-if-let (window (get-buffer-window buffer))
      (quit-window t window)
    (kill-buffer buffer)))

(defun dart--get (alist &rest keys)
  "Recursively calls `cdr' and `assoc' on ALIST with KEYS.
Returns the value rather than the full alist cell."
  (--reduce-from (cdr (assoc it acc)) alist keys))

(defmacro dart--json-let (json fields &rest body)
  "Assigns variables named FIELDS to the corresponding fields in JSON.
FIELDS may be either identifiers or (ELISP-IDENTIFIER JSON-IDENTIFIER) pairs."
  (declare (indent 2))
  (let ((json-value (make-symbol "json")))
    `(let ((,json-value ,json))
       (let ,(--map (if (symbolp it)
                        `(,it (dart--get ,json-value ',it))
                      (-let [(variable key) it]
                        `(,variable (dart--get ,json-value ',key))))
                    fields)
         ,@body))))

(defun dart--property-string (text prop value)
  "Returns a copy of TEXT with PROP set to VALUE.

Converts TEXT to a string if it's not already."
  (let ((copy (substring (format "%s" text) 0)))
    (put-text-property 0 (length copy) prop value copy)
    copy))

(defun dart--face-string (text face)
  "Returns a copy of TEXT with its font face set to FACE.

Converts TEXT to a string if it's not already."
  (dart--property-string text 'face face))

(defmacro dart--fontify-excursion (face &rest body)
  "Applies FACE to the region moved over by BODY."
  (declare (indent 1))
  (-let [start (make-symbol "start")]
    `(-let [,start (point)]
       ,@body
       (put-text-property ,start (point) 'face ,face))))

(defun dart--flash-highlight (offset length)
  "Briefly highlights the text defined by OFFSET and LENGTH.
OFFSET and LENGTH are expected to come from the analysis server,
rather than Elisp."
  (-let [overlay (make-overlay (+ 1 offset) (+ 1 offset length))]
    (overlay-put overlay 'face 'highlight)
    (run-at-time "1 sec" nil (lambda () (delete-overlay overlay)))))

(defun dart--read-file (filename)
  "Returns the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defmacro dart--with-temp-file (name-variable &rest body)
  "Creates a temporary file for the duration of BODY.
Assigns the filename to NAME-VARIABLE. Doesn't change the current buffer.
Returns the value of the last form in BODY."
  (declare (indent 1))
  `(-let [,name-variable (make-temp-file "dart-mode.")]
     (unwind-protect
         (progn ,@body)
       (delete-file ,name-variable))))

(defun dart--run-process (executable &rest args)
  "Runs EXECUTABLE with ARGS synchronously.
Returns (STDOUT STDERR EXIT-CODE)."
  (dart--with-temp-file stderr-file
    (with-temp-buffer
      (-let [exit-code
             (apply #'call-process
                    executable nil (list t stderr-file) nil args)]
        (list
         (buffer-string)
         (dart--read-file stderr-file)
         exit-code)))))

(defun dart--try-process (executable &rest args)
  "Like `dart--run-process', but only returns stdout.
Any stderr is logged using dart-log. Returns nil if the exit code is non-0."
  (-let [result (apply #'dart--run-process executable args)]
    (unless (string-empty-p (nth 1 result))
      (dart-log (format "Error running %S:\n%s" (cons executable args) (nth 1 result))))
    (if (eq (nth 2 result) 0) (nth 0 result))))

(defvar dart--do-it-again-callback nil
  "A callback to call when `dart-do-it-again' is invoked.

Only set in `dart-popup-mode'.")
(make-variable-buffer-local 'dart--do-it-again-callback)


;;; General configuration

(defcustom dart-sdk-path
  ;; Use Platform.resolvedExecutable so that this logic works through symlinks
  ;; and wrapper scripts.
  (-when-let (dart (executable-find "dart"))
    (dart--with-temp-file input
      (with-temp-file input (insert "
        import 'dart:io';

        void main() {
          print(Platform.resolvedExecutable);
        }
        "))
      (-when-let (result (dart--try-process dart input))
        (file-name-directory
         (directory-file-name
          (file-name-directory (string-trim result)))))))
  "The absolute path to the root of the Dart SDK."
  :group 'dart-mode
  :type 'directory
  :package-version '(dart-mode . "1.0.0"))

(defun dart-executable-path ()
  "The absolute path to the 'dart' executable.

Returns nil if `dart-sdk-path' is nil."
  (when dart-sdk-path
    (concat dart-sdk-path
            (file-name-as-directory "bin")
            (if (memq system-type '(ms-dos windows-nt))
                "dart.exe"
              "dart"))))


;;; CC configuration

(c-lang-defconst c-symbol-start
  dart (concat "[" c-alpha "_]"))

(c-lang-defconst c-identifier-ops
  dart nil)

(c-lang-defconst c-after-id-concat-ops
  dart nil)

(c-lang-defconst c-multiline-string-start-char
  dart ?@)

(c-lang-defconst c-opt-cpp-prefix
  dart "\\s *#\\s *")

(c-lang-defconst c-cpp-message-directives
  dart nil)

(c-lang-defconst c-cpp-include-directives
  dart nil)

(c-lang-defconst c-opt-cpp-macro-define
  dart nil)

(c-lang-defconst c-cpp-expr-directives
  dart '("import" "source" "library" "resource"))

(c-lang-defconst c-cpp-expr-functions
  dart nil)

(c-lang-defconst c-operators
  dart `((prefix "#")
         (postfix-if-paren "<" ">")
         (prefix "super")
         (left-assoc ".")
         (postfix "++" "--" "[" "]" "(" ")")
         (unary "++" "--" "+" "-" "!" "~" "negate" "new" "const")
         (left-assoc "*" "/" "%")
         (left-assoc "+" "-")
         (left-assoc "<<" ">>" ">>>")
         (left-assoc "<" ">" "<=" ">=")
         (left-assoc "==" "!=" "===" "!==" "is" "is!")
         (left-assoc "&")
         (left-assoc "^")
         (left-assoc "|")
         (left-assoc "&&")
         (left-assoc "||")
         (right-assoc-sequence "?" ":")
         (left-assoc "=>")
         (right-assoc ,@(c-lang-const c-assignment-operators))
         (left-assoc ",")))

(c-lang-defconst c-overloadable-operators
  dart '("==" "<" ">" "<=" ">=" "-" "+" "*" "/" "%" "|" "^" "&"
         "<<" ">>" ">>>" "[]=" "[]" "~" "negate"))

(c-lang-defconst c-opt-op-identifier-prefix
  dart (c-make-keywords-re t '("operator")))

(c-lang-defconst c-doc-comment-start-regexp
  dart nil)

(c-lang-defconst c-paragraph-start
  dart "$")

(c-lang-defconst c-primitive-type-kwds
  dart '("Dynamic" "void" "num" "int" "double" "bool"))

(c-lang-defconst c-class-decl-kwds
  dart '("class" "interface"))

;; Don't put these in c-modifier-kwds because they can be used without a type
;; following them.
(c-lang-defconst c-typeless-decl-kwds
  dart '("abstract" "const" "factory" "final" "operator" "static" "typedef" "var"))

(c-lang-defconst c-modifier-kwds
  dart nil)

(c-lang-defconst c-other-decl-kwds
  dart nil)

(c-lang-defconst c-decl-hangon-kwds
  dart '("get" "set" "native"))

(c-lang-defconst c-postfix-decl-spec-kwds
  dart '("extends" "implements" "factory"))

(c-lang-defconst c-type-list-kwds
  dart '("new" "const" "is" "is!" "extends" "implements" "factory"))

(c-lang-defconst c-ref-list-kwds
  dart nil)

(c-lang-defconst c-block-stmt-2-kwds
  dart '("for" "if" "switch" "while" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  dart '("break" "continue" "return" "throw"))

(c-lang-defconst c-before-label-kwds
  dart '("break" "continue"))

(c-lang-defconst c-nonlabel-token-key
  dart (concat (concat "\\s\(\\|" (c-lang-const c-nonlabel-token-key))))

(c-lang-defconst c-inexpr-class-kwds
  dart nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  dart nil)

(c-lang-defconst c-other-kwds
  dart '("in"))

(c-lang-defconst c-decl-prefix-re
  dart "\\([\{\}\([;,<]+\\)")

(c-lang-defconst c-cast-parens
  dart nil)

(c-lang-defconst c-block-prefix-disallowed-chars
  dart (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
                          '(?\" ?')))

(c-lang-defconst c-type-decl-prefix-key
  dart "\\(\(\\)\\([^=]\\|$\\)")

(c-lang-defconst c-after-suffixed-type-decl-key
  dart (concat (c-lang-const c-after-suffixed-type-decl-key) "\\|:"))

(c-lang-defconst c-opt-type-suffix-key
  dart nil)

(c-lang-defconst c-recognize-typeless-decls
  dart t)

(c-lang-defconst c-recognize-<>-arglists
  dart t)

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))

(push '(dart-brace-list-cont-nonempty . 0)
      (get 'c-offsets-alist 'c-stylevar-fallback))

(defconst dart-c-style
  '("java"
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (fill-column . 80)
    (c-offsets-alist . ((arglist-intro . ++)
                        (arglist-cont-nonempty . ++)
                        (statement-block-intro . dart-block-offset)
                        (block-close . dart-block-offset)
                        (dart-brace-list-cont-nonempty .
                         dart-brace-list-cont-nonempty-offset)
                        (case-label . +))))
  "The default Dart styles.")

(c-add-style "dart" dart-c-style)

(defvar dart-mode-map (c-make-inherited-keymap)
  "Keymap used in dart-mode buffers.")
(define-key dart-mode-map (kbd "C-c ?") 'dart-show-hover)
(define-key dart-mode-map (kbd "C-c C-g") 'dart-goto)
(define-key dart-mode-map (kbd "C-c C-f") 'dart-find-refs)
(define-key dart-mode-map (kbd "C-c C-e") 'dart-find-member-decls)
(define-key dart-mode-map (kbd "C-c C-r") 'dart-find-member-refs)
(define-key dart-mode-map (kbd "C-c C-t") 'dart-find-top-level-decls)
(define-key dart-mode-map (kbd "C-c C-o") 'dart-format)
(define-key dart-mode-map (kbd "M-/") 'dart-expand)
(define-key dart-mode-map (kbd "M-?") 'dart-expand-parameters)

;;; CC indentation support

(defvar c-syntactic-context nil
  "A dynamically-bound variable used by cc-mode.")

(defun dart-block-offset (info)
  "Calculate the correct indentation for inline functions.

When indenting inline functions, we want to pretend that
functions taking them as parameters essentially don't exist."
  (cl-destructuring-bind (syntax . anchor) info
    (let ((arglist-count
           (cl-loop for (symbol . _) in c-syntactic-context
                    count (eq symbol 'arglist-cont-nonempty))))
      (if (> arglist-count 0)
          (- (* -1 c-basic-offset arglist-count)
             (if (eq syntax 'block-close) c-basic-offset 0))
        (if (eq syntax 'block-close) 0 '+)))))

(defun dart-brace-list-cont-nonempty-offset (info)
  "Indent a brace-list line in the same style as arglist-cont-nonempty.
This could be either an actual brace-list or an optional parameter."
  (cl-destructuring-bind (_ . anchor) info
    ;; If we're in a function definition with optional arguments, indent as if
    ;; the brace wasn't there. Currently this misses the in-function function
    ;; definition, but that's probably acceptable.
    (if (and
         (save-excursion (backward-up-list) (eq (char-after) ?\[))
         (assq 'topmost-intro
               (save-excursion (goto-char anchor) (c-guess-basic-syntax))))
        '++
      ;; Otherwise, we're in an actual brace list, in which case only indent
      ;; once.
      '+)))

(defun dart-in-block-p (syntax-guess)
  "Return whether or not the immediately enclosing {} block is a code block.
The other option, of course, is a map literal.

SYNTAX-GUESS is the output of `c-guess-basic-syntax'."
  (save-excursion
    (c-safe
      ;; If we're in a continued statement within a class, we want to know we're
      ;; in a class so we can return true.
      (when (eq 'statement-cont (caar syntax-guess))
        (save-excursion
          (c-beginning-of-statement-1 nil t t)
          (setq syntax-guess (c-guess-basic-syntax))))

      (backward-up-list)
      (when (= (char-after) ?\{)
        (c-backward-comments)
        (or
         ;; Both anonymous and named functions have a ")" immediately before the
         ;; code block.
         (= (char-before) ?\))
         ;; "else" and "try" are the only keywords that come immediately before
         ;; a block.  Look only back at most 4 characters (the length of
         ;; "else") for performance reasons.
         (looking-back "\\<\\(else\\|try\\)\\>" (- (point) 4))
         ;; CC is good at figuring out if we're in a class.
         (assq 'inclass syntax-guess))))))

(defadvice c-guess-basic-syntax (after dart-guess-basic-syntax activate)
  (when (c-major-mode-is 'dart-mode)
    (let* ((syntax (car (last ad-return-value)))
           (type (car syntax)))
      (save-excursion
        (back-to-indentation)

        (or
         ;; Handle indentation in a constructor with an initializer on a
         ;; separate line.
         (when (memq type '(defun-block-intro inline-close))
           (save-excursion
             (c-safe
               (goto-char (cadr syntax))
               (when (= (char-after) ?:)
                 (c-beginning-of-statement-1)
                 (setq ad-return-value `((,type ,(point))))
                 t))))

         ;; Handle array literal indentation
         (when (memq type
                     '(arglist-intro
                       arglist-cont
                       arglist-cont-nonempty
                       arglist-close))
           (save-excursion
             (c-safe
               (backward-up-list)
               (when (= (char-after) ?\[)
                 (setq ad-return-value
                       `((,(cl-case type
                             (arglist-intro 'brace-list-intro)
                             (arglist-cont 'brace-list-entry)
                             (arglist-cont-nonempty 'dart-brace-list-cont-nonempty)
                             (arglist-close 'brace-list-close))
                          ,(cadr syntax)))))
               t)))

         ;; Handle map literal indentation
         (when (and (memq type '(label statement-block-intro statement-cont statement
                                 block-close defun-block-intro defun-close))
                    (not (dart-in-block-p ad-return-value)))
           (save-excursion
             (c-safe
               (if (= (char-after) ?\})
                   (progn
                     (backward-up-list)
                     (when (= (char-after) ?\{)
                       (back-to-indentation)
                       (setq ad-return-value `((brace-list-close ,(point))))))
                 (c-backward-comments)
                 ;; Completely reset ad-return-value here because otherwise it
                 ;; gets super-screwy.
                 (if (= (char-before) ?\{)
                     (progn
                       (back-to-indentation)
                       (setq ad-return-value `((brace-list-intro ,(point))))
                       t)
                   (backward-up-list)
                   (when (= (char-after) ?\{)
                     (forward-char)
                     (let ((contp (not (looking-at "\\s-*$"))))
                       (c-forward-comments)
                       (back-to-indentation)
                       (setq ad-return-value
                             `((,(if contp 'dart-brace-list-cont-nonempty
                                   'brace-list-entry)
                                ,(point))))
                       t))))))))))))

(defadvice c-inside-bracelist-p (after dart-inside-bracelist-p activate)
  ;; This function is only called within c-guess-basic-syntax. Since we do all
  ;; out brace-list detection in our advice, we just never report being in a
  ;; bracelist there.
  (when (c-major-mode-is 'dart-mode)
    (setq ad-return-value nil)))

(defadvice c-search-decl-header-end (around dart-search-decl-header-end activate)
  (if (not (c-major-mode-is 'dart-mode)) ad-do-it
    (let ((base (point)))
      (while (and
              (c-syntactic-re-search-forward "[;{=:]" nil 'move t t)
              (c-end-of-current-token base))
        (setq base (point)))
      ;; If we hit :, we're in a member initialization list and we want to
      ;; ignore = signs.
      (when (= (char-before) ?:)
        (while (and
                (c-syntactic-re-search-forward "[;{]" nil 'move t t)
                (c-end-of-current-token base))
        (setq base (point)))))))

(if (fboundp 'c-parse-state-1)
  (defadvice c-parse-state (around dart-c-parse-state activate)
    (if (not (c-major-mode-is 'dart-mode)) ad-do-it
      ;; c-parse-state is a wrapper around c-parse-state-1 that does some tricks
      ;; to ensure that dangling brackets in preprocessor commands don't screw up
      ;; parse information for the real world. In Dart, all "preprocessor"
      ;; directives have matched braces, so we don't need to worry about that. The
      ;; wrapper was also screwing up indentation in weird ways, so we just ignore
      ;; it.
      (setq ad-return-value (c-parse-state-1)))))


;;; Additional fontification support

(defun dart-fontify-region (beg end)
  "Use fontify the region between BEG and END as Dart.

This will overwrite fontification due to strings and comments."
  (->
   (-let [font-lock-dont-widen t]
     (narrow-to-region (- beg 1) end)
     ;; font-lock-fontify-region apparently isn't inclusive,
     ;; so we have to move the beginning back one char
     (font-lock-fontify-region (point-min) (point-max)))
   save-excursion save-match-data save-restriction))

(defun dart-limited-forward-sexp (limit &optional arg)
  "Move forward using `forward-sexp' or to limit,
whichever comes first."
  (let (forward-sexp-function)
    (condition-case err
        (save-restriction
          (narrow-to-region (point) limit)
          (forward-sexp arg))
      (scan-error
       (unless (equal (nth 1 err) "Unbalanced parentheses")
         (signal 'scan-error (cdr err)))
       (goto-char limit)))))

(defun dart-highlight-interpolation (limit)
  "Highlight interpolation (${foo})."
  (-let [start (point)]
    (when (re-search-forward "\\(\\${\\)" limit t)
      (if (elt (parse-partial-sexp start (point)) 3) ; in a string
          (save-match-data
            (forward-char -1)
            (-let [beg (point)]
              (dart-limited-forward-sexp limit)
              (dart-fontify-region (+ 1 beg) (point)))

            ;; Highlight the end of the interpolation.
            (when (eq (char-before) ?})
              (put-text-property (- (point) 1) (point) 'face font-lock-variable-name-face))
            t)
        (looking-at "\\<\\>")))))


;;; Boilerplate font-lock piping

(defcustom dart-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in DART mode.
Each list item should be a regexp matching a single identifier."
  :group 'dart-mode)

(c-override-default-keywords 'dart-font-lock-keywords)

(defconst dart-font-lock-keywords-1 (c-lang-const c-matchers-1 dart)
  "Minimal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-2 (c-lang-const c-matchers-2 dart)
  "Fast normal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-3
  (cons
   '(dart-highlight-interpolation 1 font-lock-variable-name-face prepend)
   (c-lang-const c-matchers-3 dart))
  "Accurate normal highlighting for Dart mode.")

(defvar dart-font-lock-keywords dart-font-lock-keywords-3
  "Default expressions to highlight in Dart mode.")

(defvar dart-mode-syntax-table nil
  "Syntax table used in dart-mode buffers.")
(unless dart-mode-syntax-table
  (setq dart-mode-syntax-table
        (funcall (c-lang-const c-make-mode-syntax-table dart))))


;;; Dart analysis server

(cl-defstruct
    (dart--analysis-server
     (:constructor dart--make-analysis-server))
  "Struct containing data for an instance of a Dart analysis server.

The slots are:
- `process': the process of the running server.
- `buffer': the buffer where responses from the server are written."
  process buffer)

(defgroup dart-mode nil
  "Major mode for editing Dart code."
  :group 'languages)

(defvar dart-debug nil
  "If non-nil, enables writing debug messages for dart-mode.")

(defcustom dart-enable-analysis-server nil
  "If non-nil, enables support for Dart analysis server.

The Dart analysis server adds support for error checking, code completion,
navigation, and more."
  :group 'dart-mode
  :type 'boolean
  :package-version '(dart-mode . "0.12"))

(defvar dart--analysis-server nil
  "The instance of the Dart analysis server we are communicating with.")

(defun dart--analysis-server-snapshot-path ()
  "The absolute path to the snapshot file that runs the Dart analysis server."
  (when dart-sdk-path
    (concat dart-sdk-path
            (file-name-as-directory "bin")
            (file-name-as-directory "snapshots")
            "analysis_server.dart.snapshot")))

(defvar dart-analysis-roots nil
  "The list of analysis roots that are known to the analysis server.

All Dart files underneath the analysis roots are analyzed by the analysis
server.")

(defvar dart--analysis-server-next-id 0
  "The ID to use for the next request to the Dart analysis server.")

(defvar dart--analysis-server-callbacks nil
  "An alist of ID to callback to be called when the analysis server responds.

Each request to the analysis server has an associated ID.  When the analysis
server sends a response to a request, it tags the response with the ID of the
request.  We look up the callback for the request in this alist and run it with
the JSON decoded server response.")

(defvar dart--analysis-server-subscriptions nil
  "An alist of event names to lists of callbacks to be called for those events.

These callbacks take the event object and an opaque subcription
object which can be passed to `dart--analysis-server-unsubscribe'.")

(defun dart-info (msg)
  "Logs MSG to the dart log if `dart-debug' is non-nil."
  (when dart-debug (dart-log msg)))

(defun dart-log (msg)
  "Logs MSG to the dart log."
  (let* ((log-buffer (get-buffer-create "*dart-debug*"))
         (iso-format-string "%Y-%m-%dT%T%z")
         (timestamp-and-log-string
          (format-time-string iso-format-string (current-time))))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert "\n\n\n")
      (insert (concat timestamp-and-log-string
                      "\n"
                      msg))
      (insert "\n"))))

(defun dart--start-analysis-server-for-current-buffer ()
  "Initialize Dart analysis server for current buffer.

This starts Dart analysis server and adds either the pub root
directory or the current file directory to the analysis roots."
  (unless dart--analysis-server (dart-start-analysis-server))
  ;; TODO(hterkelsen): Add this file to the priority files.
  (dart-add-analysis-root-for-file)
  (add-hook 'first-change-hook 'dart-add-analysis-overlay t t)
  (add-hook 'after-change-functions 'dart-change-analysis-overlay t t)
  (add-hook 'after-save-hook 'dart-remove-analysis-overlay t t)
  (when (featurep 'flycheck)
   (add-to-list 'flycheck-checkers 'dart-analysis-server)))

(defun dart-start-analysis-server ()
  "Start the Dart analysis server.

Initializes analysis server support for all `dart-mode' buffers."
  (when dart--analysis-server
    (-let [process (dart--analysis-server-process dart--analysis-server)]
      (when (process-live-p process) (kill-process process)))
    (kill-buffer (dart--analysis-server-buffer dart--analysis-server)))

  (let* ((process-connection-type nil)
         (dart-process
          (start-process "dart-analysis-server"
                         "*dart-analysis-server*"
                         (dart-executable-path)
                         (dart--analysis-server-snapshot-path)
                         "--no-error-notification")))
    (set-process-query-on-exit-flag dart-process nil)
    (setq dart--analysis-server
          (dart--analysis-server-create dart-process)))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'dart-mode)
        (dart--start-analysis-server-for-current-buffer)
        (when (buffer-modified-p buffer) (dart-add-analysis-overlay))))))

(defun dart--analysis-server-create (process)
  "Create a Dart analysis server from PROCESS."
  (-let [instance (dart--make-analysis-server
                   :process process
                   :buffer (generate-new-buffer (process-name process)))]
    (buffer-disable-undo (dart--analysis-server-buffer instance))
    (set-process-filter
     process
     (lambda (_ string)
       (dart--analysis-server-process-filter instance string)))
    instance))

(defun dart-add-analysis-overlay ()
  "Report to the Dart analysis server that it should overlay this buffer.

The Dart analysis server allows clients to 'overlay' file contents with
a client-supplied string.  This is needed because we want Emacs to report
errors for the current contents of the buffer, not whatever is saved to disk."
  ;; buffer-file-name can be nil within revert-buffer, but in that case the
  ;; buffer is just being reverted to its format on disk anyway.
  (when buffer-file-name
    (dart--analysis-server-send
     "analysis.updateContent"
     `((files .
              ((,buffer-file-name . ((type . "add")
                                     (content . ,(save-restriction
                                                   (widen)
                                                   (buffer-string)))))))))))

(defun dart-change-analysis-overlay
    (change-begin change-end change-before-length)
  "Report to analysis server that it should change the overlay for this buffer.

The region that changed ranges from CHANGE-BEGIN to CHANGE-END, and the
length of the text before the change is CHANGE-BEFORE-LENGTH. See also
`dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files
      . ((,buffer-file-name
          . ((type . "change")
             (edits
              . (((offset . ,(- change-begin 1))
                  (length . ,change-before-length)
                  (replacement
                   . ,(buffer-substring change-begin change-end))))))))))))

(defun dart-remove-analysis-overlay ()
  "Remove the overlay for the current buffer since it has been saved.

See also `dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files . ((,buffer-file-name . ((type . "remove"))))))))

(defun dart-add-analysis-root-for-file (&optional file)
  "Add the given FILE's root to the analysis server's analysis roots.

A file's root is the pub root if it is in a pub package, or the file's directory
otherwise.  If no FILE is given, then this will default to the variable
`buffer-file-name'."
  (let* ((file-to-add (or file buffer-file-name))
         (pub-root (locate-dominating-file file-to-add "pubspec.yaml"))
         (current-dir (file-name-directory file-to-add)))
    (if pub-root
        (dart-add-to-analysis-roots (expand-file-name pub-root))
      (dart-add-to-analysis-roots (expand-file-name current-dir)))))

(defun dart-add-to-analysis-roots (dir)
  "Add DIR to the analysis server's analysis roots.

The analysis roots are directories that contain Dart files. The analysis server
analyzes all Dart files under the analysis roots and provides information about
them when requested."
  (add-to-list 'dart-analysis-roots dir)
  (dart--send-analysis-roots))

(defun dart--send-analysis-roots ()
  "Send the current list of analysis roots to the analysis server."
  (dart--analysis-server-send
   "analysis.setAnalysisRoots"
   `(("included" . ,dart-analysis-roots)
     ("excluded" . nil))))

(defun dart--analysis-server-send (method &optional params callback)
  "Send the METHOD request to the server with optional PARAMS.

PARAMS should be JSON-encodable.  If you provide a CALLBACK, it will be called
with the JSON decoded response.  Otherwise, the output will just be checked."
  (-let [req-without-id (dart--analysis-server-make-request method params)]
    (dart--analysis-server-enqueue req-without-id callback)))

(defun dart--analysis-server-make-request (method &optional params)
  "Construct a request for the analysis server.

The constructed request will call METHOD with optional PARAMS."
  `((method . ,method) (params . ,params)))

(defun dart--analysis-server-on-error-callback (response)
  "If RESPONSE has an error, report it."
  (-when-let (resp-err (assoc-default 'error response))
    (error "Analysis server error: %s" (assoc-default 'message resp-err))))

(defun dart--analysis-server-enqueue (req-without-id callback)
  "Send REQ-WITHOUT-ID to the analysis server, call CALLBACK with the result."
  (setq dart--analysis-server-next-id (1+ dart--analysis-server-next-id))
  (-let [request
         (json-encode (cons (cons 'id (format "%s" dart--analysis-server-next-id))
                            req-without-id))]

    ;; Enqueue the request so that we can be sure all requests are processed in
    ;; order.
    (push (cons dart--analysis-server-next-id
                (or callback #'dart--analysis-server-on-error-callback))
          dart--analysis-server-callbacks)

    (cond
     ((not dart--analysis-server)
      (message "Starting Dart analysis server.")
      (dart-start-analysis-server))
     ((not (process-live-p (dart--analysis-server-process dart--analysis-server)))
      (message "Dart analysis server crashed, restarting.")
      (dart-start-analysis-server)))

    (dart-info (concat "Sent: " request))
    (process-send-string (dart--analysis-server-process dart--analysis-server)
                         (concat request "\n"))))

(cl-defun dart--analysis-server-process-filter (das string)
  "Handle the event or method response from the dart analysis server.

The server DAS has STRING added to the buffer associated with it.
Method responses are paired according to their pending request and
the callback for that request is given the json decoded response."
  (-let [buf (dart--analysis-server-buffer das)]
    ;; The buffer may have been killed if the server was restarted
    (unless (buffer-live-p buf)
      (cl-return-from dart--analysis-server-process-filter))

    ;; We use a buffer here because emacs might call the filter before the
    ;; entire line has been written out. In this case we store the
    ;; unterminated line in a buffer to be read when the rest of the line is
    ;; output.
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string)
      (-let [buf-lines (s-lines (buffer-string))]
        (delete-region (point-min) (point-max))
        (insert (-last-item buf-lines))

        (-let [messages
               (--filter (and it (not (string-empty-p it)))
                         (-butlast buf-lines))]
          (dolist (message messages)
            (dart-info (concat "Received: " message))
            (dart--analysis-server-handle-msg
             (-let [json-array-type 'list]
               (json-read-from-string message)))))))))

(defun dart--analysis-server-handle-msg (msg)
  "Handle the parsed MSG from the analysis server."
  (-if-let* ((raw-id (dart--get msg 'id))
             (id (string-to-number raw-id)))
      ;; This is a response to a request, so we should invoke a callback in
      ;; dart--analysis-server-callbacks.
      (-if-let (resp-closure (dart--get dart--analysis-server-callbacks id))
          (progn
            (setq dart--analysis-server-callbacks
                  (assq-delete-all id dart--analysis-server-callbacks))
            (funcall resp-closure msg))
        (-if-let (err (dart--get msg 'error))
            (dart--analysis-server-on-error-callback msg)
          (dart-info (format "No callback was associated with id %s" raw-id))))

    ;; This is a notification, so we should invoke callbacks in
    ;; dart--analysis-server-subscriptions.
    (-when-let* ((event (dart--get msg 'event))
                 (params (dart--get msg 'params))
                 (callbacks (dart--get dart--analysis-server-subscriptions event)))
      (dolist (callback callbacks)
        (-let [subscription (cons event callback)]
          (funcall callback params subscription))))))

(defun dart--analysis-server-subscribe (event callback)
  "Registers CALLBACK to be called for each EVENT of the given type.

CALLBACK should take two parameters: the event object and an
opaque subscription object that can be passed to
`dart--analysis-server-unsubscribe'. Returns the same opaque
subscription object."
  (-if-let (cell (assoc event dart--analysis-server-subscriptions))
      (nconc cell (list callback))
    (push (cons event (list callback)) dart--analysis-server-subscriptions))
  (cons event callback))

(defun dart--analysis-server-unsubscribe (subscription)
  "Unregisters the analysis server SUBSCRIPTION.

SUBSCRIPTION is an opaque object provided by
`dart--analysis-server-subscribe'."
  (-let [(event . callback) subscription]
    (delq callback (assoc event dart--analysis-server-subscriptions))))

;;;; Flycheck Error Reporting

(defun dart--flycheck-start (_ callback)
  "Run the CHECKER and report the errors to the CALLBACK."
  (dart-info (format "Checking syntax for %s" (current-buffer)))
  (dart--analysis-server-send
   "analysis.getErrors"
   `((file . ,(buffer-file-name)))
   (-let [buffer (current-buffer)]
     (lambda (response)
       (dart--report-errors response buffer callback)))))

(when (featurep 'flycheck)
 (flycheck-define-generic-checker
  'dart-analysis-server
  "Checks Dart source code for errors using Dart analysis server."
  :start 'dart--flycheck-start
  :modes '(dart-mode)))

(defun dart--report-errors (response buffer callback)
  "Report the errors returned from the analysis server.

The errors contained in RESPONSE from Dart analysis server run on BUFFER are
reported to CALLBACK."
  (dart-info (format "Reporting to flycheck: %s" response))
  (-let [fly-errors (--map (dart--to-flycheck-err it buffer)
                           (dart--get response 'result 'errors))]
    (dart-info (format "Parsed errors: %s" fly-errors))
    (funcall callback 'finished fly-errors)))

(defun dart--to-flycheck-err (err buffer)
  "Create a flycheck error from a dart ERR in BUFFER."
  (flycheck-error-new
   :buffer buffer
   :checker 'dart-analysis-server
   :filename (dart--get err 'location 'file)
   :line (dart--get err 'location 'startLine)
   :column (dart--get err 'location 'startColumn)
   :message (dart--get err 'message)
   :level (dart--severity-to-level (dart--get err 'severity))))

(defun dart--severity-to-level (severity)
  "Convert SEVERITY to a flycheck level."
  (cond
   ((string= severity "INFO") 'info)
   ((string= severity "WARNING") 'warning)
   ((string= severity "ERROR") 'error)))

;;;; Hover

(defun dart-show-hover (&optional show-in-buffer)
  "Displays hover information for the current point.

With a prefix argument, opens a new buffer rather than using the
minibuffer."
  (interactive "P")
  (-when-let (filename (buffer-file-name))
    (let ((show-in-buffer show-in-buffer)
          (buffer (current-buffer))
          (pos (point)))
      (dart--analysis-server-send
       "analysis.getHover"
       `(("file" . ,filename) ("offset" . ,pos))
       (lambda (response)
         (-when-let (hover (car (dart--get response 'result 'hovers)))
           (dart--json-let hover
               (offset
                length
                dartdoc
                (element-description elementDescription)
                (element-kind elementKind)
                (is-deprecated isDeprecated)
                parameter)
             (setq is-deprecated (not (eq is-deprecated :json-false)))

             ;; Briefly highlight the region that's being shown.
             (with-current-buffer buffer
               (dart--flash-highlight offset length))

             (with-temp-buffer
               (when is-deprecated
                 (insert (dart--face-string "DEPRECATED" 'font-lock-warning-face) ?\n))

               (when element-description
                 (insert (dart--highlight-description element-description)
                         (dart--face-string (concat " (" element-kind ")") 'italic))
                 (when (or dartdoc parameter) (insert ?\n)))
               (when parameter
                 (insert
                  (dart--highlight-description parameter)
                  (dart--face-string " (parameter type)" 'italic))
                 (when dartdoc) (insert ?\n))
               (when dartdoc
                 (when (or element-description parameter) (insert ?\n))
                 (insert (dart--highlight-dartdoc dartdoc (not show-in-buffer))))

               (let ((text (buffer-string)))
                 (if show-in-buffer
                     (with-current-buffer-window
                      "*Dart Analysis*" nil nil
                      (insert text)
                      (dart-popup-mode)

                      (setq dart--do-it-again-callback
                            (lambda ()
                              (save-excursion
                                (with-current-buffer buffer
                                  (goto-char pos)
                                  (dart-show-hover t))))))
                   (message "%s" text)))))))))))

(defconst dart--highlight-keyword-re
  (regexp-opt
   '("get" "set" "as" "abstract" "class" "extends" "implements" "enum" "typedef"
     "const" "covariant" "deferred" "factory" "final" "import" "library" "new"
     "operator" "part" "static" "async" "sync" "var")
   'words)
  "A regular expression that matches keywords.")

(defun dart--highlight-description (description)
  "Returns a highlighted copy of DESCRIPTION."
  (with-temp-buffer
    (insert description)
    (goto-char (point-min))

    (while (not (eq (point) (point-max)))
      (cond
       ;; A keyword.
       ((looking-at dart--highlight-keyword-re)
        (dart--fontify-excursion 'font-lock-keyword-face
          (goto-char (match-end 0))))

       ;; An identifier could be a function name or a type name.
       ((looking-at dart--identifier-re)
        (goto-char (match-end 0))
        (put-text-property
         (match-beginning 0) (point) 'face
         (if (dart--at-end-of-function-name-p) 'font-lock-function-name-face
           'font-lock-type-face))

        (cl-case (char-after)
          ;; Foo.bar()
          (?.
           (forward-char)
           (dart--fontify-excursion 'font-lock-function-name-face
             (dart--forward-identifier)))

          ;; Foo bar
          (?\s
           (forward-char)
           (dart--fontify-excursion 'font-lock-variable-name-face
             (dart--forward-identifier)))))

       ;; Anything else is punctuation that we ignore.
       (t (forward-char))))

    (buffer-string)))

(defun dart--at-end-of-function-name-p ()
  "Returns whether the point is at the end of a function name."
  (cl-case (char-after)
    (?\( t)
    (?<
     (and (looking-at (concat "\\(" dart--identifier-re "\\|[<>]\\)*"))
          (eq (char-after (match-end 0)) ?\()))))

(defun dart--highlight-dartdoc (dartdoc truncate)
  "Returns a higlighted copy of DARTDOC."
  (with-temp-buffer
    (insert dartdoc)

    ;; Cut off long dartdocs so that the full signature is always visible.
    (when truncate
      (forward-line 11)
      (delete-region (- (point) 1) (point-max)))

    (goto-char (point-min))

    (while (re-search-forward "\\[.*?\\]" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-reference-face))

    (buffer-string)))

;;;; Navigation

(defun dart-goto ()
  (interactive)
  (-when-let (filename (buffer-file-name))
    (dart--analysis-server-send
     "analysis.getNavigation"
     `(("file" . ,filename) ("offset" . ,(point)) ("length" . 0))
     (lambda (response)
       (-when-let (result (dart--get response 'result))
         (dart--json-let result (files targets regions)
           (-when-let (region (car regions))
             (let* ((target-index (car (dart--get region 'targets)))
                    (target (elt targets target-index))

                    (file-index (dart--get target 'fileIndex))
                    (offset (dart--get target 'offset))
                    (length (dart--get target 'length))

                    (file (elt files file-index)))
               (find-file file)
               (goto-char (+ 1 offset))
               (dart--flash-highlight offset length)))))))))

;;;; Search

(defun dart-find-refs (pos &optional include-potential)
  (interactive "dP")
  (-when-let (filename (buffer-file-name))
    (dart--analysis-server-send
     "search.findElementReferences"
     `(("file" . ,filename)
       ("offset" . ,pos)
       ("includePotential" . ,(or include-potential json-false)))
     (let ((buffer (current-buffer))
           (include-potential include-potential))
       (lambda (response)
         (-when-let (result (dart--get response 'result))
           (let ((name (dart--get result 'element 'name))
                 (location (dart--get result 'element 'location)))
             (dart--display-search-results
              (dart--get result 'id)
              (lambda () 
                (setq dart--do-it-again-callback
                      (lambda ()
                        (with-current-buffer buffer
                          (dart-find-refs pos include-potential))))

                (insert "References to ")
                (insert-button
                 name
                 'action (lambda (_) (dart--goto-location location)))
                (insert ":\n\n"))))))))))

(defun dart-find-member-decls (name)
  "Find member declarations named NAME."
  (interactive "sMember name: ")
  (dart--find-by-name
   "search.findMemberDeclarations" "name" name "Members named "))

(defun dart-find-member-refs (name)
  "Find member references named NAME."
  (interactive "sMember name: ")
  (dart--find-by-name
   "search.findMemberReferences" "name" name "References to "))

(defun dart-find-top-level-decls (name)
  "Find top-level declarations named NAME."
  (interactive "sDeclaration name: ")
  (dart--find-by-name
   "search.findTopLevelDeclarations" "pattern" name "Declarations matching "))

(defun dart--find-by-name (method argument name header)
  "A helper function for running an analysis server search for NAME.

Calls the given analysis server METHOD passing NAME to the given
ARGUMENT. Displays a header beginning with HEADER in the results."
  (dart--analysis-server-send
   method
   (list (cons argument name))
   (lambda (response)
     (-when-let (id (dart--get response 'result 'id))
       (dart--display-search-results
        id
        (lambda ()
          (setq dart--do-it-again-callback
                (lambda ()
                  (dart--find-by-name method argument name header)))
          (insert header name ":\n\n")))))))

(defun dart--display-search-results (search-id callback)
  "Displays search results with the given SEARCH-ID.

CALLBACK is called with no arguments in the search result buffer
to add a header and otherwise prepare it for displaying results."
  (let (buffer
        beginning-of-results
        (total-results 0))
    (with-current-buffer-window
     "*Dart Search*" nil nil
     (dart-popup-mode)
     (setq buffer (current-buffer))
     (funcall callback)
     (setq beginning-of-results (point))

     (dart--analysis-server-subscribe
      "search.results"
      (lambda (event subscription)
        (with-current-buffer buffer
          (dart--json-let event (id results (is-last isLast))
            (when (equal id search-id)
              (-let [buffer-read-only nil]
                (save-excursion
                  (goto-char (point-max))
                  (dolist (result results)
                    (let ((location (dart--get result 'location))
                          (path (dart--get result 'path))
                          (start (point)))
                      (dart--fontify-excursion '(compilation-info underline)
                        (when (cl-some
                               (lambda (element)
                                 (equal (dart--get element 'kind) "CONSTRUCTOR"))
                               path)
                          (insert "new "))

                        (insert
                         (->> path
                              (--remove (member (dart--get it 'kind)
                                                '("COMPILATION_UNIT" "FILE" "LIBRARY" "PARAMETER")))
                              (--map (dart--get it 'name))
                              (-remove 'string-empty-p)
                              nreverse
                              (s-join ".")))

                        (make-text-button
                         start (point)
                         'action (lambda (_) (dart--goto-location location))))

                      (dart--json-let location (file (line startLine) (column startColumn))
                        (insert " " file ":"
                                (dart--face-string line 'compilation-line-number) ":"
                                (dart--face-string column 'compilation-column-number) ?\n)))))

                (setq total-results (+ total-results (length results)))

                (when (eq is-last t)
                  (dart--analysis-server-unsubscribe subscription)
                  (save-excursion
                    (goto-char (point-max))
                    (insert "\nFound " (dart--face-string total-results 'bold) " results."))))))))))

    (select-window (get-buffer-window buffer))
    (goto-char beginning-of-results)))

(defun dart--goto-location (location)
  "Sends the user to the analysis server LOCATION."
  (dart--json-let location (file offset length)
    (find-file file)
    (goto-char (+ 1 offset))
    (dart--flash-highlight offset length)))

;;;; Auto-complete

(defcustom dart-expand-fallback (key-binding (kbd "M-/"))
  "The fallback command to use for `dart-expand'.

This is used when the analysis server isn't available. It
defaults to the command globally bound to M-/."
  :group 'dart-mode
  :type 'function
  :package-version '(dart-mode . "1.0.0"))

(defvar dart--last-expand-results nil
  "The results of the last call to `dart-expand'.")

(defvar dart--last-expand-beginning nil
  "The marker for the beginning of the text inserted by the last call to `dart-expand'.")

(defvar dart--last-expand-end nil
  "The marker for the end of the text inserted by the last call to `dart-expand'.")

(defvar dart--last-expand-index nil
  "The index into `dart--last-expand-results' for the last call to `dart-expand'.")

(defvar dart--last-expand-parameters-index nil
  "The index into for the last parameter suggestion from `dart-expand-parameters'.

This is an index into the paramaterNames and parameterTypes list
in the suggestion identified by `dart--last-expand-index', and
into `dart--last-expand-parameters-ranges'.")

(defvar dart--last-expand-parameters-ranges nil
  "The list of parameter ranges for the last call to `dart-expand-parameters'.

This is a list of pairs of markers. Each pair identifies the
beginning and end of a parameter in the parameter list generated
by `dart-expand-parameters'`.

Note that the end markers are placed one character after the
actual ending of the parameter. This ensures that if the marker
stayas in place when the parameter is overwritten.")

(defvar dart--last-expand-subscription nil
  "The last analysis server subscription from a call to `dart-expand'.")

(cl-defun dart-expand ()
  "Expand previous word using Dart's autocompletion."
  (interactive "*")
  (unless dart-enable-analysis-server
    (call-interactively dart-expand-fallback t)
    (cl-return-from dart-expand))

  (when (and (memq last-command '(dart-expand dart-expand-parameters))
             dart--last-expand-results)
    (cl-incf dart--last-expand-index)
    (when (>= dart--last-expand-index (length dart--last-expand-results))
      (setq dart--last-expand-index 0))
    (dart--use-expand-suggestion
     dart--last-expand-beginning
     dart--last-expand-end
     (elt dart--last-expand-results dart--last-expand-index))
    (cl-return-from dart-expand))

  (when dart--last-expand-subscription
    (dart--analysis-server-unsubscribe dart--last-expand-subscription))
  (setq dart--last-expand-results nil)
  (setq dart--last-expand-beginning nil)
  (setq dart--last-expand-end nil)
  (setq dart--last-expand-index nil)
  (setq dart--last-expand-subscription nil)

  (-when-let (filename (buffer-file-name))
    (dart--analysis-server-send
     "completion.getSuggestions"
     `(("file" . ,filename)
       ("offset" . ,(- (point) 1)))
     (let ((buffer (current-buffer))
           (first t))
       (lambda (response)
         (-when-let (completion-id (dart--get response 'result 'id))
           (dart--analysis-server-subscribe
            "completion.results"
            (setq dart--last-expand-subscription
                  (lambda (event subscription)
                    (dart--json-let event
                        (id results
                            (offset replacementOffset)
                            (length replacementLength)
                            (is-last isLast))
                      (when is-last (dart--analysis-server-unsubscribe subscription))

                      (when (equal id completion-id)
                        (with-current-buffer buffer
                          (dart--handle-completion-event results offset length first))
                        (setq first nil))))))))))))

(defun dart--handle-completion-event (results offset length first)
  "Handles a completion results event.

If FIRST is non-nil, this is the first completion event for this completion."
  ;; Get rid of any suggestions that don't match existing characters. The
  ;; analysis server provides extra suggestions to support search-as-you-type,
  ;; but we don't do that.
  (when (> length 0)
    (-let [text (buffer-substring (+ offset 1) (+ offset length 1))]
      (setq results
            (--remove (string-prefix-p text (dart--get it 'completion) t)
                      results))))

  (when (> (length results) 0)
    ;; Fill the first result so the first call does something. Just save later
    ;; results for future calls.
    (when first
      (setq dart--last-expand-index 0)
      (setq dart--last-expand-beginning (copy-marker (+ offset 1)))
      (dart--use-expand-suggestion (+ offset 1) (+ offset length 1) (car results)))

    (setq first nil)
    (setq dart--last-expand-results results)))

(defun dart--use-expand-suggestion (beginning end suggestion)
  "Inserts SUGGESTION between BEGINNING and END."
  (dart--json-let suggestion
      (completion element
       (selection-offset selectionOffset)
       (is-deprecated isDeprecated)
       (doc-summary docSummary))
    (goto-char beginning)
    (delete-region beginning end)
    (save-excursion
      (insert completion)
      (setq dart--last-expand-end (point-marker)))
    (forward-char selection-offset)

    (with-temp-buffer
      (when (eq is-deprecated t)
        (insert (dart--face-string "DEPRECATED" 'font-lock-warning-face) ?\n))

      (insert (dart--highlight-description (dart--description-of-element element)))
      (when doc-summary
        (insert ?\n ?\n (dart--highlight-dartdoc doc-summary nil)))

      (message "%s" (buffer-string)))))

(defun dart--description-of-element (element)
  "Returns a textual description of an analysis server ELEMENT."
  (dart--json-let element
      (kind name parameters
       (return-type returnType)
       (type-parameters typeParameters))
    (with-temp-buffer
      (if (equal kind "CONSTRUCTOR")
          (progn
            (insert "new " return-type)
            (unless (string-empty-p name)
              (insert "." name))
            (insert parameters)
            (insert " → " return-type))

        (cl-case kind
          ("GETTER" (insert "get "))
          ("SETTER" (insert "set ")))
        (insert name)
        (when type-parameters (insert type-parameters))
        (when parameters (insert parameters))
        (when return-type (insert " → " return-type)))
      (buffer-string))))

(cl-defun dart-expand-parameters ()
  "Adds parameters to the currently-selected `dart-expand' completion.

This will select the first parameter, if one exists."
  (interactive "*")
  (cond
   ((and (eq last-command 'dart-expand)
         dart--last-expand-results)

    ;; If this is called directly after `dart-expand', create the parameter list
    ;; and highlight the first entry.
    (setq dart--last-expand-parameters-index 0)    
    (dart--json-let (elt dart--last-expand-results dart--last-expand-index)
        ((parameter-names parameterNames)
         (argument-string defaultArgumentListString)
         (argument-ranges defaultArgumentListTextRanges))
      (unless parameter-names (cl-return-from dart-expand-parameters))

      (unless argument-string
        (insert ?\()
        (save-excursion
          (insert ?\))
          (setq dart--last-expand-end (point-marker)))
        (cl-return-from dart-expand-parameters))

      (save-excursion
        (insert ?\( argument-string ?\))
        (setq dart--last-expand-end (point-marker)))

      (setq dart--last-expand-parameters-ranges
            (cl-loop for i below (length argument-ranges) by 2
                  collect (let* ((beginning (+ (point) 1 (elt argument-ranges i)))
                                 (end (+ beginning (elt argument-ranges (+ i 1)) 1)))
                            (list (copy-marker beginning) (copy-marker end)))))

      (dart--expand-select-parameter)))

   ((and (< dart--last-expand-beginning (point) dart--last-expand-end)
         dart--last-expand-parameters-index)

    ;; If this is called when the point is within the text generated by the
    ;; last `dart-expand-parameters' call, move to the next parameter in the
    ;; list.
    (cl-incf dart--last-expand-parameters-index)
    (when (>= dart--last-expand-parameters-index (length dart--last-expand-parameters-ranges))
      (setq dart--last-expand-parameters-index 0))

    (dart--expand-select-parameter))))

(defun dart--expand-select-parameter ()
  "Selects the parameter indicated by expansion variables."
  (-let [(beginning end) (elt dart--last-expand-parameters-ranges
                              dart--last-expand-parameters-index)]
    (dart--delsel-range beginning (- end 1)))

  (dart--json-let (elt dart--last-expand-results dart--last-expand-index)
      ((parameter-names parameterNames)
       (parameter-types parameterTypes))        
    (message "%s" (dart--highlight-description 
                   (concat (elt parameter-types dart--last-expand-parameters-index) " "
                           (elt parameter-names dart--last-expand-parameters-index))))))  

(defun dart--delsel-range (beginning end)
  "Highlights the range between BEGINNING and END and enables `delete-selection-mode' temporarily."
  (setq transient-mark-mode nil)
  (goto-char beginning)
  (push-mark nil t)
  (goto-char end)

  ;; Run this in a timer because `activate-mark' doesn't seem to work
  ;; directly, and because we don't want to disable `delete-selection-mode'
  ;; when `post-command-hook' is invoked after the calling command finishes.
  (run-at-time
   "0 sec" nil
   (lambda ()
     (activate-mark)

     ;; Overwrite the current selection, but don't globally enable
     ;; delete-selection-mode.
     (unless delete-selection-mode
       (delete-selection-mode 1)
       (add-hook 'post-command-hook 'dart--disable-delsel t t)))))

(defun dart--disable-delsel ()
  "Disables `delete-selection-mode' and deactivates the mark.

Also removes this function from `post-command-hook'."
  (deactivate-mark)
  (delete-selection-mode 0)
  (remove-hook 'post-command-hook 'dart--disable-delsel t))


;;; Popup Mode

(define-derived-mode dart-popup-mode fundamental-mode "DartPopup"
  "Major mode for popups."
  :mode 'dart-popup
  (use-local-map dart-popup-mode-map))

(put 'dart-popup-mode 'mode-class 'special)

(defvar dart-popup-mode-map (make-sparse-keymap)
  "Keymap used in Dart popup buffers.")
(set-keymap-parent dart-popup-mode-map help-mode-map)

(define-key dart-popup-mode-map (kbd "g") 'dart-do-it-again)

;; Unbind help-specific keys.
(define-key dart-popup-mode-map (kbd "RET") nil)
(define-key dart-popup-mode-map (kbd "l") nil)
(define-key dart-popup-mode-map (kbd "r") nil)
(define-key dart-popup-mode-map (kbd "<XF86Back>") nil)
(define-key dart-popup-mode-map (kbd "<XF86Forward>") nil)
(define-key dart-popup-mode-map (kbd "<mouse-2>") nil)
(define-key dart-popup-mode-map (kbd "C-c C-b") nil)
(define-key dart-popup-mode-map (kbd "C-c C-c") nil)
(define-key dart-popup-mode-map (kbd "C-c C-f") nil)

(defun dart-do-it-again ()
  "Re-runs the logic that generated the current buffer."
  (interactive)
  (when dart--do-it-again-callback
    (funcall dart--do-it-again-callback)))


;;; Formatting

(defcustom dart-formatter-command-override nil
  "The command for running the Dart formatter.

Don't read this variable; call `dart-formatter-command' instead."
  :type 'string
  :group 'dart-mode
  :package-version '(dart-mode . "1.0.0"))

(defcustom dart-formatter-line-length 80
  "The line length to use when running the Dart formatter."
  :type 'integer
  :group 'dart-mode
  :package-version '(dart-mode . "1.0.0"))

(defcustom dart-format-on-save nil
  "Whether to run the Dart formatter before saving."
  :type 'boolean
  :group 'dart-mode
  :package-version '(dart-mode . "1.0.0"))

(defcustom dart-formatter-show-errors 'buffer
  "Where to display Dart formatter error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite the formatter's echo output if used from
inside a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'dart-mode)

(defun dart-formatter-command ()
  "The command for running the Dart formatter.

This can be customized by setting `dart-formatter-command-override'."
  (or dart-formatter-command-override
      (when dart-sdk-path
        (file-name-as-directory "bin")
            (if (memq system-type '(ms-dos windows-nt))
                "dartfmt.exe"
              "dartfmt"))))

(defvar dart--formatter-compilation-regexp
  '("^line \\([0-9]+\\), column \\([0-9]+\\) of \\([^ \n]+\\):" 3 1 2)
  "Regular expresion to match errors in the formatter's output.
See `compilation-error-regexp-alist' for help on their format.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'dart-formatter dart--formatter-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'dart-formatter)

(cl-defun dart-format ()
  "Format the current buffer using the Dart formatter.

By default, this uses the formatter in `dart-sdk-path'. However,
this can be overridden by customizing
`dart-formatter-command-override'."
  (interactive)
  (let* ((file (make-temp-file "format" nil ".dart"))
         (patch-buffer (get-buffer-create "*Dart formatter patch*"))
         (error-buffer (when dart-formatter-show-errors
                         (get-buffer-create "*Dart formatter errors*")))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (args `("--line-length" ,(number-to-string dart-formatter-line-length)
                 "--overwrite" ,file)))
    (unwind-protect
        (save-restriction
          (widen)

          (when error-buffer
            (with-current-buffer error-buffer
              (setq buffer-read-only nil)
              (erase-buffer)))

          (write-region nil nil file nil 'no-message)
          (dart-info (format "%s %s" (dart-formatter-command) args))

          (unless (zerop (apply #'call-process (dart-formatter-command) nil error-buffer nil args))
            (message "Formatting failed")
            (when error-buffer
              (dart--formatter-show-errors error-buffer file (buffer-file-name)))
            (cl-return-from dart-format))

          ;; Apply the format as a diff so that only portions of the buffer that
          ;; actually change are marked as modified.
          (if (zerop (call-process-region (point-min) (point-max)
                                          "diff" nil patch-buffer nil "--rcs" "-" file))
              (message "Buffer is already formatted")
            (dart--apply-rcs-patch patch-buffer)
            (message "Formatted buffer"))
          (when error-buffer (dart--kill-buffer-and-window error-buffer)))
      (kill-buffer patch-buffer)
      (delete-file file))))

(defun dart--apply-rcs-patch (patch-buffer)
  "Apply an RCS diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; The relative offset between line numbers in the buffer and in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so we have to
        ;; keep an offset when making changes to the buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it negative),
        ;; deleting lines increments it. This order simplifies the forward-line
        ;; invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid RCS patch or internal error in dart--apply-rcs-patch"))

          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (-let [start (point)]
                (forward-line len)
                (-let [text (buffer-substring start (point))]
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))

             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (cl-incf line-offset len)
                (let (kill-ring) (kill-whole-line len))))

             (t
              (error "Invalid RCS patch or internal error in dart--apply-rcs-patch")))))))))

(defun dart--formatter-show-errors (error-buffer temp-file real-file)
  "Display formatter errors in `error-buffer'.
This replaces references to TEMP-FILE with REAL-FILE."
  (with-current-buffer error-buffer
    (-let [echo (eq dart-formatter-show-errors 'echo)]
      (goto-char (point-min))
      (-let [regexp (concat "\\(" (regexp-quote temp-file) "\\):")]
        (while (search-forward-regexp regexp nil t)
          (replace-match (file-name-nondirectory real-file) t t nil 1)))

      (if echo
          (progn
            (message "%s" (buffer-string))
            (dart--kill-buffer-and-window error-buffer))
        (compilation-mode)
        (temp-buffer-window-show error-buffer)
        (select-window (get-buffer-window error-buffer))))))


;;; Initialization

;;;###autoload (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;;;###autoload
(defun dart-mode ()
  "Major mode for editing Dart files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `dart-mode-hook'.

Key bindings:
\\{dart-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table dart-mode-syntax-table)
  (setq major-mode 'dart-mode
        mode-name "Dart")
  (use-local-map dart-mode-map)
  (c-init-language-vars dart-mode)
  (c-common-init 'dart-mode)
  (c-set-style "dart")
  (when dart-enable-analysis-server
    (if (null dart-sdk-path)
        (dart-log
         "Cannot find `dart' executable or Dart analysis server snapshot.")
      (dart--start-analysis-server-for-current-buffer)))

  (add-hook (make-local-variable 'before-save-hook)
            (lambda () (when dart-format-on-save (dart-format))))

  (run-hooks 'c-mode-common-hook)
  (run-hooks 'dart-mode-hook)
  (c-update-modeline))

(provide 'dart-mode)

;;; dart-mode.el ends here
