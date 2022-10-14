;;; gk-org.el --- org-mode configuration             -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>

;;; Commentary:

;; Configurations for ‘org-mode’ and related packages.

;;; Code:

(require 'autoinsert)
(require 'custom)
(require 'elpher)
(require 'goto-addr)
(require 'ob-python)
(require 'org)
(require 'org-agenda)
(require 'org-attach)
(require 'org-capture)
(require 'org-checklist)
(require 'org-compat)
(require 'org-habit)
(require 'org-id)
(require 'org-num)
(require 'org-refile)
(require 'org-variable-pitch)
(require 'org-zotxt)
(require 'ox)
(require 'ox-icalendar)
(require 'ox-latex)
(require 'savehist)

(require 'gk-fd)
(require 'gk-minor-mode)
(require 'gk-misc)




;;;; Emphasis:
;; Slightly customise emphasis markers to work near Unicode
;; delimiters.  Also expand the range these markers work.

(defvar gk-original-org-emphasis-regexp-components
  (cl-copy-tree (default-value 'org-emphasis-regexp-components)))

(defvar gk-org-emphasis-additional-chars "‘’«»“”·﻿" ;includes zero-width-nbsp
  "New characters to add to Org emphasis delimiters")

(setq
 org-emphasis-regexp-components
 (list
  (concat (car gk-original-org-emphasis-regexp-components)
          gk-org-emphasis-additional-chars)
  (concat (cadr gk-original-org-emphasis-regexp-components)
          gk-org-emphasis-additional-chars)
  (caddr gk-original-org-emphasis-regexp-components)
  (cadddr gk-original-org-emphasis-regexp-components)
  20))

(org-reload)



;;;; Citations:

;; Citation link for org-mode, based on
;; http://bastibe.de/2014-09-23-org-cite.html.

;; For use with =gk-article= Latex class.  See =org-latex-classes=.  Uses
;; biblatex.

;; Used like =[[cite:<citation-name>]]=.  Exports to
;; =\cite{<citation-name>}=.  Add this line to be able to follow these
;; links:

;; :  #+LaTeX_HEADER: \gkbib{<bibfile>}{<style>}{<bibsectname>}

(defun gk-org-cite-follow (name)
  "Open bibliography and jump to appropriate entry.

NAME is the citation name."
  (let (citfile)
    (setf
     citfile
     (save-excursion
       (goto-char (point-min))
       (save-match-data
         (re-search-forward
          "^#\\+LATEX_HEADER: \\\\gkbib{\\([^}]+\\)}" nil t)
         (match-string 1))))
    (unless citfile
      (user-error
       "No citation file specified"))
    (find-file-other-window citfile)
    (goto-char (point-min))
    (search-forward name)))

(defun gk-org-cite-export (path desc format)
  "Export a citation link.

For   LaTeX,    export   a   link   like    [[cite:cohen93]]   as
\cite{cohen93}.   For other  formats,  this function  returns
nil, telling Org to do what it defaults to.

For PATH, DESC and FORMAT see `org-add-link-type'."
  (case format
    ((quote latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s]{%s}" desc path)))))

(org-add-link-type "cite" #'gk-org-cite-follow #'gk-org-cite-export)



;;;; Utilities:

(defun gk-org-babel-load-this-file ()
  "Load current `org-babel' file."
  (interactive)
  (org-babel-load-file (buffer-file-name)))

(defun gk-org-dir-file (path)
  "Get file PATH in `org-directory'."
  (expand-file-name
   (concat org-directory "/"
           (replace-regexp-in-string "^/+" "" path))))

(defun gk-org-dir-files (&rest paths)
  "Get a list of file PATHs in `org-directory'."
  (mapcar #'gk-org-dir-file paths))

(defun gk-org-dir-find-file ()
  "Find a file from the `org-directory'."
  (interactive)
  (let ((default-directory (concat org-directory "/")))
    (call-interactively #'find-file)))

(defun gk-org-confirm-elisp-link-function (prompt)
  "Skip confirmation PROMPT in `org-directory'."
  (when (buffer-file-name)
   (if (not (equal
             (file-truename
              (file-name-directory
               (expand-file-name "bob" org-directory)))
             (file-truename
              (file-name-directory (expand-file-name (buffer-file-name))))))
       (yes-or-no-p prompt)
     t)))

(setq org-confirm-elisp-link-function
      #'gk-org-confirm-elisp-link-function)

(defun gk-org-decrypt-element ()
  "Decrypt the element under point, show in a new buffer."
  (interactive)
  (save-excursion
    (let ((transient-mark-mode t))
      (org-mark-element)
      (epa-decrypt-region
       (region-beginning) (region-end)
       (lambda ()
         (let ((decrypted-elem (org-get-heading t t))
               (bufnam (buffer-name)))
           (prog1
               (switch-to-buffer (get-buffer-create "*Org Secret*"))
             (read-only-mode -1)
             (fundamental-mode)
             (erase-buffer)
             (insert ">>> " decrypted-elem " (" bufnam ")")
             (newline)
             (insert ">>> Hit `Q' in order to *kill* this buffer.")
             (newline 2)
             (special-mode)
             (local-set-key [?Q] 'kill-this-buffer))))))))

;; Adapted from: https://www.reddit.com/r/emacs/comments/8qm1lb/new_orgcountwords_command/
(defun gk-org-count-words-subtree ()
  "If region is active, count words in it; otherwise count words in subtree."
  (interactive)
  (if (use-region-p)
      (funcall-interactively #'count-words-region
                             (region-beginning) (region-end))
    (org-with-wide-buffer
     (cl-loop for (lines words characters)
              in (org-map-entries
                  (lambda ()
                    (gk-org-forward-to-entry-content 'unsafe)
                    (let ((end (org-entry-end-position)))
                      (list (count-lines (point) end)
                            (count-words (point) end)
                            (- end (point)))))
                  nil 'tree)
              sum lines into total-lines
              sum words into total-words
              sum characters into total-characters
              finally do
              (message
               "Subtree \"%s\" has %s lines, %s words, and %s characters."
               (org-get-heading t t)
               total-lines total-words total-characters)))))

(defun gk-org-forward-to-entry-content (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.

If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (org-back-to-heading))
  (cl-loop for element = (org-element-at-point)
           for pos = (pcase element
                       (`(headline . ,_)
                        (org-element-property :contents-begin element))
                       (`(,(or 'planning 'property-drawer 'drawer) . ,_)
                        (org-element-property :end element)))
           while pos
           do (goto-char pos)))

;; From https://orgmode.org/worg/org-hacks.html
;;
;; «Count words in an Org buffer
;;
;; Paul Sexton posted this function to count words in an Org buffer:»
;;
;; This version includes some fixes, so not only an aesthetic
;; modification.
(defun gk-org-word-count
    (beg end &optional count-latex-macro-args no-count-footnotes)
  "Report the number of words in the Org mode buffer or selected region.

Ignores:
- comments
- tables
- source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
- hyperlinks (but does count words in hyperlink descriptions)
- tags, priorities, and TODO keywords in headers
- sections tagged as 'not for export'.

If the optional argument NO-COUNT-FOOTNOTES is non-nil, footnote
text is ignored.

If the optional argument COUNT-LATEX-MACRO-ARGS is non-nil, the word count
includes LaTeX macro arguments (the material between {curly braces}).
Otherwise, and by default, every LaTeX macro counts as 1 word regardless
of its arguments.

If called interactively (including with a keybinding), report the
word count in the echo area and return nil.  If not, return the
number."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless mark-active
    (setf beg (point-min)
          end (point-max)))
  (let ((wc 0)
        (latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (cond
         ;; Ignore comments.
         ((or (org-at-comment-p) (org-at-table-p))
          nil)
         ;; Ignore hyperlinks. But if link has a description, count
         ;; the words within the description.
         ((looking-at org-bracket-link-analytic-regexp)
          (when (match-string-no-properties 5)
            (let ((desc (match-string-no-properties 5)))
              (save-match-data
                (incf wc (length (remove "" (org-split-string
                                             desc "\\W")))))))
          (goto-char (match-end 0)))
         ((looking-at org-any-link-re)
          (goto-char (match-end 0)))
         ;; Ignore source code blocks.
         ((org-between-regexps-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
          nil)
         ;; Ignore inline source blocks, counting them as 1 word.
         ((save-excursion
            (ignore-errors (backward-char))
            (equal 'inline-src-block (car (org-element-at-point))))
          (goto-char (match-end 0))
          (setf wc (+ 2 wc)))
         ;; Count latex macros as 1 word, ignoring their arguments.
         ((save-excursion
            (ignore-errors (backward-char))
            (looking-at latex-macro-regexp))
          (goto-char (if count-latex-macro-args
                         (match-beginning 2)
                       (match-end 0)))
          (setf wc (+ 2 wc)))
         ;; Ignore footnotes.
         ((and no-count-footnotes
               (or (org-footnote-at-definition-p)
                   (org-footnote-at-reference-p)))
          nil)
         (t
          (let ((contexts (org-context)))
            (cond
             ;; Ignore tags and TODO keywords, etc.
             ((or (assoc :todo-keyword contexts)
                  (assoc :priority contexts)
                  (assoc :keyword contexts)
                  (assoc :checkbox contexts))
              nil)
             ;; Ignore sections marked with tags that are
             ;; excluded from export.
             ((assoc :tags contexts)
              (if (intersection (org-get-tags-at) org-export-exclude-tags
                                :test 'equal)
                  (org-forward-heading-same-level 1)
                nil))
             (t
              (incf wc))))))
        (re-search-forward "\\w+\\W*")))
    (if (called-interactively-p 'any)
        (message (format "%d words in %s." wc
                         (if mark-active "region" "buffer")))
      ;; If non-interactive, just return the number
      wc)))

(defvar gk-org-reading-note--history '("0"))
(cl-pushnew 'gk-org-reading-note--history savehist-additional-variables)

(defun gk-org-refill-reading-note ()
  "Refill a list item when taking reading notes from a PDF.

Account for soft hyphens."
  (interactive)
  (goto-char
    (save-excursion
      (save-match-data
        (save-restriction
          (let* ((beg (or (and (looking-at "^-") (point))
                          (re-search-backward "^- ")))
                 (end (re-search-forward
                       (rx (or "\n\n" buffer-end))))
                 (lines (count-lines beg end)))
            (narrow-to-region beg (- end 2))
            (dotimes (_ lines)
              (join-line))
            ;; Deal with soft-hyphens
            (goto-char (+ 2 (point-min)))
            (while (re-search-forward "[-­‐] " nil t)
              (replace-match ""))
            (fill-paragraph)

            ;; return where to go
            (+ 2 (point-max))))))))

(defun gk-org-insert-reading-note (page &optional ellipsise merge arg)
  "Insert a reading note into the reading notes file.

A note in that file has a certain structure, i.e. a list item
with the page number as the first thing, then the quote text,
which comes from the ‘kill-ring’ via ‘yank’ wrapped in
guillemets.  PAGE is the page number, and can be any string,
given how page numbers are realised varies in the real world.

If ELLIPSISE is non-nil, call
‘gk-org-reading-notes-ellipsise-last-note’ after inserting the
note.

If MERGE is non-nil, merge with the above reading note.

ARG is the current prefix argument, passed to
‘gk-org-reading-note-merge-last-n-notes’ if MERGE is non-nil."
  (interactive
   (list
    (let ((def (if-let* ((maybe-pdf-buffer-list
                          (seq-filter
                           ($ (equal 'pdf-view-mode (buffer-local-value 'major-mode $1)))
                           (mapcar #'window-buffer (window-list))))
                         (maybe-pdf-buffer (when (= 1 (length maybe-pdf-buffer-list))
                                             (car maybe-pdf-buffer-list))))
                   (with-current-buffer maybe-pdf-buffer
                     (pdf-view-current-pagelabel))
                 (car gk-org-reading-note--history))))
      (read-string
       (format
        "Page number (default: %s, ‘0’ for no page number): "
        def)
       nil 'gk-org-reading-note--history def t))
    (y-or-n-p "Ellipsise? ")
    (y-or-n-p "Merge with last note? ")
    (or current-prefix-arg '(1))))
  (goto-char (line-beginning-position))
  (insert "- ")
  (unless (string= "0" page)
    (insert "p. " page ": "))
  (insert
   "«"
   (with-temp-buffer
     (yank)
     (string-trim
      (buffer-substring (point-min) (point-max)))))
  (insert "»\n\n")
  (gk-org-refill-reading-note)
  (when merge
    (gk-org-reading-note-merge-last-n-notes 2)
    (gk-org-refill-reading-note))
  (when ellipsise
    (gk-org-reading-notes-ellipsise-last-note)
    (gk-org-refill-reading-note)))

(defun gk-org-reading-notes-ellipsise-last-note ()
  "Ellipsise the beginning and the end of the last note.

This means, look at the first char, if lowercase, assume it’s a
partial sentence.  Then look at the last char, if not a period,
assume again it’s a partial sentence.  Then ellipsise with
brackets and an ASCII ellipsis, i.e. three consecutive dots."
  (interactive)
  (save-excursion
    (condition-case err
        (progn
          (re-search-backward "^- p." nil)
          (re-search-forward ": «" (line-end-position))
          ;; We’re at the first char of the note, right after the
          ;; lquote. If it’s lower-case, upcase it and add the
          ;; ellipsis.
          (let ((case-fold-search nil))
           (save-match-data
             (when (looking-at (rx lower))
               (let ((char (buffer-substring (point) (1+ (point)))))
                 (delete-char 1)
                 (insert "[... " (upcase char))
                 (save-excursion
                   (forward-word)
                   (insert "]"))))
             ;; We’re at the last char, right before rquote.  Check if
             ;; there is a period, or add one with ellipsis.
             (when (re-search-forward
                    "»\n" (save-excursion (org-forward-paragraph) (point)))
               (backward-char 3)
               ;; Delete punctuation
               (when (looking-at (rx (any ":;,")))
                 (delete-char 1)
                 (backward-char))
               (when (looking-at (rx (not (any ".!?\"”’"))))
                 (forward-char 1)
                 (insert "[... .]"))))))
      ('search-failed
       (user-error "Could not find note delimitation")))))

(defun gk-org-insert-reading-bibliograpy-note ()
  (interactive)
  (unless (org-insert-item)
    (goto-char (line-beginning-position))
    (insert "- "))
  (insert "-> ")
  (yank)
  (insert "\n")
  (gk-org-refill-reading-note)
  (when (y-or-n-p "Inserted bibliographic reference, save file now?")
    (save-buffer)))

(defun gk-org-reading-note-merge-last-n-notes (n)
  "Merge last N reading notes, at least 2.

In interactive mode N is read from the prefix argument.  If it’s
not given or is one, it’s taken as two.  If less than two, it’s a
user error and the command aborts."
  (interactive "p")
  ;; If no arg was given or it was one, assume two.
  (when (= n 1)
    (setq n 2))
  (unless (>= n 2)
    (user-error "Merging less than two notes is not really possible, no?"))
  (dotimes (_ (1- n))
    (save-excursion
      (condition-case err
       (progn
         (goto-char (line-beginning-position))
         (when (looking-at "^-")
           (re-search-forward "«."))
         (let ((pattern (rx (and "»\n\n- p\." (1+ (not (any "\n"))) ": «"))))
           (re-search-backward pattern)
           (delete-region (point) (re-search-forward ": «"))
           (insert " ")
           (gk-org-refill-reading-note)))
       ('search-failed
        (user-error "Could not find last research note"))))))

(cl-defun gk-org-forward-content (&optional (n 1))
  "Go forward in content view.

Toggle the contents view, go to Nth next visible entry, make it
visible.

N defaults to 1."
  (interactive "p")
  (org-content)
  (outline-next-visible-heading n)
  (org-show-entry)
  (recenter 1))

(cl-defun gk-org-backward-content (&optional (n 1))
  "Go backward in content view.

Toggle the contents view, go to Nth previous visible entry, make it
visible.

N defaults to 1."
  (interactive "p")
  (org-content)
  (outline-previous-visible-heading n)
  (org-show-entry)
  (recenter 1))

(defun gk-org-insert-all-stored-links (arg)
  "Insert the contents of ‘org-stored-links’, one per line."
  (interactive (list current-prefix-arg))
  (cond
   ((null org-stored-links)
    (user-error "No stored links available"))
   ;; at indentation but no list, start list
   ((save-excursion
      (goto-char (line-beginning-position))
      (looking-at "^ +$"))
    (insert "- "))
   ;; at empty list item, reuse
   ((let* ((el (org-element-at-point)))
      (and (eq 'item (car el))))
    (insert " "))
   ;; at non-empty list item, add sublist
   ((let* ((el (org-element-at-point)))
      (and (eq 'paragraph (car el))
           (eq 'item (save-excursion
                       (back-to-indentation)
                       (car (org-element-at-point))))))
    (newline-and-indent)
    (insert "- "))
   ;; no list item, add new
   ((eq 'plain-list (car (org-element-at-point)))
    ;; skip ‘arg’ here, we’ll do it manually on the first time
    (org-insert-item))
   ;; not in list, start new
   (t
    (insert "- ")))
  (when arg (insert "[ ] "))
  ;; for some reason using ‘(last org-stored-links)’ didn’t work, so
  ;; we’re back to counting with fingers...
  (let ((n 0))
    (dolist (link org-stored-links)
      (insert (apply #'format "[[%s][%s]]" link))
      (unless (eq (incf n) (length org-stored-links))
        (org-insert-item arg))))
  ;; clear the list
  (when (y-or-n-p "All links inserted, clear stored links?")
    (setq org-stored-links nil)))

(define-advice org-tree-to-indirect-buffer
    (:around (fn arg) arg-for-dedicated-frame)
  "Use ARG not for selection but for toggling dedicated frames.

If called with prefix arg, open in a dedicated frame.  Otherwise,
respect ‘org-indirect-buffer-display’."
  (interactive "P")
  (let ((org-indirect-buffer-display
         (if arg 'dedicated-frame
           org-indirect-buffer-display)))
    (funcall fn)))

(defun gk-org-ensure-empty-line-before-headlines (beg end)
  "Make sure there is an empty line before each headline in the region.

Interactively, default to whole buffer if region is not active,
and report how many headlines were affected."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((count 0)
        (buf (clone-indirect-buffer
              (format " *gk-org-empty-lines:%s*" (buffer-name))
              nil t)))
    (with-current-buffer buf
      ;; Region will not be active in the indirect buffer.
      (unless (and (= beg (point-min))
                   (= end (point-max)))
        (narrow-to-region beg end))
      (org-show-all)
      (goto-char (point-min))
      (org-map-entries
       ($ (forward-line -1)
          (unless (looking-at
                   (rx (or (and bol eol)
                           (and bol "\n" "* ")
                           ;; Empty entry
                           (and bol "*" (0+ anychar) "\n" "* "))))
            (forward-line 1)
            (open-line 1)
            (cl-incf count))
          (forward-line 1))))
    (kill-buffer buf)))


(defun gk-org-export-region-as-markdown (beg end)
  "Use ‘ox-hugo’ to translate the region to markdown.

Does not break current restrictions and does not write the output
to a file.  Just pops up a buffer with Markdown in Hugo format.

Beware: this means hard newlines are not expressed as two
consecutive spaces but with a <br/>, and the buffer has some
front matter added.  This means the output may beed to be
modified slightly before it’s used e.g. when posting to Reddit."
  ;; FIXME(2020-09-14): transform/eliminate hugo artefacts
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (org-hugo-export-as-md nil nil t)))


(defun gk-org-insert-list-of-stored-links ()
  "Insert ‘org-stored-links’ as a bulleted list."
  (interactive)
  (unless (= (point) (line-beginning-position))
    (newline))
  (dolist (link org-stored-links)
    (insert (apply #'format "- [[%s][%s]]\n" link))))


;; From: https://stackoverflow.com/a/28130043
(defun gk-org-todo-delayed-done (&optional arg)
  "Like org-todo-yesterday, but prompt the user for a date. The time
of change will be 23:59 on that day"
  (interactive "P")
  (let* ((hour (nth 2 (decode-time
                       (org-current-time))))
         (daysback (- (date-to-day (current-time-string))
                      (org-time-string-to-absolute (org-read-date))))
         (org-extend-today-until (+ 1 (* 24 (- daysback 1)) hour))
         (org-use-effective-time t)) ; use the adjusted timestamp for logging
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-todo arg)
      (org-todo arg))))



;;;; Variables:

(setf
 ;; Open files in the same window.
 (cdr (assoc 'file org-link-frame-setup)) 'find-file
 ;; Place tags right after the title.
 org-tags-column 0
 ;; If an #+attr_*: :width xxx available, use xxx, else, car of this
 ;; value.
 org-image-actual-width
 (if (gk-gui-p)
     (list (* 48 (aref (font-info (face-attribute 'default :family)) 10)))
   ;; Do not change if non-gui.
   org-image-actual-width)
 ;; The couple settings below are adapted from
 ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/.
 ;;
 ;; Open collapsed stuff when editing them.
 org-catch-invisible-edits 'smart
 ;; In collapsed view, hide empty lines between subtrees.
 org-cycle-separator-lines 0
 ;; RET on links follows link.
 org-return-follows-link t)

(setf
 ;; Week starts on Monday
 calendar-week-start-day 1)

;; Enforce subtasks are done before the parent.
(setf
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t)



;;;; Org files and directories:

(setq org-export-coding-system 'utf-8
      org-directory gk-notes-directory
      org-default-notes-file (gk-org-dir-file "Start.org")
      org-icalendar-combined-agenda-file (expand-file-name
                                          "~/Documents/ajanda.ics")
      org-id-locations-file (locate-user-emacs-file "etc/org-id-locations.el")
      org-archive-location (gk-org-dir-file "Attic/Arşiv.org::datetree/"))



;;;; Apps:

;; Use system app to handle PDFs.
(setcdr (assoc "\\.pdf\\'" org-file-apps) "xdg-open %s")

(setcdr (assoc 'file org-link-frame-setup)
        (lambda (&rest args)
          (apply
           (save-match-data
             (if (string-match "\\.org\\(::.*\\)?$" (car args))
                 #'find-file-other-window
               #'find-file))
           args)))



;;;; Agenda:

;; Don’t fucking kill my buffers.  Why would I ever want that?
(defalias 'org-release-buffers #'ignore)

(define-advice org-agenda-switch-to
    (:around (fn arg) in-other-window)
  "Show the buffer in a bottom side window and switch to it."
  (interactive "P")
  (let (buf ret pos)
    (setq buf (save-window-excursion
                (setq ret (funcall fn)
                      pos (point))
                (message (buffer-name))
                (current-buffer)))
    (if arg
        (display-buffer buf)
      (display-buffer-in-side-window buf '((side . bottom))))
    (select-window (get-buffer-window buf))
    (goto-char pos)
    (org-back-to-heading)
    (recenter-top-bottom 0)
    ret))

(defvar gk-org-agenda-key "p"
  "Key for my main agenda view.")

(defvar gk-org-agenda-used-p nil
  "Whether or not the Org Mode Agenda is used actively.
Mainly used in ‘gk-home’.")

(defun gk-org-agenda ()
  "Show my main agenda."
  (interactive)
  (org-agenda nil gk-org-agenda-key))

(setf
 ;; End the day at 5am next day, because I tend to stay up past
 ;; midnight.
 org-extend-today-until 5
 ;; Don't show done items.
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-timestamp-if-done t
 ;; Don't mess with my window setup.
 org-agenda-window-setup 'current-window
 ;; Make todays agenda appear first, then the rest grouped by
 ;; category.
 org-agenda-sorting-strategy
 '((agenda time-up category-up deadline-up scheduled-up
           tag-up habit-down priority-down)
   (todo priority-down category-up)
   (tags priority-down category-up)
   (search category-up))
 org-agenda-dim-blocked-tasks nil
 ;; Don’t colour unless deadline
 org-agenda-deadline-faces '((1.0 . org-warning)
                             (0.0 . default))
 ;; Compact display for scheduled items.
 org-agenda-use-time-grid nil)

(setf org-agenda-block-separator nil)

;; Custom agenda setup
(setf
 ;; Make space alloc’d prefixes slightly larger, my categories are a
 ;; bit descriptive.
 org-agenda-prefix-format
 '((agenda . " %i %-16:c%?-16t% s")
   (todo . " %i %-16:c")
   (tags . " %i %-16:c")
   (search . " %i %-16:c"))
 ;; More room for habit titles.
 org-habit-preceding-days 21
 org-habit-graph-column 49
 ;; Files to be used in agenda.
 org-agenda-files nil
 org-agenda-hide-tags-regexp "."
 org-agenda-custom-commands nil)


(defun gk-org-agenda-mode-hook ()
  (save-excursion
    (hl-line-mode +1)
    (setq-local word-wrap t)
    (setq-local truncate-lines nil)))

(add-hook 'org-agenda-mode-hook #'gk-org-agenda-mode-hook)

(defun gk-org-agenda-finalize-hook ()
  "Finishing touches to the Agenda buffer."
  (setq-local default-directory org-directory)
  (save-excursion
    (goto-char (point-min))
    ;; De-emphasise useless info.
    (while
        (ignore-errors
          (re-search-forward
           ;; $Category: ( $Scheduled | $Time | $Deadline | $Progress ):
           (rx (and bol "  " (1+ alnum) ":" (1+ space)
                    (or (and (1+ digit) ":" (1+ digit) (1+ "."))
                        (and (1+ (or alnum space ".")) ":")
                        (and "(" (1+ digit) "/" (1+ digit) "):"))
                    (1+ space)))))
      (let ((inhibit-read-only t)
            (beg (line-beginning-position))
            (end (point)))
        (put-text-property   beg end 'face 'org-ellipsis)))
    (goto-char (point-min))
    ;; Remove some useless stuff
    (while
        (ignore-errors
          (re-search-forward
           (rx (and bol "  " (1+ alnum) ":" (1+ space)
                    (and (1+ digit) ":" (1+ digit) (1+ "."))
                    (1+ space)
                    "Scheduled:"))))
      (when-let* ((inhibit-read-only t)
                  (end (point))
                  (beg (re-search-backward "\\.")))
        (delete-region beg end)))))

(add-hook 'org-agenda-finalize-hook #'gk-org-agenda-finalize-hook)



;;;; Exporting:



;;;;; Common:

;; Defaults for exporting from org mode.

(setq org-export-with-smart-quotes t
      org-export-with-sub-superscripts t
      org-export-dispatch-use-expert-ui t)

;; From: https://lists.gnu.org/archive/html/emacs-orgmode/2013-07/msg00731.html
(defun gk-org-export-comment-removal-hook (backend)
  "Remove comments from export.

Prevent comments inline in paragraphs from splitting them."
  (delete-matching-lines "^[ \t]*#\\( \\|$\\)"))

(add-hook 'org-export-before-parsing-hook
          'gk-org-export-comment-removal-hook)



;;;;; LaTeX->PDF:

(setf org-latex-compiler "lualatex"
      org-latex-pdf-process (list "latexmk -f -silent -bibtex-cond -lualatex %f")
      ;; Do remove many sorts of files the process generates...
      org-latex-remove-logfiles t
      ;; ...but keep some important log files.
      org-latex-logfiles-extensions
      (cl-remove-if ($ (member $1 (list "log" "blg")))
                    org-latex-logfiles-extensions))

;; Scale latex fragment previews up, 1.0 scale is barely legible.
(plist-put org-format-latex-options :scale 1.5)

(defun gk-org-pdf-subtree (arg)
  "Export subtree under point to pdf, show the output.

If called with  a universal prefix, org ARG is  ~'(1)~, output to
LaTeX buffer.  If region is active, narrows to region and exports
its contents."
  (interactive "p")
  (save-restriction
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (if (equal arg 1)
        (find-file (org-latex-export-to-pdf nil t))
      (org-latex-export-as-latex nil t))))

(setf org-latex-inactive-timestamp-format "%s")

(defun gk-ox-latex-format-inactive-timestamp (text backend info)
  (ignore info)
  (when (org-export-derived-backend-p backend 'latex)
    ;;; XXX  Doesn't work  as expected,  gives "30  November -001"  as
    ;;; result.
    ;; (org-timestamp-format text "%d %B %Y")
    (format-time-string
     ;; "%d \\monthname{%B} %Y"
     "\\formatdate{%d}{%m}{%Y}"
     (apply #'encode-time
            (org-parse-time-string
             text)))))

(add-to-list 'org-export-filter-timestamp-functions
             'gk-ox-latex-format-inactive-timestamp)



;;;; Refile:

(setf
 org-refile-targets
 `((,(gk-org-dir-file "Muhtelif.org") :maxlevel . 1))
 org-refile-use-outline-path 'file
 org-refile-allow-creating-parent-nodes t
 org-log-refile 'time
 ;; New notes to the beginning of the file.
 org-reverse-note-order t)

(defun gk-org-after-refile-insert-hook ()
  "Save target if user agrees."
  (ignore-errors
    (when (y-or-n-p
           (format "Save %s?"
                   (buffer-file-name
                    (current-buffer))))
      (save-buffer))))

(add-hook 'org-after-refile-insert-hook 'gk-org-after-refile-insert-hook)



;;;; Version control:

;; These following advices help keep Org files locked in RCS and edit via
;; agenda or other Org commands I frequently use seamlessly, so that they
;; don't pollute the vc buffer with unnecessary ‘edited’ bits.  Because
;; otherwise I have to keep them’t locked all the time.

;; TODO(2018-05-25): Fix repetition in Org RCS advices
;; Maybe add a macro/function =(gk-rcs-advice-for-unlock FUNCTION)= so
;; that:

;; (gk-rcs-advice-for-unlock #'org-todo)

(define-advice org-paste-subtree (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (gk-rcs-maybe-unlock f)))

;; Like above but for capturing.

(define-advice org-capture-fill-template (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let* ((buffer (org-capture-get :buffer))
         (file (buffer-file-name buffer)))
    (gk-rcs-maybe-unlock file)))

;; Like above but for ‘org-todo’.

(define-advice org-todo (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (gk-rcs-maybe-unlock f)))

;; For clocking.

(define-advice org-clock-in (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (message f)
    (gk-rcs-maybe-unlock f)))

(define-advice org-clock-out (:before (&rest args) check-rcs-lock)
  "Check to see if this file is controlled by RCS and is

unlocked, offer to lock it before pasting."
  (ignore args)
  (let ((f (buffer-file-name)))
    (message f)
    (gk-rcs-maybe-unlock f)))



;;;; Structure:

(setf
 ;; Do not indent entry content.
 org-adapt-indentation nil
 ;; Allow alphabetical list elements and those that start with * above
 ;; initial level.
 org-list-allow-alphabetical t)

;; Required after setting ‘org-list-allow-alphabetical’.
(org-element-update-syntax)




;;;;; Automatic insertion of blank lines between list items:
;; Org uses a complex heuristic to determine whether or not to insert
;; blank lines between list items.  I instead want to have a strict
;; style.  The advices below replace this heuristic with a regular
;; system:
;;
;; - Top level items are separated by a blank line.
;; - Non-top-level items are not separated.
;;
;; Besides, I advice list item indent/dedent functions to add a blank
;; line if dedenting to toplevel or remove it if indenting from
;; toplevel.

(define-advice org-list-separating-blank-lines-number
    (:override (pos struct prevs) do-it-my-way)
  "One line separates top level list items and none for other levels."
  (if (zerop (org-list-get-ind (point-at-bol) struct))
      1
    0))


(defun gk-org-after-indent-outdent-item ()
  "Insert/remove separating line like if ‘org-insert-item’ would."
  (let* ((struct (org-list-struct))
         (prevs  (org-list-prevs-alist struct))
         (p      (point))
         (nblank (org-list-separating-blank-lines-number p struct prevs))
         ;; Is the previous line blank?
         (blankp (save-excursion
                   (forward-line -1)
                   (zerop (- (line-end-position) (line-beginning-position))))))
    (cond ((and (zerop nblank) blankp)
           (save-excursion (goto-char (line-beginning-position))
                           (delete-char -1)))
          ((and (/= 0 nblank) (not blankp))
           (save-excursion (goto-char (line-beginning-position))
                           (open-line 1))))))

(add-function :after (symbol-function 'org-indent-item) #'gk-org-after-indent-outdent-item
              '((name . fix-separator-line)))
(add-function :after (symbol-function 'org-outdent-item) #'gk-org-after-indent-outdent-item
              '((name . fix-separator-line)))




;;;; Translating:

(defun gk-org-trans-show-paragraph ()
  "Show the ASCII export of current paragraph in a side window."
  (interactive)
  (let* ((org-export-show-temporary-export-buffer nil)
         (beg (save-excursion
                (save-match-data
                  (re-search-backward (make-string 2 ?\C-j)))))
         (end (save-excursion
                (save-match-data
                  (re-search-forward (make-string 2 ?\C-j)))))
         buf)
    (save-restriction
      (narrow-to-region beg end)
      (setq buf (org-ascii-export-as-ascii nil nil t))
      (with-current-buffer buf
        (save-excursion
          (save-match-data
            (push-mark (point-min) nil t)
            (re-search-forward (make-string 5 ?\C-j))
            (delete-region (region-beginning) (region-end))
            (deactivate-mark t)
            (text-mode))))
      (display-buffer-in-side-window buf '((side . bottom))))))

(defvar gk-org-trans--pageno-hist nil)
(defvar gk-org-trans--parno-hist nil)
(defvar gk-org-trans--sentenceno-hist nil)

(defun gk-org-trans-insert-sentence-reference (page paragraph sentence)
  "Insert sentence number.

PAGE is the page number.

PARAGRAPH is the paragraph number where the if a paragraph
continues to the page from the last one that paragraph is
counted as the first.

SENTENCE is the sentence number, counted similarly to
paragraphs.

Interactively, prompts for these numbers.  These numbers should
be represented as strings, in order to account for non-Arabic
numerals which regularly appear in texts."
  (interactive
   (list (read-string (format "Page # [default: %S]: "
                              (car gk-org-trans--pageno-hist))
                      nil 'gk-org-trans--pageno-hist
                      (car gk-org-trans--pageno-hist))
         (read-string (format "Paragrap # [default: %S]: "
                              (car gk-org-trans--parno-hist))
                      nil 'gk-org-trans--parno-hist
                      (car gk-org-trans--parno-hist))
         (read-string (format "Sentence # [default: %S]: "
                              (car gk-org-trans--sentenceno-hist))
                      nil 'gk-org-trans--sentenceno-hist
                      (car gk-org-trans--sentenceno-hist))))
  (insert "<<" page "." paragraph "." sentence ">> "))



;;;; Store link:

;; ‘org-store-link’ by default works in misterious ways, collecting
;; links eagerly, fucking up everything.  The below setup makes sure
;; only one link is ever stored in an ‘org-mode’ file, and that link
;; is the sanest option in the context, at least as far as my
;; preferences are concerned.
(setf
 ;; Store ‘org-id’ locations within ‘org-directory’ (so that it’s
 ;; VCS’ed), and use paths relative to ‘org-id-locations-file’ itself
 ;; when saving.
 org-id-locations-file (gk-org-dir-file ".id-locations")
 org-id-locations-file-relative t
 ;; A saner override to how ‘org-store-link’ records context.
 org-create-file-search-functions
 (list
  ;; 1. if region is active, use that.
  ($ (when (and (eq major-mode 'org-mode)
                (region-active-p))
       (buffer-substring-no-properties
        (region-beginning) (region-end))))
  ;; 2. if we’re on a target, use that.
  ($ (when (eq major-mode 'org-mode)
       (org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
       (match-string 1)))
  ;; 3. If CUSTOM_ID available, use that
  ($ (when-let* ((_ (eq major-mode 'org-mode))
                 (id (org-entry-get nil "CUSTOM_ID")))
       id))
  ;; HACK: 4. If before first heading, return empty string.  This
  ;; overrides the behaviour of ‘org-id’ which is to add a toplevel
  ;; :PROPERTIES:  drawer.
  ($ (unless (and (eq major-mode 'org-mode)
                  (ignore-errors (org-heading-components)))
       ""))))


(define-advice  org-store-link (:around (fn &rest args) fuck-pdf-version-cruft)
  "Disable context in PDFs and other non-text documents"
  (let ((org-context-in-file-links
         (and org-context-in-file-links
              (not (member major-mode
                           '(pdf-view-mode doc-view-mode))))))
    (apply fn args)))



;;;; Visuals:

;; Show sub/superscript notation with ^ _ w/ UTF8 characters by
;; default. C-c C-x \ toggles.
(setq-default org-pretty-entities t)

(when (gk-gui-p)
  (setf
   ;; If non-nil, Do not show emphasis markers //, __ etc.
   org-hide-emphasis-markers nil
   ;; Fontify all the heading line.
   org-fontify-whole-heading-line t))

;; Make today stand out more.
(set-face-attribute 'org-agenda-date-today nil :inverse-video t)

;; Make blocked todo items legible.
(set-face-attribute 'org-agenda-dimmed-todo-face nil
                    :foreground nil :slant 'italic)

;; Make some links look more distinct.
(org-link-set-parameters
 "file"
 :face '(:inherit font-lock-string-face :underline t))

(org-link-set-parameters
 "elisp"
 :face '(:inherit font-lock-keyword-face :underline t))

(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-ellipsis nil
                                :underline nil :height 1.0 :bold nil
                                :inherit 'font-lock-builtin-face)))

(setf
 org-num-face 'org-verbatim
 org-ellipsis " ◢")



;;;;; Variable pitch:

;; OVP minor mode allows to effectively use variable pitch fonts to
;; represent text in Org mode, while retaining monospace where necessary,
;; i.e. for source code, verbatim text, and structure and indentation.

(pushnew 'org-footnote org-variable-pitch-fixed-faces)
(pushnew 'highlight-indent-guides-even-face org-variable-pitch-fixed-faces)
(pushnew 'highlight-indent-guides-odd-face org-variable-pitch-fixed-faces)
(pushnew 'org-link org-variable-pitch-fixed-faces)

(defun gk-ovp-hook ()
  "Hook for ‘org-variable-pitch-minor-mode’."
  (setq-local cursor-type 'bar))

(add-hook 'org-variable-pitch-minor-mode-hook #'gk-ovp-hook)

(diminish 'org-variable-pitch-minor-mode "~")

;; (add-hook 'after-init-hook #'org-variable-pitch-setup)



;;;;; The hook:

(defun gk-org-visuals-hook ()
  "Set up how an Org buffer look."
  ;; This for some reason loves to have its own custom font setup for
  ;; some reason.  You cheeky little bastard, you do as I say alright.
  (set-face-attribute 'org-table nil :font
                      (face-attribute 'default :font nil t))
  (set-face-attribute 'org-footnote nil :underline nil))

(add-hook 'org-mode-hook 'gk-org-visuals-hook)



;;;;; LaTeX previews:

(setf
 ;; Single global location for latex-previews, don’t pollute
 ;; dirs. BEWARE the trailing ‘/’ is important!!!
 org-preview-latex-image-directory
 (expand-file-name "~/.org-latex-previews/"))

(define-advice org-format-latex
    (:around (fn &rest args) take-theme-into-account)
  "Adapt LaTeX previews to current theme.

The LaTeX preview image file names are generated using a hash
that takes various variables into account, including
‘org-format-latex-header’.  This advice prepends the value of
that variable, a string that contains some LaTeX prelude for
generating images from fragments, with the list of currently
active themes, thus allowing the currently active theme(s) to
influence which images are picked.  Thus, after the theme
changes, there’s no need to manually regenerate these images.
You can just run \\[org-mode] in the buffer after switching the
theme.  If necessary, new images will be created."
  (let ((org-format-latex-header
         (concat
          (format
           "%%%% Enabled themes: %S\n\n\n\n"
           custom-enabled-themes)
          org-format-latex-header)))
    (apply fn args)))



;;;; Custom links:



;;;;; Project links:

(org-link-set-parameters
   "gk-project"
   :follow 'gk-org-project-link-follow)

(defun gk-org-project-link-follow (path arg)
  (gk-open-project path arg))



;;;;; Gemini and Gopher links:

(dolist (proto (list "gopher" "gemini"))
  (org-link-set-parameters
   proto
   :follow 'gk-org-elpher-follow
   :store  'gk-org-elpher-store))

(defun gk-org-elpher-follow (path arg)
  (ignore arg)
  (elpher-go path))

(defun gk-org-elpher-store ()
  (when (and (eq major-mode 'elpher-mode))
    (let ((proto (elpher-address-protocol (cadr elpher-current-page))))
      (org-link-store-props
       :type proto
       :link (elpher-address-to-url (cadr elpher-current-page))
       :description (car elpher-current-page))
      t)))



;;;;; Annotations:

;; Annotations are for making unobstrusive comments on parts of
;; documents.  Supports exporting to HTML and LaTeX.

(org-add-link-type
 "comment"
 (lambda (linkstring)
   (ignore linkstring)
   (let ((elm (org-element-context))
         (use-dialog-box nil))
     (when (y-or-n-p "Delete comment? ")
       (setf (buffer-substring
	      (org-element-property :begin elm)
	      (org-element-property :end elm))
	     (cond
	      ((org-element-property :contents-begin elm)
	       (buffer-substring
		(org-element-property :contents-begin elm)
		(org-element-property :contents-end elm)))
	      (t
	       ""))))))
 (lambda (keyword desc format)
   (cond
    ((eq format 'html)
     (format
      "<abbr title=\"%s\" color=\"red\">%s</abbr>"
      keyword
      (or desc "COMMENT")))
    ((eq format 'latex)
     (format "\\todo[inline]{%s}{%s}" keyword (or desc ""))))))

(defun gk-org-add-comment (begin end)
  "Add a comment link.

BEGIN and END are bounds of the region."
  (interactive "r")
  (if (region-active-p)
      (let ((selected-text (buffer-substring begin end)))
	(setf (buffer-substring begin end)
	      (format "[[comment:%s][%s]]"
		      (read-string "Comment: ") selected-text)))
  (insert (format  "[[comment:%s]]" (read-string "Comment: ")))))



;;;;; OLP:

;; Follow the OLP on click.
(org-add-link-type
 "olp"
 (lambda (path)
   (let ((pathlist (split-string path "/" t)))
     (condition-case msg
         (progn
           (push-mark (point) t nil)
           (goto-char (org-find-olp pathlist t))
           (org-show-context))
       (error (nth 1 msg))))))



;;;;; Mairix:

(gk-dissoc! "rmail" org-link-parameters #'string=)

(org-link-set-parameters
   "mairix"
   :follow 'gk-org-mairix-open
   :store  'gk-org-mairix-store)

(defun gk-org-mairix-open (path arg)
  (mairix-search path arg))

(defun gk-org-mairix-store ()
  (when-let* ((_ (memq major-mode '(rmail-mode rmail-summary-mode)))
              (id (rmail-get-header "Message-ID"))
              (subj (or (rmail-get-header "Subject")
                        ""))
              (from (or (rmail-get-header "From")
                        "{Unknown}")))
    (org-store-link-props
     :type "mairix"
     :link (concat "mairix:m:" (string-trim id "<" ">"))
     :description (concat "Message from " from ": «" subj "»"))
    t))



;;;;; Man page:

(org-add-link-type
 "man"
 (lambda (path)
   (man (substring-no-properties path))))



;;;; Dynamic blocks:

(defun org-dblock-write:vcdirty (&rest args)
  "Update a vcdirty dynamic block.

Generates a "
  (let ((vcs-dirs (append
                   (list (expand-file-name "~/cf")
                         org-directory
                         (expand-file-name "~/Documents/not/www2")
                         (expand-file-name "~/.mozilla"))
                   (cl-remove-if
                    ($ (member $1 '("." ".." "Playground" "External" "Go" "Lisp"
                                    "Attic")))
                    (directory-files "~/co" t nil t))))
        (outbuf " gk-org-dynablock-cmd")
        (errbuf " gk-org-dynablock-cmd-err")
        dirty)
    (dolist (d vcs-dirs dirty)
      (cond
       ((file-exists-p (expand-file-name ".git" d))
        (when
            (zerop
             (let ((default-directory d))
               (shell-command "git status -s | egrep ." outbuf errbuf)))
          (push d dirty)))
       ((file-exists-p (expand-file-name ".hg" d))
        (when
            (zerop
             (let ((default-directory d))
               (shell-command "hg status | egrep ." outbuf errbuf)))
          (push d dirty)))))
    (when dirty
      (insert "Dirty repos:")
      (dolist (d dirty)
        (insert "\n[[elisp:(vc-dir \"" d "\")][" d "]]")))))



;;;; Source code:

;; Editing source code elements.

(setf
 ;; Don’t indent content’s of souce blocks.
 org-edit-src-content-indentation 0
 ;; Don't change the window layout when editing source blocks, open
 ;; them instead in the current window.
 org-src-window-setup 'current-window)



;;;; Babel:

(setf
 ;; Just do it™.
 org-confirm-babel-evaluate nil
 ;; Always wrap output in blocks.
 org-babel-min-lines-for-block-output 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (R . t)))

(setq org-babel-python-command "run-python.sh")

(defun gk-org-python-clean-spurious-indentation (body)
  (let* ((extra-indentation
	  (save-match-data
	    (string-match "\\`\\([ \t]+\\)" body)
	    (match-string 1 body)))
	 (xlen (length extra-indentation)))
    (if (zerop xlen)
	body
      (mapconcat
       (lambda (line) (if (<= (length line) xlen)
			  line
			(if (string= extra-indentation
				     (substring line 0 xlen))
			    (substring line xlen)
			  line)))
       (split-string body "\n")
       "\n"))))

(define-advice org-babel-execute:python
    (:filter-args (args) clean-spurious-indentation)
  (cons
   (gk-org-python-clean-spurious-indentation (car args))
   (cdr args)))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)



;;;; Auto-insert:

(push
 '((org-mode . "Org-mode document")
   nil
   "#+title: " - n
   "#+startup: overview")
 auto-insert-alist)

(push
 '(("okumalar/.+\\.org" . "Org-mode okuma notu")
   nil
   "#+title: "
   (let ((candidate (ignore-errors
                      (file-name-sans-extension
                       (file-name-nondirectory
                        (buffer-file-name))))))
     (read-string (format "Okunan metin için key (default: %s): "
                          candidate)
                  nil nil candidate))
   " okuma notları\n"
   "#+date: "
   (format-time-string "[%F %a]\n\n")
   "[ *Buraya full referans ekle* ]\n\n"
   "[ *Zotero’daki itemden bu dosyaya link ver* ]\n\n"
   "* Overview\n\n"
   "* Notlar\n\n"
   "* Alıntılar\n\n"
   "* Kaynakçadan\n\n")
 auto-insert-alist)



;;;; Capture:

(defun gk-org-capture-book ()
  "Capture template for a book in to-read list."
  (let ((kitap (read-string "Kitabın ismi: "))
        (yazar (read-string "yazarı: "))
        (yayınevi (read-string "yayınevi: "))
        (yılı (read-string "yayın yılı: ")))
    (when (string-empty-p kitap)
      (user-error "Kitap ismi girilmedi"))
    (concat "- [ ] " kitap
            (unless (string-empty-p yazar) (concat "; " yazar))
            (unless (string-empty-p yayınevi) (concat ".  " yayınevi))
            (unless (string-empty-p yılı) (concat " (" yılı ")"))
            ".")))


(defun gk-org-capture-annotated-photograph ()
  "Generate capture template for Fotoğraflar.org

Template generation relies on links being stored using ‘org-store’.

After completion removes used links from ‘org-stored-links’."
  (let ((template-head
         (concat "* %?\n"
                 "| author  | %^{Author|own work} |\n"
                 "| yer     | %^{Yer} |\n"
                 "| tarih   | %^{Tarih:}U |\n"))
        (selected-links
         (cl-remove-if-not
          ($ (y-or-n-p (format "Include %s" (car $1))))
          org-stored-links)))
    ;; Remove links selected for inclusion in the template from
    ;; ‘org-stored-links’.
    (setf org-stored-links (seq-difference org-stored-links selected-links))
    (concat template-head
            "\n"
            (mapconcat ($ (concat "- [[" (car $1) "][" (cadr $1) "]]"))
                       selected-links
                       "\n")
            "\n\n")))


(defun gk-org-define-capture-template (&rest args)
  "Define a capture template.

Creates a list and adds it to ‘org-capture-templates’.  ARGS is a
plist, which in addition to all the ‘additional options’
‘org-capture-templates’ accepts, takes the following and places
them accordingly: :keys, :description, :type, :target, :template;
which correspond to homonymous fields listed in
‘org-capture-template’s docstring (which see)."
  (setf org-capture-templates
        (append org-capture-templates
                (list
                 (append
                  (list (plist-get args :keys)
                        (plist-get args :description)
                        (plist-get args :type)
                        (plist-get args :target)
                        (plist-get args :template))
                  (cl-loop for i from 0 below (length args) by 2
                           unless
                           (member (nth i args)
                                   (list :keys :description
                                         :type :target :template))
                           append (list (nth i args)
                                        (plist-get
                                         args (nth i args)))))))))


(defun gk-org-capture-set-templates (&rest _)
  "Set up templates for org-capture.

Meant as a ‘:before’ advice to ‘org-capture’."
  (message "heylo")
  ;; Zero it out, the populate.
  (setf org-capture-templates nil)

  (gk-org-define-capture-template
   :keys "B"
   :description "Scholarly blurb"
   :type 'entry
   :target `(file ,(gk-org-dir-file "Studies/Blurb.org"))
   :template "* \n%u\n\n%?"
   :prepend t
   :empty-lines-before 1
   :empty-lines-after 1
   :unnarrowed nil)

  (gk-org-define-capture-template
   :keys "S"
   :description "Sunday Emacs build note"
   :type 'entry
   :target `(file+olp ,(gk-org-dir-file "Emacs.org")
                      "New stuff from recent builds")
   :prepend t
   :empty-lines-after 1
   :empty-lines-before 1
   :template (concat "* build on %u\n"
                     "- *version*: %^{Version|head}\n"
                     "- *toolkit*: %^{Toolkit|lucid}\n"
                     "-----\n\n\n"))

  (gk-org-define-capture-template
   :keys "s"
   :description "Emacs build note item"
   :type 'item
   :target `(file+function
             ,(gk-org-dir-file "emacs.org")
             (lambda ()
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (re-search-forward
                  (format-time-string "^\\*\\* build on \\[%F %a\\]"))
                 (re-search-forward "-----\n"))))
   :prepend nil
   :empty-lines-before 1
   :empty-lines-after 1
   :template
   ;; The sexp deactivates region just to save a couple keystrokes.
   "- %i%(with-current-buffer \"*Build Emacs*\" (deactivate-mark t))%?\n\n")

  (gk-org-define-capture-template
   :keys "F"
   :description "Annotated photograph(s) [store links to the photos first!]"
   :type 'entry
   :target `(file ,(gk-org-dir-file "Fotoğraflar.org"))
   :prepend nil
   :empty-lines-before 1
   :template '(function gk-org-capture-annotated-photograph))

  ;; ‘org-protocol’ templates
  (push '("P" "org-protocol templates") org-capture-templates)
  (gk-org-define-capture-template
   :keys "Pb"
   :description "New internet bookmark"
   :type 'entry
   :target `(file ,(gk-org-dir-file (format-time-string "bookmarks-%Y.org")))
   :template "* %:description\n%U\n\n%:link\n\n%?%i"
   :prepend t
   :empty-lines-before 1
   :empty-lines-after 1
   :unnarrowed nil)
  (gk-org-define-capture-template
   :keys "PB"
   :description "New internet bookmark (no edit)"
   :type 'entry
   :target `(file ,(gk-org-dir-file (format-time-string "bookmarks-%Y.org")))
   :template "* %:description\n%U\n\n%:link\n\n%?%i"
   :prepend t
   :empty-lines-before 1
   :empty-lines-after 1
   :immediate-finish t))


(add-function :before (symbol-function 'org-capture) #'gk-org-capture-set-templates)




;;;; Attachments:

(setf
 ;; Use relative paths for attachment links.
 org-attach-dir-relative t)

(defun gk-org-attach-screenshot ()
  "Little wrapper around ‘org-attach-screenshot’.

Don’t hide the frame and don’t ask me shit."
  (interactive)
  (org-attach-screenshot
   (list 4)
   (format-time-string "screenshot-%Y%m%d-%H%M%S.png")))




;;;; org-zotxt:

(setf
 ;; Sane defaults.
 org-zotxt-default-search-method :title-creator-year
 org-zotxt-link-description-style :betterbibtexkey)


(define-advice org-zotxt-insert-reference-links-to-items
    (:override (items) dont-fucking-insert-new-fucking-lines-ffs)
  "Insert links to Zotero ITEMS in buffer, without fucking everything up."
  (mapc (lambda (item)
          (org-zotxt-insert-reference-link-to-item item)
          (insert ", "))
        items)
  (delete-char -2))




;;;; Icalendar:

;; Do not sync deadlines and schedules.
(setf org-icalendar-use-scheduled nil
      org-icalendar-use-deadline  nil)



;;;; Habits:

(define-advice org-habit-build-graph
    (:filter-return (ret) dashes-not-spaces)
  "Invert colours, more readable like that.

I.e., foreground is coloured, instead of the background."
  (cl-loop
   for i from 0 to (1- (length ret))
   with replacement = '()
   do (let ((face (get-text-property i 'face ret))
            (hecho (get-text-property i 'help-echo ret))
            (char (substring ret i (1+ i))))
        (ignore-errors
          (set-face-attribute
           face nil
           :inverse-video t
           :foreground (face-attribute 'default :background)
           :bold t))
        (push
         (propertize
          (if (string= char " ") "-" char)
          'face face 'help-echo hecho)
         replacement))
   finally return (mapconcat #'identity (reverse replacement) "")))



;;;; Dynamic previews for LaTeX fragments:

;; Adapted from https://gist.github.com/cvcore/760008a4dfb2eadf42afdc9cf01ef979

(defvar-local gk-org-last-fragment nil
  "Holds the type and position of last valid fragment we were on.
Format: (FRAGMENT_TYPE FRAGMENT_POINT_BEGIN)")

(defvar gk-org-valid-fragment-type
  '(latex-fragment latex-environment link))

(defun gk-org-curr-fragment ()
  "Returns the type and position of the current fragment.

Returns the type and position of the current fragment available
for preview inside org-mode. Returns nil at non-displayable
fragments"
  (let* ((fr (org-element-context))
         (fr-type (car fr)))
    (when (memq fr-type gk-org-valid-fragment-type)
      (list fr-type
            (org-element-property :begin fr)))))

(defun gk-org--list-latex-overlays (&optional beg end)
  (cl-remove-if-not
   (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
   (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun gk-org-remove-fragment-overlay (fr)
  "Remove fragment overlay at FR."
  (let ((fr-type (nth 0 fr))
        (fr-begin (nth 1 fr)))
    (goto-char fr-begin)
    (cond ((or (eq 'latex-fragment fr-type)
               (eq 'latex-environment fr-type))
           (let ((ov (loop for ov in (gk-org--list-latex-overlays)
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov))))
          ((eq 'link fr-type) nil))))

(defun gk-org-preview-fragment (fr)
  "Preview org fragment at fr"
  (let ((fr-type (nth 0 fr))
        (fr-begin (nth 1 fr)))
    (goto-char fr-begin)
    (cond ((or (eq 'latex-fragment fr-type) ;; latex stuffs
               (eq 'latex-environment fr-type))
           ;; Only toggle preview when we're in a valid region (for
           ;; inserting in the front of a fragment).
           (when (gk-org-curr-fragment)
             (org-preview-latex-fragment)))
          ((eq 'link fr-type) ;; for images
           (let ((fr-end (org-element-property :end (org-element-context))))
             (org-display-inline-images nil t fr-begin fr-end))))))

(defun gk-org-auto-toggle-fragment-display ()
  "Automatically toggle a displayable org mode fragment"
  (and (eq 'org-mode major-mode)
       (let ((curr (gk-org-curr-fragment)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            gk-org-last-fragment
            ;; and are on a fragment now
            curr
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (equal curr gk-org-last-fragment)))

           ;; go back to last one and put image back, provided there
           ;; is still a fragment there
           (save-excursion
             (gk-org-preview-fragment gk-org-last-fragment)
             ;; now remove current image
             (gk-org-remove-fragment-overlay curr))
           ;; and save new fragment (?)
           (setq gk-org-last-fragment curr))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not curr)
            ;; but we were on one
            gk-org-last-fragment)
           ;; put image back on, provided that there is still a
           ;; fragment here.
           (save-excursion
             (gk-org-preview-fragment gk-org-last-fragment))

           ;; unset last fragment
           (setq gk-org-last-fragment nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not gk-org-last-fragment)
            ;; but now we are
            curr)
           ;; remove image
           (save-excursion
             (gk-org-remove-fragment-overlay curr)
             )
           (setq gk-org-last-fragment curr))))))




;;;; Links & files:

(setf
 ;; Open file: links in other window.
 (cdr (assoc 'file org-link-frame-setup)) 'find-file-other-window)



;;;; Keybindings:

;; Disable confusing bindings
(org-defkey org-mode-map (kbd "C-c C-x C-c") nil)

(org-defkey org-mode-map (kbd "C-M-<return>") 'org-insert-subheading)
(org-defkey org-mode-map (kbd "C-c @") 'ebib-insert-citation)

;; Heading navigation
(org-defkey org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
(org-defkey org-mode-map (kbd "M-n") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-M-n") #'gk-org-forward-content)
(define-key org-mode-map (kbd "C-M-p") #'gk-org-backward-content)
(define-key org-mode-map (kbd "C-S-n") #'org-next-item)
(define-key org-mode-map (kbd "C-S-p") #'org-previous-item)

;; Paragraphs
(define-key org-mode-map [remap backward-paragraph] nil)
(define-key org-mode-map [remap forward-paragraph] nil)
(define-key org-mode-map (kbd "C-c q") #'gk-org-refill-reading-note)
(define-key org-mode-map (kbd "C-c C-# n") #'gk-org-insert-reading-note)
(define-key org-mode-map (kbd "C-c C-# b") #'gk-org-insert-reading-bibliograpy-note)

;; Reading notes
(define-key org-mode-map (kbd "C-c M-r") #'gk-org-insert-reading-note)
(define-key org-mode-map (kbd "C-c M-e") #'gk-org-reading-notes-ellipsise-last-note)
(define-key org-mode-map (kbd "C-c M-b") #'gk-org-insert-reading-bibliograpy-note)
(define-key org-mode-map (kbd "C-c M-m") #'gk-org-reading-note-merge-last-n-notes)

;; Attachments
(define-key org-mode-map (kbd "C-c C-M-s") #'gk-org-attach-screenshot)
(define-key org-mode-map (kbd "<f2>") #'gk-org-attach-screenshot)

;; Misc
(define-key org-mode-map (kbd "C-c C-M-l") #'gk-org-insert-all-stored-links)
(define-key org-mode-map (kbd "C-c p") #'org-mark-ring-goto)

;; Prefix keymap for global bindings.
(defvar gk-org-mode-bindings
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'org-store-link)
    (define-key map "c" 'org-capture)
    (define-key map "a" 'org-agenda)
    map)
  "Keymap for ‘org-mode’-specific bindings.")

(gk-prefix-binding "o" gk-org-mode-bindings)
(gk-prefix-binding "c" #'org-capture)
(gk-prefix-binding "a" #'gk-org-agenda)



;;;; Hook:

;; I’ll activate it myself on a buffer basis, thx.
(remove-hook 'org-mode-hook 'org-eldoc-load)

(defun gk-org-hook ()
  "Default hook for `org-mode' buffers."
  (setq-local default-justification 'left)
  (auto-fill-mode 1)
  ;; Disable in org mode
  (when (boundp goto-address-mode)
    (goto-address-mode -1))
  (org-zotxt-mode +1)
  (org-num-mode +1)
  (imenu-add-to-menubar "Entries")
  (org-eldoc-load)
  (eldoc-mode)
  (setq-local truncate-lines nil)
  (setq-local indent-tabs-mode nil)
  (setq-local post-command-hook (cons #'gk-org-auto-toggle-fragment-display
                                      post-command-hook)))

(add-hook 'org-mode-hook 'gk-org-hook)



;;;; Private settings:

(gk-load (gk-org-dir-file "settings") t)



(provide 'gk-org)
;;; gk-org.el ends here
