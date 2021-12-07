;;; gk-obsolete.el --- obsolete lisp from my config  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Obsolete bits of my config, kept here for easy access when need be.

;;; Code:

;; Interact with plain-text footnotes.  These are bound to keys and
;; mouse clicks later on in this file.

(defun gk-find-text-footnote-definition ()
  (interactive)
  (when (looking-at "[[(]?\\([0-9*]+\\)[\])]?")
    (push-mark (point))
    (goto-char (point-max))
    (re-search-backward (concat "^" (match-string 1) "[^1234567890]"))))

(make-obsolete 'gk-find-text-footnote-definition nil "2021-12-07")

(defun gk-find-text-footnote-definition--mouse (&optional event)
  "Find footnote definition according to plain text conventions."
  (interactive "@e")
  (when event (goto-char (cadadr event)))
  (gk-find-text-footnote-definition))

(make-obsolete 'gk-find-text-footnote-definition--mouse nil "2021-12-07")

(defun gk-serve-directory (&optional dir port)
  (interactive (list (read-directory-name "Directory to serve: "
                                          default-directory)
                     (read-number "Port: " 8000)))
  (let ((default-directory dir))
    (async-shell-command (format "python2 -m SimpleHTTPServer %d"
                                 port))))

(make-obsolete 'gk-serve-directory nil "2021-12-07")

(defun gk-sudo (cmd)
  "Run CMD as superuser."
  (interactive (list (read-shell-command "Shell command (sudo): ")))
  (with-temp-buffer
    (cd (concat "/sudo::" (expand-file-name default-directory)))
    (prog1
        (shell-command cmd (current-buffer))
      (cd default-directory)
      (when (called-interactively-p 'any)
        ;; The command output can include ‘%’ which may cause message
        ;; to signal error.
        (message "%s" (buffer-string))))))

(make-obsolete 'gk-sudo nil "2021-12-07")

(defalias 'dmesg
  (defun gk-dmesg (&optional lines)
    (interactive "P")
    (async-shell-command (format "dmesg | tail -n %d" (or lines 10)))))

(make-obsolete 'gk-dmesg nil "2021-12-07")
(make-obsolete 'dmesg nil "2021-12-07")

(defun gk-screen-brightness (n)
  "Set screen brightness to N tenths of max.

10 >= N >= 1."
  (interactive
   (list (read-number "Brightness interval [1--10]: " 5)))
  (unless (>= 10 n 1)
    (user-error "Brightness interval not in range 10 >= N >= 1"))
  (with-current-buffer
      (find-file-noselect
       "/sudo::/sys/class/backlight/intel_backlight/brightness")
    (erase-buffer)
    (insert
     (number-to-string
      (* n
         (/
          (string-to-number
           (with-temp-buffer
             (insert-file-contents
              "/sys/class/backlight/intel_backlight/max_brightness")
             (buffer-string)))
          10))))
    (save-buffer)))

(make-obsolete 'gk-screen-brightness nil "2021-12-07")

(defmacro gk-make-thing-marker (thing)
  (let ((thingname (symbol-name thing)))
    `(defun ,(intern (concat "gk-mark-" thingname)) ()
       ,(concat "Mark the " thingname " under cursor.")
       (interactive)
       (let ((b (bounds-of-thing-at-point (quote ,thing))))
         (set-mark (point))
         (goto-char (car b))
         (push-mark (cdr b) t t)))))

(make-obsolete 'gk-make-thing-marker nil "2021-12-07")

(defvar gk-things '(list sexp defun filename url email word paragraph
                         sentence whitespace line page symbol)
  "A list of known things")

(make-obsolete 'gk-things nil "2021-12-07")

;; (dolist (thing gk-things)
;;   (eval `(gk-make-thing-marker ,thing)))

(defun gk-mark-thing ()
  "Interactively find some THING to mark."
  (interactive)
  (funcall
   (intern
    (concat
     "gk-mark-"
     (completing-read
      "What to mark (hit TAB to complete): "
      (mapcar #'symbol-name gk-things)
      nil t)))))

(make-obsolete 'gk-mark-thing nil "2021-12-07")

(defvar gk-reading-modes
  '(doc-view-mode pdf-view-mode eww-mode)
  "Modes more likely to be used for reading documents.

Used by ‘gk-reading-setup’ in order to select a buffer that
contains a document to be read in a smart manner so that there is
no need to switch windows unnecessarily.")


(defvar gk-reading-notes-file
  (expand-file-name "~/Notes/Reading.org")
  "Default reading notes file.

Used in ‘gk-reading-setup’.")


(defun gk-reading-setup ()
  "Put windows into a reading setup.

Try to find a potential window containing a document to be read
\(see ‘gk-reading-modes’), give it a big window, and open
‘gk-reading-notes-file’ in a smaller window below it."
  (interactive)
  ;; Find a suitable window which probably contains the document I
  ;; want to read. If not found, the current window will be used.
  (pcase (cl-remove-if-not
          ($ (memq (buffer-local-value 'major-mode $1)
                   gk-reading-modes))
          (mapcar #'window-buffer (window-list)))
    ((or `(,buffer)
         `(,buffer . ,buffers))
     (select-window
      (display-buffer-reuse-window buffer '((reusable-frames . nil))))))
  (delete-other-windows)
  (display-buffer-below-selected
   (find-file-noselect gk-reading-notes-file)
   `((window-height . ,(/ (window-height) 3))))
  ;; XXX(2020-04-02): if point remains in the ‘pdf-view-mode’ window,
  ;; ‘pdf-view-mode’ behaves funny. It can be remedied via calling
  ;;
  ;;   (other-window 1)
  ;;   (redisplay t)
  ;;   (other-window 1)
  ;;
  ;; but having focus on ‘gk-reading-notes-file’ is both simpler and
  ;; kinda more logical (if reading a new document I’d probably set up
  ;; the notes buffer first.
  (other-window 1))

(make-obsolete 'gk-reading-notes-file nil "2021-12-07")
(make-obsolete 'gk-reading-modes nil "2021-12-07")
(make-obsolete 'gk-reading-setup nil "2021-12-07")

;; Adapted from: https://www.reddit.com/r/emacs/comments/idz35e/g2c2c6y/
(defvar gk-save-screenshot-dir
  (expand-file-name
   "emacs-screenshots"
   (if (fboundp 'xdg-user-dir)
       (xdg-user-dir "PICTURES")
     (expand-file-name "~/Pictures")))
  "Where to save screenshots.")


(defvar gk-save-screenshot-default-output-file-name-template
  "%F%T%z.png"
  "Default basename template for ‘gk-save-screenshot’.

This string is passed to ‘format-time-string’ and then
concatenated to ‘gk-save-screenshot-dir’ using
‘expand-file-name’.")


(defun gk-save-screenshot (output-file)
  "Save a screenshot of the selected frame as an SVG image.

Save the output to OUTPUT-FILE. When called interactively, this
is read from the minibuffer, and a default value using the date
and time is provided.

Output file type is inferred from OUTPUT-FILE’s extension, which
must be one of ‘svg’, ‘pdf’, ‘ps’, ‘png’.

The default file path is constructed using
‘gk-save-screenshot-dir’ to determine the directory to save and
‘gk-save-screenshot-default-output-file-name-template’ to
generate a default file name."
  (interactive
   (list (read-file-name
          "Screenshot file name: "
          gk-save-screenshot-dir
          nil nil
          (format-time-string
           gk-save-screenshot-default-output-file-name-template))))
  ;; deps check
  (unless (fboundp 'x-export-frames)
    (user-error
     "This function depends on `x-export-frames’ which not available"))
  ;; ensure output directory
  (unless (file-directory-p gk-save-screenshot-dir)
    (make-directory gk-save-screenshot-dir t))
  ;; guess output file type
  (let ((type (downcase (file-name-extension output-file)))
        data)
    (setq type
          (cond ((string= type "svg") 'svg)
                ((string= type "pdf") 'pdf)
                ((string= type "ps")  'postscript)
                ((string= type "png") 'png)
                (t
                 (user-error
                  "Output file’s extension should be one of svg, pdf, ps or png"
                  type))))
    ;; write data
    (setq data (x-export-frames nil type))
    (with-temp-file output-file
      (insert data))
    (kill-new output-file)
    (message output-file)))

(make-obsolete 'gk-save-screenshot nil "2021-12-07")
(make-obsolete 'gk-save-screenshot-default-output-file-name-template nil "2021-12-07")
(make-obsolete 'gk-save-screenshot-dir nil "2021-12-07")

(defvar gk-can-use-midi-kbd nil
  "If non-nil, a midi keyboard is available and can be used.")

;; (when-let* ((device (car (directory-files "/dev/snd" t "midi"))))
;;   (require 'midi-kbd)

;;   (midikbd-open device)

;;   (message
;;    "A MIDI keyboard is available at %s, so MIDI keybindings can be used!"
;;    device)

;;   (setf gk-can-use-midi-kbd t))

(make-obsolete 'gk-can-use-midi-kbd nil "2021-12-07")



(provide 'gk-obsolete)
;;; gk-obsolete.el ends here
