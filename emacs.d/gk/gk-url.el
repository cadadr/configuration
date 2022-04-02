;;; gk-url.el --- urls, web browsing                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;;; Commentary:

;; Streamline URL handling and configure modes that help browse the
;; web.

;;; Code:

(require 'browse-url)
(require 'elpher)
(require 'eww)
(require 'shr)


;;;; URLs:

;; This is my URL browsing system, which is a big customisation of the
;; Emacs browse-url system which modifies some sorts of URLs for
;; better browsing experience, uses apt Emacs modes to display some
;; files instead of the browser, and prompts whether or not to open
;; URLs in EWW or not.



;;;;; Common:

;; TODO Check if still relevant when switch to Emacs 25.
;; Replacement for odd standard implementation.
;; See: http://emacshorrors.com/posts/computer-says-no.html
(defun browse-url-can-use-xdg-open ()
  "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser."
  (and window-system (executable-find "xdg-open")))

(define-error 'gk-no-external-browser
  "‘gk-urls-external-browser’ could not find a suitable external browser")

(defun gk-urls-external-browser (&rest args)
  "Find a suitable browser and pass ARGS to it."
  (apply
   (cond ((browse-url-can-use-xdg-open)
          #'browse-url-xdg-open)
         ((executable-find "firefox")
          #'browse-url-firefox)
         ((executable-find "chromium")
          #'browse-url-chromium)
         (t
          (signal 'gk-no-external-browser args)))
   args))

(defun gk-browse-url (&rest args)
  (condition-case err
      (apply #'gk-urls-external-browser args)
    ('no-external-browser
     (message "%s, using EWW" err)
     (apply #'eww-browse-url args))))

(setf browse-url-browser-function #'gk-browse-url
      browse-url-generic-program  #'gk-browse-url
      browse-url-firefox-program
      (or (gk-executable-ensure "firefox" t)
          (expand-file-name "~/Applications/firefox/firefox")))


(defvar browse-url-qutebrowser-program
  (or (executable-find "qutebrowser")
      (executable-find "~/local/_qutebrowser/bin/qutebrowser")))
(defvar browse-url-qutebrowser-arguments nil)

(defun browse-url-qutebrowser (url &optional new-window)
  "Ask Qutebrowser to load URL. "
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "qutebrowser " url) nil
           browse-url-qutebrowser-program
           (append
            browse-url-qutebrowser-arguments
            (when new-window
              '("--target=window"))
            (list url)))))



;;;;; Browser functions:

;; Functions specific for browsing some websites, and the browser
;; function.

(defun gk-urls-browse-github/gitlab-commit (url &rest args)
  "Browse a Github/Gitlab URL.

Append .diff to the url.  Pass ARGS on to ‘gk-browse-url’."
  (browse-url (concat url ".diff")))

(defun gk-urls-browse-github-file (url &rest args)
  "Browse a file on github.

Redirect to the raw file url."
  (let* ((rawprefix "https://raw.githubusercontent.com/")
         (bits (split-string
                (car (url-path-and-query (url-generic-parse-url url))) "/"))
         (rawurl
          (s-join "/" (cons (cadr bits) (cons (caddr bits) (cddddr bits))))))
    (browse-url (concat rawprefix rawurl))))

(defun gk-urls-browse-github-raw (url &rest args)
  "Browse a GitHub raw URL as an Emacs file."
  (gk-urls-browse-file
   (replace-regexp-in-string
    "\\.github\\.com/" ".githubusercontent.com/" url)))

(defun gk-urls-browse-mpv (url &rest args)
  "Browse a URL via mpv."
  (if (y-or-n-p "Watch with mpv?")
      (and
       (save-window-excursion
         (let* ((dir "~/co/External/youtube-dl"))
           (async-shell-command
            (format "PYTHONPATH=%s %s -o- '%s' | mpv -"
                    dir
                    (expand-file-name "bin/youtube-dl" dir)
                    url)
            (generate-new-buffer "*MPV*"))))
       (message "Started mpv process for: %s" url))
    (gk-urls-external-browser url)))

(defun gk-urls-with-elpher (url &rest args)
  "Visit an URL with Elpher."
  (elpher-go url))



;;;;; Download and open files with Emacs:

;; This mechanism here allows for downloading and opening files with
;; emacs where that makes sense.  See the section ‘File adapters’ for
;; the adapters.

;; To add a new adapter, simply: =(gk-urls-make-file-adapter "ext")=
;; where =ext= is the filename extension.

(defvar gk-urls-file-adapters nil)

(defun gk-urls-browse-file (url &optional ext cb)
  "Browse a file with the given extension.

URL is the URL to browse.
EXT is the extension, omit the leading dot.
CB is the optional callback, run after downloading the file,
given the path as the only argument.
Writes the data to a temporary file."
  (url-retrieve
   url (lambda (status &optional cbargs)
         (ignore cbargs)
         (unless (plist-get status :error)
           (let ((fil  (make-temp-file
                        (concat "gkbrowse-" ext)
                        nil
                        (when ext
                          (concat "." ext)))))
             (write-region
              ;; Two consequtive newlines delimit the headers section.
              (save-excursion
                (goto-char (point-min))
                (re-search-forward "\n\n") (point))
              (point-max) fil)
             (kill-buffer)
             (when cb (funcall cb fil))
             (find-file fil))))))

;; TODO(2018-05-25): Make this support regexps as EXT.
(defmacro gk-urls-make-file-adapter (ext &optional arg &rest body)
  "Create adapters for `gk-urls-browse-file'.

ARG and BODY are used to make a callback to that function, if both
provided."
  (declare (indent defun))
  (when (string= ext "file")
    ;; It would override `gk-urls-browse-file'.
    (error
     "`file' can't be an extension for `gk-urls-make-file-adapter'"))
  (let ((funsym (intern (concat "gk-urls-browse-file--" ext)))
        ;; Make case insensitive match for extension.
        (reg (concat
              "\\."
              (let* (ret
                     (bits (reverse
                            (dolist (ch (string-to-list ext) ret)
                              (push
                               (let* ((ch1 (char-to-string ch))
                                      (ch2 (upcase ch1)))
                                 (concat "[" ch2 ch1 "]"))
                               ret)))))
                (mapconcat 'identity bits ""))
              "/?$")))
    `(progn
       (pushnew
        '(,reg . ,funsym)
        gk-urls-file-adapters
        :test 'equal)
       (defun ,funsym (url &rest args)
         (ignore args)
         ,(concat (upcase ext) " adapter for `gk-urls-browse-file'.")
         (gk-urls-browse-file
          url ,ext ,(when (and arg body)
                      `(lambda (,arg) ,@body)))))))



;;;;; File adapters:

(gk-urls-make-file-adapter "pdf")
(gk-urls-make-file-adapter "jpeg")
(gk-urls-make-file-adapter "jpg")
(gk-urls-make-file-adapter "png")
(gk-urls-make-file-adapter "gif")
(gk-urls-make-file-adapter "patch")
(gk-urls-make-file-adapter "diff")
(gk-urls-make-file-adapter "txt")
(gk-urls-make-file-adapter "md")
(gk-urls-make-file-adapter "tex")
;;(gk-urls-make-file-adapter "c\\(c\\|pp\\|++\\|xx\\)?")
;;(gk-urls-make-file-adapter "h\\(h\\|pp\\|++\\|xx\\)?")
(gk-urls-make-file-adapter "el")
(gk-urls-make-file-adapter "scm")
(gk-urls-make-file-adapter "lisp")
(gk-urls-make-file-adapter "py")
(gk-urls-make-file-adapter "rb")
;;(gk-urls-make-file-adapter "p[lm]?6?")



;;;;; Set browse-url handlers:

(setf browse-url-handlers
      `(("\\(youtube\\.com\\|youtu\\.be\\)/" . gk-urls-browse-mpv)
        ("invidio\\.us/" . gk-urls-browse-mpv)
        ("^https?://\\(github\\|gitlab\\).com/.*?/.*?/\\(commit\\|compare\\)/[a-z0-9]+$" .
         gk-urls-browse-github/gitlab-commit)
        ("^https?://github\\.com/.*?/.*?/blob/" . gk-urls-browse-github-file)
        ("^https?://raw\\.github\\.com/" . gk-urls-browse-github-raw)
        ("file:///home/.+/co/lisp/doc/HyperSpec/" . gk-browse-url)
        ("^\\(gemini\\|gopher\\)://" . gk-urls-with-elpher)
        ,@gk-urls-file-adapters))



;;;; SHR:

(setf shr-use-colors nil)



;;;; EWW:

(setf eww-search-prefix "https://duckduckgo.com/html/?q=")

(defun gk-eww-download ()
  "Download URL under point."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url))
        (filename))
    (if (not url)
        (message "No URL under point")
      (setq filename
            (read-file-name "Download to: "
                            eww-download-directory
                            nil nil
                            (url-file-nondirectory url)))
      (url-retrieve url
                    'gk-eww-download-callback
                    `(,url ,filename)))))

(defun gk-eww-download-callback (status url filename)
  (ignore url)
  (unless (plist-get status :error)
    (let* ((file (eww-make-unique-file-name filename "")))
      (write-file file)
      (message "Saved %s" file))))

;; Use my version of /eww-download/ for allowing the user to set the
;; target file.
(defalias 'eww-download #'gk-eww-download)

(defun gk-eww-save-link-as-kill (point)
  (interactive "d")
  (if-let* ((uri (get-text-property point 'shr-url)))
      (prog1 (kill-new uri)
        (message "Saved ‘%s’ to kill-ring" uri))
    (user-error "No URL under point")))

(defun gk-eww-up ()
  "Remove last directory or file part from the URI, go there."
  (interactive)
  (eww (replace-regexp-in-string "^\\([a-z]+:/+.+\\)/[^/]+/?$" "\\1/"
                                 (eww-current-url))))

(define-key eww-mode-map "^" 'gk-eww-up)
(define-key eww-mode-map "k" 'gk-eww-save-link-as-kill)

(defun gk-eww-mode-hook ()
  "Set up `eww' for easier reading."
  )

(add-hook 'eww-mode-hook 'gk-eww-mode-hook)




;;;; Elpher:

(setf
 ;; Move bookmarks file to a private location.
 elpher-bookmarks-file (expand-file-name "~/Documents/elpher-bookmarks.el"))

;; Some more emacsy keybindings.
(define-key elpher-mode-map "n" #'elpher-next-link)
(define-key elpher-mode-map "p" #'elpher-prev-link)
(define-key elpher-mode-map "l" #'elpher-back)
(define-key elpher-mode-map "^" #'elpher-back-to-start)



(provide 'gk-url)
;;; gk-url.el ends here
