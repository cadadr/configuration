;;; ded-gnus.el --- gnus configuration put to sleep

;; The following is a copy of the latest state of my Gnus
;; configuration.  I do not use it, so I retired it here.  This is
;; different from the last committed version in that it's modified to
;; use Gmane as the only source of news, and does not touch mail at
;; all.  If maybe one day Gmane is usable again, revive this in order
;; to use as an NNTP frontend to Gmane, maybe.

;;;;; Gnus:



;;;;;; Files and directories:

;; Set up a consistent tree for Gnus and don't allow it to pollute the
;; user home directory or the e-mails.

(setf gnus-default-directory
      (expand-file-name "gnus" gk-mail-home))

(defun gnus-file (file)
  "Locate FILE in ‘gnus-default-directory’."
  (expand-file-name file gnus-default-directory))

(setf
 gnus-home-directory gnus-default-directory
 gnus-directory (gnus-file "news")
 message-directory (gnus-file "mail") ; nnml-directory is set from this.
 mail-source-directory (gnus-file "mail")
 gnus-startup-file (gnus-file "newsrc")
 nnmail-message-id-cache-file (gnus-file "nnmail-cache")
 nnfolder-directory (gnus-file "mail/archive")
 gnus-init-file (gnus-file "gnus-init"))



;;;;;; Methods:

(setf
 gnus-select-method '((nntp "gmane" (nntp-address "news.gmane.org"))))



;;;;;; Mime:

(setf
 mm-enable-external 'ask
 mm-discouraged-alternatives '("text/richtext" "text/html")
 ;; Resize images to fit the view.
 mm-inline-large-images 'resize
 gnus-buttonized-mime-types
 '("multipart/signed" "multipart/alternative" "text/html" "text/x-diff"
   "text/x-patch" "text/vcard" "text/x-org"))



;;;;;; Keybindings:

(define-key gnus-article-mode-map "\C-cw" 'gnus-article-browse-html-article)
(define-key gnus-summary-mode-map "BS" (gk-interactively
                                        (gnus-summary-move-article nil "spam")))
(define-key gnus-summary-mode-map "B@" (gk-interactively
                                        (gnus-summary-move-article nil "self")))



;;;;;; Settings:

(setf
 ;; «Number of seconds to wait before an nntp connection times out.»
 ;; Gmane hangs indefinitely at times.
 nntp-connection-timeout 5)



;;;;;; User interface:

(setf
 ;; U: read status
 ;; R: A if replied to,‘ ’ if not
 ;; z: zscore (char)
 ;; B: trn-style indentation based on thread level
 ;; f: contents of the from or to headers.
 ;; s: subject or empty str if not thread root
 gnus-summary-line-format "[ %U%R%z ] %B[ %(%-23,23f%) ] %s \n"
 ;; Don't mess up my window configuration.
 gnus-use-full-window nil)

(define-advice gnus-summary-exit
    (:before (&rest args) delete-article-window)
  "Before exiting summary mode, delete the related Article buffer's window."
  (let* ((w (next-window))
         (nwb (window-buffer w))
         (nwm (with-current-buffer nwb major-mode)))
    (when (equal 'gnus-article-mode nwm)
      (delete-window w))))

(setf gnus-thread-sort-functions
      '(gnus-thread-sort-by-date))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)

(define-key gnus-group-mode-map [?_] 'delete-other-windows-vertically)
(define-key gnus-summary-mode-map [?_] 'delete-other-windows-vertically)



