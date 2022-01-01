;;; gk-pim.el --- mail, contacts, etc                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>

;;; Commentary:

;; Settings for mail modes, bbdb, netrc, authinfo, etc.

;;; Code:

(require 'auth-source)
(require 'bbdb)
(require 'elfeed-curl)
(require 'elfeed-search)
(require 'elfeed-show)
(require 'forecast)
(require 'mail-source)
(require 'mairix)
(require 'message)
(require 'netrc)
(require 'parse-time)
(require 'rmail)
(require 'rmailsum)
(require 'sendmail)
(require 'simple)
(require 'solar)
(require 'time)
(require 'url-queue)

(require 'gk-mac)

;;;; Calendar:

(calendar-set-date-style 'iso)          ;The only unambiguous one.

;; Try to set lat and long from environment (most probably comes from
;; $MYSYSTEM/desktop-setup.bash)...
(when-let* ((lat  (getenv "LOCATION_LAT"))
            (long (getenv "LOCATION_LONG"))
            (desc (getenv "TZ")))
  (setf calendar-location-name desc
        calendar-latitude (string-to-number lat)
        calendar-longitude (string-to-number long)))

;; Periodically attempt to query geoclue for up-to-date location.

(defvar gk--update-geolocation-timer nil)

(when (featurep 'geoclue)
  (run-with-idle-timer
   3600
   t
   (lambda ()
     (when-let* ((whereami (geoclue-location))
                 (lat (gk-assoca 'latitude whereami))
                 (long (gk-assoca 'longitude whereami))
                 (desc (gk-assoca 'description whereami)))
       (setf calendar-location-name
             (if (string-empty-p desc)
                 "(Unknown location)"
               desc)
             calendar-latitude lat
             calendar-longitude long)
       (message "New geolocation: %s (lat: %f, long %f)"
                calendar-location-name
                lat long)))))



;;;; Date/time:

(setf
 ;; Time zones for ‘world-clock’.
 world-clock-list '(("Europe/Istanbul" "Istanbul")
                    ("Europe/London" "London")
                    ("Europe/Rome" "Rome")
                    ("America/New_York" "US East (NY)")
                    ("America/Los_Angeles" "US Pacific (Seattle)")
                    ("Asia/Hong_Kong" "Hong Kong")))



;;;; Parse-time:

;; Add Turkish month and day names, mainly for ‘org-time-stamp’ and
;; ‘org-time-stamp-inactive’.

(setf
 parse-time-months
 (append
  '(("oca" . 1) ("sub" . 2) ("mar" . 3) ("nis" . 4)  ("may" . 5)  ("haz" . 6)
    ("tem" . 7) ("agu" . 8) ("eyl" . 9) ("eki" . 10) ("kas" . 11) ("ara" . 12)

    ("ocak" . 1)   ("subat" . 2)   ("mart" . 3)  ("nisan" . 4) ("mayis" . 6)
    ("temmuz" . 7) ("agustos" . 8) ("eylul" . 9) ("ekim" . 10) ("kasim" . 11)
    ("aralik" . 12))
  parse-time-months)

 parse-time-weekdays
 (append
  '(("paz" . 0) ("pzt" . 1) ("sal" . 2) ("crs" . 3) ("prs" . 4) ("cum" . 5) ("cts" . 6)

    ("pazar" . 0) ("pazartesi" . 1) ("sali" . 2) ("carsamba" . 3) ("persembe" . 4)
    ("cuma" . 5)  ("cumartesi" . 6))
  parse-time-weekdays))



;;;; Weather:

(setq forecast-language 'en
      forecast-units 'si
      forecast-time-format "%I:%M:%S%p, %F"
      forecast-rain-symbol "☔")

;;;; BBDB:

(setf
 ;; Don’t display the record after completion.
 bbdb-completion-display-record nil)



;;;; Mail:

;; Settings for reading and writing mail, and specific to mail clients
;; and related software.



;;;;; Paths:

(defvar gk-mail-home (expand-file-name "~/posta")
  "Where all mailboxes etc. are.")

(defvar gk-mail-temporary-file-directory
  (expand-file-name "tmp" gk-mail-home))

(defvar gk-mail-inboxes
  (list (expand-file-name "inbox" gk-mail-home))
  "Where to look for mail.")

;; XXX(2018-06-06): Maybe manually add it if not defined?  Or does
;; that belong to the .profile script.
(when-let* ((spool (getenv "MAIL")))
  (pushnew spool gk-mail-inboxes))



;;;;; Authentication:

(setf netrc-file (expand-file-name "~/Documents/authinfo.gpg")
      auth-sources (list netrc-file))



;;;;; User agent:

(setf message-mail-user-agent t
      read-mail-command 'rmail)



;;;;; Posting styles:

(setf
 ;; Gmail does not like parens.
 message-from-style 'angles)



;;;;; Utilities:

(defun gk-fetch-mail (&optional callback)
  "Run mail retrieval scripts.

If CALLBACK is non-nil, it’s called with a single argument, which
is non nil if there’s new mail."
  (interactive)
  (unless (file-exists-p gk-mail-home)
    (user-error "‘%s’ not found, refusing to fetch mail" gk-mail-home))
  (make-process
   :name "gk-fetch-mail" :buffer (get-buffer-create "*Fetch Mail*")
   :command (list "mpop" "-Q" "-a")
   :sentinel
   (lambda (process event)
     (let ((msg ""))
       (unless (process-live-p process)
         (when (zerop (process-exit-status process))
           (dolist (f gk-mail-inboxes)
             (when-let* ((f (file-attribute-size (file-attributes f))))
               (when (> f 0)
                 (setf msg "You have unread mail! ")
                 (mairix-update-database))))
           (when (and (gk-gui-p) (not (string-empty-p msg)))
             (gk-send-desktop-notification "New mail" msg "mail-message-new")))
         (message "%sFetch mail process %s" msg (string-trim event))
         (when (functionp callback)
           (message "Running ‘gk-fetch-mail’ callback...")
           (funcall callback (string-empty-p msg))))))))



;;;;; Movemail program:

;; Ensure that a safe movemail is used.  I configure Emacs to use system
;; movemail at build time, but if somehow it doesn't, try to ensure it
;; does here.

(unless (string-match "with-mailutils" system-configuration-options)
 (setf mail-source-movemail-program (gk-executable-ensure "movemail")))



;;;;; Sending mail:

(setf
 message-send-mail-function 'message-send-mail-with-sendmail
 message-sendmail-f-is-evil t
 message-sendmail-envelope-from 'header
 sendmail-program (gk-executable-ensure "msmtp"))


;; Spammers are everywhere.
(setf user-mail-address (concat "self" "@" "gkayaalp" "." "com")
      user-full-name "Göktuğ Kayaalp")


(defun gk-mail-set-msmtp-account ()
  "Find account name for email address in From: line."
  (let ((from (save-excursion
                (goto-char (point-min))
                (or (re-search-forward "^From: .*? <" nil t)
                    (user-error "No From: line or an empty one"))
                (buffer-substring (point) (1- (line-end-position))))))
    (with-current-buffer (find-file-noselect "~/.msmtprc")
      (goto-char (point-min))
      (or (re-search-forward (concat "^from " from) nil t)
          (user-error "No msmtp account for ‘%s’" from))
      (re-search-backward "^account ")
      (end-of-line)
      (setf
       message-sendmail-extra-arguments
       (list "-a" (substring-no-properties (thing-at-point 'symbol)))))))

(add-hook 'message-send-mail-hook #'gk-mail-set-msmtp-account)

(defun gk-runq ()
  "Run outgoing email queue."
  (interactive)
  (async-shell-command "msmtp-runqueue.sh" "*runq*"))

(defun gk-listq ()
  "Show email queue."
  (interactive)
  (async-shell-command "msmtp-listqueue.sh" "*listq*"))




;;;;; Message mode:

(add-hook 'message-setup-hook 'bbdb-mail-aliases)

(setf
 message-citation-line-function 'message-insert-formatted-citation-line
 message-citation-line-format "On %Y-%m-%d %R %Z, %f wrote:")

(setf
 message-default-headers (format "Fcc: %s/outbox" gk-mail-home)

 ;; Drafts directory.
 message-auto-save-directory (expand-file-name "drafts" gk-mail-home)
 ;; Ask for confirmation before sending a message.
 message-confirm-send t)

(add-hook 'message-sent-hook #'bury-buffer)

(defun gk-message-mode-hook ()
  "Set up the message buffer."
  ;; EasyPG assistant's mailing helper.
  (epa-mail-mode 1))

(add-hook 'message-mode-hook 'gk-message-mode-hook)
(define-key message-mode-map (kbd "C-c C-c") 'message-send)



;;;;; Rmail:

(setf
 rmail-primary-inbox-list gk-mail-inboxes
 rmail-secondary-file-directory gk-mail-home
 rmail-secondary-file-regexp "spam\\|outbox\\|archive$"
 rmail-file-name (expand-file-name "current" gk-mail-home)
 rmail-default-file (expand-file-name "archive" gk-mail-home)
 gk-rmail-archive-file (expand-file-name "archive" gk-mail-home)
 rmail-displayed-headers
 (rx (and bol (or "to" "date" "from" "cc" "subject" "message-id" "list-id"
                  "delivered-to")))
 rmail-mime-prefer-html nil)

(defun gk-rmail-view-html-part-in-browser ()
  "View the HTML part of the message in this buffer in the browser."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (rx bol "["
         (optional digit (zero-or-more (and "/" digit)) ":")
         "text/html "
         (or "Hide" "Show")
         " Save:"))
    (point)
    (forward-char 1)
    (let ((button (button-at (point)))
          (filename
           (concat (make-temp-name
                    (expand-file-name
                     "gkrmailout" gk-mail-temporary-file-directory))
                   ".html")))
      (browse-url
       (concat "file://" (gk-rmail-mime-save-to-tmp button filename))))))

(add-function :around (symbol-function 'gk-rmail-view-html-part-in-browser)
              #'gk-protect-frame-focus)

(defun gk-rmail-mime-save-to-tmp (button output-file-name)
  "Save the attachment in BUTTON in OUTPUT-FILE-NAME.

Return the file name, expanded."
  ;; Adapted from ‘rmail-mime-save’ in order to automatically export
  ;; to HTML and open in external browser.
  (let* ((rmail-mime-mbox-buffer rmail-view-buffer)
	 (data (button-get button 'data)))
    (prog1 (expand-file-name output-file-name)
      (if (and (not (stringp data))
	       (rmail-mime-entity-truncated data))
	  (unless (y-or-n-p "This entity is truncated; save anyway? ")
	    (error "Aborted")))
      (with-temp-buffer
        (set-buffer-file-coding-system 'no-conversion)
        ;; Needed e.g. by jka-compr, so if the attachment is a compressed
        ;; file, the magic signature compares equal with the unibyte
        ;; signature string recorded in jka-compr-compression-info-list.
        (set-buffer-multibyte nil)
        (setq buffer-undo-list t)
        (if (stringp data)
	    (insert data)
	  ;; DATA is a MIME-entity object.
	  (let ((transfer-encoding (rmail-mime-entity-transfer-encoding data))
	        (body (rmail-mime-entity-body data)))
	    (insert-buffer-substring rmail-mime-mbox-buffer
				     (aref body 0) (aref body 1))
	    (cond ((string= transfer-encoding "base64")
		   (ignore-errors (base64-decode-region (point-min) (point-max))))
		  ((string= transfer-encoding "quoted-printable")
		   (quoted-printable-decode-region (point-min) (point-max))))))
        (write-region nil nil output-file-name nil nil nil t)))))

(defun gk-rmail-force-expunge-and-save ()
  "Force save the mail box, even if it seems to not be modified."
  (interactive)
  (set-buffer-modified-p t)
  (rmail-expunge-and-save))

(defun gk-rmail-advance (&optional arg)
  "Advance to the next message in default mbox.

This command will not run unless in an RMAIL buffer visiting
‘rmail-file-name’.  It will output the current message to
‘gk-rmail-archive-file’ and delete it, advancing to the next
message in the RMAIL file.  This is a utility for the email
workflow where a temporary inbox is used for working with current
email and archiving read mail in another file.

If ARG is non-nil, or called interactively with a prefix
argument, prompt for which mailbox to output to."
  (interactive "P")
  (unless (and (eq major-mode 'rmail-mode)
               (string= (buffer-file-name) rmail-file-name))
    (user-error
     "This is not your default RMAIL file, won't run ‘gk-rmail-advance’ here"))
  (let ((outfil (if (null arg)
                    gk-rmail-archive-file
                  (read-file-name
                   "Move to mailbox: " (concat gk-mail-home "/")
                   nil nil nil
                   ;; Exclude numbered split mbox files.
                   ($ (save-match-data (not (string-match "-[0-9]+\\'" $1))))))))
    (rmail-output outfil))
  (rmail-delete-forward))

(defun gk-rmail-forward-link-or-button (p)
  "Navigate both links and buttons in Rmail in a ring.

This replaces the use of ‘forward-button’ which only traverses
buttons and skips over links."
  (interactive (list (point)))
  (let (positions)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (memq (car (overlay-properties overlay))
                  '(goto-address button))
        (pushnew (overlay-start overlay) positions)))
    (setq positions (sort positions #'<))
    (if (>= p (car (last positions)))
        (goto-char (first positions))
      (goto-char (first (cl-remove-if ($ (<= $1 p)) positions))))))

(defun gk-rmail-backward-link-or-button (p)
  "Navigate both links and buttons in Rmail in a ring.

This replaces the use of ‘forward-button’ which only traverses
buttons and skips over links.

This is the reverse counterpart of
‘gk-rmail-forward-link-or-button’."
  (interactive (list (point)))
  (let (positions)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (memq (car (overlay-properties overlay))
                  '(goto-address button))
        (pushnew (overlay-start overlay) positions)))
    (setq positions (sort positions #'<))
    (if (<= p (first positions))
        (goto-char (car (last positions)))
      (goto-char (car (last (cl-remove-if ($ (>= $1 p)) positions)))))))

(defun posta ()
  "Set up and display an Rmail frame."
  (interactive)
  (gk-fetch-mail
   (lambda (_)
     (gk-with-new-frame nil
       (rmail)
       (rmail-summary)
       (window-resize (selected-window) -10)
       (other-window 1)))))

(defun gk-rmail-mode-hook ()
   (goto-address-mode +1)
   (setq-local word-wrap t))

(add-hook 'rmail-mode-hook #'gk-rmail-mode-hook)

(define-key rmail-mode-map (kbd "RET") nil) ;was: rmail-mime-toggle-hidden, brutally useless
(define-key rmail-mode-map "N" #'gk-rmail-advance)
(define-key rmail-mode-map "S" #'gk-rmail-force-expunge-and-save)
(define-key rmail-mode-map "b"
  (gk-interactively (gk-rmail-view-html-part-in-browser)
                    (gk-rmail-advance)))
(define-key rmail-mode-map (kbd "<tab>") #'gk-rmail-forward-link-or-button)
(define-key rmail-mode-map (kbd "<backtab>") #'gk-rmail-backward-link-or-button)

;; Unbind this
(define-key rmail-mode-map (kbd "C-M-f") nil)


;; ‘q’ is normally bound to ‘rmail-summary-quit’, which is simply
;; useless.
(define-key rmail-summary-mode-map "q" #'bury-buffer)

;; Quick search
(define-key rmail-mode-map "/" #'mairix-search)
(define-key rmail-summary-mode-map "/" #'mairix-search)



;;;;; Mairix:

;; XXX(2018-05-25): Use with Gnus?

(setf
 mairix-file-path (expand-file-name "mairix/" gk-mail-home)
 mairix-search-file "search")

(defalias 'search-mail 'mairix-widget-search)
(defalias 'mx 'mairix-widget-search)

(define-advice mairix-widget-search
    (:after (&rest args) enable-widget-minor-mode)
  "Activate ‘widget-minor-mode’ in the ‘mairix-widget-search’ buffer.

Wonder why this is not the default."
  (widget-minor-mode +1))

;;;; Elfeed:



;;;;; Variables:

;; Set the default filter.
(defvar gk-elfeed-default-filter "+unread ")
(setq-default elfeed-search-filter gk-elfeed-default-filter)

;; Set up the url-queue variables for swift and complete operation. The
;; defaults are too bad. Especially /url-queue-timeout/ is way too short
;; for loading feeds.
(setf url-queue-parallel-processes 20
      url-queue-timeout 10)



;;;;; Some utility functions:

(defun gk-feeds-youtube (hash)
  "Return the feed URL for the channel with HASH."
  (concat "http://www.youtube.com/feeds/videos.xml?channel_id=" hash))

(defun gk-feeds-youtube-pl (playlist-id)
  "Return the feed URL for the playlist with PLAYLIST-ID."
  (concat
   "https://www.youtube.com/feeds/videos.xml?playlist_id="
   playlist-id))

(defun gk-elfeed-browse-article ()
  "View elfeed article with browser."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (browse-url link)))

(defun gk-elfeed-catch-up ()
  "C-x h, r, g in *elfeed-search* buffer."
  (interactive)
  (when (y-or-n-p "Catch-up on visible entries?")
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)
    (elfeed-search-update--force)))

(defun gk-elfeed-filter (filter)
  "Set search filter, do not update live.

It is rather slow to do so."
  (interactive (list (read-string "Filter: " elfeed-search-filter)))
  (setq elfeed-search-filter filter)
  (elfeed-search-update :force))

(defun gk-elfeed-search-kill-url ()
  "Copy the URL for entry under point as kill."
  (interactive)
  (dolist (entry (elfeed-search-selected))
    (when-let* ((link (elfeed-entry-link entry)))
      (with-temp-buffer
        (insert link)
        (clipboard-kill-ring-save (point-min) (point-max))
        (message link)))))

(defalias 'gk-elfeed-next 'next-logical-line)
(defalias 'gk-elfeed-prev 'previous-logical-line)

(defun gk-elfeed-feeds-with-category (category &rest feeds)
  (declare (indent defun))
  (mapcar (lambda (feed)
            (append (if (listp feed) feed (list feed))
                    (if (listp category) category (list category))))
          feeds))

(defun gk-elfeed-import-from-feeder-OPML (file)
  (cons
   'setf
   (cons
    'elfeed-feeds
    (list
     (cons
      'cl-concatenate
      (cons
       ''list
       (cl-remove-if
        #'not
        (mapcar
         (lambda (cat)
           (let (tag feeds)
             (when (listp cat)
               (when (eq 'outline (car cat))
                 (setq tag (intern (downcase (gk-assoca 'title (cadr cat))))
                       feeds (cl-remove-if-not #'listp (cdddr cat)))
                 `(gk-elfeed-feeds-with-category
                    ',tag
                    ,@(mapcar (lambda (f) (gk-assoca 'xmlUrl (cadr f))) feeds))))))
         (gk-assoca
          '(opml body)
          (xml-parse-file file))))))))))

(defun gk-elfeed-search-mode-hook ()
  )

(add-hook 'elfeed-show-mode-hook 'gk-eww-mode-hook)
(add-hook 'elfeed-search-mode-hook 'gk-elfeed-search-mode-hook)



;;;;; Advices:

(add-function :around (symbol-function 'elfeed-search-browse-url) #'gk-protect-frame-focus)
(add-function :around (symbol-function 'elfeed-show-visit) #'gk-protect-frame-focus)



;;;;; Print entry function:

(defface gk-elfeed-feed-host
  '()
  "Face for displaying URLs’ host parts in Elfeed."
  :group 'gk)

(set-face-attribute 'gk-elfeed-feed-host nil :height .7 :italic nil)

(defun gk-elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer, with style."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    (cl-remove-if ($ (string= $1 "unread")) tags)
                    ","))
         (title-width (* (window-width)  .7))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               (window-width))
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (let ((f (split-string
              (buffer-local-value
               'elfeed-search-filter (get-buffer "*elfeed-search*"))
              " " t)))
      ;; If we’re not looking at a stored search, hide tags and don’t
      ;; limit title length.  Otherwise print the truncated title and
      ;; include the filtered tags.  Commit logs and VCS releases get
      ;; special treatment.
      (cond ((cl-member "software" tags :test #'string=)
             (setq-local word-wrap t)
             (setq-local truncate-lines nil)
             (let* ((url (elfeed-feed-url feed))
                    (host (url-host (url-generic-parse-url url)))
                    (path (url-filename (url-generic-parse-url url))))
               (insert
                (cond ((cl-member "commits" tags :test #'string=)  "Commit  ")
                      ((cl-member "releases" tags :test #'string=) "Release ")
                      (t                                           "News    ")))
               (insert
                (cond ((string= host "github.com")
                       (format "gh:%-27s"
                               (mapconcat #'identity (butlast (split-string path "/" t)) "/")))
                      (t
                       (format "%30s" url)))
                " ")
               (insert (propertize title 'face title-faces 'kbd-help title))))
            ((and (string= (car f) "+unread")
                  (member (cadr f) gk-elfeed-search-ring-tags))
             (setq-local word-wrap t)
             (setq-local truncate-lines nil)
             (insert (propertize title 'face title-faces 'kbd-help (concat title ": " feed-title)) " ")
             (insert (propertize (concat
                                  "("
                                  (url-host
                                   (url-generic-parse-url
                                    (elfeed-feed-url feed)))
                                  ")")
                                 'face 'gk-elfeed-feed-host)))
            (t
             (setq-local truncate-lines t)
             (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
             (when tags
               (insert "(" tags-str ")")))))))


(setf elfeed-search-print-entry-function #'gk-elfeed-search-print-entry)



;;;;; Keys:

(define-key elfeed-show-mode-map (kbd "v") #'gk-elfeed-browse-article)
(define-key elfeed-show-mode-map (kbd "!") #'gk-eww-download)
(define-key elfeed-search-mode-map (kbd "c") #'gk-elfeed-catch-up)
(define-key elfeed-search-mode-map (kbd "s") #'gk-elfeed-filter)
(define-key elfeed-search-mode-map (kbd "S") (gk-interactively (elfeed-db-save)))
(define-key elfeed-search-mode-map (kbd "J") #'elfeed-unjam)
(define-key elfeed-search-mode-map (kbd "n") #'gk-elfeed-next)
(define-key elfeed-search-mode-map (kbd "p") #'gk-elfeed-prev)
(define-key elfeed-search-mode-map (kbd "w") #'gk-elfeed-search-kill-url)



;;;;; Search ring:

;; Go cycling a list of tags in the search mode.

(defvar gk-elfeed-search-ring-tags nil)

(defvar-local gk-elfeed-search-ring-current-search nil)

(defun gk-elfeed-search-ring-next ()
  (interactive)
  (let ((search (or (cadr (member gk-elfeed-search-ring-current-search
                                  gk-elfeed-search-ring-tags))
                    (car gk-elfeed-search-ring-tags))))
    (gk-elfeed-filter
     (concat gk-elfeed-default-filter
             (setf gk-elfeed-search-ring-current-search search)))))

(defun gk-elfeed-search-ring-previous ()
  (interactive)
  (gk-elfeed-filter
   (concat gk-elfeed-default-filter
           (setf
            gk-elfeed-search-ring-current-search
            (if (or (not gk-elfeed-search-ring-current-search)
                    (string= gk-elfeed-search-ring-current-search
                             (car gk-elfeed-search-ring-tags)))
                (car (last gk-elfeed-search-ring-tags))
              (nth (1- (or (position
                            gk-elfeed-search-ring-current-search
                            gk-elfeed-search-ring-tags
                            :test #'equal)
                           (length gk-elfeed-search-ring-tags)))
                   gk-elfeed-search-ring-tags))))))

(define-key elfeed-search-mode-map [?k] 'gk-elfeed-search-ring-previous)
(define-key elfeed-search-mode-map [?j] 'gk-elfeed-search-ring-next)



;;;;; Faces:

(mapc
 (lambda (x) (apply #'set-face-attribute x))
 `((elfeed-search-title-face nil :foreground "normal" :strike-through t)
   (elfeed-search-unread-title-face nil :foreground "normal" :strike-through nil)
   (elfeed-search-tag-face nil :foreground "normal")
   (elfeed-search-date-face nil :foreground "normal")
   (elfeed-search-feed-face nil :foreground "normal" :weight bold)
   (elfeed-search-unread-count-face nil :foreground nil)))



;;;;; Scoring:

;; Adapted from http://kitchingroup.cheme.cmu.edu/blog/2017/01/05/Scoring-elfeed-articles/

(defface gk-relevant-elfeed-entry `()
  "Marks a relevant Elfeed entry.")

(defface gk-important-elfeed-entry `()
  "Marks an important Elfeed entry.")

(push '(relevant gk-relevant-elfeed-entry)
      elfeed-search-face-alist)

(push '(important gk-important-elfeed-entry)
      elfeed-search-face-alist)

(defvar gk-elfeed-scoring-patterns
  nil
  "Patterns for scoring Elfeed entries.

An association list where car is a regexp to match the title or
the body of the entry, and the cdr is the score, an integer.")

(defun gk-score-elfeed-entry (entry)
  (let ((title (elfeed-entry-title entry))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (score 0))
    (let ((case-fold-search t))
      (loop for (pattern . n) in gk-elfeed-scoring-patterns
            if (string-match pattern title)
            do (incf score n)
            if (string-match pattern content)
            do (incf score n)))

    ;; store score for later in case I ever integrate machine learning
    (setf (elfeed-meta entry :my/score) score)

    ;; (cond
    ;;  ((= score 1)
    ;;   (elfeed-tag entry 'relevant))
    ;;  ((> score 1)
    ;;   (elfeed-tag entry 'important)))
    ;;
    ;; XXX(2018-12-21): this is an experiment where anything that’s
    ;; relevant is also important, given the distinction is irrelevant
    ;; given the scale.
    (when (>= score 1)
      (elfeed-tag entry 'relevant)
      (elfeed-tag entry 'important))
    entry))

;; Uncomment to enable
;; (remove-hook 'elfeed-new-entry-hook 'gk-score-elfeed-entry)




;;;;; Show mode:

(add-hook
 'elfeed-show-mode-hook
 (defun gk-elfeed-show-mode-hook ()
   "Hook for ‘elfeed-show-mode’."
   (setq-local truncate-lines nil)
   (setq-local word-wrap t)))



;;;;; Update completion notification:

(defun gk-elfeed-notify-update-completion (url)
  (ignore url)
  (when
      (and (null elfeed-curl-queue)
           (zerop elfeed-curl-queue-active))
    (gk-send-desktop-notification
     "elfeed: updated all feeds"
     "All feeds have been updated"
     "application-rss+xml")))

(add-hook 'elfeed-update-hooks #'gk-elfeed-notify-update-completion)



;;;;; Feeds:

;; Load feeds from external source.
(with-eval-after-load 'gk-org
  (gk-load (gk-org-dir-file "elfeed-feeds") t))



(provide 'gk-pim)
;;; gk-pim.el ends here
