;;; gk-fd.el --- customised files and directory settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;;; Commentary:

;; Here I set variables that point to various files and directories
;; used by Emacs or my config, plus define a few helper variables and
;; functions.

;;; Code:

(defconst gk-dropbox-dir
  (expand-file-name "~/fil")
  "Directory of Dropbox.")

(defun gk-dropbox (path)
  "Return ‘gk-dropbox-dir’ + / + PATH."
  (expand-file-name path gk-dropbox-dir))

(define-obsolete-function-alias 'dropbox 'gk-dropbox "2021-12-07"
  "Return ~/Dropbox + PATH.")

(defconst gk-syndir
  (expand-file-name "~/syn")
  "Directory for syncing.")

(defconst gk-notes-directory
  (expand-file-name "~/Notes")
  "Directory for digital notebooks.")

(setf image-dired-dir
      (locate-user-emacs-file "etc/image-dired")
      url-configuration-directory
      (locate-user-emacs-file "etc/url")
      auto-save-list-file-prefix
      (locate-user-emacs-file "etc/auto-save-list/saves-")
      bookmark-default-file
      (dropbox "bookmarks.el")
      bbdb-file
      (expand-file-name "~/Notes/bbdb-contacts.el")
      savehist-file
      (locate-user-emacs-file "etc/history")
      eww-bookmarks-directory
      (dropbox ".")
      save-place-file
      (locate-user-emacs-file "etc/places")
      tramp-persistency-file-name
      (locate-user-emacs-file "etc/tramp")
      custom-file
      (locate-user-emacs-file "etc/custom.el")
      nsm-settings-file
      (locate-user-emacs-file "etc/network-security.data")
      mc/list-file
      (locate-user-emacs-file "etc/mc-lists.el")
      common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "~/co/Lisp/doc/HyperSpec/")))

(provide 'gk-fd)
;;; gk-fd.el ends here
