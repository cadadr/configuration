;;; gk-fd.el --- customised files and directory settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;;; Commentary:

;; Here I set variables that point to various files and directories
;; used by Emacs or my config, plus define a few helper variables and
;; functions.

;;; Code:

(require 'bbdb)
(require 'bookmark)
(require 'eww)
(require 'image-dired)
(require 'multiple-cursors-core)
(require 'nsm)
(require 'savehist)
(require 'saveplace)
(require 'tramp-cache)
(require 'url)

(defconst gk-notes-directory
  (expand-file-name "~/Notlar")
  "Directory for digital notebooks.")

(setf image-dired-dir
      (locate-user-emacs-file "etc/image-dired")
      url-configuration-directory
      (locate-user-emacs-file "etc/url")
      auto-save-list-file-prefix
      (locate-user-emacs-file "etc/auto-save-list/saves-")
      bookmark-default-file
      (expand-file-name "~/Documents/bookmarks.el")
      bbdb-file
      (expand-file-name "~/Documents/bbdb-contacts.el")
      savehist-file
      (locate-user-emacs-file "etc/history")
      eww-bookmarks-directory
      (expand-file-name "~/Documents")
      save-place-file
      (locate-user-emacs-file "etc/places")
      tramp-persistency-file-name
      (locate-user-emacs-file "etc/tramp")
      custom-file
      (locate-user-emacs-file "etc/custom.el")
      nsm-settings-file
      (locate-user-emacs-file "etc/network-security.data")
      mc/list-file
      (locate-user-emacs-file "etc/mc-lists.el"))

(provide 'gk-fd)
;;; gk-fd.el ends here
