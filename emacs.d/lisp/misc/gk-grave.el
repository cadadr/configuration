;;; gk-grave.el --- where lisp goes to die           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains various unrelated bits of code removed from my
;; main configuration, but kept for reference.

;;; Code:

(defun gk-tar-dotfiles ()
  "Create an encrypted tarball of dotfiles.

Take files from ~/RCS, find  corresponding files under $HOME, put
all into a tar  file, call it dotfiles-YYYY-MM-DD-HH:MM.tar, then
encrypt it using GPG."
  (interactive)
  (let* ((default-directory (expand-file-name "~"))
         (dir (expand-file-name "~/RCS"))
         (regex directory-files-no-dot-files-regexp)
         (vfiles (directory-files dir nil regex))
         (delv (lambda (x) (replace-regexp-in-string ",v$" "" x)))
         (files (mapcar delv vfiles))
         (addrcsdir (lambda (x) (format "./%s/%s" "RCS" x)))
         (adddot (lambda (x) (format "./%s" x)))
         (flist (append (mapcar addrcsdir vfiles)
                        (mapcar adddot files)))
         (tarfile (expand-file-name "~/dotfiles.tar"))
         (tarcmd (format "cd %s; tar cf \\\n %s %s"
                         default-directory
                         tarfile
                         (mapconcat 'identity flist " \\\n")))
         (gpgfile (concat tarfile ".gpg"))
         (backupfile (expand-file-name
                      (file-name-nondirectory gpgfile)
                      (dropbox "Backups/Dotfiles"))))
    (when (shell-command tarcmd)
      (when (epa-encrypt-file
             tarfile
             (epa-select-keys
              (epg-make-context)
              "Select key(s): "))
        (delete-file tarfile)
        (rename-file gpgfile backupfile t)))))



(defun gk-targpg-directory (dir outdir)
  "Tar up DIR, encrypt, put that into OUTDIR."
  (interactive (list (expand-file-name (read-directory-name "Directory: "))
                     (expand-file-name (read-directory-name "Output directory: "))))
  (let* ((default-directory (expand-file-name ".." dir))
         (dirname (format
                   "./%s"
                   (car (last (split-string-and-unquote dir "/")))))
         (tarfile (format "/tmp/%s.tar" (make-temp-name "gktargpg")))
         (gpgfile (format "%s.gpg" tarfile))
         (outfile (expand-file-name (replace-regexp-in-string
                                     "^\\.*\\(/\\.*\\)?" ""
                                     (format "%s.tar.gpg" dirname))
                                    outdir))
         (tarcmd (format "cd %s && tar cf %s %s"
                         default-directory tarfile dirname)))
    (when (shell-command tarcmd)
      (when (epa-encrypt-file
             tarfile
             (epa-select-keys
              (epg-make-context)
              "Select key(s): "))
        (delete-file tarfile)
        (rename-file gpgfile outfile t)))))



;; Mount helpers.
(defvar gk-mtp-dir "/mnt/mtp/"
  "Mount point for MTP devices.")

(defvar gk-ukey-dir "/mnt/ukey/"
  "Mount point for MTP devices.")

(defun gk-mount-mtp ()
  (interactive)
  (let ((ret (gk-sudo (format
                       "simple-mtpfs -oallow_other,enable-move %s" gk-mtp-dir))))
    (cond ((stringp ret) (error ret))
          ((zerop ret) (find-file gk-mtp-dir))
          (t (error "simple-mtpfs(1) returned %d" ret)))))

(defun gk-unmount-mtp ()
  (interactive)
  (let ((ret (gk-sudo (format "umount %s" gk-mtp-dir))))
    (cond ((stringp ret) (error ret))
          ((zerop ret) (message "Unmounted %s" gk-mtp-dir))
          (t (error "unmount(1) returned %d" ret)))))

(defun gk-mount-ukey (fs)
  (interactive
   (list (completing-read "Choose filesystem for ukey: " '(ufs msdos) nil t)))
  (cond
   ((string= fs "ufs") (gk-ukey-mount-ufs))
   ((string= fs "msdos") (gk-ukey-mount-msdos))))

(defun gk-ukey-mount-msdos ()
  (interactive)
  (let ((ret (gk-sudo (format "mount %s" gk-ukey-dir))))
    (cond ((stringp ret) (error ret))
          ((zerop ret) (find-file gk-ukey-dir))
          (t (error "mount(1) returned %d" ret)))))

(defun gk-ukey-mount-ufs ()
  (interactive)
  (let ((ret (gk-sudo (format "mount -t ext2fs /dev/da0p1 %s" gk-ukey-dir))))
    (cond ((stringp ret) (error ret))
          ((zerop ret) (find-file gk-ukey-dir))
          (t (error "mount(1) returned %d" ret)))))

(defun gk-unmount-ukey ()
  (interactive)
  (gk-sudo "sync")
  (let ((ret (gk-sudo (format "umount %s" gk-ukey-dir))))
    (cond ((stringp ret) (error ret))
          ((zerop ret) (message "Unmounted %s" gk-ukey-dir))
          (t (error "unmount(1) returned %d" ret)))))



(defun gk-count-my-lisp ()
  "Count the lines of Lisp in ‘user-emacs-directory’.
Returns a plist where :files is the number of files and :lines is
the number of lines."
  (interactive)
  (let ((lisp-files
         (append
          (directory-files (locate-user-emacs-file ".") t
                           "^\\(init\\|load\\).*el$")
          (directory-files (locate-user-emacs-file "gk") t "el$")
          (directory-files (locate-user-emacs-file "lisp") t "el$")
          (directory-files (locate-user-emacs-file "extras") t "el$")
          (directory-files (locate-user-emacs-file "themes") t "el$")))
        (sum 0)
        buf files)
    (setf files (length lisp-files))
    (dolist (el lisp-files sum)
      (setf buf (find-file-noselect el))
      (with-current-buffer buf
        (incf sum (count-lines (point-min) (point-max))))
      (kill-buffer buf))
    (when (called-interactively-p 'any)
      (message "%d lines of Elisp in %d files." sum files))
    (list :files files :lines sum)))



(defun gk-sanitise-dir-files (dir &optional recurse)
  "Remove problematic chars from filenames in DIR.

If RECURSE non-nil, or called interactively with a universal
prefix, recurse in the directories.

The following things are changed:

- Transform \"ı\" to \"i\".  Files with this character in name
  cannot be synced to Dropbox.

If, after tranformations, the new file name is not unique, prompt
the user for taking action."
  (interactive "DDirectory to sanitise: \nP")
  ;; If non-nil, set to t, as numeric values would result in
  ;; complications when debugging.
  (when recurse (setq recurse t))
  (let* ((items (directory-files dir t "[^/\\.+$]" t))
         (subdirs)
         (rename (lambda (f)
                   (let* ((ı (replace-regexp-in-string "ı" "i" f t))
                          (İ (replace-regexp-in-string "İ" "I" ı t))
                          (name İ))
                     (unless (string= f name)
                       (while (file-exists-p name)
                         (let ((new-name
                                (read-file-name
                                 (format
                                  "File `%s' exists.  Please provide new name: " name)
                                 (file-name-directory f))))
                           (setq name new-name))))
                     name))))
    (dolist (f items)
      (let ((n (funcall rename f)))
        (when (file-directory-p n)
          (push subdirs n))
        (when (and
               (not (string= f n))
               (y-or-n-p
                (format "Rename as following?\nOld: %s\nNew: %s" f n)))
          (rename-file f n 1993))))
    (when recurse
      (dolist (subdir subdirs)
        (gk-sanitise-dir-files subdir recurse)))))



(defun gk-org-capture-microblog (key descr file)
  "Generate a microblog capture template.
FILE is the microblog file.  KEY is the capture key.  DESCR is
the template description."
  (let ((sym (intern (symbol-name (gensym "gk-org-capture-microblog--")))))
    (defalias sym
      (lambda ()
        (let* ((id (with-temp-buffer
                     (insert-file-contents file)
                     (org-mode)
                     (org-next-visible-heading 1)
                     (number-to-string
                      (if (= (point) (point-max)) 1 ; First entry if the file is empty.
                        (1+ (string-to-number (org-entry-get (point) "CUSTOM_ID")))))))
               (title (read-string "Title: "))
               (time (current-time))
               (date (format-time-string "%F" time))
               (day (format-time-string "%a" time))
               (template
                (concat "* " title " [[#" id "][∞]]\n"
                        ":PROPERTIES:\n"
                        ":X-LINK:   #" id "\n"
                        ":X-DATE:   " date "\n"
                        ":X-TITLE:  " title "\n"
                        ":CUSTOM_ID: " id "\n"
                        ":END:\n"
                        "[" date " " day "]\n\n%?")))
          template)))
    ;; The actual template
    `(,key ,descr entry (file ,file) (function ,sym)
           :prepend t :immediate-finish nil :empty-lines-after 1)))



(setf org-caldav-inbox (gk-org-dir-file "caldav.org")
      org-caldav-files org-agenda-files
      ;; TODO: maybe don't sync anything at all.  Just sync outbound?
      org-caldav-sync-changes-to-org 'title-and-timestamp)



(provide 'gk-grave)
;;; gk-grave.el ends here
