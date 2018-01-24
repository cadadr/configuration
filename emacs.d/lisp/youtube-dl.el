;;; youtube-dl.el --- Download videos from youtube   -*- lexical-binding: t; -*-

;;; Commentary:

;; Download youtube videos using youtube-dl.

;; Requirements:
;;   - A checkout of the git repository for youtube-dl at a certain path.
;;   - Whatever youtube-dl requires for its operation.
;;   - Git for updating.

;;; Code:
(require 'cl-lib)
(require 'gk-utils)

(defvar youdl-git-repo
  (expand-file-name "~/co/External/youtube-dl")
  "The location for the youtube-dl git repo.")

(defvar youdl-script-subpath
  "bin/youtube-dl"
  "The location of the youtube-dl script within ‘youdl-git-repo’")

(defun youdl-update ()
  "Update youtube-dl via git.")

(defun youdl-run (callback &rest args)
  "Run youtube-dl script as an asynchronous subprocess.

CALLBACK is a function that takes two arguments, the exit status
of the youtube-dl process, and the process buffer.  ARGS are
arguments to the youtube-dl script, a la ‘start-process’.

Returns the process object if successfully created, raises error
otherwise."
  (let* ((process-environment
          (cons
           (format "PYTHONPATH=%s" youdl-git-repo)
           process-environment))
         (procnam (make-temp-name "youtube-dl-proc-"))
         (procbuf (get-buffer-create (make-temp-name "youtube-dl-buf-")))
         (script (expand-file-name youdl-script-subpath youdl-git-repo))
         proc)
    (unless (file-exists-p script)
      (user-error "Mistaken path to the youtube-dl script: %s" script))
    (make-process :name procnam :buffer procbuf
                  :command (cons script args)
                  :sentinel
                  (lambda (p e)
                    (unless (process-live-p p)
                      (funcall callback (process-exit-status p) procbuf))))))

(defun youdl-clear-buffers ()
  "Clear inactive youdl buffers."
  (interactive)
  (mapc (lambda (buf)
          (when (and (string-match "^youtube-dl-" (buffer-name buf))
                     (not (buffer-file-name buf)))
            (unless (get-buffer-process buf)
              (kill-buffer buf))))
        (buffer-list)))

(defun youdl-grab (file uri &optional sound-only callback noerror)
  "Grab video from URI, write it to FILE.
Extract sound only if SOUND-ONLY is non-nil.
If interactive, will query for overwrites, otherwise will error-out.
Will automatically add a relevant suffix to FILE.

CALLBACK is optional callback, run after process completion. It
receives two arguments: the process exit status, and the process
buffer.

Don't signal on errors if NOERROR is non-nil."
  (interactive (list
                (read-file-name "Target file: ")
                (read-string "URL: ")
                (y-or-n-p "Grab sound only? ")))
  (when (string-empty-p (s-trim uri))
    (user-error "Cannot use empty URI!"))
  (when (and (file-exists-p file)
             (and (interactive-p)
                  (not
                   (yes-or-no-p
                    (format "File %s exists, overwrite? " file)))))
    (funcall 
     (if (interactive-p) #'user-error #'error)
     "File %s exists, will not overwrite." file))
  (youdl-run (lambda (x b)
               (if (zerop x)
                   (prog1 t
                     (when (interactive-p)
                       (message "Successfully downloaded %s -> %s"
                                'uri file)))
                 (unless noerror
                   (signal 'youdl-error (list :message "Failed download"
                                              :exit-status x
                                              :youdl-buffer b
                                              :source-uri uri
                                              :target-file file))))
               (funcall callback x b))
             (when sound-only "-x")
             (when sound-only "--audio-format")
             (when sound-only "vorbis")
             "-o" (concat file ".%(ext)s")
             uri))

(defun youdl-songlist (songs &optional directory)
  "Download a list of songs.
SONGS is a cons structure defining songs to download. Each item
is a list whose car is the artist name, and cdr is a list of song
definitions.  Each song definition is a cons pair where the car
is the song title and the cdr is the source uri.

Each song is converted to Ogg Vorbis format and given ‘ARTIST’
and ‘TITLE’ tags.

DIRECTORY is the target directory into which the files are saved.
If nil, use default-directory.

Returns nil on success, or a list of pairs of (FILENAME . URI) to
try again."
  (unless (or (not directory) (stringp directory))
    (error "Directory must be a string."))
  (let (downloads failed)
    (dolist (artist songs)
      (let ((artist-name (car artist)))
        (dolist (song (cdr artist))
          (let ((filnam 
                 (expand-file-name
                  (replace-regexp-in-string
                   "'" "+"
                   (replace-regexp-in-string
                    " " "-"
                    (apply #'concat
                           (append (list artist-name "_" (car song))
                                   (when-let (extra (caddr song))
                                     (list "=" extra))))))
                  (or directory default-directory))))
            (pushnew `(,filnam ,(cadr song) ,(list artist-name song))
                     downloads)))))
    (youdl-songlist--do downloads)))

(defun youdl-songlist--do (downloads &optional done)
  (if downloads
      (youdl-songlist--download
       (car downloads) (cdr downloads)
       (lambda (completed failed)
         (let (retry)
           (when failed
             (setf retry
                   (y-or-n-p
                    "There are failed YouTube downloads, retry them? ")))
           (youdl-songlist--do (seq-uniq (when retry failed))
                               (seq-uniq (append completed done))))))
    (youdl--vortag done)))

(defun youdl-songlist--download 
    (current-download remaining-downloads callback
                      &optional completed failed n)
  (let ((fil (car current-download))
        (uri (cadr current-download)))
    (if (and (not remaining-downloads)	; If there's nothing to do,
             (not current-download))
        (funcall callback completed failed) ; run callback.
      (prog1 nil
        (message "Downloading %s..." fil)
        (youdl-grab fil uri t		; Otherwise, start downloading.
                    (lambda (x b)
                      (if (zerop x)
                          (message "Done downloading %s!" fil)
                        (message "Failed downloading %s!" fil))
                      (if (zerop x)
                          (push current-download completed)
                        (push current-download failed))
                      (youdl-songlist--download
                       (car remaining-downloads)
                       (cdr remaining-downloads)
                       callback
                       (if (zerop x)
                           (cons current-download completed)
                         completed)
                       (if (not (zerop x))
                           (cons current-download failed)
                         failed)
                       (1+ (or n 0))))
                    t)))))

(defun youdl--vortag (dat)
  (mapcar (lambda (item)
            (let ((filnam (concat (car item) ".ogg"))
                  (comment (caddar (cdaddr item))))
              (message "Vortag %s" filnam)
              (call-process "vorbiscomment" nil nil nil
                            "-w" filnam
                            "-t" (concat "ARTIST=" (caaddr item))
                            "-t" (concat "TITLE="  (concat (caar
                                                            (cdaddr item))
                                                           (when comment
                                                             (concat
                                                              " ("
                                                              comment
                                                              ")")))))))
          dat)
  (message "Done Vortag!"))

(provide 'youtube-dl)
;;; youtube-dl.el ends here
