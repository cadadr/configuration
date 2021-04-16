;;; setup.lisp --- set up Common Lisp environment

#+sbcl (require :sb-posix)

(defun cat (&rest strs)
  (apply #'concatenate 'string strs))

(defvar *common-lisp-dir*
  (cat (sb-posix::getenv "MYLISP") "/cl"))

(defvar *quicklisp-install-dir*
  (namestring
   (truename
    (ensure-directories-exist
     (parse-namestring "~/.quicklisp/")))))

;; Load Quicklisp, after install if missing.
(let ((qlsetup (cat *quicklisp-install-dir* "setup.lisp")))
  (unless (probe-file qlsetup)
    (format t "=> Quicklisp not found, will install...\n")
    (load (cat *common-lisp-dir* "/quicklisp.lisp") :verbose t)
    ;; Adapted from: https://stackoverflow.com/a/40904298. I have no
    ;; idea why this works but calling QUICKLISP-QUICKSTART:INSTALL
    ;; doesn’t..
    (let* ((package (find-package :quicklisp-quickstart))
           (function (unless (null package)
                       (find-symbol (string '#:install)
                                    package))))
      (if (null function)
          (error "=> Can't install quicklisp!")
          (funcall function :path *quicklisp-install-dir*)))
    (format t "=> Installed quicklisp.\n"))
  (load qlsetup)
  (format t "=> Loaded quicklisp.\n"))

