;;; manifest.scm --- packages to be installed for my profile, some customised

(use-modules (guix download)
             (guix packages)
             (srfi srfi-1))
(use-package-modules mail web-browsers python python-xyz qt)

(define gk-qutebrowser
  (package
   (inherit qutebrowser)
   (version "2.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/qutebrowser/"
                         "qutebrowser/releases/download/v"
                         version "/qutebrowser-" version
                         ".tar.gz"))
     (patches (list "qutebrowser201sip.patch"))
     (sha256
      (base32 "0hmj19bp5dihzpphxz77377yfmygj498am0h23kxg5m3y5hqv65a"))))
   (inputs
    (cons* (list "python-importlib-resources" python-importlib-resources)
           (alist-delete "python-pypeg2"
                         (package-inputs qutebrowser))))))

(packages->manifest
 (list
  gk-qutebrowser python))
