;; guix-packages.scm --- My Guix packages

(define-module (gk guix-packages)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (srfi srfi-1))

(define-public gk-qutebrowser
  (package
   (inherit qutebrowser)
   (version "2.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/qutebrowser/"
                         "qutebrowser/releases/download/v"
                         version "/qutebrowser-" version
                         ".tar.gz"))
     (sha256
      (base32 "0fxkazz4ykmkiww27l92yr96hq00qn5vvjmknxcy4cl97d2pxa28"))))
   (inputs
    (cons* (list "python-importlib-resources" python-importlib-resources)
           (alist-delete "python-pypeg2"
                         (package-inputs qutebrowser))))))

(define-public gk-mairix
  ;; bison flex gcc make
  (package
   (name "mairix")
   (version "0.24")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/rc0/mairix/archive/"
           version
           ".tar.gz"))
     (sha256
      (base32 "1v7agakz7p48g4v2hpv58l1hlba9s8s6gv306aa7r99n75vy3pvq"))))
   (native-inputs
    `(("bison" ,bison)
      ("flex" ,flex)))
   (inputs
    `(("bzip2" ,bzip2)
      ("gzip" ,gzip)))
   ;; The build system is gnu-like but looks like handmade.  Why tf?
   (build-system gnu-build-system)
   (arguments
    '(#:phases (modify-phases %standard-phases
                 (replace 'configure
                   (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "./configure"
                              (string-append "--prefix="
                                             (assoc-ref %outputs "out"))
                              "--enable-bzip-mbox"
                              "--enable-gzip-mbox")
                      #t))
                 (replace 'check
                   (lambda* (#:key outputs #:allow-other-keys)
                     (invoke "make" "check")
                     #t)))))
   (home-page "http://www.rc0.org.uk/mairix")
   (license gpl2)
   (synopsis "search email messages stored in Maildir, MH or mbox folders")
   (description "mairix is a program for indexing and searching email
messages stored in Maildir, MH, MMDF or mbox folders.

* Indexing is fast.  It runs incrementally on new messages - any
particular message only gets scanned once in the lifetime of the index
file.

* The search mode populates a \"virtual\" folder with symlinks(*)
which point to the real messages.  This folder can be opened as usual
in your mail program.

* The search mode is very fast.

* Indexing and searching works on the basis of words.  The index file
tabulates which words occur in which parts (particular headers + body)
of which messages.

The program is a very useful complement to mail programs like mutt
(http://www.mutt.org/, which supports Maildir, MH and mbox folders)
and Sylpheed (which supports MH folders).

The original author of mairix is Richard P. Curnow.  It is maintained
since 2017 by Kim Vandry.")))
