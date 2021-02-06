;; guix-packages.scm --- My Guix packages

(define-module (gk guix-packages)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
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

(define-public gk-volctl
  (package
   (name "volctl")
   (version "0.8.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/buzz/volctl/archive/v"
           version
           ".tar.gz"))
     (sha256
      (base32 "1xanqn6s2i6s2n6wysvgz6l58lsdkl3zmzz4hccla01gx7h4yx4f"))))
   (synopsis "Per-application volume control and OSD for Linux desktops")
   (home-page "https://buzz.github.io/volctl/")
   (license gpl2)
   (build-system python-build-system)
   (inputs
    `(("python-pygobject" ,python-pygobject)
      ("gtk+" ,gtk+)
      ("pulseaudio" ,pulseaudio)
      ("python-pyyaml" ,python-pyyaml)
      ("python-pycairo" ,python-pycairo)
      ("python-click" ,python-click)
      ("libxfixes" ,libxfixes)))
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (add-after
        'unpack 'cdll-patch
        ;; Adapted from: https://issues.guix.gnu.org/41134
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute*
           "volctl/lib/pulseaudio.py"
           (("CDLL\\(\"libpulse")
            (string-append "CDLL(\""
                           (assoc-ref inputs "pulseaudio") "/lib/libpulse")))
          (substitute*
           "volctl/lib/xwrappers.py"
           (("libXfixes\\.so")
            (string-append (assoc-ref inputs "libxfixes") "/lib/libXfixes.so"))))))))
   (description "I couldn't find a simple tray icon that allows to
control multiple applications easily from the task bar. So I wrote my
own.

Bug reports and patches welcome!

It's not meant to be an replacement for a full-featured mixer
application. If you're looking for that check out the excellent
pavucontrol.

Features:
- Runs on virtually every desktop environment (Needs to support the
  freedesktop system tray specs)
- Control main volumes as well as individual applications
- Mute individual applications
- Shows application icons and names
- Per-application VU meter
- Double-click opens pavucontrol (or custom mixer application)
- Mouse-wheel support
- On-screen volume display (OSD)")))
