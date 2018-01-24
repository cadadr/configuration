;; Guix user

(use-package-modules admin aspell autotools base curl dns emacs gimp gnupg
		     gnuzilla image-viewers libreoffice linux lisp mail python
		     perl ruby password-utils pkg-config ssh suckless tex texinfo
		     version-control video vim xiph xorg)

;; MISSING: bmake clamav spamassassin webalizer vorbisgain aspell-dict-{hy,pt-pt,tr}
;; TODO: cron syncthing emacs
(packages->manifest
 (list
   ;; Desktop:
   twm xterm dmenu xrdb xmodmap setxkbmap xsetroot
   ;; Version control:
   rcs mercurial git cvs subversion
   ;; Programming languages
   python python-pip python-virtualenv perl ruby sbcl
   ;; Emacs:
   emacs-pdf-tools
   ;; Security:
   openssh gnupg pinentry
   ;; Networking & mail:
   icecat curl mailutils (list isc-bind "utils") tcpdump
   ;; Images:
   gimp feh
   ;; Multimedia:
   vorbis-tools
   ;; Dictionaries:
   aspell aspell-dict-it aspell-dict-fr aspell-dict-en aspell-dict-el
   aspell-dict-de aspell-dict-grc aspell-dict-es
   ;; TeX TODO:
   ;; biber
   ;; Documentation:
   texinfo
   ;; Utilities:
   vim gnu-make ntfs-3g password-store automake autoconf pkg-config
   youtube-dl))
 
