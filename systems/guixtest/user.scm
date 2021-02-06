;;; manifest.scm --- packages to be installed for my profile, some customised

(use-modules (gnu)
             (guix download)
             (guix packages)
             (gk guix-packages)
             (srfi srfi-1))

(use-package-modules
 admin aidc aspell astronomy audio backup cmake
 commencement cran disk dns djvu emacs emacs-xyz entr
 freedesktop gdb gimp gnome gnupg graphviz gtk haskell-xyz
 hunspell inkscape julia kde kde-frameworks language libreoffice
 linux m4 mail maths moreutils networking password-utils
 patchutils perl pdf photo pv python python-xyz qt ruby
 sqlite statistics suckless tex tmux version-control video
 virtualization vim w3m web web-browsers wm xdisorg
 xorg)

;; emacs will need libjansson, also check emacs-telega for telegram
(packages->manifest
 (append
  ;; Build dependencies for Emacs.
  (cons* jansson
         (map cadr (package-inputs emacs-next)))
  ;; Build dependencies for Emacs pdf-tools.
  (map cadr (package-inputs emacs-pdf-tools))
  (list
   ;; linguistics
   praat
   ;; desktop apps
   ;; XXX okular
   audacity gk-qutebrowser cheese gimp gparted inkscape
   mpv stellarium libreoffice xpdf
   ;; tui apps
   lynx mutt tmux w3m
   ;; xorg / desktop utilities
   clipit rofi simplescreenrecorder xclip desktop-file-utils
   xinput xrdb setxkbmap
   ;; network utilities
   (list isc-bind "utils") bridge-utils ndisc6
   ;; encryption, privacy & security
   gnupg password-store pwgen
   ;; programming
   gnu-make python python-pip gcc-toolchain ruby bundler cmake gdb
   gfortran-toolchain julia python-setuptools perl m4
   ;; development libraries
   perl-image-exiftool perl-switch perl-universal-isa
   ;; maths, data, statistics & plotting
   gnuplot r r-car r-psych r-rio r-tidyverse sqlite
   units graphviz
   ;; version control (git is installed in guix-system.scm)
   cvs mercurial rcs subversion
   ;; documents & authoring
   djvulibre pandoc pandoc-citeproc pdfarranger stapler
   texlive hunspell
   (specification->package "hunspell-dict-fr-toutesvariantes")
   hunspell-dict-it-it
   hunspell-dict-en hunspell-dict-en-us hunspell-dict-en-gb
   hunspell-dict-en-ca hunspell-dict-en-au
   hunspell-dict-de
   ;; command-line utilities
   entr inotify-tools jq mailutils mb2md moreutils pv
   qrencode youtube-dl vim-full
   ;; e-mail
   mpop msmtp procmail gk-mairix
   ;; virtualisation
   qemu
   ;; system utilities
   smartmontools strace)))

;; The following names are of Debian / Ubuntu packages from old
;; `mergen' config that were not available or necessary under the new
;; GuixSD configuration.

;; missing packages: bmake cpanminus diffpdf fbi feedgnuplot git-cvs
;; git-email krop pdfsam ri hunspell-{el,es} liblocal-lib-perl
;; psychopy

;; omitted packages: calibre dconf-editor devhelp dh-python
;; exuberant-ctags guile ipython3 kdenlive lftp mu4e perlbrew perl-tk
;; racket redis vim-gtk xpra

;; libraries omitted: libcurl4-gnutls-dev libfuse-dev
;; libgirepository1.0-dev libimage-exiftool-perl libjansson-dev
;; libjs-pdf liblocal-lib-perl libpoppler-glib-dev
;; libpoppler-private-dev libreply-perl libssl-dev libswitch-perl
;; libuniversal-isa-perl linux-generic-hwe-18.04 python3-dev
;; python3-dulwich python3-fastimport python3-hglib python3-notify2
;; python3-pip python3-pip python3-pyqt5 python3-pyqt5.qtsvg
;; python3-tk python3-venv python3-wheel

