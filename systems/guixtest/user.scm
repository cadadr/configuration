;;; user.scm --- packages to be installed for my profile, some customised

(use-modules (gnu)
             (guix download)
             (guix packages)
             (gk guix-packages)
             (srfi srfi-1))

(use-package-modules
 admin aidc aspell astronomy audio backup cmake commencement
 cran disk dns djvu dunst emacs emacs-xyz entr fonts fontutils
 freedesktop gdb gimp gnome gnupg graphviz gtk haskell-xyz
 hunspell inkscape image imagemagick image-viewers javascript
 julia kde kde-frameworks language libreoffice linux m4 mail
 maths moreutils networking password-utils patchutils perl
 pdf photo pv python python-xyz pulseaudio qt ruby sqlite
 statistics suckless tex tmux version-control video
 virtualization vim w3m web web-browsers wm xdisorg
 xfce xorg)

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
   okular audacity gk-qutebrowser cheese gimp gparted inkscape
   mpv stellarium libreoffice xpdf ristretto simple-scan
   ;; tui apps
   lynx mutt tmux w3m
   ;; xorg / desktop utilities
   clipit rofi simplescreenrecorder xclip desktop-file-utils
   xinput xrdb setxkbmap kdeconnect (list redshift "gtk") dunst
   gk-volctl pavucontrol xsettingsd flameshot geoclue
   gnome-themes-standard hicolor-icon-theme breeze-icons
   xsetroot feh xprop imagemagick xauth
   ;; fonts and font utilities
   fontconfig font-gnu-unifont font-gnu-freefont xfontsel
   font-libertinus font-google-roboto font-ubuntu font-terminus
   font-liberation font-google-noto font-dseg font-dejavu
   xlsfonts font-sil-gentium font-sil-charis font-sil-andika
   font-openmoji font-opendyslexic font-inconsolata
   font-comic-neue font-adobe-source-code-pro font-util
   font-mathjax font-alias font-linuxlibertine font-public-sans
   font-blackfoundry-inria font-bitstream-vera kbd
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
   texlive texlive-latex-polyglossia texlive-latex-fontspec
   texlive-microtype hunspell
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
