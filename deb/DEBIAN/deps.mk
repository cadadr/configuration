# deps.mk --- Authoritative list of dependencies
### Prelude:
DEPS=locales
### Libraries:
DEPS+=libclass-isa-perl libswitch-perl liblockfile-bin libpam-systemd
### Utils:
DEPS+=pass telnet file reportbug lsof gawk vim tmux units	    \
	debian-goodies sudo vrms xz-utils alsa-utils bzip2 less	    \
	sqlite3 zip unzip python3-reportbug groff-base gnupg2 lshw  \
	openssh-client host texinfo cups-bsd cups-client bind9-host \
	sloccount pandoc qemu hugo dirmngr
### Services:
DEPS+=nginx syncthing openssh-server avahi-daemon avahi-autoipd
### Devtools:
DEPS+=automake autoconf autoconf-archive strace build-essential bmake \
	exuberant-ctags gdb
### Laptop:
DEPS+=laptop-mode-tools hdparm
### Desktop & Xorg:
#### Base:
DEPS+=sane sane-utils dbus-x11 feh dunst suckless-tools i3lock	\
	notify-osd libnotify-bin pinentry-gtk2
#### XMonad:
DEPS+=libghc-xmonad-dev libghc-xmonad-contrib-dev libghc-xmonad-doc \
	libghc-xmonad-contrib-doc
#### Xorg:
DEPS+=xorg xserver-xorg-input-all xserver-xorg-video-all xinput	\
	x11-apps
### Fonts:
DEPS+=fonts-freefont-ttf fonts-liberation fonts-dejavu
### Apps:
DEPS+=libreoffice libreoffice-help-en-gb		      \
	libreoffice-help-en-us xclip gimp gparted gnuplot feedgnuplot \
	gv cheese pavucontrol mpv quodlibet simple-scan evince	      \
	vokoscreen qutebrowser chromium transmission
### Misc:
#DEPS+=gnome-session		# On Ubuntu, for normal Gnome 3 session
DEPS+=gnutls-bin cups cups-browsed cups-filters			      \
	debian-archive-keyring memtest86+ pciutils netcat-traditional \
	krb5-locales bash-completion ncurses-term mime-support	      \
	pulseaudio
### Libraries:
DEPS+=libxrandr-dev
### Documentation:
DEPS+=autoconf-doc ffmpeg-doc mailutils-doc nginx-doc perl-doc	   \
	python3-doc sbcl-doc gimp-help-en git-doc gnutls-doc	   \
	doc-debian manpages man-db debian-faq gnuplot-doc bash-doc \
	linux-doc glibc-doc glibc-doc-reference manpages-dev	   \
	hyperspec doc-base make-doc
### Emacs:
DEPS+=libjansson-dev		# Jansson is for faster JSON.
DEPS+=gnutls-dev
### Internet tools:
DEPS+=bind9utils net-tools curl phantomjs corebird dnsutils w3m	\
	w3m-el webalizer wget rsync traceroute finger whois
#### VPN:
DEPS+=openvpn resolvconf
### VCS:
DEPS+=mercurial cvs git subversion rcs python-hglib python3-hglib \
	python-dulwich python-fastimport git-cvs gitk quilt
### Mail:
DEPS+=clamav mailutils spamassassin spamc mpop msmtp mairix procmail
### Multimedia:
DEPS+=vorbisgain youtube-dl vorbis-tools
### TeX:
# TeX is huge.  That's mostly because of the idiotically stupid
# documentation that it comes with: docs in PDF files.  Here we take
# the dependencies of texlive-full, remove the docs, and give
# languages a separate place, for easier editing.  And that's not
# enough, also, ‘apt-get install’ should be run with the
# --no-install-recommends flag; because most TeX packages recommend
# the related -doc packages.

#  I do not only wonder what kind of braindead has decided to publish
# docs in PDF, but also the braindead who made it a hard dependency of
# texlive-full.  Crazy waste of space and bandwidth...

#### Specifically installed packages:
DEPS+=

#### Stripped texlive-full:
DEPS+=asymptote biber chktex cm-super context dvidvi dvipng feynmf \
	fragmaster info lacheck latex-cjk-all latexdiff latexmk	   \
	lcdf-typetools lmodern prerex prosper psutils purifyeps	   \
	t1utils tex-gyre texinfo texlive-base texlive-bibtex-extra \
	texlive-binaries texlive-extra-utils texlive-font-utils	   \
	texlive-fonts-extra texlive-fonts-recommended		   \
	texlive-formats-extra texlive-games texlive-generic-extra  \
	texlive-generic-recommended texlive-htmlxml		   \
	texlive-humanities texlive-latex-base texlive-latex-extra  \
	texlive-latex-recommended texlive-luatex texlive-metapost  \
	texlive-music texlive-omega texlive-pictures		   \
	texlive-plain-extra texlive-pstricks texlive-publishers	   \
	texlive-science texlive-xetex tipa

#### Languages:
DEPS+=texlive-lang-arabic texlive-lang-cyrillic			      \
	texlive-lang-czechslovak texlive-lang-english		      \
	texlive-lang-european texlive-lang-french texlive-lang-german \
	texlive-lang-greek texlive-lang-italian texlive-lang-other    \
	texlive-lang-polish texlive-lang-portuguese		      \
	texlive-lang-spanish

### Programming languages:
#### Lisp:
DEPS+=guile-2.0 guile-2.0-doc guile-json guile-library sbcl
#### Python:
DEPS+=python-minimal python python3 python3-pip virtualenv	\
	python3-venv python-flup python3-tk python3-matplotlib
#### Perl:
DEPS+=perl perl-modules-5.24 perlbrew cpanminus liblocal-lib-perl
#### Ruby:
DEPS+=ruby bundler ri 

### Rules:
list-deps:
	@echo $(DEPS)
