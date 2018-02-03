# deps.mk --- Authoritative list of dependencies
### Prelude:
DEPS=
### Libraries:
DEPS+=libclass-isa-perl libswitch-perl liblockfile-bin libpam-systemd
### Utils:
DEPS+=pass telnet file reportbug lsof gawk vim tmux units		\
	debian-goodies sudo vrms xz-utils alsa-utils bzip2 less		\
	sqlite3 zip unzip python3-reportbug apt-listchanges		\
	groff-base gnupg2 lshw openssh-client bzip2 host texinfo	\
	cups-bsd cups-client bind9-host
### Services:
DEPS+=nginx syncthing openssh-server avahi-daemon avahi-autoipd
### Devtools:
DEPS+=automake autoconf autoconf-archive strace build-essential bmake	\
	ctags gdb
### Laptop:
DEPS+=laptop-mode-tools hdparm
### Desktop & Xorg:
DEPS+=xorg xserver-xorg-input-all xserver-xorg-video-all sane sane-utils\
	dbus-x11 xinput feh dunst
### Fonts:
DEPS+=fonts-freefont-ttf fonts-liberation fonts-dejavu
### Apps:
DEPS+=firefox libreoffice libreoffice-help-en-gb			\
	libreoffice-help-en-us xclip gimp gimp-help-en gparted		\
	gnuplot gnuplot-doc feedgnuplot gv cheese pavucontrol x11-apps	\
	vtwm i3-wm i3status suckless-tools i3lock mpv quodlibet		\
	eog eog-plugins volti simple-scan
### Misc:
#DEPS+=gnome-session		# On Ubuntu, for normal Gnome 3 session
DEPS+=gnutls-bin cups cups-browsed cups-filters				\
	debian-archive-keyring memtest86+ pciutils netcat-traditional	\
	krb5-locales bash-completion ncurses-term mime-support		\
	pulseaudio
### Libraries:
DEPS+=libxrandr-dev
### Documentation:
DEPS+=autoconf-doc ffmpeg-doc mailutils-doc nginx-doc perl-doc		\
	python3-doc sbcl-doc gimp-help git-doc gnutls-doc doc-debian	\
	manpages man-db debian-faq
### Emacs:
DEPS+=libjansson-dev		# Jansson is for faster JSON.
DEPS+=gnutls-dev
### Internet tools:
DEPS+=bind9utils net-tools curl phantomjs corebird dnsutils w3m w3m-el	\
	webalizer wget rsync traceroute finger
### VCS:
DEPS+=mercurial cvs git subversion rcs python-hglib python3-hglib	\
	python-dulwich python-fastimport git-cvs gitk
### Mail:
DEPS+=clamav mailutils spamassassin spamc
### Multimedia:
DEPS+=vorbisgain youtube-dl vorbis-tools
### TeX:
DEPS+=biber texlive-xetex texlive-full texlive-publishers 		\
	texlive-bibtex-extra
### Programming languages:
#### Lisp:
DEPS+=guile-2.0 guile-2.0-doc guile-json guile-library sbcl
#### Python:
DEPS+=python-minimal python python3 python3-pip virtualenv python3-venv	\
	python-flup python3-tk python3-matplotlib
#### Perl:
DEPS+=perl perl-modules-5.24 perlbrew cpanminus liblocal-lib-perl
#### Ruby:
DEPS+=bundler ri ruby

### Rules:
list-deps:
	@echo $(DEPS)
