# install-packages.sh --- install user packages

pkgs_Shells="zsh"

pkgs_EmacsBuildDeps="alsa-lib gnutls libxml2 jansson gpm m17n-lib \
        harfbuzz xorgproto imagemagick dbus hicolor-icon-theme \
        libxinerama libxfixes lcms2 librsvg xaw3d libxrandr"

pkgs_Desktop="xorg xorg-apps xorg-xinit xf86-video-ati xf86-video-amdgpu \
        mesa qtile i3-gaps kitty lm_sensors python-psutil python-setproctitle \
        xterm rofi rofimoji blueman network-manager-applet dex dunst \
        pcmanfm-gtk3 xclip xdotool xpra feh flameshot seahorse picom \
        i3status python-i3ipc redshift xfce4-clipman-plugin i3lock"

# TODO: jack instead of alsa
pkgs_Audio="alsa-utils pulseaudio pulseaudio-alsa pasystray pavucontrol \
        pulseaudio-bluetooth"

pkgs_Fonts="ttf-dejavu gnu-free-fonts noto-fonts ttf-croscore ttf-liberation \
         noto-fonts-cjk noto-fonts-emoji noto-fonts-extra"

pkgs_Services="sane cups bluez networkmanager kdeconnect nss-mdns"

pkgs_Apps="qutebrowser pdfjs okular audacity cheese dconf-editor diffpdf \
        gimp gimp-help-en_gb gparted inkscape mpv neomutt simplescreenrecorder \
        gsmartcontrol stellarium telegram-desktop chromium firefox newsboat \
        simple-scan libreoffice-fresh ristretto quodlibet lmms ardour"

pkgs_Utils="hunspell hunspell-en_us hunspell-en_gb hunspell-it \
        bind-tools dnsutils bridge-utils entr jq mailutils \
        mairix moreutils mpop msmtp openbsd-netcat pandoc \
        pandoc-crossref pass pwgen pdfgrep pdftk procmail \
        qrencode smartmontools sqlite strace traceroute \
        units unrar vorbis-tools w3m whois wkhtmltopdf \
        youtube-dl ghostscript mutt net-tools inetutils ntfs-3g"

pkgs_TeX="texlab biber texlive-most texlive-lang texlive-latexextra \
        texlive-langextra texlive-bibtexextra"

pkgs_Virt="qemu qemu-headless"

pkgs_Backups="borg"

pkgs_Programming="python ruby perl guile sbcl gcc julia r rust \
        python-pip ruby-bundler ctags gdb"

pkgs_BuildUtils="make automake autoconf autoconf-archive cmake \
        pkgconf"

pkgs_VCS="git mercurial cvs rcs quilt subversion darcs"

pkgs_Drivers="v4l2loopback-dkms"

pkgs_Libs="perl-json"

# TODO: psychopy
pkgs_Linguistics=""

pkgs="$pkgs_Desktop $pkgs_Audio $pkgs_Fonts $pkgs_Services $pkgs_Apps \
        $pkgs_Programming $pkgs_BuildUtils $pkgs_Linguistics $pkgs_Utils \
        $pkgs_VCS $pkgs_Backups $pkgs_Virt $pkgs_TeX $pkgs_Shells \
        $pkgs_Libs $pkgs_EmacsBuildDeps"

exec pacman -Syu $pkgs
