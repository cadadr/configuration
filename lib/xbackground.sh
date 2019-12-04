# xbackground.sh --- start background applications

compton &			# X compositing
redshift-gtk &
nm-applet &			# network manager applet
pasystray &			# pulse audio sound applet
dunst &				# notifications daemon
pcmanfm -d &			# PCManFM, can auto-mount stuff
( sleep 5 && flameshot; ) &	# screenshots
xfce4-clipman &			# clipboard manager
# Start Emacs daemon
( emacs --daemon && notify-send "Emacs ready!" ) &
# i3wm window switcher (yea need a fuckin daemon for that...)
python3 $MYLIB/i3/focus-last.py &
