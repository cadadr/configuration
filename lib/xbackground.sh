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

