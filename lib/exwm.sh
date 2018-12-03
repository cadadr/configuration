# exwm.sh --- environment to load before running EXWM.

# Disable access control
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window
# manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Let my Emacs config know that it’s being loaded to run EXWM.
export EXWM=yes
