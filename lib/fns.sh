# fns.sh --- common functions

# Generic log function that prints to stdout
say () {
    msg="$0 [$(date)]: $@"
    echo $msg
    if [ "x$GK_NOTIFY" = xyes ]; then
        # IDK why I need to do this but if I use "$@" directly in
        # notify-send command line, it doesn’t work and says invalid
        # number of arguments...
        str="$@"
        notify-send -u normal "$0" "$str"
    fi
}

# Log function for xinit scripts
logx () {
	echo \[$(date)\] $* >> ~/.xinit.errors
}
