# fns.sh --- common functions

# Generic log function that prints to stdout
say () {
    echo $0 [$(date)]: $@
}

# Log function for xinit scripts
logx () {
	echo \[$(date)\] $* >> ~/.xinit.errors
}
