# rc.zsh --- zsh specific shell setup

# Part adapted from the output of zsh-newuser-install.

HISTFILE=~/.zhistfile
HISTSIZE=100000
SAVEHIST=10000

setopt appendhistory beep extendedglob nomatch notify
unsetopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/g/.zshrc'

autoload -Uz compinit
compinit

# Prompt
# ======
# 0(?/A/B): if the exit code of last command is 0, A, else, B
# B/b: boldface start/stop
# S/s: standout start/stop
# F/f: foreground colour start/stop
# K/k: background colour start/stop
# ?: exit code of last shell command
# !: current history event number
# D: date
# *: time with seconds
# n: username
# M: host FQDN
# ~: current working directory with tilde substitution
# j: number of jobs
# L: $SHLVL
# #: `#' for root, `%' otherwise
export prompt="%0(?//%K{red}%F{black}<%?>%f%k)%B[%!; %D %*] %b%S %n@%M:%~ %s%B [%j/%L]%#>%b "

