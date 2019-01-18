# git-aliases.sh --- list git aliases

# Adapted from: https://news.ycombinator.com/item?id=18902743

git config --global --get-regexp ^alias 	\
    | sed -E 's/^alias\.(.+) (.*)/\1 \2/' 	\
    | awk '{h=$1; $1=""; printf "%15s	%s\n", h, $0;}'
