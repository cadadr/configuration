#!/bin/sh
# sync.sh --- sync mailing list archives.

. $MYLIB/fns.sh

### Config:
set -e

LISTS_DIR=$HOME/posta/lists
PARALLEL=2

### GNU lists:

GNU_FTP=ftp://lists.gnu.org
GNU_LISTS="help-gnu-emacs bug-gnu-emacs emacs-devel emacs-orgmode"

gnu_fetch_list(){
    lftp -c "set ftp:list-options -a;
	open $GNU_FTP/;
	lcd $1;
	cd $1;
	mirror --use-cache --verbose --allow-chown --parallel=$PARALLEL"
}

gnu_fetch_all(){
    for list in $GNU_LISTS; do
        say Syncing GNU:$list
        mkdir -p $list
        gnu_fetch_list "$list"
    done
}

### Run:

cd $LISTS_DIR
say In $PWD:

gnu_fetch_all
mairix				# update index

say Finished.
