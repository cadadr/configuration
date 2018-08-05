# pkglist2cmds.pl --- turn packages.list into a series of pacman commands

use v5.24;

use strict;
use warnings;
no warnings 'experimental::smartmatch';

open(my $in, '<', 'packages.list');

my @install; my @remove;

for(<$in>) {
    when(/^\+ (.*)/) { push @install, $1 }
    when(/^\- (.*)/) { push @remove, $1 }
}

say 'pacman -Sy --needed ',	join(' ', @install)	if @install;
say 'pacman -R ',		join(' ', @remove)	if @remove;
