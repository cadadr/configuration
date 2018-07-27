# pkglist2cmds.pl --- turn packages.list into a series of apt-get commands

use v5.24;

use strict;
use warnings;
no warnings 'experimental::smartmatch';

open(my $in, '<', 'packages.list');

my @builddep; my @install; my @remove;

for(<$in>) {
    when(/^\* (.*)/) { push @builddep, $1 }
    when(/^\+ (.*)/) { push @install, $1 }
    when(/^\- (.*)/) { push @remove, $1 }
}

say 'apt-get build-dep -y ',	join(' ', @builddep)	if @builddep;
say 'apt-get install -y ',	join(' ', @install)	if @install;
say 'apt-get remove -y ',	join(' ', @remove)	if @remove;
