#!/usr/bin/env perl
# qute.pl --- starter script for Qutebrowser.

# This script just starts qutebrowser with the basedir set to my own
# base directory, and passes the rest of the command line arguments to
# the qute browser itself.  The Qute binary is exec'ed at the end of
# the script.

use strict;
use warnings;
use v5.24;

my $qutedir = "$ENV{HOME}/.qute.d";
my $qutebin = "qutebrowser";

exec $qutebin, "--basedir", $qutedir, @ARGV or die "exec: $qutebin";
