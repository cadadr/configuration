#!/usr/bin/env perl
# statusx.pl --- extend i3status output

# Adapted from: https://github.com/i3/i3status/blob/master/contrib/wrapper.pl

use v5.24;
use strict;
use warnings;

use JSON;

# Skip first two lines
print scalar <STDIN>;
print scalar <STDIN>;

while (my ($statusline) = (<STDIN> =~ /^,?(.*)/)) {
    my @blocks = @{decode_json($statusline)};

    my $hostname = `hostname -f`; chomp $hostname;
    @blocks = ({
        full_text => "$ENV{'USER'}@" . $hostname,
        name => 'u@h'
    }, @blocks);

    print encode_json(\@blocks) . ",\n";
}
