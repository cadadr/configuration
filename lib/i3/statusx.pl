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

    # Hostname
    my $hostname = `hostname -f`; chomp $hostname;

    # Thesis reading progress
    open(my $reading_list, "$ENV{HOME}/Notes/MastersThesis/Notes.org");
    my $progress = '??';
    while(<$reading_list>) {
	if(m/^\* reading list/) {
	    $progress = $_;
	    last;
	}
    }
    unless($progress eq '??') {
	my @bits = split / /, $progress;
	$progress = $bits[3];
	chomp $progress;
    }
    close($reading_list);

    @blocks = (
	{
	    full_text => "$ENV{'USER'}@" . $hostname,
	    name => 'u@h'
	},
	{
	    full_text => "thesis readings: $progress",
	    name => 'thesis_readings_progress'
	},
    @blocks);

    print encode_json(\@blocks) . ",\n";
}
