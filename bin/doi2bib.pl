#!/usr/bin/env perl
# doi2bib.pl --- turn a list of DOIs into a .bib file.

=pod

=head1 doi2bib.pl --- turn a list of DOIs into a .bib file.

usage: doi2bib.pl DOI [DOI...]

For each command line argument, the relevant BibTeX data is fetched
from https://doi.org, and printed one after another, with an empty
line between them, to the standard output.  Duplicate DOIs are
skipped.

=cut

# This was inspired by https://github.com/davidagraf/doi2bib, but is
# way more compact.

use strict;
use warnings;
use v5.24;
use File::Basename qw(basename);
use HTTP::Tiny;
use List::MoreUtils qw(uniq);

my $progname = basename $0;
my $usage = "usage: $progname DOI [DOI...]";

unless (1 <= @ARGV) { say $usage; exit 1 }

my $accept = 'application/x-bibtex; charset=utf-8';
my $doiurl = 'https://doi.org/';

sub bail { say @_; exit 1 }

sub fetch {
    my $doi = shift;
    my $http = HTTP::Tiny->new;
    my $uri = $doiurl . $doi;
    my %headers = ( 'Accept' => $accept );
    my %options = ( 'headers' => \%headers );
    my $response = $http->get($uri, \%options);

    if ($response->{success}) {
        print $response->{content};
    } else {
        warn "could not fetch $uri: $response->{status}: $response->{reason}";
    }
}

my @dois = uniq @ARGV;

my $i;
while (($i, $_) = each @dois) {
    chomp;
    s/^ doi: | (https?:\/\/)? doi\.org\/ //x;
    # http://www.doi.org/doi_handbook/2_Numbering.html
    bail "Malformed DOI: $_" unless /^ 10 (\.\d+)+ \/ .+ /x;
    fetch $_;
    print "\n";
    print "\n" unless $i == $#dois; # don't output a trailing empty line.
}
