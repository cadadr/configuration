#!/usr/local/bin/perl
# vortag.pl --- mass-tag ogg files, based of their file names.

use v5.20;
use strict;
use warnings;

sub splinter {
	my ($filnam) = shift || $_;
	my $filnam1 = $filnam;
	$filnam =~ s/\.ogg$//;
	$filnam =~ s/-/ /g;
	$filnam =~ s/\+/'/g;
	my ($names,$extra) = split /=/, $filnam;
	my @authors = split /_/, $names;
	my $song = pop @authors;
	my $artist = join ', ', @authors;
	if($extra) {
		$extra =~ s/_/, /g;
		$song .= " ($extra)";
	}
	say "$filnam: ARTIST=$artist ; TITLE=$song";
	system('vorbiscomment', '-w', $filnam1, '-t', "ARTIST=$artist",
		'-t', "TITLE=$song");
}

foreach(@ARGV) {
	splinter;
}

__END__

=pod

=head1 vortag.pl --- mass-tag ogg files.

usage:

=over

vortag.pl file [file...]

=back

This file name convention is assumed:

=over

(artist-name_)+song-name(=other)?\.ext

=back

Transformation rules are as follows:

=over

=item With the 'other' part:

=over

=item underscore becomes a comma and a space

=item hyphen becomes a space

=item 'other' part is wrapped in parens and appended to song name.

=back

=item With names part:

=over

=item hyphen becomes space

=item plus becomes apostrophe

=back

=back

=cut
