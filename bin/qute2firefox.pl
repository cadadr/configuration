#!/usr/bin/env perl

=pod

=head1 qute2firefox.pl --- Convert Qutebrowser bookmarks/quickmarks to HTML

=over

usage: qute2firefox.pl [DIRECTORY]

=back

This utility converts Qutebrowser bookmarks/quickmarks to a "Netscape
Bookmark File", which is an HTML file that Firefox or Chrom(e|ium) can
import.

The optional command line argument DIRECTORY specifies the root of the
qutebrowser configuration tree, which is by default at
~/.config/qutebrowser/ (the script respects XDG_HOME_DIR if set).

The resulting HTML is output to the standard output stream, which one
can capture into a file using Unix shell input redirection.

If you use a custom base directory with Qutebrowser (e.g. use the
--basedir command line argument), point this utility to the ‘config’
directory under that tree.

=cut

use v5.24;
use strict;
use warnings;

# Find the qute directory and chdir to it.
my $xdg = $ENV{XDG_HOME_DIR} || $ENV{HOME} . "/.config";
my $qutedir = shift @ARGV || "$xdg/qutebrowser/";
chdir $qutedir or die "can not cd to $qutedir: $!";

# We have two files:
my $quickmarks_file = "quickmarks";
my $bookmarks_file = "bookmarks/urls";

# The format for $bookmarks_file is:
#
# URL <space> TITLE
#
# whereas the format for $quickmarks_file is:
#
# TITLE(can include spaces) <space> URL


# Parse a given line in the bookmark format.
sub read_bookmark {
    my $line = shift || $_;
    $line =~ /^([^ ]*) (.*)/;
    return {url => $1, title => $2}
}

# Will collect the URLs and titles into this array from both files.
my @shortcuts = ();

open(my $bookmarks_fh, "<", $bookmarks_file)
    or die "open < $bookmarks_file: $!";

open(my $quickmarks_fh, "<", $quickmarks_file)
    or die "open < $quickmarks_file: $!";

while(<$bookmarks_fh>) { chomp; push(@shortcuts, read_bookmark); }

# Hack: we can reuse read_bookmark if we reverse the lines from
# quickmarks, then reverse the resulting strings.
while(<$quickmarks_fh>) {
    chomp;
    $_ = scalar reverse;
    my $s = read_bookmark;
    my ($u, $t) = (scalar reverse($s->{url}),
		   scalar reverse($s->{title}));
    push(@shortcuts, {url => $u, title => $t});
}

# We're now done parsing.  We will emit a "Netscape Bookmark File".  The
# spec is at:
# https://docs.microsoft.com/en-us/previous-versions/windows/internet-explorer/ie-developer/platform-apis/aa753582(v=vs.85)

my $nbf_head = <<"EOT";
<!DOCTYPE NETSCAPE-Bookmark-file-1>
<!-- This is an automatically generated file.
     It will be read and overwritten.
     DO NOT EDIT! -->
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<Title>Bookmarks</Title>
<H1>Bookmarks</H1>
<DL>
  <DT><H3 FOLDED>Qutebrowser Import (qute2firefox.pl)</H3>
  <DL><p>
EOT

my $nbf_foot = "  </DL><p>\n</DL>";

# We convert a shortcut to an item, which in the format is called a
# "shortcut.".  The format is:
# <DT><A HREF="{url}" ADD_DATE="{date}" LAST_VISIT="{date}"
#        LAST_MODIFIED="{date}">{title}</A>

sub shortcut_to_nbf_item {
    my $s = shift || $_;
    my $u = $s->{url};
    my $t = $s->{title};
    return "<DT><A HREF=\"$u\">$t</A>";
}

# We've completed all the steps required for the conversion.  Now we can
# compose and print out the bookmarks file ready to be imported by
# Firefox or Chrom(e|ium).

chomp $nbf_head;
say $nbf_head;
for(@shortcuts){
    say "    ", shortcut_to_nbf_item;
}
say $nbf_foot;
