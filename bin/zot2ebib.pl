#!/usr/bin/env perl
# zot2ebib.pl --- convert Zotero exports to my Ebib setup

use v5.30;

use strict;
use warnings;

use feature qw(signatures);

use Data::Dumper;
use File::Basename qw(dirname fileparse);
use File::Compare;
use File::Copy;
use File::Path qw(make_path);
use Getopt::Long;
use Path::Tiny;
use Text::BibTeX (':nodetypes');

sub usage {
    say "usage: zot2ebib.pl INFILE ATTACHDIR [> OUTFILE]";
    exit 2;
}

my $verbose = 0;

GetOptions ("verbose|v" => \$verbose)
    or die "command line error: $!";

my $finnam = shift @ARGV or usage;
my $attachdir = shift @ARGV or usage;

unless (-d $attachdir) {
    make_path $attachdir or die "create directory: $!";
}

sub nag { say STDERR @_; }
sub annoy { nag if $verbose; }

sub uniquify {
    my ($realpath, $dir, $citekey, $suffix) = @_;

    my $candidate =
        Path::Tiny::path($attachdir)->child($citekey . $suffix);

    # If file exists and is identical, use it.
    if (-f $candidate && (compare($candidate, $realpath) == 0)) {
        annoy "file exists, is identical, skipping: $candidate (source: $realpath)";
        return $candidate;
    }
    # If file exists and is different, uniquify name.
    if (-f $candidate) {
        nag "file exists: $candidate; duplicate key or multiple files of same type? ($citekey)";
        my $n = 1;
        do {
            my $p = "${citekey}_$n$suffix";
            $candidate = Path::Tiny::path($attachdir)->child($p);
            $n++;
        } while (-f $candidate);
        nag "deduplicated to $candidate ($citekey)";
    }

    return $candidate;
}

sub maybe_copy {
    my ($realpath, $citekey) = @_;
    my ($onam, $odir, $suffix) = fileparse($realpath, qr/\.[^.]*/);
    my $destpath = uniquify $realpath, $attachdir, $citekey, $suffix;

    copy($realpath, $destpath) or
        die "copy $realpath -> $destpath: $!";

    return $destpath;
}

sub maybe_rename_and_copy {
    my ($entry, $file_field) = @_;
    my $citekey = $entry->key;
    my @fils = split /;/, $file_field->value(0)->text;
    my %new_fils;

    foreach my $f (@fils) {
        # we use ‘lookbehind’ because a match like ‘...pdf:...’
        # produces ‘...pd’ otherwise.
        my ($alias, $path, $mimetype) = split /(?<=[^\\]):/, $f;
        # relativise path
        my $realpath;
        if (defined($path) && $path =~ m{^/}) {
            $realpath = $path
        } elsif (defined($path)) {
            $realpath = Path::Tiny::path(
                dirname($finnam))->child($path)->stringify;
        } else {
            die "unexpected file name: $f (did you export with *Better* Bib(La)TeX?)";
        }

        # File not found, so print entry with comment stating that.
        unless (-e $realpath) {
            nag "file not found: $realpath (entry: $citekey)";
        }
        # File exists.  Copy it to a proper location and create
        # updated ‘file’ field.
        else {
            my $newpath = maybe_copy($realpath, $citekey);
            $new_fils{$mimetype} = $newpath;
        }
    }

    return \%new_fils;
}

my $fin = Text::BibTeX::File->new($finnam) or die "open $finnam: $!";
$fin->preserve_values(1);      # preserve whitespace, etc.

while (my $entry = Text::BibTeX::Entry->new($fin))
{
    next unless $entry->parse_ok;

    my $fil = $entry->get("file");

    if (defined $fil) {
        my $new_files = maybe_rename_and_copy($entry, $fil);
        # Put the PDF file first.
        my $maybe_pdf = $new_files->{"application/pdf"};
        delete $new_files->{"application/pdf"} if defined $maybe_pdf;
        my @vals = values %{$new_files};
        unshift(@vals, $maybe_pdf) if defined $maybe_pdf;
        my $new_field = join "; ", @vals;
        $entry->set("file", $new_field);
    }

    say $entry->print_s;

}

$fin->close()

__END__

=pod

=encoding utf8

=head1 zot2ebib.pl --- convert Zotero exports to my Ebib setup

=over

zot2ebib.pl [-v|--verbose] INFILE ATTACHDIR [> OUTFILE]

=back

=head2 Flags

=over

-v|--verbose

=over

Verbose reporting.

=back

=back

=head2 Arguments

=over

INFILE

=over

Input BibTeX/BibLaTeX file.

=back

=back

=over

ATTACHDIR

=over

Directory to copy attachments under

=back

=back

=over

OUTFILE

=over

zot2ebib.pl writes its Bib(La)TeX output to standard out, you need to
use shell redirection in order to capture it.

=back

=back

=head2 Operation

zot2ebib.el is a simple Perl 5 script I wrote while moving from Zotero
to Ebib.  Its main function is to copy attachments of collections
exported from Zotero to a flat directory structure, renaming files
using the citekeys of entries, and generating and updated Bib(La)TeX
file.

Using this scripts requires some preparation.

First, in Zotero, make sure all items in the collections to be
processed have parent items.  This is important, because if they
don’t, they won’t appear in the Bib(La)TeX file Zotero exports, and
thus will be ignored by this script.

=over

Caution!

This scripts excepts a vanilla BibTeX or BibLaTeX export from Zotero
itself, do *not* use the Better BibTeX’s export options if you have
that extension installed.

=back

After that’s done, export the collection to some known location, for
example ~/Bibliography.  Then, go to that directory, and run
zot2ebib.pl as follows:

=over

$ cd ~/Bibliography
$ zot2ebib.pl example/example.bib Attachments > Example.bib

=back

With the above command, zot2ebib.pl will read "example/example.bib",
and copy files it references under "Attachments".  The command will
write the updated Bib(La)TeX file to the standard output, which the
above command line redirects to "Example.bib". The files will be
renamed as follows:

=over

<citekey>(_N)?.<suffix>

=back

<citekey> is the identification key of each bibliography entry.  I
suggest you use Better BibTeX extension for Zotero to generate
concise, unique citekeys in Zotero before exporting.

"_N" stands for a literal underscore plus a number.  This part is only
added when an entry refers to multiple files of the same type, and
serves to generate a unique file name for each of those.

<suffix> is the file type suffix of the original file.

After the conversion step, it’s opportune to check using diff(1) to
verify that nothing was broken.  A command like below should reduce
the noise in diffs and make them workable:

=over

$ diff -wu example/example.bib Example.Bib

=back

This script does not modify neither the Bib(La)TeX file it reads
(INFILE) nor the files that it refers to.

=cut
