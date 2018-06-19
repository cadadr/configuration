#!/usr/bin/env perl

=pod

=head1 save-vreddit.pl --- save a reddit video

usage:

=over

save-vreddit.pl URL FILE-OR-DIRECTORY

=back

Save a reddit video given a post URL.  If FILE-OR-DIRECTORY is a file,
write to it.  If it's an existing directory, create a new file using
the post title slug and the hash in the URL and write to it.

=cut

use v5.24;

use strict;
use warnings;

use JSON::PP qw(decode_json);
use URI;
use LWP::UserAgent;
use Unicode::Normalize qw(NFC);
use Text::Unidecode qw(unidecode);
use File::Spec::Functions qw(catfile);

sub usage {
    die "$0: usage: save-vreddit.pl URL FILE-OR-DIRECTORY";
}

my $uri = shift @ARGV or usage;
my $file = shift @ARGV or usage;

$uri = URI->new($uri);

my $json_uri = $uri->scheme . "://" . $uri->host . $uri->path . ".json";

my $ua = LWP::UserAgent->new;
my $res = $ua->get($json_uri);

die "connection failure: " . $res->status_line unless $res->is_success ;

my $json = decode_json $res->decoded_content
    or die "decode_json: $!";

my $dat = $json->[0]->{'data'}->{'children'}->[0]->{'data'};

die "not a video!" unless $dat->{'is_video'};

my $title = $dat->{'title'};

my $slug = $title;

# from https://stackoverflow.com/questions/4009281/how-can-i-generate-url-slugs-in-perl
$slug = NFC($slug);
$slug = unidecode($slug);
$slug =~ s/[^\w\s-]//g;
$slug =~ s/^\s+|\s+$//g;
$slug = lc($slug);
$slug =~ s/[-\s]+/-/g;

my $id = $dat->{'id'};

my $sub = $dat->{'subreddit'};

my $media = $dat->{'media'}->{'reddit_video'};
my $media_url = $media->{'fallback_url'};
my $media_is_gif = $media->{'is_gif'};

my $tell = 0;

if(-d $file) {
    $file = catfile($file, "$id-${sub}_$slug.mp4");
    $tell = 1;
}

die "file exists: $file" if -e $file;

{
    my $res = $ua->get($media_url, ':content_file' => $file);
    die "downloading video: " . $res->status_line unless($res->is_success);
}

say "Wrote $file." if $tell;
