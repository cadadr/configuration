#!/usr/bin/env bash
# pdfdeannot.bash --- remove annotations from PDF files

# Adapted from: https://stackoverflow.com/a/49614525

infil="$1"
outfil="$2"
name="$(basename $0)"

usage="$name: usage: $name INFILE OUTFILE"

[ -n "$infil" ] || {
    echo "$name: error: no input file specified"
    echo
    echo "$usage"
    exit 2
}

[ -n "$outfil" ] || {
    echo "$name: error: no output path specified"
    echo
    echo "$usage"
    exit 2
}

[ -f "$infil" ] || {
    echo "$name: file not found: $infil"
    echo
    echo "$usage"
    exit 2
}

[ -f "$outfil" ] && {
    echo "$name: error: $outfil exists, will not override"
    echo
    echo "$usage"
    exit 2
}

tmp1="$(mktemp --tmpdir --suffix .pdf ${name}_XXXXXXXXXX)"
tmp2="$(mktemp --tmpdir --suffix .pdf ${name}_XXXXXXXXXX)"

pdftk "$infil" output "$tmp1" uncompress

LANG=C sed -n '/^\/Annots/!p' "$tmp1" > "$tmp2"

pdftk "$tmp2" output "$outfil" compress
