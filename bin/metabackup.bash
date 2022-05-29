#!/usr/bin/env bash
# metabackup.bash --- create a metabackup

. $MYLIB/fns2.sh

script_name="$(basename $0)"
script_usage=""

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

source_dir="/run/media/cadadr/cadadr-backup"
target_dir="/run/media/cadadr/igk-attic"
target_name_template="metabackup-%s.tar.zstd"

if [ ! -d "$source_dir" ]; then
    exit_with_error_message 2 "source directory unavailable ($source_dir)"
fi

if [ ! -d "$target_dir" ]; then
    exit_with_error_message 2 "target directory unavailable ($target_dir)"
fi

source_size="$(du --exclude=lost\\+found -sxb $source_dir/ \
                   | awk '{ printf $1 }')"

target_file_path="$(printf $target_dir/$target_name_template $(date +%F))"

if [ -e "$target_file_path" ]; then
    exit_with_error_message 2 "target file exists ($target_file_path)"
fi

message "source directory:	$source_dir"
message "output file:		$target_file_path"

echo

exec tar --create --exclude lost+found --file=- "$source_dir" \
    | pv -s "$source_size"                                    \
    | zstd --quiet - -o "$target_file_path"
