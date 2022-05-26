#!/bin/sh
# fns2.sh --- common functions, improved

# This library is the successor to $MYLIB/fns.sh, whose functions are
# not fit for interactive scripts.

# After sourcing this, set parameters as such:
# - script_name="$(basename $0)"
# - script_usage="[ARGSLIST]"

# Definitions with the prefix ‘fn2_’ prefix are internal.

fn2_errcode_usage=1

fns2_make_timestamp_prefix () {
    echo "$script_name [$(date)]: $@"
}

# usage: log MESSAGE...
log () {
    echo "$(fns2_make_timestamp_prefix)" "$@"
}

# usage: exit_with_error_log ERROR_CODE MESSAGE...
exit_with_error_log () {
    errcode=$1
    shift
    log "$@" >&2
    exit $errcode
}

# usage: message MESSAGE...
message () {
    echo "$@"
}

# usage: exit_with_error_message ERROR_CODE MESSAGE...
exit_with_error_message () {
    errcode=$1
    shift
    message="$@"
    message "$script_name: error: $message" >&2
    exit $errcode
}

# usage: exit_with_usage
exit_with_usage () {
    message "$script_name: usage: $script_usage"
    exit $fn2_errcode_usage
}
