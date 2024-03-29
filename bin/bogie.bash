#!/usr/bin/env bash
# bogie.bash --- trainspotting with mpv

### TODOs:

# TODO(2021-11-10): prevent mpv for video playlist from stealing
# window focus

### Usage:

# This Bash script plays a little video on the top-right corner of the
# screen and an audio stream in the background.

# The intended use is to play a playlist of videos, of trains or not,
# in the corner, and in the background a music playlist.

# Dependencies: bash, mpv
# Optionally: youtube-dl or yt_dlp

# The default video playlist is the ‘Ferrovie Internationali Cadadr’
# playlist on YouTube, which is a bunch of no-talk train commute
# videos, and the default audio stream, or ‘radio’ is the famous "lofi
# hip hop radio - beats to relax/study to" live stream by the Lofi
# Girl channel.

# The video playlist can be controlled by the BOGIE_PLAYLIST
# environment variable, and the radio can be controlled similarly by
# BOGIE_RADIO.

# Instead of a YouTube url, the video playlist can also be set to some
# local directory, video file, or playlist, eg

#    BOGIE_PLAYLIST=~/Videos/trains

# mpv(1) doesn’t mind, so it’s okay.  You can download eg the default
# playlist to a known location so that it doesn’t need to be
# downloaded all the time.

# Same goes for the radio: you can set it to a local file, playlist or
# directory

#    BOGIE_RADIO=~/Music/lofi

# Additionally, you can set BOGIE_RADIO=off to turn off music.

# Finally, if you just want the audio from BOGIE_PLAYLIST, set:

#    BOGIE_NOVIDEO=yes

# The script will wait on the two mpv(1) processes it spawns, and it
# also catches signals SIGINT (Ctrl+C) and SIGTERM so that when it
# exits, both mpv processes are killed.

### Bash strict mode:
set -euo pipefail
IFS=$'\n\t'

### Defaults:

# The ‘Ferrovie Internazionali Cadadr’ playlist on YouTube
default_playlist="https://www.youtube.com/playlist?list=PLN4x4CnYyRYq-3VC6Kyl0_H5GkeeieGUC"

# "lofi hip hop radio - beats to relax/study to" by the Lofi Girl
# channel on YouTube
default_radio="https://www.youtube.com/watch?v=jfKfPfyJRdk"

### Actual config:

playlist="${BOGIE_PLAYLIST-$default_playlist}"
radio="${BOGIE_RADIO-$default_radio}"
# If audio only requested, set $audio_only.  That is the default value
# of $video_args, but if unset, alternative is the args for the video
# setup.  See bash(1) for this abuse of parameter expansions.
audio_only="${BOGIE_NOVIDEO:+--no-video}"
video_args="${audio_only:=--geometry=320x180-0+0 --no-border --title=bogie-trainvids --ontop}"

### Start streams:

# Start videos at half Wide 360p resolution, at the top-right corner
# of the workspace.
echo video source: $playlist
eval "mpv $video_args \
    --ytdl-format='bestvideo[height<=360]+bestaudio/best[height<=360]' \
    --shuffle '$playlist'" >/dev/null 2>/dev/null &

# Collect PIDs to wait on.
mpv_pids="$!"

if [ ! "$radio" = "off" ]; then
    # ‘--shuffle’ is useless with the default but useful if it’s a
    # playlist or a directory.
    echo radio: $radio
    mpv --ytdl-format='worstvideo+bestaudio/best' --volume=70 \
        --shuffle --no-video "$radio" >/dev/null 2>/dev/null &
    mpv_pids="$! $mpv_pids"
fi

echo "mpv PIDs: $mpv_pids"

### Postamble:

# Kill mpv processes when we die.
trap "kill $mpv_pids" SIGINT SIGTERM

eval "wait $mpv_pids"

