#!/bin/sh

_volume_pipe=/tmp/volume-pipe
rm $_volume_pipe ; mkfifo $_volume_pipe

_backlight_pipe=/tmp/backlight-pipe
rm $_backlight_pipe ; mkfifo $_backlight_pipe
