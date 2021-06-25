#!/bin/sh

_volume_pipe=/tmp/volume-pipe
[ ! -f $_volume_pipe ] && mkfifo $_volume_pipe

_backlight_pipe=/tmp/backlight-pipe
[ ! -f $_backlight_pipe ] && mkfifo $_backlight_pipe
