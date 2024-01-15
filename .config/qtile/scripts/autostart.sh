#!/usr/bin/env sh

xrandr --output DisplayPort-0 --primary --mode 2560x1440 -r 170 --pos 0x0 --rotate normal --output DisplayPort-1 --mode 1920x1080 -r 170 --pos 2560x0 --rotate left --output DisplayPort-2 --off --output HDMI-A-0 --off &
/usr/bin/emacs --daemon --no-x-resources &
volumeicon &
nm-applet &
dunst &
