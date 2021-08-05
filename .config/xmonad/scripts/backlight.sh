#!/bin/sh

get_bri() {
cur_bri="$(cat /sys/class/backlight/intel_backlight/actual_brightness)"
bri_pct="$(expr $cur_bri \* 100 / 120000)"
}

get_bri
case $1 in
  "up")
    xbacklight -inc +2 >/dev/null
    ;;
  "down")
    xbacklight -dec +2- > /dev/null
    ;;
  *)
    exit 0
    ;;
esac
