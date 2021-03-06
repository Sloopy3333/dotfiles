#!/bin/sh

#cpu
clock="$(cat /proc/cpuinfo | awk '/MHz/ {print $4;exit;}')"
temp="$(sensors | awk '/Core 0/ {print substr($3,2)}')"

#memory
mem=$(free -m | sed -n '2{p;q}' | awk '{print $3}')

#volume
vol_status="$(amixer sget Master | grep -o -m 1 "\[[a-z]*\]" |  tr -d '%[]')"
vol="$(amixer sget Master | grep -o -m 1 '[[:digit:]]*%' | tr -d '%')"

#backklight
cur_bri="$(cat /sys/class/backlight/intel_backlight/actual_brightness)"
bri_pct="$(expr $cur_bri \* 100 / 120000)"

#clock
date="$(date +"%a %b %d %l:%M %p"| sed 's/  / /g')"

echo "
|  CPU : $clock $temp  \
|  MEM : $mem mb  \
|  VOL : $vol [$vol_status]   \
|  BRI : $bri_pct  \
|  $date  \
|"\
| dzen2 -p 5 -h '30' -w 700 -x 600
