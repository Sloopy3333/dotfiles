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

#battery
cap="$(cat /sys/class/power_supply/BAT1/capacity)"
status="$(cat /sys/class/power_supply/BAT1/status)"
estimated="$(acpi -b | grep -E 'remaining|until' | awk '{print $5}')"
bat_status="BAT"
[ $status = "Charging" ] && bat_status="AC"

#clock
date="$(date +"%a %b %d %l:%M %p"| sed 's/  / /g')"

echo "
|  CPU : $clock Mhz $temp  \
|  MEM : $mem mb  \
|  VOL : $vol [$vol_status]   \
|  BRI : $bri_pct  \
|  $bat_status $cap ($estimated)  \
|  $date  \
|"\
| dzen2 -p 5 -h '20' -w 900 -x 500
