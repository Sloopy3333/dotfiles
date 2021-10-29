#!/bin/bash
cap="$(cat /sys/class/power_supply/BAT1/capacity)"
status="$(cat /sys/class/power_supply/BAT1/status)"
estimated="$(acpi -b | grep -E 'remaining|until' | awk '{print $5}')"
if [ $status = "Charging" ]; then
   state="AC"
      # if [ $cap -ge 95 ] ;then
      # notify-send "battery $cap"
    #fi
fi
if [ $status = "Discharging" ]; then
    state="BAT"
      # if [ $cap -le 40 ] ;then
      # notify-send "battery $cap"
    #fi
fi
#echo -e "$state $cap ($estimated)"
printf "%d%% [%s (%s)]" "$cap" "$state" "$estimated"
