#!/bin/bash
cap="$(cat /sys/class/power_supply/BAT1/capacity)"
status="$(cat /sys/class/power_supply/BAT1/status)"
estimated="$(acpi -b | grep -E 'remaining|until' | awk '{print $5}')"
if [ $status = "Charging" ]; then
   state="AC"
fi
if [ $status = "Full" ]; then
   state="Full"
fi
if [ $status = "Discharging" ]; then
    state="BAT"
fi
#echo -e "$state $cap ($estimated)"
printf "%d%% [%s (%s)]" "$cap" "$state" "$estimated"
