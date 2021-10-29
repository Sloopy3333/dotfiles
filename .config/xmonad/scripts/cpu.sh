#!/bin/sh

clock="$(cat /proc/cpuinfo | awk '/MHz/ {print $4;exit;}')"
temp="$(sensors | awk '/Core 0/ {print substr($3,2)}')"
printf "CPU %0.0f MHz %s" "$clock" "$temp"
