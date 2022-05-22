#!/usr/bin/sh

manual="$(man -k . | awk '{print $1}'| rofi -dmenu -p "Manpage:")"
st -e man $manual
