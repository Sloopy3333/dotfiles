#!/usr/bin/sh

manual="$(man -k . | awk '{print $1}'| rofi -dmenu -p "Manpage:")"
devour xterm -e man $manual
