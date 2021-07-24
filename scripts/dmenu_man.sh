#!/usr/bin/sh
# simple dmenu man page script

manual="$(man -k . | awk '{print $1}'| dmenu -p "Manpage:")"
devour st -e man $manual
