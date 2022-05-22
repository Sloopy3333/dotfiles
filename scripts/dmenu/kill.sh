#!/bin/sh

selected="$(ps -a -u $USER | rofi -dmenu -p "select process to kill:" | awk '{print $1" "$4}')"
[ ! -z "$selected" ] && ans="$(echo "Yes\nNO" | rofi -dmenu -p "kill $selected ?")"
[ "$ans" = "Yes" ] && kill -9 "$(echo $selected | awk '{print $1}')"
