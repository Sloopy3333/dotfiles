#!/bin/sh

opt='suspend
lock
poweroff
reboot '

selected="$(printf  "$opt" | rofi -dmenu -p "power options:")"
[ -z $selected ] && exit
confirm="$(echo 'Yes\nNo' | rofi -dmenu -p "$selected ?")"
[ -z $confirm ] && exit
if [ $confirm = "Yes" ]; then
    case $selected in
	lock)
	    slock -m "$(date +"%a %b %d %l:%M %p"| sed 's/  / /g')"
	;;
	*)
	    sudo systemctl $selected
	;;
    esac
fi
