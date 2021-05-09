#!/bin/sh

opt='suspend
lock
poweroff
reboot '

selected="$(printf  "$opt" | dmenu -p "power options:")"
[ -z $selected ] && exit
confirm="$(echo 'Yes\nNo' | dmenu -p "$selected ?")"
[ -z $confirm ] && exit
if [ $confirm = "Yes" ]; then
    case $selected in 
	lock)
	    slock -m "$(date +"%a %b %d %l:%M %p"| sed 's/  / /g')"
	;;
	*)
	    systemctl $selected
	;;
    esac
fi

