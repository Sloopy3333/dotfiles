#!/bin/sh

selected="$(rbw list | sort | dmenu)"
opt=$(echo "username\npassword" | dmenu -p "$selected")

case $opt in
    "username")
	echo "$(rbw ls --fields=name,user)" | awk -v pat="$selected" '$0 ~ pat {print $2}'| xclip -r -selection clipboard

	exit
	;;
    "password")
	echo "$(rbw get $selected)" | xclip -r -selection clipboard
	exit
	;;
esac
