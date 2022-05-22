#!/bin/bash

declare -A colors
colors[black]="#282a36"
colors[red]="#ff5555"
colors[green]="#5af78e"
colors[yellow]="#f1fa8c"
colors[blue]="#57c7ff"
colors[pink]="#ff79c6"
colors[grey]="#44475a"
colors[grey_1]="#6272a4"
colors[magenta]="#ff6ac1"
colors[cyan]="#8be9fd"
colors[white]="#f1f1f0"
colors[orange]="#ffb86c"
colors[purple]="#bd9cf9"

selected="$(printf '%s\n' "${!colors[@]}" | dmenu -p "color:")"
[ -z $selected ] && exit
echo "${colors["${selected}"]}" | xclip -r -selection clipboard
