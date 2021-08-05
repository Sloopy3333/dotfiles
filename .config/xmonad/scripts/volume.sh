#!/bin/sh

get_vol(){
status="$(amixer sget Master | grep -o -m 1 "\[[a-z]*\]" |  tr -d '%[]')"
vol="$(amixer sget Master | grep -o -m 1 '[[:digit:]]*%' | tr -d '%')"
if [ $status = off ]; then
    myvol="M"
else
    myvol=$vol
fi
}

case $1 in
  "up")
    amixer set Master 5%+ >/dev/null
    get_vol
    ;;
  "down")
    amixer set Master 5%- > /dev/null
    get_vol
    ;;
  "toggle")
    amixer set Master "toggle" >/dev/null
    get_vol
    ;;
  "")
    get_vol
    exit 0
    ;;
esac
