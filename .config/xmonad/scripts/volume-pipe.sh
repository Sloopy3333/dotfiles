#!/bin/sh

get_vol(){
status="$(amixer sget Master | grep -o -m 1 "\[[a-z]*\]" |  tr -d '%[]')"
vol="$(amixer sget Master | grep -o -m 1 '[[:digit:]]*%' | tr -d '%')"
if [ $status = off ]; then
    myvol="ﱝ"
else
    myvol=" $vol"
fi
echo $myvol | tee /tmp/volume-pipe
}

get_vol

case $1 in
  "")
    ;;
  "up")
    amixer set Master 5%+ >/dev/null
    ;;
  "down")
    amixer set Master 5%- > /dev/null
    ;;
  "toggle")
    amixer set Master "toggle" >/dev/null
    ;;
  *)
    exit 0
    ;;
esac
