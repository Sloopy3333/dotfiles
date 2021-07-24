#!/bin/sh

get_vol(){
status="$(pamixer --get-mute)"
vol="$(pamixer --get-volume)"
if [ $status = "true" ]; then
    myvol="ﱝ "
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
    pamixer -i 5 >/dev/null
    ;;
  "down")
    pamixer -d 5 > /dev/null
    ;;
  "toggle")
    pamixer -t >/dev/null
    ;;
  *)
    exit 0
    ;;
esac
