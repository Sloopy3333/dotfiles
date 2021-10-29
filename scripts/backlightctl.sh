#!/bin/sh

PER="${2:-5}"

send_notification(){
    brightness=$(xbacklight | cut -d '.' -f 1)
    notify-send "Brightness is $brightness %"
}
case $1 in
    up)
        xbacklight -inc $PER
        send_notification
        ;;
    down)
        xbacklight -dec $PER
        send_notification
        ;;
    *)
        PER=$(rofi -dmenu -p "brightness vlaue")
        xbacklight -set $PER
        send_notification
        ;;
esac
