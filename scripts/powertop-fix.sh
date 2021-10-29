#!/bin/sh
# script to stop auto suspending mouse in powertop

sleep 5;
MOUSE='/sys/bus/usb/devices/1-4/power/control'
KEYBOARD1='/sys/bus/usb/devices/1-1/power/control'
KEYBOARD2='/sys/bus/usb/devices/1-3/power/control'
[ -f $MOUSE ] && echo 'on' > $MOUSE;
[ -f $KEYBOARD1 ] && echo 'on' > $KEYBOARD1;
[ -f $KEYBOARD2 ] && echo 'on' > $KEYBOARD2;
