#! /bin/sh

sleep 1 && killall trayer
trayer --edge bottom\
       --align right\
       --SetDockType true\
       --SetPartialStrut true\
       --expand true\
       --height 20\
       --distance 4\
       --distancefrom right\
       --transparent true\
       --alpha 0\
       --tint 0x282828\
       --widthtype request\
       --monitor 0\
       --monitor 1\
       --margin 0\
