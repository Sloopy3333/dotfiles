#! /bin/sh

sleep 1 && killall trayer
trayer --edge bottom\
       --align right\
       --SetDockType true\
       --SetPartialStrut true\
       --expand true\
       --height 20\
       --distance 2\
       --distancefrom right\
       --transparent true\
       --alpha 0\
       --tint 0x002b36\
       --widthtype request\
       --monitor 0\
       --margin 0
