# dash shell
ENV=$XDG_CONFIG_HOME/dash/dashrc; export ENV

[[ ! $DISPLAY && $(tty) = /dev/tty1 ]] && startx /usr/bin/qtile start &> /dev/null
#[[ ! $DISPLAY && $(tty) = /dev/tty1 ]] && /usr/bin/qtile start -b wayland &> /dev/null
