# dash shell
ENV=$XDG_CONFIG_HOME/dash/dashrc; export ENV

[[ ! $DISPLAY && $(tty) = /dev/tty1 ]] && exec startx "$HOME/.config/X11/Xinitrc" &> /dev/null
