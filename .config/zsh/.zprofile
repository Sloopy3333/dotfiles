# dash shell
ENV=$XDG_CONFIG_HOME/dash/dashrc; export ENV

[[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx "$HOME/.config/X11/Xinitrc"  -- vt1 &> /dev/null
