# XDG
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_BIN_HOME=${XDG_BIN_HOME:="$HOME/.local/bin"}
export XDG_RUNTIME_DIR=/run/user/$(id -u)

# applications
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/scripts:$PATH"
export PATH="/opt/idea-IC-222.3739.54/bin/"
export EDITOR="emacsclient -c -n"
export BROWSER="chromium"
export PAGER=less

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history
export HISTFILE="$XDG_DATA_HOME"/zsh/history
export LESSHISTFILE='-'

export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME"/xmonad
export XMONAD_DATA_DIR="$XDG_DATA_HOME"/xmonad
export XMONAD_CACHE_DIR="$XDG_CACHE_HOME"/xmonad
export WEECHAT_HOME="$XDG_CONFIG_HOME"/weechat
export STACK_ROOT="$XDG_DATA_HOME"/stack
export GHCUP_DIR="$XDG_DATA_HOME/ghucp"
export GHCUP_BIN="$XDG_BIN_HOME"
export CABAL_CONFIG="$XDG_CONFIG_HOME/cabal/conf"
export CABAL_DIR="$XDG_DATA_HOME/cabal"
export GHCUP_USE_XDG_DIRS="YES"
