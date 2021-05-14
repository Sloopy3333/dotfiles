#aliAs
# ls
alias ls='ls --color=auto'

# grep
alias grep="grep --color=auto"

# dot file repo
alias dot="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias status="dot status"
alias stage="dot stage"
alias stageall="dot stage -A"
alias commit="dot commit -m"
alias push="dot push origin master"

#network
alias scwifi="nmcli d wifi list --rescan yes"
alias cntwifi="nmcli --ask device wifi connect"
alias wifion="nmcli radio wifi on"
alias wifioff="nmcli radio wifi off"

#misc
alias vi='nvim'
alias vim='nvim'
alias build='rm -f config.h && sudo make install'
alias mbsync='mbsync -c "$XDG_CONFIG_HOME"/isync/mbsyncrc'
alias vifm='~/.config/vifm/scripts/vifmrun'
alias aw='wiki-search'
alias cat='bat'
alias yay='paru'
alias doom='$HOME/.config/emacs/bin/doom'
alias emacs='/usr/bin/emacs'


# devour
alias steam="devour prime-run steam"
alias sxiv="devour sxiv"
alias mpv="devour mpv"
alias zathura="devour zathura"

# files
alias -s json=$EDITOR
alias -s txt=$EDITOR
alias -s md=$EDITOR
alias -s MD=$EDITOR
alias -s vim=$EDITOR
alias -s zsh=$EDITOR
alias -s config=$EDITOR
alias -s py=$EDITOR
alias -s rc=$EDITOR

# fzf
# run sudo pacman -Fy once before running
alias pacfind="pacman -Slq | fzf -m --preview 'cat <(pacman -Si {1}) <(pacman -Fl {1})' | xargs -ro sudo pacman -S"
alias yayfind="paru -Slq | fzf -m --preview 'cat <(paru -Si {1}) <(paru -Fl {1})' | xargs -ro paru -S"