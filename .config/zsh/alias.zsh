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

# fzf
# run sudo pacman -Fy once before running
alias pacfind="pacman -Slq | fzf -m --preview 'cat <(pacman -Si {1}) <(pacman -Fl {1})' | xargs -ro sudo pacman -S"
alias yayfind="yay -Slq | fzf -m --preview 'cat <(yay -Si {1}) <(yay -Fl {1})' | xargs -ro yay -S" # avoid using since it pings aur on every keystroke
