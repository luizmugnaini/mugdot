export ZSH="/home/luiz/.oh-my-zsh"
ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# ** set by zsh **
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v

# The following lines were added by compinstall
zstyle :compinstall filename '/home/luiz/.zshrc'

autoload -Uz compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# ** vi mode **
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
# Notice that there are two main options:
#   - block: '\e[1 q'
#   - beam:  '\e[5 q'
vim_normal='\e[1 q'
vim_insert='\e[1 q'
function zle-keymap-select {
  # normal, visual, cmd modes
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne $vim_normal
  # insert mode
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then 
    echo -ne $vim_insert
  fi
}

zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne $vim_insert
}

zle -N zle-line-init
echo -ne $vim_insert # Use beam shape cursor on startup.

preexec() { echo -ne $vim_insert ;} # Use cursor shape for each new prompt.

# ** env variables **
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="alacritty"
export BROWSER="firefox"

# ** path exports **
#export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

# zoxide
eval "$(zoxide init zsh)"

# ** aliases **
alias vim="nvim"
alias ls="exa --long --header --icons --git"
alias cat="bat --theme=gruvbox-dark"
alias bat="bat --theme=gruvbox-dark"
alias ..="z .."
alias cd="cd"
alias blue="bluetoothctl"
alias red="redshift -P -O"
alias kbd="setxkbmap us -option caps:swapescape"
alias kbdbr="setxkbmap br -option caps:swapescape"
alias glog="git log --decorate --graph"
alias gst="git status"
alias wallbg="feh --bg-fill --no-fehbg" 
alias ipy="ipython --profile=mugipy"
alias n="nnn -Hde"

# ** tmux sessionizer binding **
bindkey -s ^f "~/.config/tmux/tmux-sessionizer\n"

# ** ipython **
export IPYTHONDIR="~/.config/ipython"

# ** nnn integration **
export NNN_OPENER="xdg-open"
export NNN_PLUG="p:preview-tui;f:fzcd;i:imgview"

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
