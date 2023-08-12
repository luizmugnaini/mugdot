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

# ** vterm config **

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Clear vterm history with C-c C-l
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# update name string of the current buffer for the current working directory
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

# Directory and prompt tracking
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

# ** env variables **
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="kitty"
export BROWSER="firefox"

# ** path exports **
# Local binaries
export PATH="$HOME/.local/bin:$PATH"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# Emacs
export PATH="$HOME/.emacs.d/bin:$PATH"

# zoxide
eval "$(zoxide init zsh)"

# ** aliases **

# Editor
alias vim="nvim"

# Terminal utilities
alias ls="exa --long --header --icons --git"
alias bat="bat --theme=OneHalfDark"
alias cd="z"
alias ..="z .."

# nnn file explorer
alias n="nnn -Hde"

# Screen control
alias red="redshift -P -O"
alias bright="xrandr --output eDP-1 --brightness"

# Keyboard stuff
alias kbd="setxkbmap us -option ctrl:nocaps && xset r rate 500 60"
alias kbdbr="setxkbmap br -option ctrl:nocaps && xset r rate 500 60"
alias kbdbr2="setxkbmap br -variant nodeadkeys -option ctrl:nocaps && xset r rate 500 60"
alias kbdset="xset r rate 500 60"

# Bluetooth
alias blue="bluetoothctl"
alias wallbg="feh --bg-fill --no-fehbg"

# Git
alias glog="git log --decorate --graph"
alias gst="git status"

# Python
alias ipy="ipython --profile=mugipy"
export NLTK_DATA="~/.cache/nltk_data"
alias pip="python3 -m pip"
alias py="python3"
alias python="python3"
alias pypy="pypy3"
export JUPYTERLAB_DIR=$HOME/.local/share/jupyter/lab
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
export IPYTHONDIR="~/.config/ipython"

#
export npm_config_prefix="$HOME/.local"

# golang
export GOPATH="$HOME/.go"
export PATH=$PATH:/usr/local/go/bin

# ** tmux sessionizer binding **
bindkey -s ^f "tmux-sessionizer\n"

# Makes my Ctrl-k output an Esc so that I can use vim mode smoothly
bindkey -s ^k "^["

# Monitor stuff
alias monitor="xrandr --output HDMI-1 --auto && xrandr --output eDP-1 --auto --right-of HDMI-1"
alias monitor-auto="xrandr --output HDMI-1 --auto"
alias monitor-right="xrandr --output eDP-1 --auto --right-of HDMI-1"

# ** nnn integration **
export NNN_OPENER="xdg-open"

# Prompt stuff
eval "$(starship init zsh)"

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

source /home/mug/.config/broot/launcher/bash/br
