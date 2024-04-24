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

# Vim mode ----------------------------------------------------------------------------------------

bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

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

# Emacs vterm -------------------------------------------------------------------------------------

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

# Path --------------------------------------------------------------------------------------------

# Local binaries
export PATH="$HOME/.local/bin:$PATH"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# Emacs
export PATH="$HOME/.emacs.d/bin:$PATH"

# Haskell
export PATH="$HOME/.cabal/bin:$PATH"

# Terminal utilities and env variables -----------------------------------------------------------

export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="alacritty"
export BROWSER="firefox"

alias vim="nvim"
alias v="nvim"

# Makes my Ctrl-k output an Esc so that I can use zsh vim mode smoothly
bindkey -s ^k "\x1b"

alias l="eza --long --icons --git --group-directories-first"
alias ls="eza --icons --group-directories-first"

alias b="bat --theme=Coldark-Dark"

# File explorer
alias n="nnn -Hde"
export NNN_OPENER="xdg-open"

# zoxide
alias cd="z"
alias ..="z .."
eval "$(zoxide init zsh)"

# Prompt stuff
export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"
eval "$(starship init zsh)"

# Screen control ----------------------------------------------------------------------------------

alias red="redshift -P -O"
alias bright="xrandr --output eDP-1 --brightness"

# Keyboard control --------------------------------------------------------------------------------

alias kbd="setxkbmap us -option ctrl:nocaps && xset r rate 500 60"
alias kbdbr="setxkbmap br -variant nodeadkeys -option ctrl:nocaps && xset r rate 500 60"
alias kbdbr2="setxkbmap br -option ctrl:nocaps && xset r rate 500 60"
alias kbdset="xset r rate 500 60"

# Bluetooth ---------------------------------------------------------------------------------------

alias blue="bluetoothctl"
alias wallbg="feh --bg-fill --no-fehbg"

# -------------------------------------------------------------------------------------------------

# Git
alias glog="git log --decorate --graph"
alias gst="git status"

# Python ------------------------------------------------------------------------------------------

alias ipy="ipython3 --colors=Linux"
export IPYTHONDIR="~/.config/ipython"
export NLTK_DATA="~/.cache/nltk_data"
export JUPYTERLAB_DIR="$HOME/.local/share/jupyter/lab"

alias pip="python3 -m pip"

alias p="python3"
alias py="python3"
alias python="python3"

export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# JavaScript --------------------------------------------------------------------------------------

export npm_config_prefix="$HOME/.local"

# Golang ------------------------------------------------------------------------------------------

export GOPATH="$HOME/.go"
export GOBIN="$HOME/.go/bin"
export PATH="$PATH:$GOBIN"

# Tmux --------------------------------------------------------------------------------------------

bindkey -s ^f "tmux-sessionizer\n"

# Monitor control ---------------------------------------------------------------------------------

alias monitor="xrandr --output HDMI-1 --auto && xrandr --output eDP-1 --auto --right-of HDMI-1"
alias monitor-auto="xrandr --output HDMI-1 --auto"
alias monitor-right="xrandr --output eDP-1 --auto --right-of HDMI-1"

# Wacom control -----------------------------------------------------------------------------------

alias wacom-hdmi="xsetwacom set 'Wacom One by Wacom S Pen stylus' MapToOutput HDMI-1"

# -------------------------------------------------------------------------------------------------

# Syntax highlighting for zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
