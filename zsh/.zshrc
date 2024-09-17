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

# Oh my zsh stuff ---------------------------------------------------------------------------------

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="refined"
plugins=(git zsh-autosuggestions)
source $ZSH/oh-my-zsh.sh

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

# Path --------------------------------------------------------------------------------------------

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"

# Terminal utilities and env variables -----------------------------------------------------------

export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="wezterm"
export BROWSER="firefox"

# Makes my Ctrl-k output an Esc so that I can use zsh vim mode smoothly
bindkey -s ^k "\x1b"

alias ls="ls -g --color=always --group-directories-first --human-readable -X --almost-all"

# Vim/Neovim
alias vim="nvim"
alias vi="vim"

# File explorer
alias n="nnn -Hde"
export NNN_OPENER="xdg-open"

# zoxide
alias cd="z"
alias ..="z .."
eval "$(zoxide init zsh)"

# Debugger
alias gdb="gdb -q"

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

# Python ------------------------------------------------------------------------------------------

export IPYTHONDIR="~/.config/ipython"
export NLTK_DATA="~/.cache/nltk_data"
export JUPYTERLAB_DIR="$HOME/.local/share/jupyter/lab"

alias ipy="ipython3 --colors=Linux"
alias pip="python3 -m pip"
alias p="python3"
alias python="python3"

# export PYENV_ROOT="$HOME/.pyenv"
# command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"

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

# Misc --------------------------------------------------------------------------------------------

# Syntax highlighting for zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
