#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="kitty"
export BROWSER="brave"
. "$HOME/.cargo/env"
