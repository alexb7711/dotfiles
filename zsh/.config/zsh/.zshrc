#===============================================#
# 	Alex's config for the Zoomer Shell	#
#	 _________  _   _ ____   ____		#
#	|__  / ___|| | | |  _ \ / ___|		#
#	  / /\___ \| |_| | |_) | |    		#
#	 / /_ ___) |  _  |  _ <| |___ 		#
#	/____|____/|_| |_|_| \_\\____|		#
#===============================================#

#===============================================================================#
# ZSH CONFIGURATION
#===============================================================================#
# Auto CD
setopt autocd 

# Configure Display
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0

# VI Mode
bindkey -v
export KEYTIMEOUT=1

GPG_TTY=$(tty)
export GPG_TTY

#===============================================================================#
# CURSOR
#===============================================================================#
# Change cursor shape for different VI modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

#===============================================================================#
# FILES TO SOURCE
#===============================================================================#
# Load aliases and shortcuts if existent.
[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

#===============================================================================#
# PROMPT
#===============================================================================#
# Enable colors and change prompt:
autoload -U colors && colors
PS1="$fg[green]%B[%@ ]%b : %U$fg[cyan]%d%u$fg[white]
└─ "

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:git:*' formats "(%b)"

# History in cache directory:
HISTSIZE=100
SAVEHIST=100

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

#===============================================================================#
# TAB COMPLETION
#===============================================================================#
# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# # Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

#===============================================================================#
# PLUGGINS
# -> Should be Last
#===============================================================================#
# Load zsh-syntax-highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

