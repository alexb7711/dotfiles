#!/bin/sh

# Add to PATH
export PATH="$PATH:/opt/flutter/bin"

[ -d "$HOME/Code/Scripts/" ] && export PATH="$PATH:$HOME/Code/Scripts"
[ -d "$HOME/Code/scripts/" ] && export PATH="$PATH:$HOME/Code/scripts"

# Default Programs
export BROWSER="brave"
export EDITOR="nvim"
export TERMINAL="st"
export VIFMRC="~/.config/vifm/vifmrc"

# ~/ Clean Up
export ANDROID_SDK_HOME="$HOME/.config/android"
export CARGO_HOME="$HOME/.local/cargo"
export CUDA_CACHE_PATH=$HOME/.cache/nv
export GNUPGHOME="$HOME/.local/share/.gnupg"
export _JAVA_AWT_WM_NONREPARENTING=1 # <-- So matlab will load
export LESSHISTFILE="-"
export NOTMUCH_CONFIG="$HOME/.config/notmuch/notmuchrc"
export NPM_CONFIG_USERCONFIG="$HOME/.config/npm/npmrc" 
export RUSTUP_HOME=$HOME/.local/share/rustup
export PASSWORD_STORE_DIR="$HOME/.local/share/password-store"
export ZDOTDIR="$HOME/.config/zsh/"
