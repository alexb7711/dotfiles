#!/bin/sh

# Add to PATH
export PATH="$PATH:/opt/flutter/bin"
export PATH="$PATH:$HOME/Code/Scripts/"

# Default Programs
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="brave"

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
