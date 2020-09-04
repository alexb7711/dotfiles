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
export GNUPGHOME="$HOME/.local/share/.gnupg"
export _JAVA_OPTIONS="-"
export LESSHISTFILE="-"
export NOTMUCH_CONFIG="$HOME/.config/notmuch/notmuchrc"
export NPM_CONFIG_USERCONFIG="$HOME/.config/npm/npmrc" # if things break, this is why 
export PASSWORD_STORE_DIR="$HOME/.local/share/password-store"
export ZDOTDIR="$HOME/.config/zsh/"

# Android Studio
export _JAVA_AWT_WM_NONREPARENTING=1

# ROS Environment Variables
source /opt/ros/melodic/setup.zsh
ROS_PACKAGE_PATH=$ROS_PACKAGE_PATH:/home/alex/Code/ROS/catkin/src
