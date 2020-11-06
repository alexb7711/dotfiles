#!/bin/sh

# Startup Applications
monitorSwapper
setWallpaper
compton -c -l0 -t0 -r0 -o.00 -b 
redshift &
unclutter &
dwmblocks 2>&1 >/dev/null &
notify-send "Welcome Back Alex!"
brave&

# relaunch DWM if the binary changes, otherwise bail
csum=$(sha1sum $(which dwm))
new_csum=""
while true
do
    if [ "$csum" != "$new_csum" ]
    then
        csum=$new_csum
        dwm
    else
        exit 0
    fi
    new_csum=$(sha1sum $(which dwm))
    sleep 0.5
done
