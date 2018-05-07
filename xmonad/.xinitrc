#!/bin/bash

# Set up tray
(sleep 1 && trayer --edge top --align right --SetDockType true \
                   --SetPartialStrut true --expand true --widthtype pixel \
                   --width 200 --transparent true --tint 0x000000 --height 24) &

# Setup cursor
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps
unclutter -root -idle 3 &

# Start screensaver deamon
xscreensaver -no-splash &

# Start tray applications
nm-applet &
parcellite &
redshift-gtk &

exec xmonad
