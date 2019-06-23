#!/bin/bash

# Set up tray
(sleep 1 && trayer --edge top --align right --SetDockType true \
                   --SetPartialStrut true --expand true --widthtype pixel \
                   --width 200 --transparent true --tint 0x000000 --height 24) &

# Set background
source $HOME/.fehbg

# Setup X resources
xrdb ~/.Xresources

# Setup keyboard
setxkbmap -layout "us,se" -option grp:rctrl_toggle,ctrl:nocaps

# Setup cursor
xsetroot -cursor_name left_ptr
unclutter -root -idle 3 &

# Start screensaver deamon
xscreensaver -no-splash &

# Start tray applications
nm-applet &
parcellite &
redshift-gtk &

exec myxmonad
