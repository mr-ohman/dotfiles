#!/bin/bash

# Bootstraps installation of aurman

git clone https://aur.archlinux.org/aurman.git _aurman
cd _aurman
makepkg -irs --noconfirm

read -p "Do you want to delete the build directory? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    cd ..
    rm -rf _aurman
fi
