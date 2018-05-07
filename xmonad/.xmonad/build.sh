#!/bin/bash

cd $HOME/.xmonad

# TODO: Check and compare version
if [[ $* == *--update* ]]; then
  if [ -d .cabal-sandbox ]; then
    if [ -d .cabal-sandbox-prev ]; then
      rm -rf .cabal-sandbox-prev
    fi
    mv .cabal-sandbox .cabal-sandbox-prev
  fi
fi

# Check if cabal sandbox exists, otherwise create it and install xmonad
if [ ! -d .cabal-sandbox ]; then
  cabal sandbox init
  cabal install xmonad xmonad-contrib
  stow -t $HOME/.bin .cabal-sandbox/bin
fi

# Build the xmonad config
cabal build
if [ -f xmonad-x86_64-linux ]; then
  mv xmonad-x86_64-linux xmonad-x86_64-linux.prev
fi
mv dist/build/myxmonad/myxmonad xmonad-x86_64-linux
