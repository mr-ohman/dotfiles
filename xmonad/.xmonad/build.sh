#!/bin/bash

cd $HOME/.xmonad

# TODO: Check and compare version
if [[ $* == *--update* && -d .cabal-sandbox ]]; then
  if [ -d .cabal-sandbox-prev ]; then
    rm -rf .cabal-sandbox-prev
  fi
  mv .cabal-sandbox .cabal-sandbox-prev
fi

# Check if cabal sandbox exists, otherwise create it and install xmonad
if [ ! -d .cabal-sandbox ]; then
  cabal sandbox init
  cabal install xmonad xmonad-contrib
  cd .cabal-sandbox
  stow -t $HOME/.bin bin
  cd ..
fi

# Build the xmonad config
cabal build
if [[ -f dist/build/myxmonad/myxmonad && -f xmonad-x86_64-linux ]]; then
  mv xmonad-x86_64-linux xmonad-x86_64-linux.prev
fi
mv dist/build/myxmonad/myxmonad xmonad-x86_64-linux
