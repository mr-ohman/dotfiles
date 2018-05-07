#!/bin/bash

# Install packages
bootstrap/pacman-packages.sh

# Bootstrap aurman
bootstrap/aurman.sh

# Bootstrap cabal-install
bootstrap/cabal.sh

# Cabal-install packages
bootstrap/cabal-packages.sh

# Stow dotfiles
bootstrap/stow.sh
