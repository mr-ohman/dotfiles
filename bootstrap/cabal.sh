
# Install ghc
pacman -S --noconfirm ghc ghc-static

# Install stack for bootstrapping cabal-install
aurman -S --noconfirm --noedit stack-bin

stack setup --system-ghc
stack install --system-ghc cabal-install

$HOME/.local/bin/cabal update

mkdir -p $HOME/.cabal-bin/bin
mkdir -p $HOME/.cabal-bin/cabal
cd $HOME/.cabal-bin/cabal
$HOME/.local/bin/cabal sandbox init
$HOME/.local/bin/cabal install cabal-install

ln -s $HOME/.cabal-bin/cabal/.cabal-sandbox/bin/cabal $HOME/.cabal-bin/bin/cabal
