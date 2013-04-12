#!/bin/bash

CLEAN=false

URL=http://xk.csail.mit.edu/public/fingerprint.git
TOP=$HOME/fingerprint

if [[ $CLEAN ]]; then
  rm -rf $HOME/.ghc
  rm -rf $HOME/.cabal
fi

# update
if [[ ! -e $TOP ]]; then
  git clone $URL $TOP
else
  (cd $TOP; git pull)
fi

# build
{
  cd $TOP
  cabal update
  cabal install --only-dependencies
  cabal configure
  cabal build

  # check if it runs
  dist/build/scan/scan

  # tools
  git --help
  gcc --version
  python --version
}
