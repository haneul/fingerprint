#!/bin/bash -x

if [[ -d haskell ]]; then
  echo "Already installed?"
fi

sudo yum -y install --nogpgcheck gmp-devel make freeglut-devel
sudo yum -y install --nogpgcheck gcc zlib-devel

mkdir -p haskell
cd haskell

if [[ ! -e ghc-7.4.2-i386-unknown-linux.tar.bz2 ]]; then
  wget -q http://www.haskell.org/ghc/dist/7.4.2/ghc-7.4.2-i386-unknown-linux.tar.bz2
fi

if [[ ! -e haskell-platform-2012.4.0.0.tar.gz ]]; then
  wget -q http://lambda.haskell.org/platform/download/2012.4.0.0/haskell-platform-2012.4.0.0.tar.gz
fi

tar zxvf  haskell-platform-2012.4.0.0.tar.gz
tar xvfj ghc-7.4.2-i386-unknown-linux.tar.bz2

{
  cd ghc-7.4.2
  ./configure
  sudo make install
  cd ..
}

{
  cd haskell-platform-2012.4.0.0
  ./configure
  make
  sudo make install
  cd ..
}

# wget fingerprint
cabal update
cabal install
cabal build
