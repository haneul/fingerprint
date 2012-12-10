#!/bin/bash

if [[ $# < 4 ]]; then
  echo "[usage] $0 remote dir from to $nr"
  exit 1
fi

REMOTE=$1
DIR=$2
FROM=$3
TO=$4
NR=${5:-5}

git archive --format tar.gz -o /dev/stdout HEAD     \
  | ssh $REMOTE "mkdir -p $DIR;                     \
                 cd $DIR;                           \
                 tar zxvf -;                        \
                 cabal install --only-dependencies; \
                 cabal configure;                   \
                 cabal build;                       \
                 ./run.sh $NR $FROM $TO"
