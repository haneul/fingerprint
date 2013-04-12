#!/bin/bash

OPT_TEST=${TEST:-false}
OPT_VERBOSE=${VERBOSE:-true}
OPT_MACHINES=${MACHINES:-machines}

TMP=$(mktemp -d)
PCS=( $(cat $OPT_MACHINES) )

if $OPT_TEST; then
  PCS=( ${PCS[0]} )
fi

echo "See. $TMP"

if [[ ! -n $1 ]]; then
  echo "[usage] ./$0 [local-script]"
  exit 1
fi

log() {
  if $OPT_VERBOSE; then
    echo "[$(date)] $@"
  fi
}

for m in ${PCS[@]}; do
  log "Run $@ on $m"
  {
    SCR=$(basename $1)
    URL=http://xk.csail.mit.edu/public/fingerprint/planet
    BUF=$(mktemp)
    ssh -q -tt \
      -o UserKnownHostsFile=/dev/null \
      -o StrictHostKeyChecking=no \
      -o ConnectTimeout=3 \
      -i syhan.pkey -l uw_scanner $m \
      -- "wget -q $URL/$SCR -O /tmp/$SCR -nc; bash /tmp/$SCR" &> $BUF
    mv $BUF $TMP/$m
    log "Done $m"
  } &
done

while true; do
  DONE=($TMP/*)
  if [[ ${#DONE[@]} == ${#PCS[@]} ]]; then
    echo "See. $TMP"
    break
  fi
  sleep 1
done

./check-diff.sh $TMP