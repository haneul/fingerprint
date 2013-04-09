#!/bin/bash

VER=1
TMP=$(mktemp -d)
PCS=( $(cat machines) )

log() {
  if [[ $VER == 1 ]]; then
    echo "[$(date)] $@"
  fi
}

for m in ${PCS[@]}; do
  log "Run $@ on $m"
  {
    BUF=$(mktemp)
    ssh -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no \
      -i syhan.pkey -l uw_scanner $m $@ &> $BUF
    mv $BUF $TMP/$m
    log "Done $m"
  } &
done

while true; do
  DONE=($TMP/*)
  if [[ ${#DONE[@]} == ${#PCS[@]} ]]; then
    echo "See. $TMP"
    exit 0
  fi
  sleep 1
done

