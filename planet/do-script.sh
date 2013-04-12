#!/bin/bash

OPT_TEST=${TEST:-false}
OPT_VERBOSE=${VERBOSE:-true}

TMP=$(mktemp -d)
PCS=( $(cat machines) )

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
    BUF=$(mktemp)
    cat $1 | ssh -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no \
      -i syhan.pkey -l uw_scanner $m -- sh &> $BUF
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

OUT=($TMP/*)
ONE=${OUT[0]}

for f in ${OUT[@]}; do
  DIFF=$(diff -urN $ONE $f)
  printf "%-35s: " $(basename $f)
  if [[ -n $DIFF ]]; then
    echo ""
    echo "------"
    echo "$DIFF"
    echo "------"
  else
    echo " OK"
  fi
done

echo "See. $TMP"