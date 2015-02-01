#!/bin/bash

if [[ $# < 3 ]]; then
  echo "[usage] from to out"
  echo " ex) $0 5 out-$(date +%F)"
  exit 1
fi
ulimit -n 1024

IP1=$1
IP2=$2
OUT=$3

# NC=$(ulimit -n)
NC=${NC:-1000}
NC=$(($NC - 48))

echo "[$(date)] Run with $NC fds"

mkdir -p $OUT

echo "BEG: $(date) $IP1 ~ $IP2" >> $OUT/log
  ./dist/build/scan/scan $NC "$IP1" "$IP2" 2>&1 >> $OUT/$IP1-$IP2
echo "END: $(date) $IP1 ~ $IP2" >> $OUT/log
