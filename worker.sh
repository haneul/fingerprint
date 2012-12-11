#!/bin/bash

if [[ $# < 2 ]]; then
  echo "[usage] ip out"
  echo " ex) $0 5 out-$(date +%F)"
  exit 1
fi
ulimit -n 1024

IP=$1
OUT=$2

NC=$(ulimit -n)
NC=$(($NC - 48))

echo "[$(date)] Run with $NC fds"

mkdir -p $OUT

echo "BEG: $(date) $IP.*.*.*" >> $OUT/log
  ./dist/build/scan/scan $NC "$IP.0.0.0" "$IP.255.255.255" &>> $OUT/$IP
echo "END: $(date) $IP.*.*.*" >> $OUT/log
