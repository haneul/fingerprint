#!/bin/bash

ulimit -n 1024

IP=$1
OUT=$2

NC=$(ulimit -n)
NC=$(($NC - 48))

echo "[$(date)] Run with $NC fds"

echo "BEG: $(date) $IP.*.*.*" >> $OUT/log
for sub in `seq 0 255`; do
  ./dist/build/scan/scan $NC "$IP.$sub.*.*" &>> $OUT/$IP
done
echo "END: $(date) $IP.*.*.*" >> $OUT/log
