#!/bin/bash

ulimit -n 4096

NC=$(ulimit -n)
NC=$(($NC - 128))
OUT=out-$(date +%F)

echo "Run with $NC at $(date)"

mkdir -p $OUT
for ip in `seq 1 255`; do
  echo "BEG: $(date) $ip.*.*.*" >> $OUT/log
  for sub in `seq 0 255`; do
    ./dist/build/scan/scan $NC "$ip.$sub.*.*" &>> $OUT/$ip
  done
  echo "END: $(date) $ip.*.*.*" >> $OUT/log
done
