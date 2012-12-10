#!/bin/bash

NC=$(ulimit -n)
NC=$(($NC - 128))
OUT=out-$(date +%F)

mkdir -p $OUT
for ip in `seq 1 255`; do
  echo "BEG: $(date) $ip.*.*.*" >> $OUT/log
  ./dist/build/scan/scan $NC "$ip.*.*.*" &>>$OUT/$ip
  echo "END: $(date) $ip.*.*.*" >> $OUT/log
done
