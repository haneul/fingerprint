#!/bin/bash

OUT=out-$(date +%F)
mkdir -p $OUT
for ip in `seq 1 255`; do
  echo "BEG: $(date) $ip.*.*.*" >> $OUT/log
  for j in `seq 0 255`; do
    ./dist/build/scan/scan "$ip.*.$j.*" &>>$OUT/$ip
  done
  echo "END: $(date) $ip.*.*.*" >> $OUT/log
done
