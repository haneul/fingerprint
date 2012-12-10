#!/bin/bash

OUT=out-$(date +%F)
mkdir -p $OUT
for ip in `seq 1 255`; do
  echo "BEG: $(date) $ip.*.*.*" > $OUT/log
  ./dist/build/scan/scan "$ip.*.*.*" 2>$OUT/$ip
  echo "END: $(date) $ip.*.*.*" > $OUT/log
done
