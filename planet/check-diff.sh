#!/bin/bash

OUT=($1/*)
ONE=${OUT[0]}

for f in ${OUT[@]}; do
  DIFF=$(diff -urN $ONE $f)
  printf "%-40s: " $(basename $f)
  if [[ -n $DIFF ]]; then
    echo ""
    echo "$DIFF" | sed 's/^/  |/g'
  else
    echo " OK"
  fi
done