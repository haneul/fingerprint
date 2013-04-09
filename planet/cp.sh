#!/bin/bash

VERBOSE=1

for m in $(cat machines); do
  scp -i syhan.pkey $@ uw_scanner@$m:
done
