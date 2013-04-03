#!/bin/bash

VERBOSE=1

for m in $(cat machines); do
  ssh -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no \
    -i syhan.pkey -l uw_scanner $m $@
done