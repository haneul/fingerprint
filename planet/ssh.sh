#!/bin/bash

ssh -q -t \
  -o UserKnownHostsFile=/dev/null \
  -o ConnectTimeout=3 \
  -o StrictHostKeyChecking=no \
  -i syhan.pkey -l uw_scanner "$@"
