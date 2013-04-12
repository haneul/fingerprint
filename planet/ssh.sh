#!/bin/bash

ssh -q \
  -o UserKnownHostsFile=/dev/null \
  -o ConnectTimeout=3 \
  -o StrictHostKeyChecking=no \
  -i syhan.pkey -l uw_scanner "$@"
