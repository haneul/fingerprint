#!/bin/bash
cat machines \
  | xargs -n1 ping -c2 -W 1 \
  | grep '100% packet loss' -B1 \
  | grep '^---' \
  | cut -d' ' -f2