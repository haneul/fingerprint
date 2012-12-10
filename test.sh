#!/bin/bash

ip='143.*.5.*'

echo "BEG: $(date) $ip"
runhaskell scan.hs "$ip"
echo "END: $(date) $ip"
