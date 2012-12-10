#!/bin/bash

ip='202.2.*.*'

echo "BEG: $(date) $ip"
./dist/build/scan/scan "$ip"
echo "END: $(date) $ip"
