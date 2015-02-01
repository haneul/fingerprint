#!/bin/bash

TOP=$HOME/fingerprint
{
  cd $TOP
  python worker-planet.py &>/dev/null &
  disown
}
