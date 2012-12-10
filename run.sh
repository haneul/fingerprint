#!/bin/bash
runhaskell run.hs "$@" > /tmp/log &
disown
