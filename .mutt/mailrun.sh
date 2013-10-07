#!/bin/bash

PID=$(pgrep offlineimap)

if [[ -n $PID ]]; then
   echo "Suspending process..."
   kill -STOP $PID
   echo "Running offlineimap update..."
   offlineimap -o
   echo "Restarting process..."
   kill -CONT $PID
   exit 0
fi

offlineimap

exit 0
