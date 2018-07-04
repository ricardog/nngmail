#!/bin/bash

set -e

#PID=$(/bin/pidof nngmail.sh)
PID=

if [ "${PID}" == "" ]; then
    echo "Starting nngmail..."
    #export FLASK_ENV=development
    export FLASK_APP=src/nngmail:app
    if [ $# -eq 0 ]; then
	args="run --with-threads --host 127.0.0.1 --port 5544"
    else
	args=$*
    fi
    echo flask ${args}
    exec -a nngmail flask ${args}
else
    echo "nngmail already running (${PID})"
fi


