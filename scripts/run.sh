#!/bin/sh
set -e

# Start chat service in the background
exec ${EJABBERDCTL} "foreground" &
child=$!
${EJABBERDCTL} started

# Run post startup scripts
${EJABBERD_HOME}/scripts/create_users.sh

wait $child
