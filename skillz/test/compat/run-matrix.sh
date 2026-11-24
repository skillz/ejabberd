#!/bin/sh
# Runs matrix.js against the local stack after clearing per-user state (privacy lists, offline queue).
# usage: run-matrix.sh [db-name]   needs SDK_CORE_NODE_MODULES and PORTAL_PLAY (paths) in the environment.
set -eu
DIR=$(cd "$(dirname "$0")" && pwd)
DB=${1:-ejabberd}
docker exec ejproof-mysql mysql -uroot -proot "$DB" -e "delete from privacy_list_data; delete from privacy_list; delete from privacy_default_list; delete from spool;" 2>/dev/null
docker exec ejproof-ejabberd sh -c "${EJCTL:-ejabberdctl} clear_cache" >/dev/null 2>&1 || true
BUNDLE=$(mktemp -d)/web-xmpp.cjs
"$PORTAL_PLAY/node_modules/.bin/esbuild" "$PORTAL_PLAY/src/components/chat/xmpp.ts" --bundle --platform=node --format=cjs --log-level=warning --outfile="$BUNDLE"
ROOM=${ROOM:-$(( $(date +%s) % 100000 ))} WEB_BUNDLE="$BUNDLE" NODE_PATH="$SDK_CORE_NODE_MODULES:$PORTAL_PLAY/node_modules" \
  node "$DIR/matrix.js" 2>&1 | grep -v -E '^(SET DEFAULT|FETCH_VCARDS|ROOMCREATE)'
