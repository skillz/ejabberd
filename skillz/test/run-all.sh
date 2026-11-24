#!/bin/sh
# Every proof check against the running local stack (start it with ../run-local.sh up http).
# needs SDK_CORE_NODE_MODULES and PORTAL_PLAY (paths) in the environment.
set -u
DIR=$(cd "$(dirname "$0")" && pwd)
BUNDLE=$(mktemp -d)/web-xmpp.cjs
"$PORTAL_PLAY/node_modules/.bin/esbuild" "$PORTAL_PLAY/src/components/chat/xmpp.ts" --bundle --platform=node --format=cjs --log-level=warning --outfile="$BUNDLE"
"$DIR/compat/run-matrix.sh" | grep -E 'BREAKS|SUMMARY'
for t in p1-auth-http p14-kick-session p9-vcard-multi p12-subscriptions-order; do
  NODE_PATH="$SDK_CORE_NODE_MODULES" node "$DIR/$t.js" 2>&1 | grep -E ' ok|FAIL|TIMEOUT|order'
done
for t in send-stanza-check p3-destroy-recreate; do
  WEB_BUNDLE="$BUNDLE" NODE_PATH="$PORTAL_PLAY/node_modules" node "$DIR/$t.js"
done
"$DIR/api-compat.sh" | grep -v ' 200 ' || true
