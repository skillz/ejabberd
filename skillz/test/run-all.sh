#!/bin/sh
# Every proof check against the running local stack: ../run-local.sh up http (26.09), or
# FORK_PLUGINS=1 ../run-local.sh up-fork with FORK=1 here (19.02 baseline with its plugins).
# needs SDK_CORE_NODE_MODULES and PORTAL_PLAY (paths) in the environment.
set -u
DIR=$(cd "$(dirname "$0")" && pwd)
BUNDLE=$(mktemp -d)/web-xmpp.cjs
"$PORTAL_PLAY/node_modules/.bin/esbuild" "$PORTAL_PLAY/src/components/chat/xmpp.ts" --bundle --platform=node --format=cjs --log-level=warning --outfile="$BUNDLE"
if [ -n "${FORK:-}" ]; then
  export EJCTL=/opt/chat-service/sbin/ejabberdctl DB=ejabberd_fork TARGET=fork
fi
export WEB_BUNDLE="$BUNDLE" NODE_PATH="$SDK_CORE_NODE_MODULES:$PORTAL_PLAY/node_modules"
"$DIR/compat/run-matrix.sh" ${DB:-} | grep -E 'BREAKS|SUMMARY'
for t in p1-auth-http p14-kick-session p9-vcard-multi p12-subscriptions-order; do
  [ -n "${FORK:-}" ] && [ $t = p1-auth-http ] && continue # the fork baseline uses internal auth
  node "$DIR/$t.js" 2>&1 | grep -E ' ok|FAIL|TIMEOUT|order'
done
for t in send-stanza-check p3-destroy-recreate; do
  node "$DIR/$t.js"
done
# The matrix leaves privacy lists (1002 blocks 1001) that P10 must not see; P16 runs before P10 because
# both send 1001 -> 1002 friend requests, which alternate pushes. Start from a fresh container.
docker exec ejproof-mysql mysql -uroot -proot "${DB:-ejabberd}" -e "delete from privacy_list_data; delete from privacy_list; delete from privacy_default_list;" 2>/dev/null
docker exec ejproof-ejabberd sh -c "${EJCTL:-ejabberdctl} clear_cache" >/dev/null 2>&1 || true
for t in p7-mucsub-offline p16-push p10-block p17-pottymouth p5-room-summary p13-role-rules p15-statsd; do
  echo "== $t"
  node "$DIR/$t.js" 2>&1 | grep -E '^( ok|FAIL|CRASH|INFO|NOT)|TIMEOUT|rows'
done
"$DIR/api-compat.sh" | grep -v ' 200 ' || true
echo "== m1-preload-subject (restarts the server)"
node "$DIR/m1-preload-subject.js" 2>&1 | grep -E '^( ok|FAIL)|TIMEOUT'
