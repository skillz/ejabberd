#!/bin/sh
# Affiliation changes in a room whose name has "-" (per-room affiliations) and in a game room
# (no "-", global user_affiliation table), against a fork image on a throwaway MySQL 8.
# usage: tests/hyphen-room-affiliation.sh <image>   (image: a core-only build of this checkout; the root Dockerfile also
# needs the plugin repos)
# needs docker and node >= 22 (global WebSocket). Host ports bind to 127.0.0.1 only.
set -eu
IMAGE=${1:?usage: $0 <image>}
DIR=$(cd "$(dirname "$0")/.." && pwd)
NET=ejfix
EJ=ejfix-ejabberd
DB=ejfix-mysql
SVC=conference.chat.skillz.com
CONF=$(mktemp)
trap 'docker rm -f $EJ $DB >/dev/null 2>&1; docker network rm $NET >/dev/null 2>&1; rm -f "$CONF"' EXIT

cat > "$CONF" <<'EOF'
loglevel: 4
hosts: ["chat.skillz.com"]
listen:
  - port: 5280
    module: ejabberd_http
    request_handlers:
      "/websocket": ejabberd_http_ws
auth_method: internal
default_db: sql
sql_type: mysql
sql_server: "ejfix-mysql"
sql_database: "ejabberd"
sql_username: "root"
sql_password: "root"
acl:
  admin:
    - user: "skillz-cas@chat.skillz.com"
access_rules:
  local:
    - allow
  c2s:
    - allow
  muc_create:
    - allow
api_permissions:
  "console commands":
    from: [ejabberd_ctl]
    who: all
    what: "*"
modules:
  mod_admin_extra: {}
  mod_muc:
    access: [allow]
    access_admin:
      allow: admin
    access_create: muc_create
    access_persistent: muc_create
    default_room_options:
      persistent: true
      members_by_default: true
      moderated: true
  mod_muc_admin: {}
EOF

ctl() { docker exec $EJ /opt/chat-service/sbin/ejabberdctl "$@"; }
sql() { docker exec $DB mysql -uroot -proot -N ejabberd -e "$1" 2>/dev/null; }
wait_ej() { n=0; until ctl status >/dev/null 2>&1; do n=$((n+1)); [ $n -lt 60 ] || { docker logs $EJ 2>&1 | tail -5; exit 1; }; sleep 2; done; }
register() { for u in skillz-cas 1001 1002 1003 1004; do ctl register $u chat.skillz.com pw-$u >/dev/null 2>&1 || true; done; }
FAILS=0
check() { name=$1; shift; if "$@" >/dev/null 2>&1; then echo "  ok: $name"; else echo "  FAIL: $name"; FAILS=$((FAILS+1)); fi; }
has() { printf '%s' "$1" | grep -q "$2"; }
affs() { echo "  get_room_affiliations $1:"; ctl get_room_affiliations "$1" $SVC | sed 's/^/    /'; }
# outcast <room> <user> [reason]: a muc#admin outcast from the service admin; reason "muted" is the mute.
outcast() {
  ctl send_stanza skillz-cas@chat.skillz.com/t "$1@$SVC" \
    "<iq type='set' id='o$1' to='$1@$SVC'><query xmlns='http://jabber.org/protocol/muc#admin'><item jid='$2@chat.skillz.com' affiliation='outcast'><reason>${3:-}</reason></item></query></iq>"
}
# join <user> <room>: joins over websocket, prints the self-presence (status 110) affiliation and role, then
# posts one groupchat and prints post=ok (echoed back) or post=<error condition>.
join() {
  node - "$1" "$2" <<'EOF'
const [user, room] = process.argv.slice(2);
const ws = new WebSocket('ws://127.0.0.1:15280/websocket', 'xmpp');
const open = '<open xmlns="urn:ietf:params:xml:ns:xmpp-framing" to="chat.skillz.com" version="1.0"/>';
let step = 0;
const done = (msg) => { console.log(msg); ws.close(); process.exit(0); };
let joined = '';
const body = `hi-${user}-${Date.now()}`;
setTimeout(() => done(`  ${user} in ${room}: ${joined || 'join='}TIMEOUT`), 10000);
const cond = (data) => (data.match(/<(\w[\w-]*) xmlns=['"]urn:ietf:params:xml:ns:xmpp-stanzas['"]/) || [])[1];
ws.onopen = () => ws.send(open);
ws.onmessage = ({ data }) => {
  if (step === 0 && data.includes('PLAIN')) {
    step = 1;
    const plain = Buffer.from(`\0${user}\0pw-${user}`).toString('base64');
    ws.send(`<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="PLAIN">${plain}</auth>`);
  } else if (step === 1 && data.includes('<success')) {
    step = 2; ws.send(open);
  } else if (step === 2 && data.includes('urn:ietf:params:xml:ns:xmpp-bind')) {
    step = 3;
    ws.send('<iq type="set" id="b"><bind xmlns="urn:ietf:params:xml:ns:xmpp-bind"><resource>t</resource></bind></iq>');
  } else if (step === 3 && /id=['"]b['"]/.test(data)) {
    step = 4;
    ws.send(`<presence to="${room}@conference.chat.skillz.com/u${user}"><x xmlns="http://jabber.org/protocol/muc"/></presence>`);
  } else if (step === 4 && data.includes('<presence') && /type=['"]error['"]/.test(data)) {
    done(`  ${user} in ${room}: join=${cond(data)}`);
  } else if (step === 4 && data.includes('<presence') && /code=['"]110['"]/.test(data)) {
    const attr = (k) => (data.match(new RegExp(`${k}=['"](\\w+)['"]`)) || [])[1];
    joined = `affiliation=${attr('affiliation')} role=${attr('role')} `;
    step = 5;
    ws.send(`<message type="groupchat" id="p" to="${room}@conference.chat.skillz.com"><body>${body}</body></message>`);
  } else if (step === 5 && data.includes('<message') && /type=['"]error['"]/.test(data)) {
    done(`  ${user} in ${room}: ${joined}post=${cond(data)}`);
  } else if (step === 5 && data.includes('<message') && data.includes(`<body>${body}</body>`)) {
    done(`  ${user} in ${room}: ${joined}post=ok`);
  }
};
EOF
}

docker network create $NET >/dev/null
docker run -d --name $DB --network $NET -p 127.0.0.1:15306:3306 -e MYSQL_ROOT_PASSWORD=root -e MYSQL_DATABASE=ejabberd mysql:8.0 >/dev/null
# TCP ping: the image's first-boot init server listens on the socket only.
until docker exec $DB mysqladmin ping -h 127.0.0.1 -uroot -proot --silent >/dev/null 2>&1; do sleep 2; done
docker exec -i $DB mysql -uroot -proot ejabberd < "$DIR/sql/mysql.sql" 2>/dev/null
docker exec -i $DB mysql -uroot -proot ejabberd < "$DIR/sql/2019-07-26-19-user-affiliation-table.sql" 2>/dev/null
docker create --name $EJ --network $NET --hostname ej.local -e ERLANG_COOKIE=ejfix -p 127.0.0.1:15280:5280 "$IMAGE" >/dev/null
chmod 644 "$CONF"
docker cp "$CONF" $EJ:/opt/chat-service/etc/ejabberd/ejabberd.yaml
docker start $EJ >/dev/null
wait_ej
register
echo "== image $IMAGE"
ctl create_room 9-room $SVC chat.skillz.com
ctl create_room 9 $SVC chat.skillz.com

echo "-- hyphen room: set_room_affiliation 1001 owner"
ctl set_room_affiliation 9-room $SVC 1001@chat.skillz.com owner
affs 9-room

echo "-- hyphen room: mute 1003 (outcast, reason muted), ban 1004 (outcast, no reason)"
outcast 9-room 1003 muted
outcast 9-room 1004
affs 9-room

echo "-- game room: set_room_affiliation 1002 owner, mute 1003"
ctl set_room_affiliation 9 $SVC 1002@chat.skillz.com owner
outcast 9 1003 muted
sleep 1
affs 9
echo "  user_affiliation rows (enabled):"
sql "select nick, affiliation from user_affiliation where enabled = 1 order by nick" | sed 's/^/    /'

echo "-- restart the server (rooms stop; the hyphen room restores from muc_room on join)"
ctl restart >/dev/null
sleep 3
wait_ej
echo "  online rooms after restart: [$(ctl muc_online_rooms global | tr '\n' ' ')]"
P1=$(join 1001 9-room); echo "$P1"
P3=$(join 1003 9-room); echo "$P3"
P4=$(join 1004 9-room); echo "$P4"
HROOM=$(ctl get_room_affiliations 9-room $SVC)
affs 9-room
P2=$(join 1002 9); echo "$P2"

echo "-- checks"
check "hyphen room keeps owner 1001 after restart" has "$HROOM" '^1001.*owner'
check "hyphen room self-presence of 1001 is owner" has "$P1" 'affiliation=owner'
check "hyphen room keeps 1003 muted after restart" has "$HROOM" '^1003.*muted'
check "muted 1003 joins the hyphen room as visitor" has "$P3" 'role=visitor'
check "muted 1003 cannot post in the hyphen room" has "$P3" 'post=forbidden'
check "banned 1004 cannot join the hyphen room" has "$P4" 'join=forbidden'
check "game room per-room list stays empty" [ -z "$(ctl get_room_affiliations 9 $SVC)" ]
check "game room owner and mute in user_affiliation" [ "$(sql 'select group_concat(nick, affiliation order by nick) from user_affiliation where enabled = 1')" = '1002owner,1003muted' ]
check "game room self-presence of 1002 is owner" has "$P2" 'affiliation=owner'
exit $FAILS
