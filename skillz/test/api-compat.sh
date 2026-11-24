#!/bin/sh
# The admin API calls our services make today, with their exact request bodies (section C of the spike).
# Prints status + body per call so the fork baseline and 26.09 can be diffed. usage: api-compat.sh [api-url]
API=${1:-http://127.0.0.1:15443/api}
R=$(( $(date +%s) % 100000 + 600000 ))
call() { printf '%-58s ' "$1"; shift; curl -s -o /tmp/ejproof-api.$$ -w '%{http_code} ' "$@"; head -c 160 /tmp/ejproof-api.$$ | tr '\n' ' '; echo; }
post() { n=$1; c=$2; b=$3; call "$n" -X POST "$API/$c" -d "$b"; }
STANZA="<message type='groupchat' to='$R@conference.chat.skillz.com' id='b1'><body>broadcast</body><skillz_sdk xmlns='xmpp:skillz'><message_type>0</message_type><user_id>0</user_id></skillz_sdk></message>"
post "uds start_room {name,title}"                      start_room "{\"name\":\"$R\",\"title\":\"Game Chat\"}"
call "uds get_room_options GET ?name&service"           "$API/get_room_options?name=$R&service=conference.chat.skillz.com"
post "uds/cbs send_stanza into a started room"          send_stanza "{\"from\":\"skillz-cas@chat.skillz.com\",\"to\":\"$R@conference.chat.skillz.com\",\"stanza\":\"$STANZA\"}"
S2=$(echo "$STANZA" | sed "s/$R@/$((R+1))@/")
post "cbs send_stanza into a room never started"        send_stanza "{\"from\":\"skillz-cas@chat.skillz.com\",\"to\":\"$((R+1))@conference.chat.skillz.com\",\"stanza\":\"$S2\"}"
post "uds get_roster {user,server}"                      get_roster '{"user":"1001","server":"chat.skillz.com"}'
post "old-arg set_room_affiliation {name,service,jid}"   set_room_affiliation "{\"name\":\"$R\",\"service\":\"conference.chat.skillz.com\",\"jid\":\"1001@chat.skillz.com\",\"affiliation\":\"member\"}"
post "get_room_occupants {name,service}"                 get_room_occupants "{\"name\":\"$R\",\"service\":\"conference.chat.skillz.com\"}"
post "public get_room_summary"                           get_room_summary "{\"name\":\"$R\",\"service\":\"conference.chat.skillz.com\"}"
post "unsubscribe_room {user,room}"                      unsubscribe_room "{\"user\":\"1004@chat.skillz.com\",\"room\":\"$R@conference.chat.skillz.com\"}"
post "cron rooms_unused_destroy {host,days}"             rooms_unused_destroy '{"host":"chat.skillz.com","days":3}'
post "26.09 form rooms_unused_destroy {service,days}"    rooms_unused_destroy '{"service":"conference.chat.skillz.com","days":3}'
post "destroy_room {name,service}"                       destroy_room "{\"name\":\"$R\",\"service\":\"conference.chat.skillz.com\"}"
rm -f /tmp/ejproof-api.$$
