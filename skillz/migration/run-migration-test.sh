#!/bin/sh
# Migration + rollback test on a restored MySQL dump. Local only: never point it at a shared DB.
# Reuses the proof configs (skillz/conf/ejabberd.yml, skillz/test/compat/fork-baseline.yml) with the
# vhost of the dump and the ejmig-* container names. Host ports bind to 127.0.0.1 only.
# usage: run-migration-test.sh restore <dump-dir> | up-26 | up-fork | schema <out-file> | ctl <args> | down
#   VHOST (default chat.staging.skillz.com): XMPP domain of the dump.
#   WS_PORT, API_PORT (default 15280, 15443): host ports for /websocket and /api.
set -eu
DIR=$(cd "$(dirname "$0")/.." && pwd)
VHOST=${VHOST:-chat.staging.skillz.com}
WS_PORT=${WS_PORT:-15280}
API_PORT=${API_PORT:-15443}
NET=ejmig
SQL="docker exec -i ejmig-mysql mysql -uroot -proot"
RENDER="s/chat\.skillz\.com/$VHOST/g; s/ejproof-mysql/ejmig-mysql/g; s/ejproof-authstub/ejmig-authstub/g; s/\"ejabberd_fork\"/\"ejabberd\"/"

wait_up() {
  until docker exec ejmig-ejabberd sh -c "$1 status" >/dev/null 2>&1; do
    docker ps --format '{{.Names}}' | grep -qx ejmig-ejabberd || { docker logs ejmig-ejabberd 2>&1 | tail -20; exit 1; }
    sleep 2
  done
}

case "${1:-}" in
restore)
  # Same server flags as the staging Aurora reader where they change behaviour (sql_mode, FULLTEXT token size).
  docker network inspect $NET >/dev/null 2>&1 || docker network create $NET >/dev/null
  docker run -d --name ejmig-mysql --network $NET -e MYSQL_ROOT_PASSWORD=root -e MYSQL_DATABASE=ejabberd -e MYSQL_USER=ejabberd -e MYSQL_PASSWORD=ejabberd \
    mysql:8.0 --sql-mode= --innodb-ft-min-token-size=3 --innodb-buffer-pool-size=1536M --max-allowed-packet=64M \
    --skip-log-bin --character-set-server=utf8mb4 --collation-server=utf8mb4_general_ci >/dev/null
  until $SQL -e 'select 1' >/dev/null 2>&1; do sleep 2; done
  for f in "$2"/*.sql; do echo "restore $(basename "$f")"; $SQL ejabberd < "$f" 2>&1 | grep -v 'Using a password' || true; done
  ;;
up-26)
  docker rm -f ejmig-ejabberd >/dev/null 2>&1 || true
  # preload_rooms (default true) reads every muc_room row at start: 1.9M rooms on staging. The fork never did.
  sed "s/@AUTH_METHOD@/internal/; $RENDER" "$DIR/conf/ejabberd.yml" \
    | perl -pe 's/^(\s+)hibernation_timeout: .*\n/$&$1preload_rooms: false\n/' > "$DIR/migration/.ejabberd-26.rendered.yml"
  grep -q 'preload_rooms: false' "$DIR/migration/.ejabberd-26.rendered.yml"
  docker run -d --name ejmig-ejabberd --network $NET \
    -p 127.0.0.1:$WS_PORT:5280 -p 127.0.0.1:$API_PORT:5290 \
    -v "$DIR/migration/.ejabberd-26.rendered.yml:/opt/ejabberd/conf/ejabberd.yml:ro" \
    -v "$DIR/modules:/opt/ejabberd/.ejabberd-modules/sources" \
    "${IMG:-ejproof-ejabberd:26.09-skillz}" >/dev/null
  wait_up ejabberdctl
  ;;
up-fork)
  docker rm -f ejmig-ejabberd >/dev/null 2>&1 || true
  sed "$RENDER" "$DIR/test/compat/fork-baseline.yml" > "$DIR/migration/.ejabberd-fork.rendered.yml"
  docker run -d --name ejmig-ejabberd --network $NET --hostname ejproof -e ERLANG_COOKIE=proof \
    -p 127.0.0.1:$WS_PORT:5280 -p 127.0.0.1:$API_PORT:5290 \
    -v "$DIR/migration/.ejabberd-fork.rendered.yml:/opt/chat-service/etc/ejabberd/ejabberd.yaml:ro" \
    "${FORK_IMAGE:-ejproof-fork:f505dbe46}" >/dev/null
  wait_up /opt/chat-service/sbin/ejabberdctl
  ;;
schema)
  # Columns + indexes only, sorted, so two runs diff cleanly.
  $SQL -N ejabberd -e "select table_name, column_name, column_type, is_nullable, ifnull(column_default,'NULL') from information_schema.columns where table_schema='ejabberd' order by 1,2;
    select table_name, index_name, non_unique, group_concat(column_name order by seq_in_index), index_type from information_schema.statistics where table_schema='ejabberd' group by 1,2,3,5 order by 1,2;" 2>/dev/null > "$2"
  ;;
ctl)
  shift; docker exec ejmig-ejabberd sh -c "\$(command -v ejabberdctl || echo /opt/chat-service/sbin/ejabberdctl) $*"
  ;;
down)
  docker rm -fv ejmig-ejabberd ejmig-mysql >/dev/null 2>&1 || true
  docker network rm $NET >/dev/null 2>&1 || true
  ;;
*) echo "usage: $0 restore <dump-dir> | up-26 | up-fork | schema <out> | ctl <args> | down" >&2; exit 2 ;;
esac
