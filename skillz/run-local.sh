#!/bin/sh
# Local proof stack: MySQL 8 + ejabberd 26.09 with the Skillz modules. Host ports bind to 127.0.0.1 only.
# usage: run-local.sh up [internal|http] | down | ctl <ejabberdctl args>
set -eu
DIR=$(cd "$(dirname "$0")" && pwd)
IMG=${IMG:-ghcr.io/processone/ejabberd:26.09}
NET=ejproof

# HTTP stand-in for push-notification-service, user-details-service and support tickets, under the
# hostnames the 19.02 fork's plugins hardcode.
stub() {
  docker ps --format '{{.Names}}' | grep -qx ejproof-stub && return
  docker run -d --name ejproof-stub --network $NET \
    --network-alias user-details-service.user-details-service --network-alias user-support-service.user-support-service \
    --network-alias lb-api.prod.skillz.com -v "$DIR/test/service-stub.js:/stub.js:ro" node:20-alpine node /stub.js >/dev/null
}

stub_ip() { docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ejproof-stub; }

case "${1:-}" in
up)
  AUTH=${2:-internal}
  docker network inspect $NET >/dev/null 2>&1 || docker network create $NET >/dev/null
  if ! docker ps --format '{{.Names}}' | grep -qx ejproof-mysql; then
    docker run -d --name ejproof-mysql --network $NET -p 127.0.0.1:${MYSQL_PORT:-15306}:3306 \
      -e MYSQL_ROOT_PASSWORD=root -e MYSQL_DATABASE=ejabberd -e MYSQL_USER=ejabberd -e MYSQL_PASSWORD=ejabberd \
      mysql:8.0 >/dev/null
    until docker exec ejproof-mysql mysql -uejabberd -pejabberd -e 'select 1' ejabberd >/dev/null 2>&1; do sleep 2; done
  fi
  stub
  if [ "$AUTH" = http ] && ! docker ps --format '{{.Names}}' | grep -qx ejproof-authstub; then
    docker run -d --name ejproof-authstub --network $NET -v "$DIR/test/auth-stub.js:/stub.js:ro" node:20-alpine node /stub.js >/dev/null
  fi
  docker rm -f ejproof-ejabberd >/dev/null 2>&1 || true
  sed -e "s/@AUTH_METHOD@/$AUTH/" -e "s/@STATSD_IP@/$(stub_ip)/" "$DIR/conf/ejabberd.yml" > "$DIR/conf/.ejabberd.rendered.yml"
  docker run -d --name ejproof-ejabberd --network $NET --add-host host.docker.internal:host-gateway \
    -p 127.0.0.1:15222:5222 -p 127.0.0.1:15280:5280 -p 127.0.0.1:15443:5290 \
    -v "$DIR/conf/.ejabberd.rendered.yml:/opt/ejabberd/conf/ejabberd.yml:ro" \
    -v "$DIR/modules:/opt/ejabberd/.ejabberd-modules/sources" \
    $IMG >/dev/null
  until docker exec ejproof-ejabberd ejabberdctl status >/dev/null 2>&1; do
    docker ps --format "{{.Names}}" | grep -qx ejproof-ejabberd || { docker logs ejproof-ejabberd 2>&1 | tail -5; exit 1; }
    sleep 2
  done
  # auth_method internal keeps accounts in Mnesia, which this container does not persist.
  for u in skillz-cas mod1 1001 1002 1003 1004; do docker exec ejproof-ejabberd ejabberdctl register $u chat.skillz.com pw-$u >/dev/null 2>&1 || true; done
  docker exec ejproof-ejabberd ejabberdctl register skillz-cas chat-admin.skillz.com pw-skillz-cas >/dev/null 2>&1 || true
  ;;
up-fork)
  # Baseline: the 19.02 fork image built from Dockerfile.fork-baseline (core only) on the same MySQL, db
  # ejabberd_fork. FORK_PLUGINS=1: the image from Dockerfile.fork-plugins with ModPushSkillz and mod_pottymouth,
  # on a node name that the fork's skillz_util reads as production (chat.skillz.com, chat-admin.skillz.com).
  docker rm -f ejproof-ejabberd >/dev/null 2>&1 || true
  stub
  sed "s/@STATSD_IP@/$(stub_ip)/" "$DIR/test/compat/fork-baseline.yml" > "$DIR/conf/.fork.rendered.yml"
  if [ -n "${FORK_PLUGINS:-}" ]; then
    cat "$DIR/test/compat/fork-plugins.yml" >> "$DIR/conf/.fork.rendered.yml"
    FORK_IMAGE=${FORK_IMAGE:-ejproof-fork-plugins:f505dbe46}
  fi
  docker run -d --name ejproof-ejabberd --network $NET --hostname ej.production.local -e ERLANG_COOKIE=proof \
    -p 127.0.0.1:15280:5280 -p 127.0.0.1:15443:5290 \
    -v "$DIR/conf/.fork.rendered.yml:/opt/chat-service/etc/ejabberd/ejabberd.yaml:ro" \
    "${FORK_IMAGE:-ejproof-fork:f505dbe46}" >/dev/null
  until docker exec ejproof-ejabberd /opt/chat-service/sbin/ejabberdctl status >/dev/null 2>&1; do
    docker ps --format "{{.Names}}" | grep -qx ejproof-ejabberd || { docker logs ejproof-ejabberd 2>&1 | tail -5; exit 1; }
    sleep 2
  done
  for u in skillz-cas mod1 1001 1002 1003 1004; do docker exec ejproof-ejabberd /opt/chat-service/sbin/ejabberdctl register $u chat.skillz.com pw-$u >/dev/null 2>&1 || true; done
  docker exec ejproof-ejabberd /opt/chat-service/sbin/ejabberdctl register skillz-cas chat-admin.skillz.com pw-skillz-cas >/dev/null 2>&1 || true
  ;;
down)
  docker rm -f ejproof-ejabberd ejproof-authstub ejproof-stub ejproof-mysql >/dev/null 2>&1 || true
  docker network rm $NET >/dev/null 2>&1 || true
  ;;
ctl)
  shift; docker exec ejproof-ejabberd ejabberdctl "$@"
  ;;
*) echo "usage: $0 up [internal|http] | down | ctl ..." >&2; exit 2 ;;
esac
