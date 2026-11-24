#!/bin/sh
# Local proof stack: MySQL 8 + ejabberd 26.09 with the Skillz modules. Host ports bind to 127.0.0.1 only.
# usage: run-local.sh up [internal|http] | down | ctl <ejabberdctl args>
set -eu
DIR=$(cd "$(dirname "$0")" && pwd)
IMG=ghcr.io/processone/ejabberd:26.09
NET=ejproof

case "${1:-}" in
up)
  AUTH=${2:-internal}
  docker network inspect $NET >/dev/null 2>&1 || docker network create $NET >/dev/null
  if ! docker ps --format '{{.Names}}' | grep -qx ejproof-mysql; then
    docker run -d --name ejproof-mysql --network $NET -p 127.0.0.1:15306:3306 \
      -e MYSQL_ROOT_PASSWORD=root -e MYSQL_DATABASE=ejabberd -e MYSQL_USER=ejabberd -e MYSQL_PASSWORD=ejabberd \
      mysql:8.0 >/dev/null
    until docker exec ejproof-mysql mysql -uejabberd -pejabberd -e 'select 1' ejabberd >/dev/null 2>&1; do sleep 2; done
  fi
  if [ "$AUTH" = http ] && ! docker ps --format '{{.Names}}' | grep -qx ejproof-authstub; then
    docker run -d --name ejproof-authstub --network $NET -v "$DIR/test/auth-stub.js:/stub.js:ro" node:20-alpine node /stub.js >/dev/null
  fi
  docker rm -f ejproof-ejabberd >/dev/null 2>&1 || true
  sed "s/@AUTH_METHOD@/$AUTH/" "$DIR/conf/ejabberd.yml" > "$DIR/conf/.ejabberd.rendered.yml"
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
  ;;
up-fork)
  # Baseline: the 19.02 fork image built from Dockerfile.baseline (core only) on the same MySQL, db ejabberd_fork.
  docker rm -f ejproof-ejabberd >/dev/null 2>&1 || true
  docker run -d --name ejproof-ejabberd --network $NET --hostname ejproof -e ERLANG_COOKIE=proof \
    -p 127.0.0.1:15280:5280 -p 127.0.0.1:15443:5290 \
    -v "$DIR/test/compat/fork-baseline.yml:/opt/chat-service/etc/ejabberd/ejabberd.yaml:ro" \
    "${FORK_IMAGE:-ejproof-fork:f505dbe46}" >/dev/null
  until docker exec ejproof-ejabberd /opt/chat-service/sbin/ejabberdctl status >/dev/null 2>&1; do
    docker ps --format "{{.Names}}" | grep -qx ejproof-ejabberd || { docker logs ejproof-ejabberd 2>&1 | tail -5; exit 1; }
    sleep 2
  done
  for u in skillz-cas mod1 1001 1002 1003 1004; do docker exec ejproof-ejabberd /opt/chat-service/sbin/ejabberdctl register $u chat.skillz.com pw-$u >/dev/null 2>&1 || true; done
  ;;
down)
  docker rm -f ejproof-ejabberd ejproof-authstub ejproof-mysql >/dev/null 2>&1 || true
  docker network rm $NET >/dev/null 2>&1 || true
  ;;
ctl)
  shift; docker exec ejproof-ejabberd ejabberdctl "$@"
  ;;
*) echo "usage: $0 up [internal|http] | down | ctl ..." >&2; exit 2 ;;
esac
