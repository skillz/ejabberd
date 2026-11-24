#!/bin/sh
# StatefulSet pod entrypoint (run under `tini -g`): starts ejabberd, registers the internal
# service accounts, joins the cluster.
#   ERLANG_NODE_ARG           ejabberd@<statefulset>-<ordinal>.<discovery service FQDN>
#   CHAT_SERVICE_USERS        "user@host:password ..." (internal auth is Mnesia, so on every start)
#   EJABBERD_K8S_AUTOCLUSTER  true to join the other pods
#   EJABBERD_REPLICAS         StatefulSet replicas
set -e

ejabberdctl foreground &
child=$!
until ejabberdctl started; do
  kill -0 $child 2>/dev/null || exit 1
done

for entry in ${CHAT_SERVICE_USERS:-}; do
  jid=${entry%%:*}
  ejabberdctl register "${jid%%@*}" "${jid#*@}" "${entry#*:}" >/dev/null 2>&1 || true
done

if [ "${EJABBERD_K8S_AUTOCLUSTER:-}" = true ]; then
  host=${ERLANG_NODE_ARG#*@}
  pod=${host%%.*}
  domain=${host#*.}
  first="ejabberd@${pod%-*}-0.$domain"
  last="ejabberd@${pod%-*}-$((EJABBERD_REPLICAS - 1)).$domain"
  if [ "${pod##*-}" != 0 ]; then
    ejabberdctl join_cluster "$first"
  # Pod 0 restarts last in a rolling update: join the pods that are still running.
  elif [ "$last" != "$first" ] && ejabberdctl --node "$last" status >/dev/null 2>&1; then
    ejabberdctl join_cluster "$last"
  fi
fi

wait $child
