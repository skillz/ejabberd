#!/bin/sh
set -e

# Erlang Node configuration from assigned FQDN
export ERLANG_NODE="ejabberd@(hostname -f)"
cat << EOF > ${EJABBERD_HOME}/etc/ejabberd/ejabberdctl.cfg
POLL=true
ERL_MAX_ETS_TABLES=20000
ERL_MAX_PORTS=50000
ERLANG_NODE=${ERLANG_NODE}
ERL_PROCESSES=100000
ERL_FULLSWEEP_AFTER=0
EOF

cat << EOF > ${HOME}/.erlang.cookie
${ERLANG_COOKIE}
EOF
# erlang cookie must be readable only by the ejabberd user
chmod 0400 ${HOME}/.erlang.cookie

# Start chat service in the background
exec ${EJABBERDCTL} "foreground" &
child=$!
${EJABBERDCTL} started

# Run post startup scripts
${EJABBERD_HOME}/scripts/create_users.sh
if [ "${EJABBERD_K8S_AUTOCLUSTER}" = true ]; then
    ${EJABBERD_HOME}/scripts/join_k8s_cluster.sh
fi

wait $child
