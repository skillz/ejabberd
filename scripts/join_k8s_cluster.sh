#!/bin/sh
set -e

# Join the cluster
# Because this is deployed in a kubernetes StatefulSet we can assume that
# Pods are created in sequential order starting from 0
pod_ordinal=$(hostname | rev | cut -d '-' -f 1 | rev) # Grabs the ordinal from the generated unique hostname
num_replicas=$( echo $1 | xargs ) # This removes the trailing space we had to add to cast this to a string

# Special clustering case for the first node
if [ $pod_ordinal -eq 0 ]; then
    echo "This is the first node, this is either a new cluster, or all the other nodes have already been updated"

    # Check if the last node of the statefulSet is up and running
    let "last_pod_ordinal = $num_replicas - 1"
    ping_response=$(${EJABBERDCTL} ping ejabberd@${EJABBERD_POD_PREFIX}-${last_pod_ordinal}.${EJABBERD_INTERNAL_SVC}.${EJABBERD_NAMESPACE}.svc.cluster.local)

    # if it is this is a rolling update and we should join it
    if [ "$ping_response" = "pong" ]; then
        echo "Joining ejabberd@${EJABBERD_POD_PREFIX}-${last_pod_ordinal}.${EJABBERD_INTERNAL_SVC}.${EJABBERD_NAMESPACE}.svc.cluster.local"
        ${EJABBERDCTL} join_cluster ejabberd@${EJABBERD_POD_PREFIX}-${last_pod_ordinal}.${EJABBERD_INTERNAL_SVC}.${EJABBERD_NAMESPACE}.svc.cluster.local
    # If it is not there is no need to do any clustering, the following nodes will join this node
    else
        echo "We are the first node"
    fi
else
    echo "This is not the first node of the cluster coming up, joining the first node"
    ${EJABBERDCTL} join_cluster ejabberd@${EJABBERD_POD_PREFIX}-0.${EJABBERD_INTERNAL_SVC}.${EJABBERD_NAMESPACE}.svc.cluster.local
fi
