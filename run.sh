#!/bin/bash
exec /root/my-ejabberd/sbin/ejabberdctl "foreground" &
child=$!
sleep 10s
/root/ejabberd/100_create_default_users.sh &
wait $child
