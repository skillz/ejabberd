#!/bin/bash

CTL="/opt/ejabberd/sbin/ejabberdctl"

printf "Registering default users\n"
$CTL register admin chat.dev.skillz.com password
$CTL register admin localhost password
$CTL register skillz-cas localhost password
$CTL register skillz-cas chat.dev.skillz.com password
