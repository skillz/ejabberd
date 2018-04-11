#!/bin/bash

for i in `seq 5000 6000 `;
do
    `./sbin/ejabberdctl create_room test-$i conference.chat.qa.skillz.com chat.qa.skillz.com`
done
