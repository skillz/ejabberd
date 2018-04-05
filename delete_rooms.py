# This should be ran from the default directory of the `ejabberd` user!

import subprocess
import os
from time import sleep

# JID is userId1-userId2@conference.chat.skillz.com  Global rooms have no '-', so they're unaffected.
# Load all rooms with a - (dms and group messages) 
from_ctl = subprocess.Popen(['./sbin/ejabberdctl', 'muc_online_rooms', 'global'], stdout=subprocess.PIPE)
all_rooms = subprocess.Popen(['grep', '-'], stdin = from_ctl.stdout, stdout = subprocess.PIPE)
from_ctl.stdout.close()

muc_rooms, stderr = all_rooms.communicate()

# Loop through all muc rooms and delete them.
for muc in muc_rooms.split(os.linesep):
    name_service_list = muc.split('@')
    if len(name_service_list) > 1:
        subprocess.Popen(['./sbin/ejabberdctl', 'destroy_room', name_service_list[0], name_service_list[1]])
        sleep(.5)

