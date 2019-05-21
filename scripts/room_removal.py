# This should be ran from the default directory of the `ejabberd` user!

from subprocess import Popen, PIPE
import os
from time import sleep

# JID is userId1-userId2@conference.chat.skillz.com  
# Global rooms have no '-', so they're unaffected.
from_ctl = Popen(
    ['./sbin/ejabberdctl', 'muc_online_rooms', 'global'],
    stdout=PIPE
)
all_rooms = Popen(['grep', '-'], stdin = from_ctl.stdout, stdout = PIPE)
from_ctl.stdout.close()

muc_rooms, stderr = all_rooms.communicate()

# Loop through all muc rooms and delete them if there are no occupants.
for muc in muc_rooms.split(os.linesep):
    name_service_list = muc.split('@')

    # Some garbage lines are produced and throw errors.
    if len(name_service_list) <= 1:
        continue

    name = name_service_list[0]
    service = name_service_list[1]

    get_occupants = Popen(
        ['./sbin/ejabberdctl', 'get_room_occupants_number', name, service],
        stdout=PIPE
    )
    occupants, stderr = get_occupants.communicate()
    sleep(.3)

    if occupants == "0\n":
        Popen(['./sbin/ejabberdctl', 'destroy_room', name, service])
        sleep(.5)
