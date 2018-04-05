# Should be ran from the default directory

import subprocess

host = "chat.qa.skillz.com"
service = "conference.%s" % host

for index in range(5000, 10000):
    room_name = "test-%s" % index
    subprocess.Popen(['./sbin/ejabberdctl', 'create_room', room_name, service, host])

