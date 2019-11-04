#!/bin/sh

# Do not exit if users already registered
set +e

register_user() {
    local user=$1
    local domain=$2
    local password=$3

    ${EJABBERDCTL} register ${user} ${domain} ${password}
}

# register users from environment $CHAT_SERVICE_USERS with given password.
# Use whitespace to separate users.
register_all_users() {
    for user in ${CHAT_SERVICE_USERS} ; do
        local jid=${user%%:*}
        local password=${user#*:}

        local username=${jid%%@*}
        local domain=${jid#*@}

        [[ "${password}" == "${jid}" ]] \
            && password=$(randpw)

        register_user ${username} ${domain} ${password}
    done
}

# Register users if the environment variable exists
[ -n "${CHAT_SERVICE_USERS}" ] \
    && register_all_users

exit 0
