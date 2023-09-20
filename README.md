# Ejabberd Skillz Fork

- [Fork Source](https://github.com/processone/ejabberd)

- [On Call Documentation](https://docs.google.com/document/d/1kFnZfS4uzFCheDs49AhOgYdvkQxUPT1M-oUlZsJFxLw)

## Libraries and Setup

```shell
xcode-select --install ## for mac

brew install libyaml # ejabberd config dependency
brew install automake # compiling dependency 
brew install expat # ejabberd xml parsing dependency
brew install pidgin # XMPP chat client used for testing

# pin version 2.69 bc it seems to be the highest compatible version for :fast_tls, used by ejabberd
brew install openssl@1.1
# only need this if you had openssl installed previously, but it doesn't hurt to run it
brew link --overwrite openssl@1.1

# for mac: 
# create symlink from openssl 1.1 to include folder, which will help the compiler find the files it wants for :fast_tls
ln -s /usr/local/Cellar/openssl@1.1/1.1.1l/include/openssl /usr/local/include/openssl

# pin version 2.69 bc it seems to be the highest compatible version for erlang 22
brew install autoconf@2.69 # compiling dependency
# only need this if you had autoconf installed previously, but it doesn't hurt to run it
brew link --overwrite autoconf@2.69

# install and start mysql, the backend used by our ejabberd nodes
brew install mysql
brew install --cask mysqlworkbench
brew services start mysql

# asdf will let us install erlang versions and pre-compiled elixir versions with compatible OTP
brew install asdf

asdf plugin add erlang
asdf plugin add elixir

# install erlang and compatible elixir
asdf install erlang 22.1.7
asdf install elixir 1.12.3-otp-22

# mark both of those versions as globally available
asdf global elixir 1.12.3-otp-22
asdf global erlang 22.1.7

# add asdf to PATH
# NOTE: you only need to run this if asdf is not already on your PATH
# eventually, you will want to also permanently add the below to your path
# the steps to do this can be different on mac vs linux
export PATH="$PATH:~/.asdf/shims:~/.asdf/bin"

# create mysql ejabberd database and user
mysqladmin -u root password 'some secretpassword'
mysql -u root -p # might need sudo here
## in mysql prompt:
# create database ejabberd;
# create user 'ejabberd'@'localhost' identified by 'ejabberd';
# grant all privileges on *.* to 'ejabberd'@'localhost' with grant option;
# flush privileges;
# \q
```

## Quick Start
- This is a quick start to get a local ejabberd server running using whatever local code you have checked out / are iterating on.
- You can start this local server to test out code changes quickly and can even connect open-source chat clients to it for local testing.

- Starting local server:

```shell
# install libraries from above
git clone git@github.com:skillz/ejabberd.git
cd ejabberd
mix deps.clean --all # clean any previous dependency compilations for a fresh start
mix deps.get # get dependencies
mix deps.compile # copile dependencies
iex -S mix # compile and start ejabberd; inject terminal into a live elixir ejabberd session
# you now have a local ejabberd server running!
# and your terminal is now inside a live elixir session with ejabberd loaded
# you can issue elixir commands against ejabberd code, view responses, etc
# eg: :mod_muc.get_online_rooms("conference.localhost")
# ctrl-c twice to exit and stop the local server
```

- Connecting a chat client (Pidgin):
  - Open Pidgin
  - On Basic Tab:
    - Protocol: XMPP
    - Username: testuser (or whatever you want)
    - Domain: localhost
    - Resource: conference
    - Password: 1234 (or whatever you want)
    - Check Remember Password
  - On Advanced Tab:
    - Connection Security: Use encryption if available
    - Check Allow Plaintext Auth Over Unencrypted Streams
    - Connect port: 5222
  - Save
- In Pidgin, you can now create rooms on your local ejabberd server, create more users and test out DMs and groupchats, etc.

## General Outline of Skillz Chat Services
- Ejabberd is the chat server for Skillz. Ejabberd is a clustered set of erlang nodes that uses mnesia and MySQL as backends.
- All user chats (DMs, game-wide chat rooms) exist in ejabberd and are facilitated by Ejabberd.
- A chat-administration-service (CAS) exists as a separate service that performs certain tasks in ejabberd for us.
  - Most importantly, CAS serves chat settings to the SDK, which includes information about how the client can connect to ejabberd.
    - And when CAS gets a settings call for a room, it will create the room in ejabberd if it does not exist already.
    - CAS will also add the `skillz-cas` user to the room if the user is not already present.
    - The `skillz-cas` user is used to send admin messages to users and also to fetch recent messages for the SDK so it must be present in every game-wide room.
  - CAS is used to fetch recent messages in each game-wide room for the SDK (get_room_summary). 
  - CAS runs a job to remove unused rooms on a schedule.
- Two separate projects exist outside of ejabberd, but are added to ejabberd in the build process
  - mod_push_skillz
    - this is an elixir project added to ejabberd at build time, which handles push notifications to users who have received DMs or @-mentions in a game-wide room.
  - mod_pottymouth
    - this is an elixir project added to ejabberd at build time, which handles foul language masking and the auto-moderation feature.
- Ejabberd is configured [here](https://github.com/skillz/helm-charts/blob/master/charts/ejabberd/templates/ejabberd-config-configmap.yaml)
  - the file above creates several yaml files that are used in one [ejabberd.yaml file](https://github.com/skillz/helm-charts/blob/master/charts/ejabberd/ejabberd-config/ejabberd.yaml)
  - with api permissions configured [here](https://github.com/skillz/helm-charts/blob/master/charts/ejabberd/ejabberd-config/api-permissions.yaml)

## Core Skillz Modules and Features
- auto-moderator feature, most of which resides in the mod_pottymouth project
  - this module is outside of this codebase and is copied into ejabberd at compile time via Docker
- push notifications feature, which resides in the mod_push_skillz project
  - this module is outside of this codebase and is copied into ejabberd at compile time via Docker
- ability to leverage read replicas in ejabberd_sql_sup and ejabberd_sql
- skillz-util module
- mod_block_incoming module
  - For a list of users (moderators), auto block friend requests so they are not spammed
- mod_block_nonfriends module
  - Do not allow non-friends to send messages to one-another
- request multiple vcards at once feature in mod_vcard
- ban/mute (set_user_affiliation) api endpoint in mod_muc_admin and mod_muc_room
- get_room_summary api endpoint in mod_muc_admin and mod_muc_room
- mnesia config optimization in ejabberdctl.template

## FAQ
- How to make a new http api?
  - http apis are created (usually in *_admin modules) using the `ejabberd_commands` module. See mod_muc_admin module for details.
  - mainly, `ejabberd_commands` accepts a function present and exported in the same module, a list of typed args, and a result shape.
  - there are no optional args, but in your http call, `&arg_name=` will send in an empty value, eg `<<>>` for binary args.
  - also, for GET requests, all params come in as binary so just mark them that way and parse / convert in your function.
  - note: `ejabberd_commands` will create both GET and POST apis for you, and you can call either.
    - for GETs just use the arg names as request params.
    - for POSTs, use a json object with arg names as properties, and use a content-type header of `application/json`.
  - the result shape is the most difficult one to get right.
    - take a look at mod_muc_admin ejabberd command `get_room_summary` for a good example. The response will look like `[ {id: string, from: string, body: string, userRole: integer}, ... ]`.
  - you will also need to allow access to the api by adding it to the `what` section of `api-permissions` [here](https://github.com/skillz/helm-charts/blob/master/charts/ejabberd/ejabberd-config/api-permissions.yaml).
- How to set ejabberd configs?
  - ejabberd configs live in helm: [Ejabberd Configs](https://github.com/skillz/helm-charts/blob/master/charts/ejabberd/templates/ejabberd-config-configmap.yaml).
  - see the modules.yaml section for ways to configure specific modules.
  - the various yaml files the config above creates are consolidated into one config [here](https://github.com/skillz/helm-charts/blob/master/charts/ejabberd/ejabberd-config/ejabberd.yaml).
  - note: you must increment the helm Chart version for ejabberd in tandem with any changes you make.
- How to set erlang or mnesia configs?
  - erlang (and mnesia) options are specified in the file: `./ejabberdctl.template` in this project.
  - look for `ERLANG_OPTS=` to see where they are defined
  - an example are recent mnesia tuning configs we added: `-mnesia dump_log_write_threshold 10000 -mnesia dc_dump_limit 40`
- How to upgrade ejabberd using fork source?
    - find a safe commit to use for auto-accepting the source repo's code on collisions
      - look for a file that has not been edited since the last upgrade (most files should be this way)
      - find the latest commit hash using `git log` or git history on the file
      - if the latest commit was for a previous upgrade, then this is a safe commit
        - ie, when files have this commit hash as their latest commit, we can auto-accept the source repo's changes since we are losing no custom code on collision
    - add the source git as a remote
      - git remote add upstream https://github.com/processone/ejabberd.git
      - git fetch upstream
    - make an upgrade branch off of the fork master
      - git checkout master
      - git checkout -b master-upgrade
    - merge the source master into the new branch
      - git merge upstream/master
    - automatically merge files from `theirs` if the last commit in the file is X
      - in the example below, X is bcdd2e947ad02094b4fc2f00d3250e15b3dc5fc2
      - `grep -rw . -e '<<<<<<< HEAD' -l | xargs -I$ echo "git log $ | head -1 | grep bcdd2e947ad02094b4fc2f00d3250e15b3dc5fc2 | xargs -I% -r echo 'git checkout --theirs -- $ && echo \"Merged: $\"' | bash" | bash`


## Build

### Mac instructions
- For `xcode-select --install` agree to the prompt, that pops-up after running the command
- For the below `ln -s` command please use whatever version of openssl that was installed by brew or previously on your system for part of the path i.e. `1.0.2k`
```shell
xcode-select --install
brew install openssl
ln -s /usr/local/Cellar/openssl/1.0.2k/include/openssl /usr/local/include/openssl
brew install libyaml
brew install erlang
brew install autoconf
brew install automake
brew install expat
brew install elixir

git clone git@github.com:skillz/ejabberd.git
cd ejabberd/
chmod +x autogen.sh
./build.sh

cd $HOME/my-ejabberd/
```

See [here](https://github.com/skillz/chat-administration-service/wiki/Local-Ejabberd-Development) For instructions on local development!

### Ubuntu install

```shell
sudo apt install make
sudo apt install gcc
sudo apt install expat
sudo apt install libyaml-dev
sudo apt install erlang
sudo apt install openssl
sudo apt install elixir
sudo apt install zlib1g-dev
sudo apt install libpng-dev
sudo apt install libexpat-dev
sudo apt -y install git
sudo apt -y install automake
sudo apt -y install openssl-devel
sudo apt -y install libssl-dev
sudo apt -y install g++

git clone git@github.com:skillz/ejabberd.git
cd ejabberd/
./autogen.sh
./configure --enable-elixir
make && sudo make install
```

- copy https://github.com/skillz/docker-compose/blob/master/ts/ejabberd-conf/ejabberd.yml into `~/my-ejabberd/etc/ejabberd`
- You can also make `$HOME/.ejabberd-modules`, if it doesn't exist, for later module development.
- `make && make install` should be run to re-build any local changes then restart ejabberd with `./ejabberdctl restart` or `./ejabberdctl stop` then `./ejabberdctl start` (ejabberdctl is in the sbin directory of your installation on mac and in the bin directory on ubuntu).

## Differences between Fork and Original
- In `/src/mod_muc_room.erl` the flow for an incoming muc subscription message is changed.
  -  Now when a user is online with a muc subscription to a room and a message is sent in that room, the user with the subscription will not get the message directly and the offline message flow will occur (in the original, the user would get the message directly sent to them).
- At line 4036 the `is_privacy_allow` method was added and from lines 4059 until the end of the `send_wrapped` method; the flow was modified.
- We removed muc rooms from being loaded into memory upon startup.  See [this](https://github.com/skillz/ejabberd/pull/25) PR.
- When a room does not exist during a join room attempt, it is created.  See [this](https://github.com/skillz/ejabberd/pull/50/files) PR.
- When a room is created the archive table is queried for the latest 20 messages from that room.  See [this](https://github.com/skillz/ejabberd/pull/58) PR.
- Admins can now re-create rooms.  See [this](https://github.com/skillz/ejabberd/pull/33) PR.
- A new SQL table is created for subscriptions.  Subscriptions used to only reside in the muc objects, and when they were removed we lost subscriptions.  See [this](https://github.com/skillz/ejabberd/pull/56) PR.
- When all occupants leave a muc room, it is now removed from memory.  See [this](https://github.com/skillz/ejabberd/pull/57) PR.
- Dockerfile refers to private modules that must exist in `.ejabberd_modules`

## Staging/Production troubleshooting guide

[Here is a link to the troubleshooting guide!](https://github.com/skillz/chat-administration-service/wiki/Ejabberd-Troubleshooting-Guide)


