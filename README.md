# Ejabberd Skillz Fork

- See [ejabberd original](https://github.com/processone/ejabberd) for information about the original; it's recommended that you use it instead

## Staging/Produciton troubleshooting guide

[Here is a link to the troubleshooting guide!](https://github.com/skillz/chat-administration-service/wiki/Ejabberd-Troubleshooting-Guide)

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

### Differences between Fork and Original
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

