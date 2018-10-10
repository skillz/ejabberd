FROM phusion/baseimage:0.9.22

# Use baseimage-docker's init system.
CMD ["/sbin/my_init"]

# install dependencies
RUN apt-get update \
    && apt-get install -y \
    make \
    gcc \
    expat \
    libyaml-dev \
    erlang \
    erlang-p1-mysql \
    openssl \
    elixir \
    zlib1g-dev \
    libpng-dev \
    libexpat-dev \
    git \
    automake \
    libssl-dev \
    g++

# install ejabberd
ARG EJABBERD_HOME=/root/ejabberd
ARG MY_EJABBERD_HOME=/root/my-ejabberd

RUN mkdir $EJABBERD_HOME
RUN git clone https://github.com/skillz/ejabberd.git --branch development $EJABBERD_HOME
COPY ejabberd.yml $EJABBERD_HOME/config/ejabberd.yml
RUN cd $EJABBERD_HOME && ./autogen.sh
RUN cd $EJABBERD_HOME && ./configure --prefix=$MY_EJABBERD_HOME --enable-elixir
RUN cd $EJABBERD_HOME && make && make install
COPY ejabberd.yml $MY_EJABBERD_HOME/etc/ejabberd/ejabberd.yml

# wrapper to start ejabberd
ADD ./run.sh /sbin/run
COPY 100_create_default_users.sh $EJABBERD_HOME
ENTRYPOINT ["run"]

# Clean up APT.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

