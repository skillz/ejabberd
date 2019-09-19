FROM ubuntu:14.04

ARG CHATPATH=/tmp/ejabberd
ARG CHATBUILDPATH=$CHATPATH/build
ARG INSTALLPATH=/opt/ejabberd
ARG MODULEPATH=$CHATBUILDPATH/.ejabberd-modules/
ARG VERSION=0.3.174

ENV DEBIAN_FRONTEND=noninteractive
ENV MIX_ENV prod

# Install required dependencies
WORKDIR /
## Just updates
RUN apt-get update && apt-get upgrade -y
## Everything else
RUN apt-get update && apt-get install expat g++ gcc libexpat-dev libpng-dev libssl-dev libyaml-dev zlib1g-dev -y

# Install erlang + elixir
WORKDIR /tmp/erlang
RUN apt-get update && apt-get install wget -y
RUN apt-get update && apt-get install gnupg -y
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN dpkg -i erlang-solutions_1.0_all.deb
RUN wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
RUN apt-key add erlang_solutions.asc
RUN apt-get update && apt-get install esl-erlang -y
RUN apt-get install elixir -y

# Install ejabberd
WORKDIR $CHATBUILDPATH
COPY skillz-ejabberd-dev_${VERSION}_amd64.deb .
RUN dpkg -i skillz-ejabberd-dev_0.3.174_amd64.deb

# Install rebar
WORKDIR /
RUN wget https://s3.amazonaws.com/rebar3/rebar3 -O /usr/bin/rebar3
RUN chmod +x /usr/bin/rebar3

# Install iconv library
WORKDIR /tmp/iconv
RUN wget https://github.com/processone/iconv/archive/1.0.10.tar.gz
RUN tar xvzf 1.0.10.tar.gz
WORKDIR /tmp/iconv/iconv-1.0.10
RUN rebar3 compile
WORKDIR /tmp/iconv/iconv-1.0.10/_build/default/lib/
RUN ls -la iconv
RUN mkdir /usr/lib/erlang/lib/iconv-1.0.10
RUN cp -Lr iconv/ebin iconv/priv iconv/src /usr/lib/erlang/lib/iconv-1.0.10
RUN mkdir /usr/lib/erlang/lib/p1_utils_iconv-1.0.10
RUN cp -Lr p1_utils/LICENSE.txt p1_utils/README.md p1_utils/ebin p1_utils/include p1_utils/src /usr/lib/erlang/lib/p1_utils_iconv-1.0.10

# Set up ejabberd user
RUN useradd -s /bin/bash -M -d /opt/ejabberd-${VERSION} ejabberd
RUN chown -R ejabberd /opt/ejabberd-${VERSION}

USER ejabberd
WORKDIR /opt/ejabberd-${VERSION}

ENV EJABBERD_HOME /opt/ejabberd-${VERSION}
ENV CONTRIB_MODULES_PATH /opt/ejabberd-${VERSION}/.ejabberd_modules

ENV LC_ALL=C.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    VERSION=${VERSION:-latest}

EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["./sbin/ejabberdctl"]
CMD ["foreground"]
