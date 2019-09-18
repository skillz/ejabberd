FROM ubuntu:14.04

ARG CHATPATH=/tmp/ejabberd
ARG CHATBUILDPATH=$CHATPATH/build
ARG INSTALLPATH=/opt/ejabberd
ARG MODULEPATH=$CHATBUILDPATH/.ejabberd-modules/

ENV DEBIAN_FRONTEND=noninteractive
ENV MIX_ENV prod

# Install required dependencies
## Just updates
RUN apt-get update && apt-get upgrade -y
## Everything else
# RUN apt-get update && apt-get install git -y
# RUN apt-get update && apt-get install make -y
# RUN apt-get update && apt-get install automake -y
# RUN apt-get update && apt-get install autoconf -y
# RUN apt-get update && apt-get install g++ -y
# RUN apt-get update && apt-get install gcc -y
# RUN apt-get update && apt-get install build-essential -y
# RUN apt-get update && apt-get install expat -y
# RUN apt-get update && apt-get install libssl-dev -y
# RUN apt-get update && apt-get install libyaml-dev -y
# RUN apt-get update && apt-get install rebar -y
# RUN apt-get update && apt-get install zlib1g-dev -y
# RUN apt-get update && apt-get upgrade -y

WORKDIR /
RUN apt-get update && apt-get install expat g++ gcc libexpat-dev libpng-dev libssl-dev libyaml-dev zlib1g-dev -y

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
COPY skillz-ejabberd-dev_0.3.174_amd64.deb .
RUN dpkg -i skillz-ejabberd-dev_0.3.174_amd64.deb

# Move ejabberd to the final installation location
# WORKDIR $INSTALLPATH
# RUN cp -R $CHATBUILDPATH/* .
# RUN rm -rf $CHATPATH

# COPY docker/ejabberd.yml /etc/ejabberd/

EXPOSE 1883 4369-4399 5222 5269 5280 5443

# ENTRYPOINT ["/opt/ejabberd/sbin/ejabberdctl"]
# CMD ["foreground"]
