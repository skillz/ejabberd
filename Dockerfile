FROM alpine:3.10
FROM alpine:3.9

ARG CHATPATH=/tmp/ejabberd
ARG CHATBUILDPATH=$CHATPATH/build
ARG INSTALLPATH=/opt/ejabberd
ARG MODULEPATH=$CHATBUILDPATH/.ejabberd-modules/

ENV MIX_ENV prod

# Install required dependencies
# RUN apk upgrade --update musl \
#  && apk add build-base git zlib-dev openssl-dev yaml-dev expat-dev sqlite-dev \
#             gd-dev jpeg-dev libpng-dev libwebp-dev autoconf automake bash \
#             elixir erlang-dev erlang-crypto erlang-eunit erlang-mnesia erlang-erts erlang-hipe \
#             erlang-tools erlang-os-mon erlang-syntax-tools erlang-parsetools \
#             erlang-runtime-tools erlang-reltool file curl \
#  && rm -rf /var/cache/apk/*

RUN apk upgrade --update musl \
 && apk add build-base git zlib-dev openssl-dev yaml-dev expat-dev sqlite-dev \
            gd-dev jpeg-dev libpng-dev libwebp-dev autoconf automake bash \
            elixir= erlang-dev file curl \
 && rm -rf /var/cache/apk/*

# Install ejabberd
COPY . $CHATPATH

WORKDIR $CHATPATH
RUN ./autogen.sh
RUN ./configure \
 --enable-elixir \
 --prefix=$CHATBUILDPATH \
 --enable-mysql
RUN make
RUN make install
RUN sed -i "s+$CHATBUILDPATH+$INSTALLPATH+g" $CHATBUILDPATH/sbin/ejabberdctl

# TODO Install modules
RUN mix local.hex --force
RUN mix local.rebar --force

# Move ejabberd to the final installation location
WORKDIR $INSTALLPATH
RUN cp -R $CHATBUILDPATH/* .
RUN rm -rf $CHATPATH

# COPY docker/ejabberd.yml /etc/ejabberd/

EXPOSE 1883 4369-4399 5222 5269 5280 5443

# ENTRYPOINT ["/opt/ejabberd/sbin/ejabberdctl"]
# CMD ["foreground"]
