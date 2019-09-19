FROM alpine:3.9

ARG CHATPATH=/tmp/ejabberd
ARG CHATBUILDPATH=$CHATPATH/build
ARG INSTALLPATH=/opt/ejabberd
ARG MODULEPATH=$CHATBUILDPATH/.ejabberd-modules/

ENV MIX_ENV prod

# Install required dependencies
RUN apk upgrade --update musl \
    && apk add build-base git zlib-dev openssl-dev yaml-dev expat-dev sqlite-dev \
        gd-dev jpeg-dev libpng-dev libwebp-dev autoconf automake bash \
        elixir erlang-dev erlang-crypto erlang-eunit erlang-mnesia erlang-erts erlang-hipe \
        erlang-tools erlang-os-mon erlang-syntax-tools erlang-parsetools \
        erlang-runtime-tools erlang-reltool file curl wget \
    && rm -rf /var/cache/apk/*

 # Install rebar
WORKDIR /
RUN wget https://s3.amazonaws.com/rebar3/rebar3 -O /usr/bin/rebar3
RUN chmod +x /usr/bin/rebar3

# Install iconv
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

# Install chat service
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

EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["/opt/ejabberd/sbin/ejabberdctl"]
CMD ["foreground"]
