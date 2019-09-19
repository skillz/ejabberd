FROM alpine:3.9 AS base

# Create chat service user -- needed for both build and runtime
RUN addgroup ejabberd -g 9000 \
    && adduser -D -G ejabberd ejabberd -u 9000


FROM base AS builder

ARG CHATPATH=/tmp/ejabberd
ARG INSTALLPATH=/opt/chat-service
ARG MODULEPATH=$CHATBUILDPATH/.ejabberd-modules/

ENV MIX_ENV prod

# Create chat service user -- required for ejabberd install
# RUN addgroup ejabberd -g 9000 \
#     && adduser -D -G ejabberd ejabberd -u 9000

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

# Build iconv erlang/elixir library
WORKDIR /tmp/iconv
RUN wget https://github.com/processone/iconv/archive/1.0.10.tar.gz \
    && tar xvzf 1.0.10.tar.gz
WORKDIR /tmp/iconv/iconv-1.0.10
RUN rebar3 compile
WORKDIR /tmp/iconv/iconv-1.0.10/_build/default/lib/
RUN mkdir /usr/lib/erlang/lib/iconv-1.0.10 \
    && cp -Lr iconv/ebin iconv/priv iconv/src /usr/lib/erlang/lib/iconv-1.0.10
RUN mkdir /usr/lib/erlang/lib/p1_utils_iconv-1.0.10 \
    && cp -Lr p1_utils/LICENSE.txt p1_utils/README.md p1_utils/ebin p1_utils/include p1_utils/src /usr/lib/erlang/lib/p1_utils_iconv-1.0.10

# Build chat service
COPY . $CHATPATH
WORKDIR $CHATPATH
RUN ./autogen.sh
RUN ./configure \
    --prefix=/opt/chat-service \
    --enable-user=ejabberd \
    --enable-elixir \
    --enable-mysql
RUN make
RUN make install

# TODO Build chat service modules
RUN mix local.hex --force
RUN mix local.rebar --force


# Runtime container
FROM base AS runtime

# Create chat service user
# RUN addgroup ejabberd -g 9000 \
#     && adduser -D -G ejabberd ejabberd -u 9000

# Install required dependencies
RUN apk upgrade --update musl \
    && apk add build-base git zlib-dev openssl-dev yaml-dev expat-dev sqlite-dev \
        gd-dev jpeg-dev libpng-dev libwebp-dev autoconf automake bash \
        elixir erlang-dev erlang-crypto erlang-eunit erlang-mnesia erlang-erts erlang-hipe \
        erlang-tools erlang-os-mon erlang-syntax-tools erlang-parsetools \
        erlang-runtime-tools erlang-reltool file curl wget \
    && rm -rf /var/cache/apk/*

# Install iconv erlang/elixir library
COPY --from=builder /usr/lib/erlang/lib/iconv-* /usr/lib/erlang/lib/
COPY --from=builder /usr/lib/erlang/lib/p1_utils_iconv-* /usr/lib/erlang/lib/

# Install chat service
WORKDIR /opt/chat-service
COPY --from=builder /opt/chat-service .
# RUN chmod 755 sbin/ejabberdctl

USER ejabberd
WORKDIR /home/ejabberd

EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["/opt/chat-service/sbin/ejabberdctl"]
CMD ["foreground"]
