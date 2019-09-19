FROM alpine:3.9 AS alpine
RUN addgroup ejabberd -g 9000 \
    && adduser -D -G ejabberd ejabberd -u 9000


FROM alpine AS builder

ARG ICONV_VERSION=1.0.10
ENV MIX_ENV prod

# Install required dependencies from package manager
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
RUN wget https://github.com/processone/iconv/archive/${ICONV_VERSION}.tar.gz \
    && tar xvzf ${ICONV_VERSION}.tar.gz
WORKDIR /tmp/iconv/iconv-${ICONV_VERSION}
RUN rebar3 compile
WORKDIR /tmp/iconv/iconv-${ICONV_VERSION}/_build/default/lib/
RUN mkdir /usr/lib/erlang/lib/iconv-${ICONV_VERSION} \
    && cp -Lr iconv/ebin iconv/priv iconv/src /usr/lib/erlang/lib/iconv-${ICONV_VERSION} \
    && mkdir /usr/lib/erlang/lib/p1_utils_iconv-${ICONV_VERSION} \
    && cp -Lr p1_utils/LICENSE.txt p1_utils/README.md p1_utils/ebin p1_utils/include p1_utils/src /usr/lib/erlang/lib/p1_utils_iconv-${ICONV_VERSION}

# Build chat service
COPY . /tmp/chat-service
WORKDIR /tmp/chat-service
RUN ./autogen.sh \
    && ./configure \
        --prefix=/opt/chat-service \
        --enable-user=ejabberd \
        --enable-elixir \
        --enable-mysql \
    && make && make install

# TODO Build chat service modules
RUN mix local.hex --force \
    && mix local.rebar --force


FROM alpine AS runtime

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

USER ejabberd
WORKDIR /home/ejabberd

EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["/opt/chat-service/sbin/ejabberdctl"]
CMD ["foreground"]
