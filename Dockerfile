FROM elixir:1.8-otp-22-alpine AS alpine
RUN addgroup ejabberd -g 9000 \
    && adduser -D -G ejabberd ejabberd -u 9000


FROM alpine AS builder

ARG REBAR_VERSION=2.6.4
ARG ICONV_VERSION=1.0.10
ENV MIX_ENV dev

# Install required dependencies from package manager
RUN apk upgrade --update musl \
    && apk add \
        build-base \
        git \
        zlib-dev \
        openssl-dev \
        yaml-dev \
        expat-dev \
        autoconf \
        automake \
    && rm -rf /var/cache/apk/*

# Install rebar (now deprecated in favor of rebar3)
# iconv compilation can use rebar3 but modules explicitly use rebar
WORKDIR /tmp/rebar
RUN wget https://github.com/rebar/rebar/wiki/rebar -O /usr/bin/rebar \
    && chmod +x /usr/bin/rebar

# Build iconv erlang/elixir library
WORKDIR /tmp/iconv
RUN wget https://github.com/processone/iconv/archive/${ICONV_VERSION}.tar.gz \
    && tar xvzf ${ICONV_VERSION}.tar.gz
WORKDIR /tmp/iconv/iconv-${ICONV_VERSION}
RUN rebar3 compile
WORKDIR /tmp/iconv/iconv-${ICONV_VERSION}/_build/default/lib/
RUN mkdir /usr/local/lib/erlang/lib/iconv-${ICONV_VERSION} \
    && cp -Lr iconv/ebin iconv/priv iconv/src /usr/local/lib/erlang/lib/iconv-${ICONV_VERSION} \
    && mkdir /usr/local/lib/erlang/lib/p1_utils_iconv-${ICONV_VERSION} \
    && cp -Lr p1_utils/LICENSE.txt p1_utils/README.md p1_utils/ebin p1_utils/include p1_utils/src /usr/local/lib/erlang/lib/p1_utils_iconv-${ICONV_VERSION}

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

# Build chat service modules
RUN mkdir -p /opt/chat-service/.ejabberd-modules/sources
RUN mix local.hex --force \
    && mix local.rebar --force

# Module push skillz
WORKDIR /opt/chat-service/.ejabberd-modules/sources
COPY .ejabberd_modules/sources/mod_push_skillz mod_push_skillz
WORKDIR /opt/chat-service/.ejabberd-modules/sources/mod_push_skillz
RUN mix deps.get \
    && mix module_install ModPushSkillz

# Module pottymouth
WORKDIR /opt/chat-service/.ejabberd-modules/sources
COPY .ejabberd_modules/sources/mod_pottymouth mod_pottymouth
WORKDIR /opt/chat-service/.ejabberd-modules/sources/mod_pottymouth
RUN mix deps.get \
    && mix module_install ModPottymouth

# Module beam stats
WORKDIR /opt/chat-service/.ejabberd-modules/sources
COPY .ejabberd_modules/sources/mod_beam_stats mod_beam_stats
WORKDIR /opt/chat-service/.ejabberd-modules/sources/mod_beam_stats
RUN mix deps.get \
    && mix module_install ModBeamStats

# Clean up chat service module sources
RUN rm -rf /opt/chat-service/.ejabberd-modules/sources


FROM alpine AS runtime

# Install required dependencies
RUN apk upgrade --update musl \
 && apk add \
    expat \
    libstdc++ \
    openssl \
    yaml \
    zlib \
 && rm -rf /var/cache/apk/*

# Install iconv erlang/elixir library
COPY --from=builder /usr/local/lib/erlang/lib/iconv-* /usr/local/lib/erlang/lib/
COPY --from=builder /usr/local/lib/erlang/lib/p1_utils_iconv-* /usr/local/lib/erlang/lib/

# Install chat service
WORKDIR /opt/chat-service
COPY --from=builder /opt/chat-service .
COPY --from=builder /tmp/chat-service/scripts scripts

USER ejabberd
WORKDIR /home/ejabberd

ENV EJABBERD_HOME /opt/chat-service
ENV EJABBERDCTL /opt/chat-service/sbin/ejabberdctl

EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["/opt/chat-service/scripts/run.sh"]
