# Multi-stage build for ejabberd
FROM elixir:1.19.5-otp-29-alpine as builder

RUN apk add --no-cache \
    git \
    build-base \
    libstdc++ \
    libyaml-dev \
    openssl-dev \
    expat-dev

WORKDIR /build

COPY . .

RUN ./autogen.sh && \
    ./configure --enable-user=ejabberd && \
    make install 2>&1 | tail -100

# Runtime stage
FROM alpine:latest

RUN apk add --no-cache \
    erlang-runtime \
    openssl \
    libyaml

# Copy built ejabberd from builder
COPY --from=builder /usr/local/lib/ejabberd /usr/local/lib/ejabberd
COPY --from=builder /usr/local/bin/ejabberd* /usr/local/bin/
COPY --from=builder /usr/local/sbin/ejabberd* /usr/local/sbin/
COPY --from=builder /usr/local/etc/ejabberd /usr/local/etc/ejabberd

RUN adduser -D -h /var/lib/ejabberd ejabberd && \
    chown -R ejabberd:ejabberd /var/lib/ejabberd /usr/local/etc/ejabberd

USER ejabberd

EXPOSE 5222 5269 5280 5290

ENTRYPOINT ["/usr/local/sbin/ejabberdctl"]
CMD ["foreground"]
