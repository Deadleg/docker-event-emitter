FROM ubuntu

RUN apt-get update && \
    apt-get install -y libgmp-dev netbase

COPY .stack-work/install/x86_64-linux/lts-6.10/7.10.3/bin/docker-event-emitter /usr/local/bin/docker-event-emitter

ENTRYPOINT ["docker-event-emitter"]
