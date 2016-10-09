FROM ubuntu

RUN apt-get update && \
    apt-get install -y libgmp-dev netbase

COPY docker-event-emitter /usr/local/bin/docker-event-emitter

ENTRYPOINT ["docker-event-emitter"]
