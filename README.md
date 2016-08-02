# Docker Event Emitter

Subscribe to the `/events` Docker api and forward events to Redis or a RESTful endpoint.

On the container start event, container info from the `/container/{id}/json` api will be added to the event in the `docker.event.emitter.container` field. This is useful if you want to log container labels/ip addresses etc.

These is no guarantee that the events are emitted in the same order they are received, but in general order will be preserved. Container start events might be a bit delayed due to the call to `/container/{id}/json`.

# Usage

```
Docker Event Emitter - relay docker events to somewhere else

Usage: openresty-docker-exe (-b|--backend BACKEND) (-e|--endpoint ENDPOINT)
  Emit docker events to a subscriber such as Redis or a RESTful endpoint.

Available options:
  -h,--help                Show this help text
  -b,--backend BACKEND     Backend type: redis | web
  -e,--endpoint ENDPOINT   Redis: hostname:port | web: full url
```

Note that there are no defaults, you must specify what backend you want to use and the endpoint for it.

Currently only redis and web endpoints are supported.

## Redis

For every event, the JSON for that event is published to the `container:event` channel using Redis's inbuilt pub/sub mechanism.

## Web

For every event, the JSON is POSTed to the supplied endpoint.

# Dockerfile

To create a Docker image, just run

```
docker built -t {your tag}  .
```

Then run with 

```
docker run -v /var/run/docker.sock:/var/run/docker.sock --name dee {your tag} -b redis -e redis:6379
```

# Development

Cloning and running `stack build` should build everything with no problem.