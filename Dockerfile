# Dorkerfile --- test alpha-debian

# meant to be run from repository root

FROM debian:stable
WORKDIR /app
COPY . /app

CMD sh systems/alpha/scripts/test.sh
